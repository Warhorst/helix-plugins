(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")

(provide tree-toggle)

;;@doc
;; Is the file tree currently open?
(define *tree-open?* #f)
;;@doc
;; Is the file tre currently focused?
(define *tree-focused?* #f)
;;@doc
;; List which contains all currently shown files in the file tree.
;; Each entry is another list which consists of the following elements:
;; path: The path to the file
;; indent: The indent, which is a string of spaces. It defines how much a file is pushed to the right, creating the tree look
;; marker: If the path is a dir, this is an arrow indicating the dir is open or closed. For a file, this is an empty string.
;; name: The file name of the file (basically the last part of the path)
(define *tree* '())
;;@doc
;; All files in the file tree. Used for searching
(define *tree-all-files* '())
;;@doc
;; Hashmap which stores all directories in the working directory and their open state (as true / false).
;; TODO The forest implementation of the tree seems to use false as "open" and "true" as closed
;; TODO I think this could be a simple set
(define *open-directories* (hashset))
;;@doc
;; The current widht of the file tree
(define *tree-width* 32) ;
;;@doc
;; The min widht of the file tree
(define *tree-min-width* 16)
;;@doc
;; The max widht of the file tree
(define *tree-max-width* 60)
;;@doc
;; The position of the cursor in the file tree (the currently selected row)
(define *tree-cursor* 0)
;;@doc
;; TODO needs doc
(define *window-start* 30)
;;@doc
;; TODO needs doc
(define *visible-height* 30)
;;@doc
;; The name of the file tree ui component
(define *tree-component-name* "file-tree")


;;@doc
;; Toggle the file tree.
;; - If the tree is not open, open and focus it
;; - If the tree is open but not focused, focus it
;; - If the tree is open and focused, close it
(define (tree-toggle)
  (cond
    [(not *tree-open?*)
      (set! *tree-open?* #t)
      (set! *tree-focused?* #t)
      (set! *open-directories* (hashset-insert *open-directories* (helix-find-workspace)))
      (build-tree!)
      ;; Add the file tree to the render stack
      (push-component! (make-tree-component))
    ]

    [(not *tree-focused?*)
      (set! *tree-focused?* #t)
    ]

    [else
      (set! *tree-open?* #f)
      (set! *tree-focused?* #f)

      ;;Remove the file tree from the render stack
      (pop-last-component-by-name! *tree-component-name*)

      ;; Reset the editor clip
      ;; Wrapping in this callback is required for some reason, or it will not be applied
      (enqueue-thread-local-callback
        (lambda () (set-editor-clip-left! 0))
      )
    ]
  )
)

(define (scan-files!)
  (define root (helix-find-workspace))
  (define root-prefix (string-append root (path-separator)))
  (define acc '())

  (define (walk dir)
    (for-each
      (lambda (p)
        (define name (file-name p))
        (if (is-dir? p)
          (walk p)
          (set! acc (cons p acc))
        )
     )
     (with-handler (lambda (_) '()) (read-dir dir)))
    )
  (walk root)

  (set!
    *tree-all-files*
    (sort
      (map
        (lambda (p) (substring p (string-length root-prefix) (string-length p)))
        acc
      )
      string<?
    )
  )
)

(define (build-tree!)
  (define result '())

  (define (walk path depth)
    (define name (file-name path))
    (define indent (repeat-str "  " depth))
    (define marker
      (if (is-dir? path)
        (dir-marker path)
        "  "
      )
    )

    (set! result (cons (list path indent marker name) result))

    (when (and (is-dir? path) (hashset-contains? *open-directories* path))
      ;; If the directory is currently open, add its children to the tree data
      (for-each
        (lambda (child) (walk child (+ depth 1)))
        (sort-path-entries (read-dir path))
      )
    )
  )

  (walk (helix-find-workspace) 0)
  (set! *tree* (reverse result))
)

(define (repeat-str s n)
  (if (<= n 0)
    ""
    (string-append s (repeat-str s (- n 1))))
)

(define (dir-marker path)
  (if (hashset-contains? *open-directories* path)
      "▼ "
      "▶ "
  )
)

;; dirs before files, alphabetic oder
(define (sort-path-entries lst)
  (define dirs (sort (filter is-dir? lst) string<?))
  (define files (sort (filter (lambda (p) (not (is-dir? p))) lst) string<?))
  (append dirs files)
)

(struct TreeState ())

(define (make-tree-component)
  (new-component!
    *tree-component-name*
    (TreeState)
    render-tree
    (hash "handle_event" handle-event)
  )
)

(define (render-tree state rect frame)
  (define x0 0)
  (define y0 0)
  (define width *tree-width*)
  (define height (area-height rect))

  (define text-style (theme-scope-ref "ui.text"))
  (define background-style (theme-scope-ref "ui.background"))
  (define highlight-style (theme-scope-ref "ui.menu.selected"))
  (define border-style (if *tree-focused?* text-style background-style))

  (define panel-area (area x0 y0 width height))

  (set-editor-clip-left! width)

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame panel-area background-style)

  (block/render frame panel-area (make-block background-style border-style "all" "double"))

  (define search-area (area x0 y0 width 3))
  (block/render frame search-area (make-block background-style border-style "all" "double"))

  (define list-y0 (+ y0 3))

  (define tree-x 1)

  ;; TODO i need to determine the files like the forst plugin does
  (let loop ([items *tree*] [row 0])
    (unless (or (null? items) (>= row *visible-height*))
      ;; Get the current element of the list and extract its parameters
      (define entry (car items))
      (define path (list-ref entry 0))
      (define indent (list-ref entry 1))
      (define marker (list-ref entry 2))
      (define name (list-ref entry 3))

      (define abs-idx (+ *window-start* row))
      (define prefix (string-append indent marker))
      (define dir? (is-dir? path))
      (define y (+ list-y0 row))
      (define prefix-w (string-length prefix))

      (define icon (if dir? (dir-icon name) (icon name)))
      (define icon-color (if dir? (dir-icon-color name) (icon-color name)))
      (define highlighted? (= abs-idx *tree-cursor*))
      (define row-style (if highlighted? highlight-style text-style))

      (frame-set-string! frame tree-x y prefix row-style)
      (frame-set-string! frame (+ tree-x prefix-w) y icon (style-with-fg-color row-style "#000000"))
      ;; TODO the name needs truncation, or it will be rendered outside of the tree panel
      (frame-set-string! frame (+ tree-x prefix-w 2) y name row-style)

      (loop (cdr items) (+ row 1))
    )
  )
)

(define (handle-event state event)
  ;; makes the editor receive events while the panel is unfocused
  event-result/ignore
)

;;@doc
;; Map of file extension to their icons and colors
(define *extensions*
  (hash
    "7z" (cons "󰗄" "#eca517")
    "aac" (cons "󰈣" "#00afff")
    "ai" (cons "" "#cbcb41")
    "aif" (cons "󰈣" "#00afff")
    "applescript" (cons "󰀵" "#6d8085")
    "ass" (cons "󰨖" "#ffb713")
    "astro" (cons "" "#e23f67")
    "awk" (cons "" "#4d5a5e")
    "bat" (cons "󰯂" "#C1F12E")
    "bazel" (cons "" "#89e051")
    "bib" (cons "󱉟" "#cbcb41")
    "bicep" (cons "" "#519aba")
    "bicepparam" (cons "" "#9f74b3")
    "blp" (cons "󰠡" "#5796E2")
    "bmp" (cons "󰈟" "#a074c4")
    "bz" (cons "󰗄" "#eca517")
    "bz2" (cons "󰗄" "#eca517")
    "bz3" (cons "󰗄" "#eca517")
    "bzl" (cons "" "#89e051")
    "c" (cons "󰙱" "#599eff")
    "cast" (cons "󰈫" "#FD971F")
    "cbl" (cons "󱌼" "#005ca5")
    "ccm" (cons "󰙲" "#f34b7d")
    "cjs" (cons "󰌞" "#cbcb41")
    "clj" (cons "" "#8dc149")
    "cljc" (cons "" "#8dc149")
    "cljs" (cons "" "#519aba")
    "cmake" (cons "󱁤" "#6d8086")
    "cob" (cons "󱌼" "#005ca5")
    "cpp" (cons "󰙲" "#519aba")
    "cppm" (cons "󰙲" "#519aba")
    "cr" (cons "" "#c8c8c8")
    "cs" (cons "󰌛" "#596706")
    "csproj" (cons "󰗀" "#512bd4")
    "css" (cons "󰌜" "#42a5f5")
    "csv" (cons "" "#89e051")
    "cts" (cons "󰛦" "#519aba")
    "cu" (cons "" "#89e051")
    "cue" (cons "󰝚" "#ed95ae")
    "cuh" (cons "" "#a074c4")
    "cxx" (cons "󰙲" "#519aba")
    "cxxm" (cons "󰙲" "#519aba")
    "dart" (cons "" "#03589C")
    "desktop" (cons "󰍹" "#563d7c")
    "diff" (cons "󰦓" "#41535b")
    "doc" (cons "󱎒" "#185abd")
    "docx" (cons "󱎒" "#185abd")
    "dot" (cons "󱎒" "#30638e")
    "eex" (cons "" "#a074c4")
    "el" (cons "" "#8172be")
    "elm" (cons "" "#519aba")
    "epp" (cons "" "#FFA61A")
    "erb" (cons "󰴭" "#701516")
    "erl" (cons "" "#B83998")
    "exe" (cons "󰖳" "#9F0500")
    "exs" (cons "" "#a074c4")
    "f90" (cons "󱈚" "#734f96")
    "fish" (cons "" "#4d5a5e")
    "flac" (cons "󰈣" "#0075aa")
    "fnl" (cons "" "#fff3d7")
    "fsi" (cons "" "#519aba")
    "fsx" (cons "" "#519aba")
    "gd" (cons "" "#6d8086")
    "gemspec" (cons "󰴭" "#701516")
    "gif" (cons "󰵸" "#a074c4")
    "go" (cons "󰟓" "#519aba")
    "gql" (cons "󰡷" "#e535ab")
    "graphql" (cons "󰡷" "#e535ab")
    "gv" (cons "󱁉" "#30638e")
    "gz" (cons "󰗄" "#eca517")
    "h" (cons "󰫵" "#a074c4")
    "haml" (cons "󰅴" "#eaeae1")
    "hbs" (cons "󰌞" "#f0772b")
    "heex" (cons "" "#a074c4")
    "hex" (cons "󰋘" "#2e63ff")
    "hh" (cons "󰙲" "#a074c4")
    "hpp" (cons "󰙲" "#a074c4")
    "hrl" (cons "" "#B83998")
    "hs" (cons "󰲒" "#a074c4")
    "html" (cons "󰌝" "#e44d26")
    "hurl" (cons "󰫵" "#ff0288")
    "hx" (cons "󰫵" "#ea8220")
    "hxx" (cons "󰙲" "#a074c4")
    "ini" (cons "󰯂" "#6d8086")
    "ino" (cons "" "#56b6c2")
    "ipynb" (cons "󰠮" "#51a0cf")
    "ixx" (cons "󰙲" "#519aba")
    "java" (cons "󰬷" "#cc3e44")
    "jl" (cons "" "#a270ba")
    "jpeg" (cons "󰈥" "#a074c4")
    "jpg" (cons "󰈥" "#a074c4")
    "js" (cons "󰌞" "#cbcb41")
    "json" (cons "󰘦" "#cbcb41")
    "json5" (cons "󰘦" "#cbcb41")
    "jsonc" (cons "󰘦" "#cbcb41")
    "jsx" (cons "" "#20c2e3")
    "kt" (cons "󱈙" "#7F52FF")
    "kts" (cons "󱈙" "#7F52FF")
    "leex" (cons "" "#a074c4")
    "less" (cons "󰌜" "#563d7c")
    "lhs" (cons "" "#a074c4")
    "lib" (cons "󰫳" "#4d2c0b")
    "liquid" (cons "" "#95BF47")
    "lrc" (cons "󰫹" "#ffb713")
    "lua" (cons "󰢱" "#51a0cf")
    "luau" (cons "󰢱" "#00a2ff")
    "m3u" (cons "󰲸" "#ed95ae")
    "m3u8" (cons "󰲸" "#ed95ae")
    "m4a" (cons "󰈣" "#00afff")
    "m4v" (cons "󰈫" "#FD971F")
    "md" (cons "󰍔" "#dddddd")
    "mjs" (cons "󰌞" "#f1e05a")
    "mkv" (cons "󰈫" "#FD971F")
    "ml" (cons "" "#e37933")
    "mli" (cons "" "#e37933")
    "mo" (cons "󰫴" "#9772FB")
    "mov" (cons "󰈫" "#FD971F")
    "mp3" (cons "󰈣" "#00afff")
    "mp4" (cons "󰈫" "#FD971F")
    "mpp" (cons "󰙲" "#519aba")
    "msf" (cons "󰬅" "#137be1")
    "mts" (cons "󰛦" "#519aba")
    "mustache" (cons "󱗞" "#e37933")
    "nim" (cons "" "#f3d400")
    "nix" (cons "󱄅" "#7ebae4")
    "nu" (cons "" "#3aa675")
    "obj" (cons "󰆧" "#888888")
    "ogg" (cons "󰈣" "#0075aa")
    "org" (cons "" "#77AA99")
    "pdf" (cons "󰈦" "#b30b00")
    "php" (cons "󰌟" "#a074c4")
    "pls" (cons "󰆼" "#ed95ae")
    "png" (cons "󰸭" "#a074c4")
    "po" (cons "󰗊" "#2596be")
    "pot" (cons "󰗊" "#2596be")
    "ppt" (cons "󱎐" "#cb4a32")
    "prisma" (cons "" "#5a67d8")
    "ps1" (cons "󰨊" "#4273ca")
    "psd1" (cons "󰨊" "#6975c4")
    "psm1" (cons "󰨊" "#6975c4")
    "pxd" (cons "󰫽" "#5aa7e4")
    "pxi" (cons "󰫽" "#5aa7e4")
    "py" (cons "󰌠" "#ffbc03")
    "pyi" (cons "󰌠" "#ffbc03")
    "pyx" (cons "󰫽" "#5aa7e4")
    "qml" (cons "󰫾" "#40cd52")
    "rake" (cons "󰴭" "#701516")
    "rar" (cons "󰗄" "#eca517")
    "rb" (cons "󰴭" "#701516")
    "res" (cons "󰫿" "#cc3e44")
    "resi" (cons "󰫿" "#f55385")
    "rmd" (cons "󰍔" "#519aba")
    "rs" (cons "󱘗" "#dea584")
    "rss" (cons "󰗀" "#FB9D3B")
    "sass" (cons "󰟬" "#f55385")
    "sbt" (cons "" "#cc3e44")
    "scad" (cons "" "#f9d72c")
    "scala" (cons "" "#cc3e44")
    "scm" (cons "󰘧" "#eeeeee")
    "scss" (cons "󰟬" "#f55385")
    "sh" (cons "" "#4d5a5e")
    "sln" (cons "󰘐" "#854CC7")
    "sml" (cons "󰘧" "#e37933")
    "so" (cons "" "#dcddd6")
    "sol" (cons "" "#519aba")
    "srt" (cons "󰨖" "#ffb713")
    "ssa" (cons "󰨖" "#ffb713")
    "stp" (cons "󰬀" "#839463")
    "styl" (cons "󰴒" "#8dc149")
    "sub" (cons "󰚩" "#ffb713")
    "sv" (cons "󰍛" "#019833")
    "svelte" (cons "" "#ff3e00")
    "svg" (cons "󰜡" "#FFB13B")
    "svh" (cons "󰍛" "#019833")
    "swift" (cons "󰛥" "#e37933")
    "tcl" (cons "󰛓" "#1e5cb3")
    "templ" (cons "󰬁" "#dbbd30")
    "tf" (cons "󱁢" "#5F43E9")
    "tfvars" (cons "󱁢" "#5F43E9")
    "tgz" (cons "󰗄" "#eca517")
    "toml" (cons "" "#9c4221")
    "tres" (cons "" "#6d8086")
    "ts" (cons "󰛦" "#519aba")
    "tscn" (cons "" "#6d8086")
    "tsx" (cons "" "#1354bf")
    "twig" (cons "" "#8dc149")
    "txt" (cons "󰈙" "#89e051")
    "txz" (cons "󰗄" "#eca517")
    "ui" (cons "󰗀" "#0c306e")
    "vala" (cons "󰬝" "#7239b3")
    "vhd" (cons "󰍛" "#019833")
    "vhdl" (cons "󰍛" "#019833")
    "vim" (cons "" "#019833")
    "vsh" (cons "" "#5d87bf")
    "vue" (cons "󰡄" "#8dc149")
    "wav" (cons "󰈣" "#00afff")
    "webm" (cons "󰈫" "#FD971F")
    "webmanifest" (cons "󰘦" "#f1e05a")
    "webp" (cons "󰈟" "#a074c4")
    "wma" (cons "󰈣" "#00afff")
    "wrl" (cons "󰬃" "#888888")
    "x" (cons "󰫿" "#599eff")
    "xls" (cons "󱎏" "#207245")
    "xlsx" (cons "󱎏" "#207245")
    "xul" (cons "󰗀" "#e37933")
    "xz" (cons "󰗄" "#eca517")
    "yaml" (cons "" "#6d8086")
    "yml" (cons "" "#6d8086")
    "zig" (cons "" "#f69a1b")
    "zip" (cons "󰗄" "#eca517")
    "zsh" (cons "" "#89e051")
    "zst" (cons "󰗄" "#eca517")
  )
)

(define *directories*
  (hash ".git" (cons "" "#f69a1b")
        ".github" (cons "" "#3aa6e0")
        ".config" (cons "󱁿" "#22d3ee")
        "node_modules" (cons "" "#4caf50")
        "src" (cons "󰴉" "#9d7cd8")
        "lib" (cons "󰲂" "#cbcb41")
        "test" (cons "󱞊" "#599eff")
        "tests" (cons "󱞊" "#599eff")
        "build" (cons "󱧼" "#6d8086")
        "Documents" (cons "󱧶" "#f69a1b")
        "Downloads" (cons "󰉍" "#f69a1b")
        "Desktop" (cons "󰚝" "#f69a1b")
        "Music" (cons "󱍙" "#f69a1b")
        "Pictures" (cons "󰉏" "#f69a1b")
        "Videos" (cons "󱞊" "#f69a1b")))

;;@doc
;; Return the icon for the given folder name
(define (dir-icon name)
  (define entry (hash-try-get *directories* (trim-end-matches name "/")))
  (if entry
    (car entry)
    "󰉋"
  )
)

;;@doc
;; Return the color of the icon for the given folder name
(define (dir-icon-color name)
  (define entry (hash-try-get *directories* (trim-end-matches name "/")))
  (if entry
    (cdr entry)
    "#000000"
  )
)

;;@doc
;; Return the icon for the given file name
(define (icon name)
  (define entry (hash-try-get *extensions* (file-extension name)))
  (if entry
    (car entry)
    "?"
  )
)

;;@doc
;; Return the color of the item for the given file name
(define (icon-color name)
  (define entry (hash-try-get *extensions* (file-extension name)))
  (if entry
    (cdr entry)
    "#000000"
  )
)

;;@doc
;; Return the file extension of the given file name
(define (file-extension name)
  (let ([parts (split-many name ".")])
    (if (> (length parts) 1)
        (list-ref parts (- (length parts) 1))
        ""
    )
  )
)

;;@doc
;; Create a new style for the given one withe the given foreground color.
(define (style-with-fg-color style hex)
  (style-fg style (hex->color hex))
)

;;@doc
;; #rrggbb to Color
(define (hex->color hex)
  (Color/rgb (hex->byte hex 1) (hex->byte hex 3) (hex->byte hex 5))
)

;;@doc
;; Convert a hex string to a byte value
(define (hex->byte hex start)
  (string->number (substring hex start (+ start 2)) 16)
)
