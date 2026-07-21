(require "helix/components.scm")

;;@doc
;; Map of file extension to their icons and colors
(define *extensions*
  (hash
    "7z" "󰗄"
    "aac" "󰈣"
    "ai" ""
    "aif" "󰈣"
    "applescript" "󰀵"
    "ass" "󰨖"
    "astro" ""
    "awk" ""
    "bat" "󰯂"
    "bazel" ""
    "bib" "󱉟"
    "bicep" ""
    "bicepparam" ""
    "blp" "󰠡"
    "bmp" "󰈟"
    "bz" "󰗄"
    "bz2" "󰗄"
    "bz3" "󰗄"
    "bzl" ""
    "c" "󰙱"
    "cast" "󰈫"
    "cbl" "󱌼"
    "ccm" "󰙲"
    "cjs" "󰌞"
    "clj" ""
    "cljc" ""
    "cljs" ""
    "cmake" "󱁤"
    "cob" "󱌼"
    "cpp" "󰙲"
    "cppm" "󰙲"
    "cr" ""
    "cs" "󰌛"
    "csproj" "󰗀"
    "css" "󰌜"
    "csv" ""
    "cts" "󰛦"
    "cu" ""
    "cue" "󰝚"
    "cuh" ""
    "cxx" "󰙲"
    "cxxm" "󰙲"
    "dart" ""
    "desktop" "󰍹"
    "diff" "󰦓"
    "doc" "󱎒"
    "docx" "󱎒"
    "dot" "󱎒"
    "eex" ""
    "el" ""
    "elm" ""
    "epp" ""
    "erb" "󰴭"
    "erl" ""
    "exe" "󰖳"
    "exs" ""
    "f90" "󱈚"
    "fish" ""
    "flac" "󰈣"
    "fnl" ""
    "fsi" ""
    "fsx" ""
    "gd" ""
    "gemspec" "󰴭"
    "gif" "󰵸"
    "go" "󰟓"
    "gql" "󰡷"
    "graphql" "󰡷"
    "gv" "󱁉"
    "gz" "󰗄"
    "h" "󰫵"
    "haml" "󰅴"
    "hbs" "󰌞"
    "heex" ""
    "hex" "󰋘"
    "hh" "󰙲"
    "hpp" "󰙲"
    "hrl" ""
    "hs" "󰲒"
    "html" "󰌝"
    "hurl" "󰫵"
    "hx" "󰫵"
    "hxx" "󰙲"
    "ini" "󰯂"
    "ino" ""
    "ipynb" "󰠮"
    "ixx" "󰙲"
    "java" "󰬷"
    "jl" ""
    "jpeg" "󰈥"
    "jpg" "󰈥"
    "js" "󰌞"
    "json" "󰘦"
    "json5" "󰘦"
    "jsonc" "󰘦"
    "jsx" ""
    "kt" "󱈙"
    "kts" "󱈙"
    "leex" ""
    "less" "󰌜"
    "lhs" ""
    "lib" "󰫳"
    "liquid" ""
    "lrc" "󰫹"
    "lua" "󰢱"
    "luau" "󰢱"
    "m3u" "󰲸"
    "m3u8" "󰲸"
    "m4a" "󰈣"
    "m4v" "󰈫"
    "md" "󰍔"
    "mjs" "󰌞"
    "mkv" "󰈫"
    "ml" ""
    "mli" ""
    "mo" "󰫴"
    "mov" "󰈫"
    "mp3" "󰈣"
    "mp4" "󰈫"
    "mpp" "󰙲"
    "msf" "󰬅"
    "mts" "󰛦"
    "mustache" "󱗞"
    "nim" ""
    "nix" "󱄅"
    "nu" ""
    "obj" "󰆧"
    "ogg" "󰈣"
    "org" ""
    "pdf" "󰈦"
    "php" "󰌟"
    "pls" "󰆼"
    "png" "󰸭"
    "po" "󰗊"
    "pot" "󰗊"
    "ppt" "󱎐"
    "prisma" ""
    "ps1" "󰨊"
    "psd1" "󰨊"
    "psm1" "󰨊"
    "pxd" "󰫽"
    "pxi" "󰫽"
    "py" "󰌠"
    "pyi" "󰌠"
    "pyx" "󰫽"
    "qml" "󰫾"
    "rake" "󰴭"
    "rar" "󰗄"
    "rb" "󰴭"
    "res" "󰫿"
    "resi" "󰫿"
    "rmd" "󰍔"
    "rs" "󱘗"
    "rss" "󰗀"
    "sass" "󰟬"
    "sbt" ""
    "scad" ""
    "scala" ""
    "scm" "󰘧"
    "scss" "󰟬"
    "sh" ""
    "sln" "󰘐"
    "sml" "󰘧"
    "so" ""
    "sol" ""
    "srt" "󰨖"
    "ssa" "󰨖"
    "stp" "󰬀"
    "styl" "󰴒"
    "sub" "󰚩"
    "sv" "󰍛"
    "svelte" ""
    "svg" "󰜡"
    "svh" "󰍛"
    "swift" "󰛥"
    "tcl" "󰛓"
    "templ" "󰬁"
    "tf" "󱁢"
    "tfvars" "󱁢"
    "tgz" "󰗄"
    "toml" ""
    "tres" ""
    "ts" "󰛦"
    "tscn" ""
    "tsx" ""
    "twig" ""
    "txt" "󰈙"
    "txz" "󰗄"
    "ui" "󰗀"
    "vala" "󰬝"
    "vhd" "󰍛"
    "vhdl" "󰍛"
    "vim" ""
    "vsh" ""
    "vue" "󰡄"
    "wav" "󰈣"
    "webm" "󰈫"
    "webmanifest" "󰘦"
    "webp" "󰈟"
    "wma" "󰈣"
    "wrl" "󰬃"
    "x" "󰫿"
    "xls" "󱎏"
    "xlsx" "󱎏"
    "xul" "󰗀"
    "xz" "󰗄"
    "yaml" ""
    "yml" ""
    "zig" ""
    "zip" "󰗄"
    "zsh" ""
    "zst" "󰗄"
  )
)

;;@doc
;; Map of a specific directory name to an icon an color
(define *directories*
  (hash
    ".git" ""
    ".github" ""
    ".config" "󱁿"
    "node_modules"""
    "src" "󰴉"
    "lib" "󰲂"
    "test" "󱞊"
    "tests" "󱞊"
    "build" "󱧼"
    "Documents""󱧶"
    "Downloads"  "󰉍"
    "Desktop" "󰚝"
    "Music" "󱍙"
    "Pictures" "󰉏"
    "Videos" "󱞊"
  )
)

(provide dir-icon)
;;@doc
;; Return the icon for the given folder name
(define (dir-icon name)
  (define entry (hash-try-get *directories* (trim-end-matches name "/")))
  (if entry
    entry
    "󰉋"
  )
)

(provide icon)
;;@doc
;; Return the icon for the given file name
(define (icon name)
  (define entry (hash-try-get *extensions* (file-extension name)))
  (if entry
    entry
    "?"
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

(provide style-with-fg-color)
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
