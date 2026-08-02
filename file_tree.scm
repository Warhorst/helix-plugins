(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "util.scm")
(#%require-dylib "libhelix_plugins_native" (only-in create-file))

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
;; 0 path: The path to the file
;; 1 indent: The indent, which is a string of spaces. It defines how much a file is pushed to the right, creating the tree look
;; 2 marker: If the path is a dir, this is an arrow indicating the dir is open or closed. For a file, this is an empty string.
;; 3 name: The file name of the file (basically the last part of the path)
;; 4 depth: The depth in the tree. A folder might have depth i and a file in it i + 1
(define *tree* '())
;;@doc
;; Hashmap which contains a path as key and void as value. Each path is expected to be a directory
;; which is currently open.
(define *open-directories* (hash))
;;@doc
;; The current widht of the file tree
(define *tree-width* 32)
;;@doc
;; The current height of the tree. This might get updated when the height of the terminal frame changes.
(define *tree-height* 30)
;;@doc
;; The minimum withd the rendered file tree can have
(define *min-tree-width* 22)
;;@doc
;; The maximum width the rendred file tree can have
(define *max-tree-widht* 60)
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
;; The path (string) to a directory or file which is marked for movement.
;; Set to #f if nothing is currently marked.
(define *marked-file* #f)
;;@doc
;; The name of the file tree ui component
(define *tree-component-name* "file-tree")
;;@doc
;; The name of the ui component which handles the file tree controlls
(define *event-handler-component-name* "event-handler")
;; @doc
;; The name of a prompt which might be open to receive user input
(define *prompt-name* "prompt")
;;@doc
;; A set of file names which should be listed before other files in the file tree
(define *important-files* (hashset "mod.rs" "lib.rs" "main.rs"))


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
      (set! *open-directories* (hash-insert *open-directories* (helix-find-workspace) void))
      (build-tree!)
      (enqueue-thread-local-callback
        (lambda () (set-editor-clip-left! *tree-width*))
      )

      (push-component! (make-tree-component))
      (push-component! (make-handle-event-component))
    ]

    [(not *tree-focused?*)
      (set! *tree-focused?* #t)
      (push-component! (make-handle-event-component))
    ]

    [else
      (set! *tree-open?* #f)
      (set! *tree-focused?* #f)

      ;;Remove the file tree from the render stack
      (pop-last-component-by-name! *tree-component-name*)
      (pop-last-component-by-name! *event-handler-component-name*)

      ;; Reset the editor clip
      ;; Wrapping in this callback is required for some reason, or it will not be applied
      (enqueue-thread-local-callback
        (lambda () (set-editor-clip-left! 0))
      )
    ]
  )
)

(define (build-tree!)
  (define result '())
  (unmark-file)

  (define (walk path depth)
    (define name (file-name path))
    (define indent (repeat-str "  " depth))
    (define marker
      (if (is-dir? path)
        (dir-marker path)
        "  "
      )
    )

    (set! result (cons (list path indent marker name depth) result))

    (when (and (is-dir? path) (hash-contains? *open-directories* path))
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
  (if (hash-contains? *open-directories* path)
      "▼ "
      "▶ "
  )
)

;;@doc
;; Sort the given list of paths. First the directories are listed in alphabetical order, then the
;; important files, then the remaining files.
(define (sort-path-entries lst)
  ;; Dirs first
  (define dirs (sort (filter is-dir? lst) string<?))

  ;; Important files second
  (define important-files (sort (filter (
    lambda (p) (and (is-file? p) (hashset-contains? *important-files* (file-name p)))
  ) lst) string<?))

  ;; All other files last
  (define files (sort (filter (
    lambda (p) (and (is-file? p) (not (hashset-contains? *important-files* (file-name p))))
  ) lst) string<?))

  (append dirs important-files files)
)

(struct TreeState ())

;;@doc
;; Create the component which renders the file tree, but does not handle the events
(define (make-tree-component)
  (new-component!
    *tree-component-name*
    (TreeState)
    render-tree
    (hash "handle_event" (lambda (_ _) event-result/ignore))
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
  (define marked-style (style-fg (theme-scope-ref "ui.highlight") Color/Cyan))
  (define border-style (if *tree-focused?* text-style background-style))

  (define panel-area (area x0 y0 width height))

  ;;-1 to not clip through the bottom
  (set! *tree-height* (- height 2))

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame panel-area background-style)

  (block/render frame panel-area (make-block background-style border-style "all" "double"))

  (define root-x 1)
  (define root-y 1)

  ;; TODO scrolling
  (let loop ([items *tree*] [row 0])
    (unless (or (null? items) (>= row *tree-height*))
      ;; Get the current element of the list and extract its parameters
      (define entry (car items))
      (define path (list-ref entry 0))
      (define indent (list-ref entry 1))
      (define marker (list-ref entry 2))
      (define name (list-ref entry 3))

      (define y (+ root-y row))
      (define prefix (string-append indent marker))
      (define icon (if (is-dir? path) (dir-icon name) (icon name)))
      (define marked? (equal? path *marked-file*))
      (define highlighted? (= row *tree-cursor*))
      (define row-style (if highlighted? highlight-style text-style))

      (when highlighted?
        (frame-set-string! frame (+ x0 1) y (make-string (- width 2) #\space) highlight-style)
      )

      (define text-style (if marked? marked-style row-style))

      (define line (string-append prefix icon " " name))
      (when (> (string-length line) (- *tree-width* 2))
        ;; The line is longer than the tree width, so cut it
        (set! line (substring line 0 (- *tree-width* 2)))
      )
      
      (frame-set-string! frame root-x y line text-style)

      (loop (cdr items) (+ row 1))
    )
  )
)

;;@doc
;; Crate the component which handles the file tree input.
;; This does not render anything, but handles the key events. This allows
;; To remove this component and still render it, but disable key input
(define (make-handle-event-component)
  (new-component!
    *event-handler-component-name*
    (TreeState)
    (lambda (_ _ _) void)
    (hash "handle_event" handle-key-event)
  )
)

(define (handle-key-event _ event)
  (define ch (key-event-char event))
  (cond
    [(key-event-escape? event)
      (unfocus-tree)
      event-result/consume
    ]

    [(key-event-enter? event)
      (open-file)
    ]

    [(char? ch)
      (cond
        ;; Used to still be able to close the tree with Alt+1 when the tree is open and focused.
        ;; TODO there is also key-event-modifier. I might be able to make this cleaner using this
        [(equal? ch #\1)
          event-result/ignore
        ]
      
        [(equal? ch #\j)
          (cursor-down)
          event-result/consume
        ]

        [(equal? ch #\k)
          (cursor-up)
          event-result/consume
        ]

        [(equal? ch #\l)
          (open-tree-dir)
          event-result/consume
        ]

        [(equal? ch #\h)
          (close-tree-dir)
          event-result/consume
        ]

        [(equal? ch #\a)
          (prompt-add)
          event-result/consume
        ]

        [(equal? ch #\R)
          (build-tree!)
          event-result/consume
        ]
        
        [(equal? ch #\r)
          (prompt-rename)
          event-result/consume
        ]

        [(equal? ch #\d)
          (prompt-delete)
          event-result/consume
        ]

        [(equal? ch #\x)
          (mark-selected-file)
          event-result/consume
        ]

        [(equal? ch #\p)
          (prompt-move)
          event-result/consume
        ]

        [(equal? ch #\-)
          (decrease-width)
          event-result/consume
        ]

        [(equal? ch #\+)
          (increase-width)
          event-result/consume
        ]

        [else
          event-result/consume
        ]
      )
    ]

    [else event-result/consume]
  )
)

;; TODO when moving up and down, the window start must be moved if I would move out of the visible area,
;; causing a scroll

(define (cursor-down)
  (when (< *tree-cursor* (- (length *tree*) 1))
    (set! *tree-cursor* (+ *tree-cursor* 1))
  )
)

(define (cursor-up)
  (when (> *tree-cursor* 0)
    (set! *tree-cursor* (- *tree-cursor* 1))
  )
)

;;@doc
;; Open the directory at *tree-cursor*
(define (open-tree-dir)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (when (is-dir? path)
    (set! *open-directories* (hash-insert *open-directories* path void))
    (build-tree!)
  )
)

;;@doc
;; Close the directory at *tree-cursor*. This closes
;; all sub-directories of the directory.
;; If *tree-cursor* is currently not at a directory, it sets the cursor
;; to the index of its parent instead.
(define (close-tree-dir)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (if (and (is-dir? path) (hash-contains? *open-directories* path))
    (begin
      (define currently-open-paths (hash-keys->list *open-directories*))

      (for-each
        (lambda (open)
          (when (starts-with? open path)
            (set! *open-directories* (hash-remove *open-directories* open))
          )
        )
        currently-open-paths
      )

      (build-tree!)
    )
    (set! *tree-cursor* (get-parent-dir-index))
  )
)

;;@doc
;; Return the index of the parent directory of the file where *tree-cursor*
;; is currently at.
(define (get-parent-dir-index)
  (define (index-inner index current-depth)
    (define entry (list-ref *tree* index))
    (define depth (list-ref entry 4))

    (cond
      ;; Reached root, so just return the root index
      [(equal? 0 index)
        0
      ]

      ;; First entry where the depth is smaller,
      ;; so this must be the parent
      [(< depth current-depth)
        index
      ]

      ;; Check the file above the current one
      [else
        (index-inner (- index 1) current-depth)
      ]
    )
  )

  (define entry (list-ref *tree* *tree-cursor*))
  (define current-depth (list-ref entry 4))

  (index-inner *tree-cursor* current-depth)
)

;;@doc
;; Open the file the tree cursor is currently at, if it is not a directory
(define (open-file)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (when (is-file? path)
    (enqueue-thread-local-callback (lambda () (helix.open path)))
    (unfocus-tree)
  )
  
  event-result/consume
)

(struct PromptState ())

;;@doc
;; The type of the currently open prompt. Must be one of the following: add, rename, delete, move
(define *prompt-type* 'add)
;;@doc
;; The title which is displayed on top of the prompt.
(define *prompt-title* "")
;;@doc
;; The input the user put into the currently open prompt.
(define *prompt-input* "")

(define (prompt-add)
  (open-prompt 'add)
)

;;@doc
;; Open a prompt to rename the currently selected file, defined by *tree-cursor*.
;; If the selected file is the project root, nothing happens.
(define (prompt-rename)
  ;; The root directory should not be renameable
  (unless (equal? *tree-cursor* 0)
    (open-prompt 'rename)
  )
)

;; TODO when deleting a file, it should be closed if a buffer for it exists
;;@doc
;; Open a prompt to delete the currently selected file, defined by *tree-cursor*.
;; If the selected file is the project root, nothing happens.
(define (prompt-delete)
  ;; The root directory should not be deletable
  (unless (equal? *tree-cursor* 0)
    (open-prompt 'delete)
  )
)

(define (prompt-move)
  (when *marked-file*
    (open-prompt 'move)
  )
)

(define (open-prompt type)
  (set! *prompt-type* type)
  (set! *prompt-title* "")
  (set! *prompt-input* "")

  (cond
    [(equal? *prompt-type* 'add)
      (set! *prompt-title* (string-append "Add file to directory '" (get-add-move-target-name) "/'"))
    ]

    [(equal? *prompt-type* 'rename)
      (define entry (list-ref *tree* *tree-cursor*))
      (define name (list-ref entry 3))
      (set! *prompt-title* "Rename file")
      (set! *prompt-input* name)
    ]

    [(equal? *prompt-type* 'delete)
      (define entry (list-ref *tree* *tree-cursor*))
      (define path (list-ref entry 0))
      (define name (list-ref entry 3))

      (if (is-file? path)
        (begin
          (set! *prompt-title* (string-append "Do you want to delete the file '" name "'?"))
          (set! *prompt-input* name)
        )
        (begin
          (set! *prompt-title* (string-append "Do you want to delete the directory '" name "/' and all its content?"))
          (set! *prompt-input* (string-append name "/"))
        )
      )
    ]

    [(equal? *prompt-type* 'move)
      (set! *prompt-title* (string-append "Move file to directory '" (get-add-move-target-dir) "/'?"))
      (set! *prompt-input* *marked-file*)
    ]
  )

  (push-component!
    (new-component!
      *prompt-name*
      (PromptState)
      render-prompt
      (hash
        "handle_event" handle-prompt-event
      )
    )
  )
)


(define (render-prompt _ rect frame)
  (define x0 (+ *tree-width* 3))
  (define y0 *tree-cursor*)
  ;; At least 30 tiles large, or if larger the maximum of the title or the input length.
  ;; +2 at the title for the prompt borders
  ;; +4 at the input for the prompt borders and the "> " string
  (define width (max (+ (string-length *prompt-title*) 2) (+ (string-length *prompt-input*) 4) 30))
  (define height 6)
  (define prompt-area (area x0 y0 width height))

  (define text-style (theme-scope-ref "ui.text"))
  (define background-style (theme-scope-ref "ui.background"))

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame prompt-area background-style)
  (block/render frame prompt-area (make-block background-style text-style "all" "double"))
  
  (frame-set-string! frame (+ x0 1) (+ y0 1) *prompt-title* text-style)
  (frame-set-string! frame (+ x0 1) (+ y0 2) (string-append "> " *prompt-input*) text-style)
  (frame-set-string! frame (+ x0 1) (+ y0 4) "<Enter> Confirm <Esc> Cancel" text-style)
)

(define (handle-prompt-event _ event)
  (define ch (key-event-char event))
  (cond
    [(key-event-escape? event)
      event-result/close
    ]

    [(key-event-enter? event)
      (cond
        [(equal? 'add *prompt-type*)
          (create-new-file)
          event-result/close
        ]
      
        [(equal? 'rename *prompt-type*)
          (rename-selected-file)
          event-result/close
        ]

        [(equal? 'delete *prompt-type*)
          (delete-selected-file)
          event-result/close
        ]

        [(equal? 'move *prompt-type*)
          (move-file)
          event-result/close
        ]

        [else
          event-result/consume
        ]
      )
    ]

    [(key-event-backspace? event)
      (define len (string-length *prompt-input*))
      (when (> len 0)
        (set! *prompt-input* (substring *prompt-input* 0 (- len 1)))
      )
      event-result/consume
    ]

    [(and (char? ch) (not (equal? 'delete *prompt-type*)))
      (set! *prompt-input* (string-append *prompt-input* (string ch)))
      event-result/consume
    ]

    [else event-result/consume]
  )
)

;;@doc
;; Create a new file based on the *tree-cursor* and *prompt-input*
(define (create-new-file)
  (when (> (string-length *prompt-input*) 0)

    (define cursor-path (list-ref (list-ref *tree* *tree-cursor*) 0))
    (define file-name (string-append (get-add-move-target-dir) "/" *prompt-input*))

    (if (ends-with? *prompt-input* "/")
      (create-directory! file-name)
      (create-file file-name)
    )

    (build-tree!)
  )
)

;;@doc
;; Rename the file at *tree-cursor* to the contents of the prompt input.
(define (rename-selected-file)
  (when (> (string-length *prompt-input*) 0)
    (define entry (list-ref *tree* *tree-cursor*))
    (define path (list-ref entry 0))
    (define name (list-ref entry 3))
    (define dir (trim-end-matches path (string-append (path-separator) name)))
    (define new-path (string-append dir (path-separator) *prompt-input*))
    (rename-file-or-directory! path new-path)
    (build-tree!)
  )
)

;;@doc
;; Delete the selected file or directory at *tree-cursor*
(define (delete-selected-file)
  (define path (list-ref (list-ref *tree* *tree-cursor*) 0))

  (if (is-file? path)
    (delete-file! path)
    (begin
      ;; This might rebuild the tree twice (once here when closing the directories and
      ;; once at the end of this function), but this should be fine as deleting a tree does
      ;; not happen that often.
      (delete-directory! path)
      (close-tree-dir)
    )
  )
  (build-tree!)
)

;;@doc
;; Move the currently marked file to the directory the cursor is currently targeting.
(define (move-file)
  (define marked-file-name (file-name *marked-file*))
  (define target-file (string-append (get-add-move-target-dir) "/" marked-file-name))

  (rename-file-or-directory! *marked-file* target-file)
  (build-tree!)
)

;;@doc
;; Return the target directory name for add and move operations, based
;; on the current *tree-cursor*. If the cursor is on a directory,
;; the directory is the target. If not, the parent directory of the hovered
;; file is.
(define (get-add-move-target-name)
    (define cursor-entry (list-ref *tree* *tree-cursor*))
    (define cursor-path (list-ref cursor-entry 0))

    (if (is-dir? cursor-path)
      (string-append (list-ref cursor-entry 3))
      (begin
        (define parent-index (get-parent-dir-index))
        (define parent-entry (list-ref *tree* parent-index))
        (define parent-dir (list-ref parent-entry 3))
        parent-dir
      )
    )
)

;;@doc
;; Return the target directory path for add and move operations, based
;; on the current *tree-cursor*. If the cursor is on a directory,
;; the directory is the target. If not, the parent directory of the hovered
;; file is.
(define (get-add-move-target-dir)
    (define cursor-entry (list-ref *tree* *tree-cursor*))
    (define cursor-path (list-ref cursor-entry 0))

    (if (is-dir? cursor-path)
      (string-append (list-ref cursor-entry 0))
      (begin
        (define parent-index (get-parent-dir-index))
        (define parent-entry (list-ref *tree* parent-index))
        (define parent-dir (list-ref parent-entry 0))
        parent-dir
      )
    )
)

;;@doc
;; Mark the file which is currently selected by *tree-cursor*.
;; If the same file is marked again, it gets unmarked.
(define (mark-selected-file)
  (define path (list-ref (list-ref *tree* *tree-cursor*) 0))

  (if (equal? path *marked-file*)
    (set! *marked-file* #f)
    (set! *marked-file* path)
  )
)

;;@doc
;; Unmark the currently selected file.
;; Has no effect if no file is currently marked.
(define (unmark-file)
  (set! *marked-file* #f)
)

;;@doc
;; Reduce the width of the tree, unless the minimum width is reached.
(define (decrease-width)
  (when (> *tree-width* *min-tree-width*)
    (set! *tree-width* (- *tree-width* 1))
    (enqueue-thread-local-callback
      (lambda () (set-editor-clip-left! *tree-width*))
    )
  )
)

;;@doc
;; Increase the width of the tree, unless the maximum widht is reached.
(define (increase-width)
  (when (< *tree-width* *max-tree-widht*)
    (set! *tree-width* (+ *tree-width* 1))
    (enqueue-thread-local-callback
      (lambda () (set-editor-clip-left! *tree-width*))
    )
  )
)

;;@doc
;; Set the tree unfocused. 
(define (unfocus-tree)
  (set! *tree-focused?* #f)
  ;; Pop the event handler to stop receiving input in the tree
  (pop-last-component-by-name! *event-handler-component-name*)
)
