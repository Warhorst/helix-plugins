(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "util.scm")

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
;; depth: The depth in the tree. A folder might have depth i and a file in it i + 1
(define *tree* '())
;;@doc
;; Hashmap which contains a path as key and void as value. Each path is expected to be a directory
;; which is currently open.
;; TODO When steel some day supports hashset closes this could be a simple hashset
;; TODO when closing a parent directory, all its child directories should be closed
(define *open-directories* (hash))
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
(define *window-start* 0)
;;@doc
;; TODO needs doc
(define *visible-height* 30)
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

;; dirs before files, alphabetic oder
(define (sort-path-entries lst)
  (define dirs (sort (filter is-dir? lst) string<?))
  (define files (sort (filter (lambda (p) (not (is-dir? p))) lst) string<?))
  (append dirs files)
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
  (define border-style (if *tree-focused?* text-style background-style))

  (define panel-area (area x0 y0 width height))

  ;;-1 to not clip through the bottom
  (set! *visible-height* (- height 2))

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame panel-area background-style)

  (block/render frame panel-area (make-block background-style border-style "all" "double"))

  (define tree-x0 1)
  (define tree-y0 1)

  ;; TODO scrolling
  ;; TODO clip the file names
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
      (define y (+ tree-y0 row))
      (define prefix-w (string-length prefix))

      (define icon (if dir? (dir-icon name) (icon name)))
      (define highlighted? (= abs-idx *tree-cursor*))
      (define row-style (if highlighted? highlight-style text-style))
      (when highlighted?
        (frame-set-string! frame x0 y (make-string width #\space) highlight-style)
      )

      (frame-set-string! frame tree-x0 y prefix row-style)
      (frame-set-string! frame (+ tree-x0 prefix-w) y icon row-style)
      ;; TODO the name needs truncation, or it will be rendered outside of the tree panel
      (frame-set-string! frame (+ tree-x0 prefix-w 2) y name row-style)

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

        [(equal? ch #\r)
          (prompt-rename)
          event-result/consume
        ]

        [(equal? ch #\d)
          (prompt-delete)
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

(define (open-tree-dir)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (when (is-dir? path)
    (set! *open-directories* (hash-insert *open-directories* path void))
    (build-tree!)
  )
)

(define (close-tree-dir)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (if (and (is-dir? path) (hash-contains? *open-directories* path))
    (begin
      (set! *open-directories* (hash-remove *open-directories* path))
      (build-tree!)
    )
    (set! *tree-cursor* (get-parent-dir-index))
  )
)

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

;; TODO for the add / delete / rename functions, see the functions named "forest-prompt-xxx"

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

;; TODO because steel sucks, I cannot change fields in a struct and use it neatly as
;; the holder of my ui state. Instead, I use global variables like a dev from the 50s.
;; (This comment might be loaded with frustration and anger)
(struct PromptState ())

;;@doc
;; The type of the currently open prompt. Must be one of the following: add, rename, delete
(define *prompt-type* 'add)
;;@doc
;; The input the user put into the currently open prompt.
(define *prompt-input* "")

(define (prompt-add)
  (open-prompt 'add)
)

(define (prompt-rename)
  (open-prompt 'rename)
)

(define (prompt-delete)
  (open-prompt 'delete)
)

(define (open-prompt type)
  (set! *prompt-type* type)
  (set! *prompt-input* "")

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
  (define x0 20)
  (define y0 20)
  (define width 30)
  (define height 3)
  (define prompt-area (area x0 y0 width height))

  (define text-style (theme-scope-ref "ui.text"))
  (define background-style (theme-scope-ref "ui.background"))

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame prompt-area background-style)
  (block/render frame prompt-area (make-block background-style text-style "all" "double"))

  (define title (cond
    [(equal? 'add *prompt-type*)
      "Create file"
    ]

    [(equal? 'rename *prompt-type*)
      "Rename file"
    ]

    [(equal? 'delete *prompt-type*)
      "Delete file"
    ]

    [else
      "Unknown type"
    ]
  ))
  
  (frame-set-string! frame (+ x0 1) y0 title text-style)
  (frame-set-string! frame (+ x0 1) (+ y0 1) *prompt-input* text-style)

  void
)

(define (handle-prompt-event _ event)
  (define ch (key-event-char event))
  (cond
    [(key-event-escape? event)
      event-result/close
    ]

    [(key-event-enter? event)
      event-result/close
    ]

    [(key-event-backspace? event)
      (define len (string-length *prompt-input*))
      (when (> len 0)
        (set! *prompt-input* (substring *prompt-input* 0 (- len 1)))
      )
      event-result/consume
    ]

    [(char? ch)
      (set! *prompt-input* (string-append *prompt-input* (string ch)))
      event-result/consume
    ]

    [else event-result/consume]
  )
)

;;@doc
;; Set the tree unfocused. 
(define (unfocus-tree)
  (set! *tree-focused?* #f)
  ;; Pop the event handler to stop receiving input in the tree
  (pop-last-component-by-name! *event-handler-component-name*)
)
