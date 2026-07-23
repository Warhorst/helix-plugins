(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
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
(define *tree* '())
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

  ;; -3 for the search box, and -1 to not clip through the bottom
  (set! *visible-height* (- height 3 1))

  ;; Clear the area wher the file tree will be displayed
  (buffer/clear-with frame panel-area background-style)

  (block/render frame panel-area (make-block background-style border-style "all" "double"))

  (define search-area (area x0 y0 width 3))
  (block/render frame search-area (make-block background-style border-style "all" "double"))

  (define tree-x0 1)
  (define tree-y0 (+ y0 3))

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
      (frame-set-string! frame (+ tree-x0 prefix-w) y icon (style-with-fg-color row-style "#000000"))
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
      (set! *tree-focused?* #f)
      event-result/close
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
        ]

        [(equal? ch #\k)
          (cursor-up)
        ]

        [(equal? ch #\l)
          (open-tree-dir)
        ]

        [(equal? ch #\h)
          ;; TODO when pressing h, the tree should collapse the directory i am currently in,
          ;; and move the cursor to that directory index
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
  event-result/consume
)

(define (cursor-up)
  (when (> *tree-cursor* 0)
    (set! *tree-cursor* (- *tree-cursor* 1))
  )
  event-result/consume
)

;; TODO steel has no hashset remove for some reason, so to implement the close, I need to use a hashmap (or implement the remove
;; the close myself, which would just create a new hashset but without the removed element)
(define (open-tree-dir)
  (define entry (list-ref *tree* *tree-cursor*))
  (define path (list-ref entry 0))

  (when (is-dir? path)
    (set! *open-directories* (hashset-insert *open-directories* path))
    (build-tree!)
  )
  
  event-result/consume
)
