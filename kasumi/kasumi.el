(defvar kasumi-mode-hook nil)
(defvar kasumi-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-q" 'kasumi-dive)
    (define-key map "\M-u" 'kasumi-surface)
    (define-key map "\M-q" 'kasumi-to-caller)
    (define-key map "\C-xd" 'kasumi-decline)
    (define-key map "\C-xa" 'kasumi-accept)
    (define-key map "\C-xi" 'kasumi-idle)
    (define-key map "\C-xe" 'kasumi-select-explore-sub)
    (define-key map "\C-xj" 'kasumi-send-flymark-jump)
    map)
  "Keymap for Kasumi Major mode")

(defcustom kasumi-where-is-shiranui nil
  "Path where shiranui locates.")

(defconst kasumi-font-lock-keywords
  (list
   '("let[ \t\n]*[a-zA-Z_]+[0-9]*" 0 font-lock-function-name-face)
   '("mut[ \t\n]*[a-zA-Z_]+[0-9]*" 0 font-lock-variable-name-face)
   ;; overwrite for let,mut.
   '( "\\<\\(else\\|if\\|let\\|mut\\|then\\|return\\|for\\|assert\\)\\>" 0 font-lock-keyword-face t)
   '("\\(\\\\\\)" 0 font-lock-constant-face)
   '("\\<\\(and\\|or\\|in\\)\\>" 0 font-lock-builtin-face)
   '("not\\>" 0 font-lock-builtin-face)
   '("\\<[\\-+]*[0-9]*\\.?[0-9]+\\>" 0 font-lock-constant-face)
   '("\/\/[^\n]*\n" 0 font-lock-comment-face t)
   ))

(defconst kasumi-command-load
  "load")
(defconst kasumi-command-change
  "change")
(defconst kasumi-command-dive
  "dive")
(defconst kasumi-command-surface
  "surface")
(defconst kasumi-command-move-to-caller
  "move_to_caller")
(defconst kasumi-command-lift
  "lift")
(defconst kasumi-command-syntaxerror
  "syntaxerror")
(defconst kasumi-command-runtimeerror
  "runtimeerror")
(defconst kasumi-command-idleflyline
  "idleflyline")
(defconst kasumi-command-goodflyline
  "goodflyline")
(defconst kasumi-command-badflyline
  "badflyline")
(defconst kasumi-command-debug-print
  "debug")
(defconst kasumi-command-dive-explore
  "dive_explore")
(defconst kasumi-command-dive-strike
  "dive_strike")
(defconst kasumi-command-dive-clear
  "dive_clear")
(defconst kasumi-command-dive-highlight
  "dive_highlight")
(defconst kasumi-command-lock-flyline
  "lock_flyline")
(defconst kasumi-command-flymark-result
  "flymark_result")
(defconst kasumi-command-flymark-index
  "flymark_index")
(defconst kasumi-command-lift-result
  "lift_result")
(defconst kasumi-command-flymark-jump
  "flymark_jump")

;; getline needs newline("\n")
(defun buffer-string-no-properties ()
  (buffer-substring-no-properties (point-min) (point-max)))


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun count-if (condp lst)
  (length (filter condp lst)))

(defun count-line-string (s)
  (if (= (length s) 0)
      0
    (+ 1 (count-if (lambda (x) (= x ?\n)) s))
    ))

(defun calc-point (row col)
  (save-excursion
    (progn
      (goto-line row)
      (move-to-column (- col 1))
      (point)
      )))

(defun take-nth-sub (lis n one)
  (if (= n 0)
      (cons (reverse one) lis)
    (take-nth-sub (cdr lis) (- n 1)
                  (cons (car lis) one))))

;; [a] -> ([a] . [a])
(defun take-nth (l n)
  (take-nth-sub l n '()))
(defun string-join (lis sep)
  (mapconcat 'identity lis sep))
(defun string-rstrip (s)
  (replace-regexp-in-string "\\'[ \r\n\t]*" "" s))
(defun string-lstrip (s)
  (replace-regexp-in-string "[ \r\n\t]*\\'" "" s))
(defun string-strip (s)
  (string-lstrip (string-rstrip s)))

(defun kasumi-split-splited-expressions (s)
  (reverse (kasumi-split-splited-expressions-sub s "" (make-hash-table) '()))
  )

(defun kasumi-split-splited-expressions-sub (rest cache paren-table ret)
  (if (= (length rest) 0)
      (cons cache ret)
    (let ((bracket (gethash 'bra paren-table 0))
          (dquote  (gethash 'dq paren-table 0))
          (head    (substring rest 0 1))
          (tail    (substring rest 1)))
      (if (and (= dquote 0) (= bracket 0) (string= head ","))
          (kasumi-split-splited-expressions-sub tail "" paren-table (cons cache ret))
        (progn
          (cond
           ((string= head "[")
            (puthash 'bra (+ bracket 1) paren-table))
           ((string= head "]")
            (puthash 'bra (- bracket 1) paren-table))
           ((string= head "\"")
            (puthash 'dq (if (= dquote 1) 0 1) paren-table)))
          (kasumi-split-splited-expressions-sub tail (concat cache head) paren-table ret))))))



;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html#Asynchronous-Processes
(defun kasumi-start-shiranui (program)
  (let ((process-connection-type nil)) ;; start in pipe
    ;; start-process needs absolute path?
  (apply 'start-process "shiranui" "shiranui" (file-truename program) '("--server"))))

;; string -> (command loadcount value rest)
(defun kasumi-parse-sub (str)
  (let* ((lines (split-string str "\n"))
         (first-line (split-string (car lines) " "))
         (command-line-length (string-to-number (car first-line)))
         (loadcount (nth 1 first-line))
         (command (string-join (cddr first-line) " "))
         (value-and-rest (take-nth (cdr lines) command-line-length)))
    (list command loadcount (string-join (car value-and-rest) "\n")
          (string-join (cdr value-and-rest) "\n"))))

;; string -> [(command . value)]
(defun kasumi-parse (str)
  (if (= (length str) 0)
      '()
    (let ((command-loadcount-value-rest (kasumi-parse-sub str)))
      (cons (car (take-nth command-loadcount-value-rest 3))
            (kasumi-parse (nth 3 command-loadcount-value-rest))))))

(defconst DEBUG nil)
(defun kasumi-debug-print (str &optional force)
  (if (or DEBUG force)
      (save-current-buffer
        (set-buffer  (process-buffer shiranui-process))
        (goto-char (point-max))
        (insert str)
        (insert "\n"))))


;; need newline end of string?
(defun kasumi-send-command (command value)
  (process-send-string shiranui-process
   (concat (number-to-string (count-line-string value)) " "
           (number-to-string load-count) " " command "\n" value "\n")
   ))

(defun kasumi-process-filter (process str)
  (kasumi-debug-print "--receive--")
  (kasumi-debug-print str)
  (kasumi-debug-print "-----------")

  (let ((list-command-loadcount-value (kasumi-parse (string-strip str))))
    (progn
      (kasumi-process-pairs list-command-loadcount-value))))

(defun kasumi-process-sentinel (process stat)
  (message "something occured in shiranui"))

(defun kasumi-process-pair (list-command-loadcount-value)
  (let* ((command (car list-command-loadcount-value))
         (lc      (string-to-number (nth 1 list-command-loadcount-value)))
         (correct-load-count (= lc load-count))
         (value   (nth 2 list-command-loadcount-value))
        )
    (cond
     ((string= command kasumi-command-debug-print)
      (kasumi-debug-print value))
     ((string= command kasumi-command-dive-clear)
      (kasumi-receive-dive-clear))

     ((not correct-load-count)
      (kasumi-debug-print (format "loadcount %d is old.ignore it." lc)))
     ((string= command kasumi-command-syntaxerror)
      (kasumi-receive-syntaxerror value))
     ((string= command kasumi-command-goodflyline)
      (kasumi-receive-goodflyline value))
     ((string= command kasumi-command-badflyline)
      (kasumi-receive-badflyline value))
     ((string= command kasumi-command-idleflyline)
      (kasumi-receive-idleflyline value))
     ((string= command kasumi-command-runtimeerror)
      (kasumi-receive-runtimeerror value))
     ((string= command kasumi-command-dive-strike)
      (kasumi-receive-dive-strike value))
     ((string= command kasumi-command-dive-highlight)
      (kasumi-receive-dive-highlight value))
     ((string= command kasumi-command-dive-explore)
      (kasumi-receive-dive-explore value))
     ((string= command kasumi-command-lock-flyline)
      (kasumi-receive-lock-flyline value))
     ((string= command kasumi-command-flymark-result)
      (kasumi-receive-flymark-result value))
     ((string= command kasumi-command-flymark-index)
      (kasumi-receive-flymark-index value))
     ((string= command kasumi-command-lift-result)
      (kasumi-receive-lift-result value))
     (t (message "unknown command:%s " command))
     )))

(defun kasumi-process-pairs (list-command-loadcount-value)
  (if (null list-command-loadcount-value)
      '()
    (cons (kasumi-process-pair (car list-command-loadcount-value))
          (kasumi-process-pairs (cdr list-command-loadcount-value)))))

;; Should use original position?
(defun kasumi-fix-point-sub (p lis)
  (cond
   ((null lis) p)
   ((> p (car (car lis))) (kasumi-fix-point-sub (+ p (cdr (car lis))) (cdr lis)))
   (t (kasumi-fix-point-sub p (cdr lis)))))

(defun kasumi-fix-point (p)
  (kasumi-fix-point-sub p (reverse point-diff)))

(defun kasumi-orig-point-sub (p lis)
  (if (null lis) p
    (let ((where (car (car lis)))
          (diff  (cdr (car lis))))
      (cond
       ((and (> diff 0) (> p (+ where diff)))
        (kasumi-orig-point-sub (- p diff) (cdr lis)))
       ((and (< diff 0) (> p (- where diff)))
        (kasumi-orig-point-sub (- p diff) (cdr lis)))
       (t (kasumi-orig-point-sub p (cdr lis)))))))

(defun kasumi-orig-point (p)
  (kasumi-orig-point-sub p point-diff))

(defun kasumi-add-diff (where size)
  (if (not (= size 0))
      (setq point-diff (cons (cons where size) point-diff))))

(defun print-point-diff ()
  (interactive)
  (print-point-diff-sub point-diff))

(defun print-point-diff-sub (diffs)
  (if (null diffs)
      nil
    (progn
      (message "%d %d" (car (car diffs)) (cdr (car diffs)))
      (print-point-diff-sub (cdr diffs)))))

(defun kasumi-string-to-fix-point (str)
  (kasumi-fix-point (string-to-number str)))

(defun kasumi-receive-syntaxerror (value)
  (message "there is syntaxerror")
  (if (not (= (length value) 0))
      (let ((beg-end-list (split-string value " ")))
        (kasumi-put-syntaxerror (kasumi-string-to-fix-point (nth 0 beg-end-list))
                                (kasumi-string-to-fix-point (nth 1 beg-end-list)))
        )))
(defun kasumi-receive-runtimeerror (value)
  (if (not (= (length value) 0))
      (let ((beg-end-list (split-string value " ")))
        (kasumi-put-runtimeerror (kasumi-string-to-fix-point (nth 0 beg-end-list))
                                 (kasumi-string-to-fix-point (nth 1 beg-end-list)))
        )))

(defun kasumi-receive-goodflyline (value)
    (let* ((lines         (split-string value "\n"))
           (target        (split-string (nth 0 lines) " "))
           (where         (string-to-number (nth 0 (split-string (nth 1 lines) " "))))
           (remove_length (string-to-number (nth 1 (split-string (nth 1 lines) " "))))
           (inhibit-modification-hooks t))
    (save-excursion
      (progn
        (goto-char (kasumi-fix-point where))
        (delete-region (kasumi-fix-point where)
                       (+ (kasumi-fix-point where) remove_length))
        (add-change (kasumi-fix-point where) remove_length "")
        (kasumi-add-diff (kasumi-fix-point where)
                         (- remove_length))

        (kasumi-put-goodflyline (kasumi-string-to-fix-point (nth 0 target))
                                (kasumi-string-to-fix-point (nth 1 target)))))))

(defun kasumi-receive-badflyline (value)
  (let* ((lines         (split-string value "\n"))
         (target        (split-string (nth 0 lines) " "))
         (where         (string-to-number (nth 0 (split-string (nth 1 lines) " "))))
         (remove_length (string-to-number (nth 1 (split-string (nth 1 lines) " "))))
         ;; TODO:support newline.
         (value (nth 2 lines))
         (inhibit-modification-hooks t))
    (save-excursion
      (progn
        (goto-char (kasumi-fix-point where))
        (delete-region (kasumi-fix-point where)
                       (+ (kasumi-fix-point where) remove_length))

        (insert value)

        (add-change (kasumi-fix-point where) remove_length value)
        (kasumi-add-diff (kasumi-fix-point where)
                         (- (length value) remove_length))

        ;; (kasumi-debug-print (format "%S" point-diff))
        (kasumi-put-badflyline (kasumi-string-to-fix-point (nth 0 target))
                                (kasumi-string-to-fix-point (nth 1 target)))))))

(defun kasumi-receive-dive-strike (value)
  (let ((beg-end-list (split-string value " ")))
    (kasumi-put-dive-strike (kasumi-string-to-fix-point (nth 0 beg-end-list))
                            (kasumi-string-to-fix-point (nth 1 beg-end-list)))
    ))

(defun kasumi-receive-dive-highlight (value)
  (let ((beg-end-list (split-string value " ")))
    (kasumi-put-dive-highlight (kasumi-string-to-fix-point (nth 0 beg-end-list))
                               (kasumi-string-to-fix-point (nth 1 beg-end-list)))
    ))


(defun kasumi-receive-dive-explore (value)
  (let* ((lines        (split-string value "\n"))
         (beg-end-list (split-string (car lines) " "))
         (start        (kasumi-string-to-fix-point (nth 0 beg-end-list)))
         (end          (kasumi-string-to-fix-point (nth 1 beg-end-list)))
         (value        (string-join (cdr lines) "\n"))
         )
    ;; (kasumi-debug-print (format "(%d,%d) = %s" start end value))))
    (progn
      (kasumi-debug-print (format "%s at [%d,%d] = %s" (buffer-substring-no-properties start end)
                                  start end value) t)
      (kasumi-put-explore start end)
      (setq explore-data (cons (list start end value) explore-data))
      )
    ))

;; beg end <- target
;; beg remove_length
;; value
(defun kasumi-receive-idleflyline (value)
  (let* ((lines         (split-string value "\n"))
         (target        (split-string (nth 0 lines) " "))
         (where         (string-to-number (nth 0 (split-string (nth 1 lines) " "))))
         (remove_length (string-to-number (nth 1 (split-string (nth 1 lines) " "))))
         ;; TODO:support newline.
         (value (nth 2 lines))
         (inhibit-modification-hooks t))
    (save-excursion
      (progn
        (goto-char (kasumi-fix-point where))
        (delete-region (kasumi-fix-point where)
                       (+ (kasumi-fix-point where) remove_length))

        (insert value)

        (add-change (kasumi-fix-point where) remove_length value)
        (kasumi-add-diff (kasumi-fix-point where)
                         (- (length value) remove_length))

        ;; (kasumi-debug-print (format "%S" point-diff))
        (kasumi-put-idleflyline (kasumi-string-to-fix-point (nth 0 target))
                                (kasumi-string-to-fix-point (nth 1 target)))))))

(defun kasumi-receive-lock-flyline (value)
  (let ((beg-end-list (split-string value " ")))
    (kasumi-remove-all-lock-overlay)
    (kasumi-put-lock-flyline (kasumi-string-to-fix-point (nth 0 beg-end-list))
                             (kasumi-string-to-fix-point (nth 1 beg-end-list)))
    ))

(defun kasumi-receive-flymark-result (value)
  (let* ((lines         (split-string value "\n"))
         (target        (split-string (nth 0 lines) " "))
         (where         (string-to-number (nth 0 (split-string (nth 1 lines) " "))))
         (remove_length (string-to-number (nth 1 (split-string (nth 1 lines) " "))))
         ;; TODO:support newline.
         (value (nth 2 lines))
         (inhibit-modification-hooks t))
    (save-excursion
      (progn
        (goto-char (- (kasumi-fix-point where) 3))
        (let* ((s (+ (search-forward "->") 1))
               (e (- (search-forward ";") 1))
               (will-delete (- e s)))
          (delete-region s e)
          (backward-char)
          (insert value)
          ;; (add-change (kasumi-fix-point where) will-delete value)
          ;; (kasumi-add-diff (+ (kasumi-fix-point s) (length value))
          ;;                  (- (length value) will-delete))
          (add-change (kasumi-fix-point where) will-delete value)
          ;; (kasumi-add-diff (kasumi-fix-point (+ where (length value))) (- (length value) will-delete))
          (kasumi-add-diff (kasumi-fix-point where) (- (length value) will-delete))

          ;; (kasumi-add-diff (+ (kasumi-fix-point s) (length value))
          ;;                  (- (+ (length value) (- will-delete))))

          )))))


(defun kasumi-foldr (lis op id-el)
  (if (null lis)
      id-el
    (funcall op (car lis) (kasumi-foldr (cdr lis) op id-el))))

(defun kasumi-receive-flymark-index (value)
  (let* ((lines      (split-string value "\n"))
         (target     (split-string (nth 0 lines) " "))
         (start      (string-to-number (nth 0 target)))
         (end        (string-to-number (nth 1 target)))
         (index      (string-to-number (nth 1 lines))))
    (save-excursion
      (progn
        (goto-char (kasumi-fix-point start))
        (let* ((s (+ (search-forward "->") 1))
               (e (- (search-forward ";") 1))
               (splited (kasumi-split-splited-expressions (buffer-substring-no-properties s e)))
               ;; + index for commna
               (hs (+ s (+ index (kasumi-foldr (mapcar 'length (car (take-nth splited index))) '+ 0))))
               (he (+ hs (length (nth index splited)))))
          (kasumi-put-flymark-index hs he))
        ))))

(defun move-and-do (where f default)
  (save-excursion
    (progn
      (goto-char where)
      (let ((r (funcall f)))
        (if r r default)))))

(defun kasumi-search-backward-and-recovery (where what)
  (move-and-do where
               (lambda () (search-backward what nil
                                           (lambda () nil))) -1))

(defun kasumi-search-forward-and-recovery (where what)
  (move-and-do where
               (lambda () (search-forward what nil
                                          (lambda () nil))) (point-max)))

;; TODO: check its source code ok
(defun kasumi-send-flymark-jump ()
  (interactive)
  (if (< (kasumi-search-backward-and-recovery (point) ";")
         (kasumi-search-backward-and-recovery (point) "#*")
         (kasumi-search-backward-and-recovery (point) "->")
         (kasumi-search-forward-and-recovery  (point) ";"))
      (save-excursion
        (let* ((h  (point))
               (s  (+ (search-backward "->") 2))
               (e  (- (search-forward ";") 1))
               (i  (- (length (kasumi-split-splited-expressions
                               (buffer-substring-no-properties s h))) 1)))
          (message "%d %d" (kasumi-orig-point s) i)
          (kasumi-send-command kasumi-command-flymark-jump
                               (format "%d %d" (kasumi-orig-point s) i))
          ))
    (message "this is not flymark")))

(defun kasumi-receive-lift-result (value)
  (let* ((lines (split-string value "\n"))
         (what (nth 0 lines)))
    (progn
      (message what)
      (kill-new (format "#+ %s -> ;" what))
      )))

(defun kasumi-send-change-sub  (change)
  (let ((point         (number-to-string (nth 0 change)))
        (remove_length (number-to-string (nth 1 change)))
        (value         (nth 2 change)))
    (if (= (length value) 0)
        (concat point " " remove_length " " (number-to-string (count-line-string value)))
      (concat point " " remove_length " " (number-to-string (count-line-string value)) "\n" value))))

(defun kasumi-send-change ()
  (kasumi-send-command kasumi-command-change
   (mapconcat 'kasumi-send-change-sub (reverse changes) "\n")))

(defface kasumi-syntaxerror-face
  '((t :underline (:style wave :color "red1")))
    "Used for syntaxerror")

(defface kasumi-runtimeerror-face
  '((t :underline (:color "red1")))
    "Used for runtimeerror")

(defface kasumi-goodflyline-face
  '((t :underline (:color "green1")))
    "Used for flyline that passed test")

(defface kasumi-badflyline-face
  '((t :underline (:color "red1")))
    "Used for flyline that didn't pass test")

(defface kasumi-idleflyline-face
  '((t :underline (:color "blue1")))
    "Used for flyline that run")

(defface kasumi-lock-flyline-face
  '((t :weight bold))
  "lock flyline")

(defface kasumi-dive-strike-face
  '((t :strike-through t))
  "strike for dive")

(defface kasumi-dive-highlight-face
  '((t :inherit highlight))
  "highlight for dive")

(defface kasumi-explore-face
  '((t :underline (:color "red1")))
  "where you can explore"
  )
(defface kasumi-flymark-index-face
  '((t  :underline (:color "green1")))
  "current index of flymark"
  )

(defun kasumi-put-face (face beg end)
  (save-restriction
    (let ((ol (make-overlay beg end)))
      (progn
        (overlay-put ol 'category 'kasumi-face)
        (overlay-put ol 'face face)
        ol))))

(defun kasumi-put-dive-face (face beg end)
  (save-restriction
    (let ((ol (make-overlay beg end)))
      (progn
        (overlay-put ol 'category 'kasumi-dive-face)
        (overlay-put ol 'face face)
        ol))))

(defun kasumi-put-lock-face (face beg end)
  (save-restriction
    (let ((ol (make-overlay beg end)))
      (progn
        (overlay-put ol 'category 'kasumi-lock-face)
        (overlay-put ol 'face face)
        ol))))

(defun kasumi-put-syntaxerror (beg end)
  (kasumi-put-face 'kasumi-syntaxerror-face beg end))

(defun kasumi-put-goodflyline (beg end)
  (kasumi-put-face 'kasumi-goodflyline-face beg end))

(defun kasumi-put-badflyline (beg end)
  (kasumi-put-face 'kasumi-badflyline-face beg end))

(defun kasumi-put-idleflyline (beg end)
  (kasumi-put-face 'kasumi-idleflyline-face beg end))

(defun kasumi-put-runtimeerror (beg end)
  (kasumi-put-face 'kasumi-runtimeerror-face beg end))

(defun kasumi-put-dive-strike (beg end)
  (kasumi-put-dive-face 'kasumi-dive-strike-face beg end))

(defun kasumi-put-dive-highlight (beg end)
  (kasumi-put-dive-face 'kasumi-dive-highlight-face beg end))

(defun kasumi-put-lock-flyline (beg end)
  (kasumi-put-lock-face 'kasumi-lock-flyline-face beg end))

(defun kasumi-put-explore (beg end)
  (kasumi-put-dive-face 'kasumi-explore-face beg end))

(defun kasumi-put-flymark-index (beg end)
  (kasumi-put-dive-face 'kasumi-flymark-index-face beg end))

(defun kasumi-remove-all-overlay ()
  (remove-overlays (point-min) (point-max) 'category 'kasumi-face))

(defun kasumi-remove-all-dive-overlay ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'kasumi-dive-face))

(defun kasumi-remove-all-lock-overlay ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'kasumi-lock-face))

(defun add-change (begin length insertion)
  (setq changes (cons (list begin length insertion) changes)))

(defun kasumi-refresh (beg end length)
  (progn
    (add-change beg length (buffer-substring-no-properties beg end))
    (kasumi-add-diff beg (- (- end beg) length))
    (accept-process-output shiranui-process 0)
    (setq load-count (+ load-count 1))
    (kasumi-debug-print (concat "loadcount: " (number-to-string load-count)))
    (kasumi-send-change)
    (setq changes '())
    (setq point-diff '())
    (setq explore-data '())
    (kasumi-remove-all-overlay)))

(defun kasumi-count-lines ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun kasumi-dive ()
  (interactive)
  (progn
    (setq explore-data '())
    (message (format "dive at %d" (kasumi-orig-point (point))))
    (kasumi-send-command kasumi-command-dive
                         (number-to-string (kasumi-orig-point (point))))
  ))

(defun kasumi-receive-dive-clear ()
  (progn
    (kasumi-remove-all-dive-overlay)
    (if (not DEBUG)
        (save-current-buffer
          (progn
            (set-buffer (process-buffer shiranui-process))
            (erase-buffer))))))

(defun kasumi-select-explore-sub ()
  (interactive)
  (if mark-active
      (kasumi-select-explore (region-beginning) (region-end))
    (message "please select region")
    ))

(defun kasumi-select-explore (from to)
  (let* ((included (filter (lambda (start-end-value)
                             (let ((start (nth 0 start-end-value))
                                   (end   (nth 1 start-end-value)))
                               (<= from start end to)))
                           explore-data))
         (sorted (sort included (lambda (left right)
                                  (let ((left-width (- (nth 1 left) (nth 0 left)))
                                        (right-width (- (nth 1 right) (nth 0 right))))
                                    (> left-width right-width)))))
         )
    (if (= (length sorted) 0)
        (progn
          (message "there is no candidate")
          nil)
      (progn
        (kasumi-debug-print
         (mapconcat (lambda (start-end-value)
                      (let* ((start (nth 0 start-end-value))
                             (end   (nth 1 start-end-value))
                             (value (nth 2 start-end-value))
                             (where (buffer-substring-no-properties start end))
                             )
                        (format "%s at [%d,%d] = %s" where start end value)))
                    sorted "\n"))
        (let ((ret-start (nth 0 (nth 0 sorted)))
              (ret-end   (nth 1 (nth 0 sorted)))
              (ret-value (nth 2 (nth 0 sorted))))

          (message (format "%s = %s" (buffer-substring-no-properties ret-start ret-end) ret-value))
          ;; for test
          (kasumi-lift-sub ret-start ret-end)
          (list ret-start ret-end)
        )
      )
    )
  ))

(defun kasumi-select-highlight ()
  (interactive)
  (kasumi-select-clear)
  (if mark-active
      (progn
        (kasumi-select-highlight-sub (region-beginning) (region-end)
                                     explore-data (length explore-data))
    )))

(defun kasumi-select-highlight-sub (from to data index)
  (if (not (null data))
      (let* ((head  (car data))
             (hfrom (nth 0 head))
             (hto   (nth 1 head))
             (value (nth 2 head))
             (tail (cdr data)))
        (if (<= from hfrom hto to)
            (save-current-buffer
              (set-buffer (process-buffer shiranui-process))
              (goto-char (point-min))
              (kasumi-put-dive-highlight (line-beginning-position index)
                                         (line-beginning-position (+ index 1)))))
        (kasumi-select-highlight-sub from to tail (- index 1)))))

(defun kasumi-select-clear ()
  (interactive)
  (save-current-buffer
    (set-buffer (process-buffer shiranui-process))
    (kasumi-remove-all-dive-overlay)))

(defun kasumi-move-hook ()
  (or executing-kbd-macro
      (input-pending-p)
      (progn
        (kasumi-select-highlight)
        )))
(defun kasumi-lift-sub (from to)
  (kasumi-send-command kasumi-command-lift
                       (format "%d %d"
                               (kasumi-orig-point from)
                               (kasumi-orig-point to))))


(defun kasumi-surface ()
  (interactive)
  (kasumi-send-command kasumi-command-surface ""))

(defun kasumi-to-caller ()
  (interactive)
  (kasumi-send-command kasumi-command-move-to-caller ""))

(defun kasumi-accept ()
  (interactive)
  (let ((is-idle-flyline
         (save-excursion
           (beginning-of-line)
           (let ((head (buffer-substring-no-properties (point) (+ (point) 2))))
             (string= head "#+")))))
    (if is-idle-flyline
        (save-excursion
          (beginning-of-line)
          (forward-char)
          (delete-char 1)
          (insert "-"))
      (message "This is not idle-flyline"))))

(defun kasumi-idle ()
  (interactive)
  (let ((is-idle-flyline
         (save-excursion
           (beginning-of-line)
           (let ((head (buffer-substring-no-properties (point) (+ (point) 2))))
             (string= head "#-")))))
    (if is-idle-flyline
        (save-excursion
          (beginning-of-line)
          (forward-char)
          (delete-char 1)
          (insert "+"))
      (message "This is not accepted flyline"))))

(defun kasumi-decline ()
  (interactive)
  (let ((is-idle (string= (buffer-substring-no-properties (line-beginning-position)
                                               (+ (line-beginning-position) 2)) "#+"))
        (prev-length (- (line-end-position) (line-beginning-position)))
        (inhibit-modification-hooks t))
    (if is-idle
        (progn
          (beginning-of-line)
          (forward-char)
          (delete-char 1)
          (insert "-")
          (let* ((s (search-forward "->"))
                 (e (search-forward ";")))
            (delete-region s (- e 1))
            (backward-char)
            (insert " "))
          (kasumi-refresh (line-beginning-position) (line-end-position) prev-length)
          )
        (message "This is not idle-flyline"))))


(defun kasumi-indent ()
  (interactive)
  (let ((cursor-depth (- (point)
                         (line-beginning-position)
                         (current-indentation))))
    (beginning-of-line)
    (if (bobp)
        (progn
          (indent-line-to 0)
          (forward-char cursor-depth)
          )
      (let ((not-indented t)
            cur-indent)
        (if (looking-at "^[ \t]*}")
            (save-excursion
              (while not-indented
                (forward-line -1)
                ;; check forward-line has {.
                (if (looking-at "^.*{")
                    (setq cur-indent (current-indentation))
                  (setq cur-indent (max (- (current-indentation) default-tab-width) 0)))
                (setq not-indented nil)))
          (save-excursion
            (while not-indented
              (forward-line -1)
              (cond
               ((looking-at "^.*{")
                (progn
                  (setq cur-indent (+ (current-indentation) default-tab-width))
                  (setq not-indented nil)))
               ((looking-at "^[ \t]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil)))
               ((bobp)
                (progn
                  (setq cur-indent 0)
                  (setq not-indented nil)))))))
        (indent-line-to (if cur-indent cur-indent 0))
        (forward-char cursor-depth)
        ))))

;; http://www.emacswiki.org/emacs/ModeTutorial
(defun kasumi-mode ()
  "Major mode for editing Shiranui files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map kasumi-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(kasumi-font-lock-keywords))
  (set (make-local-variable 'comment-start) "//")
  (make-local-variable 'receive-in-progress)
  (make-local-variable 'receiving-str)
  ;; ((where diff))
  (set (make-local-variable 'point-diff) '())
  (set (make-local-variable 'load-count) 0)
  ;; changes ((point remove_length insert))
  (set (make-local-variable 'changes) '())
  (set (make-local-variable 'indent-line-function) 'kasumi-indent)
  (set (make-local-variable 'shiranui-process)
       (if (null kasumi-where-is-shiranui)
           (kasumi-start-shiranui (read-file-name "Shiranui Path:"))
         (kasumi-start-shiranui kasumi-where-is-shiranui)))
  (set (make-local-variable 'explore-data) '())
  (set-process-filter shiranui-process 'kasumi-process-filter)
  (set-process-sentinel shiranui-process 'kasumi-process-sentinel)
  (set-process-query-on-exit-flag shiranui-process nil)
  ;; (set-syntax-table kasumi-mode-syntax-table)

  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Hooks.html
  ;; append to last,local.
  (add-hook 'after-change-functions 'kasumi-refresh t t)
  (setq major-mode 'kasumi-mode)

  (setq mode-name "Kasumi")

  (add-hook 'post-command-hook 'kasumi-move-hook t t)
  ;;  first boot.
  (kasumi-remove-all-overlay)
  (kasumi-refresh (point-min) (point-max) 0)
  (run-hooks 'kasumi-mode-hook))

(provide 'kasumi-mode)
