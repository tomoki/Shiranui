(defvar kasumi-mode-hook nil)
(defvar kasumi-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-q" 'kasumi-dive)
    (define-key map "\M-q" 'kasumi-surface)
    (define-key map "\C-xd" 'kasumi-decline)
    (define-key map "\C-xa" 'kasumi-accept)
    (define-key map "\C-xi" 'kasumi-idle)
    (define-key map "\C-xe" 'kasumi-select-explore-sub)
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
(defconst kasumi-command-lock-flyline
  "lock_flyline")
(defconst kasumi-command-flymark-result
  "flymark_result")
(defconst kasumi-command-lift-result
  "lift_result")

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

(defun kasumi-debug-print (str)
  (let ((prev (current-buffer)))
    (progn
      (switch-to-buffer (process-buffer shiranui-process))
      (goto-char (point-max))
      (insert str)
      (insert "\n")
      (switch-to-buffer prev))))


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
      (kasumi-remove-all-dive-overlay))
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
     ((string= command kasumi-command-dive-explore)
      (kasumi-receive-dive-explore value))
     ((string= command kasumi-command-lock-flyline)
      (kasumi-receive-lock-flyline value))
     ((string= command kasumi-command-flymark-result)
      (kasumi-receive-flymark-result value))
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
  (cond
   ((null lis) p)
   ((> p (+ (car (car lis)) (cdr (car lis))))
    (kasumi-orig-point-sub (- p (cdr (car lis))) (cdr lis)))
   (t (kasumi-orig-point-sub p (cdr lis)))))

(defun kasumi-orig-point (p)
  (kasumi-orig-point-sub p point-diff))

(defun kasumi-add-diff (where size)
  (setq point-diff (cons (cons where size) point-diff)))

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

(defun kasumi-receive-dive-explore (value)
  (let* ((lines (split-string value "\n"))
         (beg-end-list (split-string (car lines) " "))
         (start (kasumi-string-to-fix-point (nth 0 beg-end-list)))
         (end (kasumi-string-to-fix-point (nth 1 beg-end-list)))
         (value (string-join (cdr lines) "\n"))
         )
    ;; (kasumi-debug-print (format "(%d,%d) = %s" start end value))))
    (progn 
      (kasumi-debug-print (format "%s at [%d,%d] = %s" (buffer-substring-no-properties start end)
                                  start end value))
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
        (goto-char (kasumi-fix-point where))
        (delete-region (kasumi-fix-point where)
                       (+ (kasumi-fix-point where) remove_length))

        (insert value)

        (add-change (kasumi-fix-point where) remove_length value)
        (kasumi-add-diff (kasumi-fix-point where)
                         (- (length value) remove_length))))))

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
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
    "Used for syntaxerror")

(defface kasumi-runtimeerror-face
  '((((supports :underline (:style wave)))
     :underline (:color "Red1"))
    (t
     :underline t :inherit error))
    "Used for runtimeerror")

(defface kasumi-goodflyline-face
  '((((supports :underline (:style wave)))
     :underline (:color "Green1"))
    (t
     :underline t :inherit error))
    "Used for flyline that passed test")

(defface kasumi-badflyline-face
  '((((supports :underline (:style wave)))
     :underline (:color "Red1"))
    (t
     :underline t :inherit error))
    "Used for flyline that didn't pass test")

(defface kasumi-idleflyline-face
  '((((supports :underline (:style wave)))
     :underline (:color "Yellow1"))
    (t
     :underline t :inherit error))
    "Used for flyline that run")

(defface kasumi-lock-flyline-face
  '((t :weight bold)
    )
  "lock flyline")

(defface kasumi-dive-strike-face
  '((t :strike-through t)
    )
  "strike for dive")

(defface kasumi-explore-face
  '((((supports :underline (:style wave)))
     :underline (:color "Red1"))
    (t
     :underline t :inherit error))
  "where you can explore"
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

(defun kasumi-put-lock-flyline (beg end)
  (kasumi-put-dive-face 'kasumi-lock-flyline-face beg end))

(defun kasumi-put-explore (beg end)
  (kasumi-put-dive-face 'kasumi-explore-face beg end))

(defun kasumi-remove-all-overlay ()
  (remove-overlays (point-min) (point-max) 'category 'kasumi-face))

(defun kasumi-remove-all-dive-overlay ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'kasumi-dive-face))

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
    (kasumi-send-command kasumi-command-dive
                         (number-to-string (kasumi-orig-point (point))))
  ))



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

(defun kasumi-lift-sub (from to)
  (kasumi-send-command kasumi-command-lift
                       (format "%d %d"
                               (kasumi-orig-point from)
                               (kasumi-orig-point to))))


(defun kasumi-surface ()
  (interactive)
  (kasumi-send-command kasumi-command-surface ""))

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
  ;;  first boot.
  (kasumi-refresh (point-min) (point-max) 0)
  (run-hooks 'kasumi-mode-hook))

(provide 'kasumi-mode)
