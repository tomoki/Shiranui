(defvar kasumi-mode-hook nil)
(defvar kasumi-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-\\" 'kasumi-refresh) ;; inspect?
    map)
  "Keymap for Kasumi Major mode")

(defcustom kasumi-where-is-shiranui nil
  "Path where shiranui locates.")

(defconst kasumi-font-lock-keywords
  (list
   '("let[ \t\n]*[a-zA-Z]+[0-9]*" 0 font-lock-function-name-face)
   '("mut[ \t\n]*[a-zA-Z]+[0-9]*" 0 font-lock-variable-name-face)
   ;; overwrite for let,mut.
   '( "\\<\\(else\\|if\\|let\\|mut\\|then\\|return\\|for\\)\\>" 0 font-lock-keyword-face t)
   '("\\(\\\\\\)" 0 font-lock-constant-face)
   '("\\<\\(and\\|or\\|in\\)\\>" 0 font-lock-builtin-face)
   '("not\\>" 0 font-lock-builtin-face)
   '("\\<[\\-+]*[0-9]*\\.?[0-9]+\\>" 0 font-lock-constant-face)
   ))

(defconst kasumi-command-load
  "load")
(defconst kasumi-command-syntaxerror
  "syntaxerror")
(defconst kasumi-command-idleflyline
  "idleflyline")
(defconst kasumi-command-goodflyline
  "goodflyline")
(defconst kasumi-command-badflyline
  "badflyline")
(defconst kasumi-command-debug-print
  "debug")


;; getline needs newline("\n")
(defun buffer-string-no-properties ()
  (buffer-substring-no-properties (point-min) (point-max)))

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

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html#Asynchronous-Processes
(defun kasumi-start-shiranui (program)
  (let ((process-connection-type nil)) ;; start in pipe
    ;; start-process needs absolute path?
  (apply 'start-process "shiranui" "shiranui" (file-truename program) '("--server"))))

;; string -> (command value rest)
(defun kasumi-parse-sub (str)
  (let* ((lines (split-string str "\n"))
         (first-line (split-string (car lines) " "))
         (command-line-length (string-to-number (car first-line)))
         (command (string-join (cdr first-line) " "))
         (value-and-rest (take-nth (cdr lines) command-line-length)))
    (list command (string-join (car value-and-rest) "\n")
                  (string-join (cdr value-and-rest) "\n"))))

;; string -> [(command . value)]
(defun kasumi-parse (str)
  (if (= (length str) 0)
      '()
    (let ((command-value-rest (kasumi-parse-sub str)))
      (cons (cons (car command-value-rest) (cadr command-value-rest))
            (kasumi-parse (caddr command-value-rest))))))

(defun kasumi-debug-print (str)
  (let ((prev (current-buffer)))
    (progn
      (switch-to-buffer (process-buffer shiranui-process))
      (insert str)
      (insert "\n")
      (switch-to-buffer prev))))


;; need newline end of string?
(defun kasumi-send-command (command value)
  (process-send-string shiranui-process
   (concat (number-to-string (count-line-string value)) " " command "\n" value "\n")
   ))

(defun kasumi-process-filter (process str)
  ;; (kasumi-debug-print str)
  (let ((pairs-command-value (kasumi-parse (string-strip str))))
    (progn
      (kasumi-process-pairs pairs-command-value))))

(defun kasumi-process-sentinel (process stat)
  (message "something occured in shiranui"))

(defun kasumi-process-pair (pair-command-value)
  (let ((command (car pair-command-value))
        (value   (cdr pair-command-value)))
    (cond ((string= command kasumi-command-syntaxerror)
           (kasumi-receive-syntaxerror value))
          ((string= command kasumi-command-goodflyline)
           (kasumi-receive-goodflyline value))
          ((string= command kasumi-command-badflyline)
           (kasumi-receive-badflyline value))
          ((string= command kasumi-command-idleflyline)
           (kasumi-receive-idleflyline value))
          ((string= command kasumi-command-debug-print)
           (kasumi-debug-print value))
          (t (message "unknown command:%s " command))
          )))

(defun kasumi-process-pairs (pairs-command-value)
  (if (null pairs-command-value)
      '()
    (cons (kasumi-process-pair (car pairs-command-value))
          (kasumi-process-pairs (cdr pairs-command-value)))))


;; Should use original position?
(defun kasumi-fix-point-sub (p lis)
  (cond
   ((null lis) p)
   ((> p (car (car lis))) (kasumi-fix-point-sub (+ p (cdr (car lis))) (cdr lis)))
   (t (kasumi-fix-point-sub p (cdr lis)))))

(defun kasumi-fix-point (p)
  (kasumi-fix-point-sub p point-diff))

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

(defun kasumi-receive-goodflyline (value)
  (let ((beg-end-list (split-string value " ")))
    (kasumi-put-goodflyline (kasumi-string-to-fix-point (nth 0 beg-end-list))
                            (kasumi-string-to-fix-point (nth 1 beg-end-list)))
    ))
(defun kasumi-receive-badflyline (value)
  (let ((beg-end-list (split-string value " ")))
    (kasumi-put-badflyline (kasumi-string-to-fix-point (nth 0 beg-end-list))
                           (kasumi-string-to-fix-point (nth 1 beg-end-list)))
    ))

;; beg end <- target
;; beg end <- remove
;; beg   <- insert here
;; value <- this.
(defun kasumi-receive-idleflyline (value)
  (let* ((lines (split-string value "\n"))
         (target (split-string (nth 0 lines) " "))
         (remove (split-string (nth 1 lines) " "))
         (where  (split-string (nth 2 lines) " "))
         ;; TODO: string-join.
         (value  (nth 3 lines))
         (inhibit-modification-hooks t))
    (save-excursion
      (progn
        (delete-region (kasumi-string-to-fix-point (nth 0 remove))
                       (kasumi-string-to-fix-point (nth 1 remove)))


        (goto-char (kasumi-string-to-fix-point (nth 0 where)))
        (kasumi-add-diff (kasumi-string-to-fix-point (nth 0 remove))
                         (- (kasumi-string-to-fix-point (nth 0 remove))
                            (kasumi-string-to-fix-point (nth 1 remove))))

        (insert value)
        (kasumi-add-diff (kasumi-string-to-fix-point (nth 0 where))
                         (length value))
        (kasumi-put-idleflyline (kasumi-string-to-fix-point (nth 0 target))
                                (kasumi-string-to-fix-point (nth 1 target)))))))

(defun kasumi-send-load ()
  (interactive)
  ;; (kasumi-debug-print "LOAD\n\n")
  (kasumi-send-command kasumi-command-load (buffer-string-no-properties)))

(defface kasumi-syntaxerror-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
    "Used for syntaxerror")

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

(defun kasumi-put-face (face beg end)
  (save-restriction
    (let ((ol (make-overlay beg end)))
      (progn
        (overlay-put ol 'category 'kasumi-face)
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

(defun kasumi-remove-all-overlay ()
  (remove-overlays (point-min) (point-max) 'category 'kasumi-face))

(defun kasumi-refresh (beg end length)
  (progn
    (kasumi-send-load)
    (setq point-diff '())
    (setq load-count (+ load-count 1))
    (kasumi-remove-all-overlay)))

;; http://www.emacswiki.org/emacs/ModeTutorial
(defun kasumi-mode ()
  "Major mode for editing Shiranui files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map kasumi-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(kasumi-font-lock-keywords))
  (setq-local comment-start "#")
  (make-local-variable 'receive-in-progress)
  (make-local-variable 'receiving-str)
  ;; ((where diff))
  (set (make-local-variable 'point-diff) '())
  (set (make-local-variable 'load-count) 0)
  (set (make-local-variable 'shiranui-process)
       (if (null kasumi-where-is-shiranui)
           (kasumi-start-shiranui (read-file-name "Shiranui Path:"))
           (kasumi-start-shiranui kasumi-where-is-shiranui)))
  (set-process-filter shiranui-process 'kasumi-process-filter)
  (set-process-sentinel shiranui-process 'kasumi-process-sentinel)
  ;; (set-syntax-table kasumi-mode-syntax-table)
  (add-hook 'after-change-functions 'kasumi-refresh t t)
  (setq major-mode 'kasumi-mode)
  (setq mode-name "Kasumi")
  (run-hooks 'kasumi-mode-hook))

(provide 'kasumi-mode)
