(defvar kasumi-mode-hook nil)
(defvar kasumi-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-\\" nil) ;; inspect?
    map)
  "Keymap for Kasumi Major mode")

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html#Asynchronous-Processes
(defun start-shiranui (program)
  (let ((process-connection-type nil)) ;; start in pipe
    ;; start-process needs absolute path?
    (start-process "shiranui" "shiranui" (file-truename program))
    ))

;; getline needs newline("\n")
(defun buffer-string-no-properties
  (buffer-substring-no-properties (point-min) (point-max)))

;; http://www.emacswiki.org/emacs/ModeTutorial
(defun kasumi-mode ()
  "Major mode for editing Shiranui files"
  (interactive)
  (kill-all-local-variables)
  ;; (set-syntax-table kasumi-mode-syntax-table)
  (set-local-map kasumi-mode-map))

(provide 'kasumi-mode)
