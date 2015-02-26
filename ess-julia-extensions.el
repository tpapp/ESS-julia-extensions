(defun julia-preceding-module-name (start end)
  "If there is a Julia module defined between START and END,
return NIL, otherwise return the module name (if any) searching
backwards from START.

Useful for evaluating Julia code in a region within a given module."
  ;; FIXME this is a quick fix, and does not handle sub/nested modules.
  ;; A proper solution would be parsing the module structure, wait until
  ;; julia-mode is merged into ESS.
  (interactive "r")
  (let* ((module-regexp "module\s-*\\(\\sw*\\)"))
    (save-excursion
      (goto-char start)
      (when (search-backward-regexp module-regexp nil t)
        (match-string-no-properties 1)))))

(defun julia-send-region (process start end)
  "Send the region between START and END to a Julia process. Evaluated in the current module when applicable, uses the correct line numbers."
  (let ((line (line-number-at-pos start))
        (module (julia-preceding-module-name start end)) ; FIXME see note for function
        (file buffer-file-truename))
    (ess-send-string process (format "ESSx.eval_string(\"%s\", %d, \"%s\"%s)"
                                     (buffer-substring-no-properties start end)
                                     line file
                                     (if module
                                         (format ", [:%s]" module)
                                       "")))))

;;; code below this line is for experimentating with these extensions,
;;; and not meant to be integrated into ESS

(defun julia--eval-region (start end)
  "Evaluate a region in the current process."
  ;; FIXME is this the right way to ensure process is running?
  (ess-force-buffer-current)
  (julia-send-region (ess-get-process ess-current-process-name) start end))

(defun julia-eval-region (start end)
  "Evaluate the active region."
  ;; FIXME should this test for MARK-ACTIVE?
  (interactive "r")
  (julia--eval-region start end))

(defun julia-eval-line ()
  "Evaluate the current line of code."
  (interactive)
  (julia--eval-region (line-beginning-position) (line-end-position)))

(defun julia-eval-paragraph ()
  "Evaluate the paragraph that contains the point."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (julia--eval-region (point) end))))

(defun julia-eval-dwim ()
  "Evaluate the region when active, otherwise the paragraph that contains the point."
  (interactive)
  (if (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))
      (let ((end (region-end)))
        (julia--eval-region (region-beginning) end)
        (goto-char end))
    (julia-eval-paragraph)))

(defun customize-julia-extensions ()
  "Establish bindings for experimenting with REPL interaction from a Julia code buffer."
  (interactive)
  (local-set-key (kbd "C-c <C-return>") 'julia-eval-line)
  (local-set-key (kbd "C-c C-r") 'julia-eval-region)
  (local-set-key (kbd "C-c C-c") 'julia-eval-dwim))

(add-hook 'julia-mode-hook 'customize-julia-extensions)
