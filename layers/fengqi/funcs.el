(defun fengqi/aligh-repeat-whitespace (start end)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r")
  (align-regexp start end "\\([[:blank:]]+\\)[^[:space:]]+" 1 1 t))

(defun fengqi/right-align-rectangle (start end)
  "Right-align selected rectangle.
See https://stackoverflow.com/questions/10914813/generic-right-align-function"
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (apply-on-rectangle (lambda (c0 c1)
                          (move-to-column c1 t)
                          (let ((start (- (point) (- c1 c0)))
                                (end   (point)))
                            (when (re-search-backward "\\S-" start t)
                              (transpose-regions start (match-end 0) (match-end 0) end))))
                        start end))
  (when indent-tabs-mode (tabify start end)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (sp-kill-sexp))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun fengqi/define-key (keymap &rest bindings)
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(defun fengqi/upcase-previous-WORD ()
  "Make the WORD before cursor upper case."
  (interactive)
  (save-excursion
    (let ((end (point))
          (beg (1+ (re-search-backward "[^[:alnum:]-_]"))))
      (upcase-region beg end))))

(defmacro fengqi|???-region-or-symbol-at-point (name)
  (let ((new-func (intern (concat "fengqi/" name "-region-or-symbol-at-point")))
        (case-func (intern (concat name "-region"))))
    `(defun ,new-func ()
       ,(concat "Convert region or symbol at point to " name " case.")
       (interactive)
       (save-excursion
         (if (use-region-p)
             (,case-func (region-beginning) (region-end))
           (let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (end (car bounds))
                  (beg (cdr bounds)))
             (,case-func beg end)))))))

(fengqi|???-region-or-symbol-at-point "upcase")
(fengqi|???-region-or-symbol-at-point "downcase")

(defun fengqi/insert-current-buffer-name ()
  "Insert the full path file name into the current buffer.

See URL `https://unix.stackexchange.com/questions/45125/how-to-get-current-buffers-filename-in-emacs/243679'."
  (interactive)
  (insert (buffer-name)))
;; (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(defun fengqi/copy-current-buffer-name ()
  "Copy the name of current buffer."
  (interactive)
  (kill-new (buffer-name)))
;; (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(defun fengqi/buffer-contains-pattern? (pattern)
  "Check if current buffer contains string <pattern>.

See URL `https://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string'."
  (interactive "sPattern: ")
  (save-excursion
    (goto-char (point-min))
    (search-forward pattern nil t)))

(defun fengqi/set-compile-command ()
  "Set compile-command to compile current file."
  (interactive)
  (let ((library-opt  (concat
                       (when (fengqi/buffer-contains-pattern? "<gtest") " -lgtest -lgtest_main -lpthread")
                       (when (fengqi/buffer-contains-pattern? "BOOST_") " -lboost_unit_test_framework")
                       (when (fengqi/buffer-contains-pattern? "<fmt")   " -lfmt")))
        (static-opt   (if (fengqi/buffer-contains-pattern? "BOOST_") " -static" ""))
        (c++17-opt    (if (string= major-mode "c++-mode") " -std=c++17" ""))
        (file-name    (file-name-nondirectory (buffer-file-name)))
        (compiler-opt (cond ((string= major-mode "c++-mode")  "clang++ ")
                            ((string= major-mode "c-mode")    "clang ")
                            ((string= major-mode "java-mode") "javac ")
                            (t ""))))
    (when (not (string= compiler-opt ""))
      (setq compile-command (concat compiler-opt file-name c++17-opt library-opt static-opt)))))

(defun fengqi/move-current-buffer-file ()
  "Move current buffer file."
  (interactive)
  (let* ((filename-old (buffer-file-name))
         (buffer-old   (current-buffer))
         (dirname      (read-directory-name "Move to: "))
         (filename-new (concat dirname (file-name-nondirectory filename-old))))
    ;; (call-interactively 'write-file)
    (copy-file filename-old filename-new)
    (delete-file filename-old)
    (kill-buffer buffer-old)
    (find-file filename-new)
    (message "Move `%s' to `%s' successfully." filename-old filename-new)))

(defun fengqi/string-reverse (beg end)
  (interactive "r")
  (save-excursion
    (when (use-region-p)
      (kill-region beg end)
      (insert (string-reverse (substring-no-properties (car kill-ring)))))))

(defun fengqi/describe-buffer-file-coding-system ()
  (interactive)
  (message "%s" buffer-file-coding-system))

(defun fengqi/remove-highlight ()
  "Remove highlights putted by evil-search and symbol-overlay."
  (interactive)
  (evil-search-highlight-persist-remove-all)
  (symbol-overlay-remove-all))

(defun fengqi/delete-frame ()
  "Save some buffers before delete current frame."
  (interactive)
  (save-some-buffers nil t)
  (delete-frame))

(defun fengqi/untabify-region-or-buffer (&optional style)
  "Untabify region or buffer with `untabify' according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (untabify (region-beginning) (region-end) style)
          (message "Untabified region"))
      (progn
        (untabify (point-min) (point-max) style)
        (message "Untabified buffer %s" (buffer-name))))))

(defun fengqi/tabify-region-or-buffer (&optional style)
  "Tabify only leading spaces in region or buffer with `tabify' according to STYLE."
  (interactive)
  (save-excursion
    (let* ((tabify-regexp "^\t* [ \t]+"))
      (if (region-active-p)
          (progn
            (tabify (region-beginning) (region-end) style)
            (message "Tabified region"))
        (progn
          (tabify (point-min) (point-max) style)
          (message "Tabified buffer %s" (buffer-name)))))))

(defun fengqi/narrow-to-region-or-defun (&optional style)
  "Narrow to current region or defun with fancy-narrow."
  (interactive)
  (progn
    (unless fancy-narrow-mode (fancy-narrow-mode))
    (if (region-active-p)
        (fancy-narrow-to-region (region-beginning) (region-end))
      (fancy-narrow-to-defun))
    (deactivate-mark)))

(defun fengqi/widen (&optional style)
  "Widen built-in or fancy-narrowed region."
  (interactive)
  (if (fancy-narrow-active-p) (fancy-widen)
    (widen)))

(defun fengqi/dired-sort-other ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html' with some modifications."
  (interactive)
  (let (($sort-by (ido-completing-read "Sort by:" '("size" "date" "name" "directory-first"))))
    (cond ((equal $sort-by "size") (setq $arg "-alhS"))
          ((equal $sort-by "date") (setq $arg "-alht"))
          ((equal $sort-by "name") (setq $arg "-alh"))
          ((equal $sort-by "directory-first") (setq $arg "-alh --group-directories-first"))
          (t (error "logic error 09535")))
    (dired-sort-other $arg)))

(defun fengqi--osc52-send (content)
  "Send string using OSC 52."
  (send-string-to-terminal (concat "\e]52;c;" (base64-encode-string content t) "\07")))

(defun fengqi/kill-and-osc52-send (begin end)
  "Add selected text to kill ring and send by osc52."
  (interactive "r")
  (let ((content (buffer-substring-no-properties begin end)))
    (kill-new content)
    (fengqi--osc52-send content)
    (deactivate-mark)
    (message "osc52 sent: `%s'" content)))

(defmacro fengqi|eval-region-or-line-by-??? (name args)
  (let ((new-func (intern (concat "fengqi/eval-region-or-line-by-" name))))
    `(defun ,new-func ()
       ,(concat "Run region by " name)
       (interactive)
       (let* ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
              (end (if (use-region-p) (region-end) (line-end-position)))
              (cmd (buffer-substring-no-properties beg end))
              (exe (concat ,name " " ,args " '" cmd "'")))
         (message "eval: %s" exe)
         (shell-command exe)
         (deactivate-mark)))))

(fengqi|eval-region-or-line-by-??? "zsh" "-i -c")
(fengqi|eval-region-or-line-by-??? "bash" "-i -c")
(fengqi|eval-region-or-line-by-??? "python3" "-c")

(defun fengqi/generate-number-sequence (step)
  (interactive "P")
  (let ((start (string-to-number (read-string "Range start (1): " nil nil "1")))
        (end   (string-to-number (read-string "Range end (7): "   nil nil "7")))
        (step  (if step step 1))
        (separator (read-string "Separator ( ): " nil nil " ")))
    (message "[%d%s%d%s%d]" start separator step separator end)
    (or (and (> step 0) (< start end))
        (and (< step 0) (> start end))
        (user-error "Infinite sequence"))
    (insert (mapconcat 'number-to-string (number-sequence start end step) separator))))

(defun fengqi/join (sequence separator)
  "Concat sequence as string."
  (let ((fn (cond ((seq-empty-p sequence)           #'identity)
                  ((seq-every-p #'numberp sequence) #'number-to-string)
                  ((seq-every-p #'symbolp sequence) #'symbol-name)
                  ((seq-every-p #'stringp sequence) #'identity)
                  (t (user-error "Sequence of different type")))))
    (mapconcat fn sequence separator)))

(defun fengqi/qrencode-from-region-or-clipboard ()
  ;; I didn't use `shell-command-to-string' for I have to escape the characters
  ;; shell used if so. And shell-command-on-region wouldn't have this problem.
  (interactive)
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-kill 0))))
    (deactivate-mark)
    (with-temp-buffer
      (insert input)
      (shell-command-on-region (point-min) (point-max) "qrencode -t utf8" t t)
      (message "QR code for `%s':\n%s" input
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun fengqi/set-frame-position-width-height (x y w h)
  (let ((frame (selected-frame)))
    (set-frame-position frame x y)
    (set-frame-width frame w)
    (set-frame-height frame h)))

(defun fengqi/go ()
  (interactive)
  (fengqi/set-frame-position-width-height 1040 30 96 51))
