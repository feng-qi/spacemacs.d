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

(defun fengqi/define-key-for-keymaps (bindings-list)
  (while bindings-list
    (eval `(fengqi/define-key ,@(pop bindings-list)))))

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
(fengqi|???-region-or-symbol-at-point "capitalize")

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
        (file-name    (file-name-nondirectory (buffer-file-name)))
        (compiler-opt (cond ((string= major-mode "c++-mode")  "clang++ ")
                            ((string= major-mode "c-mode")    "clang ")
                            ((string= major-mode "java-mode") "javac ")
                            (t ""))))
    (when (not (string= compiler-opt ""))
      (setq compile-command (concat compiler-opt static-opt file-name library-opt)))))

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
    (if (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
      (narrow-to-defun))
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
  (let* ((sort-by (ivy-read "Sort by:" '("name" "date" "origin" "size" "directory-first")))
         (arg (cond ((equal sort-by "size") "-alhS")
                    ((equal sort-by "date") "-alht")
                    ((equal sort-by "name") "-alh")
                    ((equal sort-by "origin") "-al")
                    ((equal sort-by "directory-first") "-alh --group-directories-first")
                    (t (error "logic error 09535")))))
    (dired-sort-other arg)))

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
              (exe (concat ,name " " ,args " '" cmd "'"))
              ;; use bash for async-shell-command, zsh handles differently for
              ;;     sed -n -e 's/#.*//' -e "3 p" ./release-tests.txt
              (shell-file-name "/bin/bash"))
         (message "eval: %s" exe)
         (async-shell-command exe)
         (deactivate-mark)))))

(fengqi|eval-region-or-line-by-??? "zsh" "-c")
(fengqi|eval-region-or-line-by-??? "bash" "-c")
(fengqi|eval-region-or-line-by-??? "python3" "-c")

(with-eval-after-load 'evil

  (evil-define-operator fengqi/show-different-radix (beg end type)
    (interactive "<R>")
    (require 'calc-bin)
    (let ((num-str (buffer-substring beg end)))
      (if (eq type 'block)
          (message "Execute on block not supported yet.")
        (let* ((value (string-to-number (calc-eval num-str)))
               (twos-complement (string-to-number (substring (math-format-twos-complement value) 4)))
               (decimal (math-format-radix value))
               (cdecimal (math-format-radix twos-complement))
               (binary (math-format-binary value))
               (cbinary (math-format-binary twos-complement))
               (calc-number-radix 8)
               (octal (math-format-radix value))
               (coctal (math-format-radix twos-complement))
               (calc-number-radix 16)
               (chex (math-format-radix twos-complement))
               (hex (math-format-radix value)))
          (forward-line 1)
          (insert (format "bin: %32s\ndec: %32s\noct: %32s\nhex: %32s\ncbin:%32s\ncdec:%32s\ncoct:%32s\nchex:%32s\n"
                          binary decimal octal hex cbinary cdecimal coctal chex))))))

  (evil-define-operator fengqi/eshell-command (beg end type)
    (interactive "<R>")
    (let ((command (buffer-substring beg end)))
      (if (eq type 'block)
          (message "Execute on block not supported yet.")
        (progn (message "eshell-command: %s" command)
               (eshell-command command)))))

  (evil-define-operator fengqi/calc-eval (beg end type)
    (interactive "<R>")
    (save-excursion
      (let ((expression (concat "evalv(" (buffer-substring beg end) ")")))
        (if (eq type 'block)
            (message "Eval on block not supported yet.")
          (let ((result (calc-eval expression)))
            (kill-new result)
            (forward-line 1)
            (message "%s\n= %s" expression result)
            (insert (format "= %s\n" result))))))))

(with-eval-after-load 'term
  (defun fengqi/toggle-term-char-or-line-mode ()
    (interactive)
    (if (term-in-char-mode)
        (term-line-mode)
      (term-char-mode)))

  (define-key term-mode-map (kbd "C-c C-j") #'fengqi/toggle-term-char-or-line-mode)
  (define-key term-mode-map (kbd "C-c C-k") #'fengqi/toggle-term-char-or-line-mode)
  (define-key term-raw-map (kbd "C-c C-j") #'fengqi/toggle-term-char-or-line-mode)
  (define-key term-raw-map (kbd "C-c C-k") #'fengqi/toggle-term-char-or-line-mode))

(defun fengqi/bing-dict-search ()
  (interactive)
  (if (region-active-p)
      (bing-dict-brief (string-trim (evil-get-selection)))
    (bing-dict-brief-at-point)))

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

(defun fengqi/toggle-lsp-signature-render-documentation ()
  "Toggle lsp mode for showing details of function signature or not."
  (interactive)
  (setq lsp-signature-render-documentation (not lsp-signature-render-documentation))
  (message "lsp-signature-render-documentation %s"
           (if lsp-signature-render-documentation "enabled" "disabled")))

(defun fengqi/set-frame-position-width-height (x y w h)
  (let ((frame (selected-frame)))
    (set-frame-position frame x y)
    (set-frame-width frame w)
    (set-frame-height frame h)))

(defun fengqi/go ()
  (interactive)
  (fengqi/set-frame-position-width-height 1040 30 96 51))

(defun bing-dict-brief-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties)))
    (message "bing dict fetching `%s'..." word)
    (bing-dict-brief word)))

(defun fengqi/dired-toggle-read-only-in-evil-normal ()
  (interactive)
  (progn (dired-toggle-read-only) (evil-normal-state)))

(defun the-fastest-elpa-mirror ()
  (interactive)
  (require 'chart)
  (let* ((urls (mapcar (lambda (part) (concat "http://" part "archive-contents"))
                       '("melpa.org/packages/"
                         "www.mirrorservice.org/sites/melpa.org/packages/"
                         "emacs-china.org/melpa/"
                         "mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"
                         "mirrors.163.com/elpa/melpa/"
                         "mirrors.cloud.tencent.com/elpa/melpa/")))
         (durations (mapcar (lambda (url)
                              (let ((start (current-time)))
                                (message "Fetching %s" url)
                                (call-process "curl" nil nil nil "--max-time" "10" url)
                                (float-time (time-subtract (current-time) start))))
                            urls)))
    (chart-bar-quickie
     'horizontal
     "The fastest elpa mirror"
     (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "Elpa"
     (mapcar (lambda (d) (* 1e3 d)) durations) "ms")
    (message "%s" durations)))

(defun fengqi/calc-on-rectangle ()
  (interactive)
  (let* ((values (progn (copy-rectangle-to-register 255 (region-beginning) (region-end))
                        (mapcar 'string-to-number (get-register 255))))
         (maxv (apply 'max values))
         (minv (apply 'min values))
         (sum (apply '+ values))
         (average (/ sum (length values))))
    (message "max: %f, min: %f, sum: %f, average: %f" maxv minv sum average)))

(defun fengqi/touch-file-now (file)
  (let ((touch-cmd (concat "touch " (shell-quote-argument (expand-file-name file)))))
    (shell-command touch-cmd)
    (message "touched: %s" file)))

(defun fengqi/touch-current-buffer-file-now ()
  (interactive)
  (fengqi/touch-file-now (if (derived-mode-p 'dired-mode)
                             (dired-get-file-for-visit)
                           buffer-file-name)))

(defun fengqi/async-run-file (file)
  (async-shell-command file))

(defun fengqi/ivy-rg-find-file-in-project ()
  "Forward to `describe-function'."
  (interactive)
  (ivy-read "Find file in ~/.spacemacs.d: "
            (split-string
             (shell-command-to-string
              (format "cd %s && rg --files --null" (shell-quote-argument (expand-file-name "~/.spacemacs.d"))))
             "\0")
            :keymap counsel-describe-map
            ;; :preselect (ivy-thing-at-point)
            ;; :history 'fengqi/ivy-rg-find-file-in-project-history
            ;; :require-match t
            :action (lambda (x) (find-file (expand-file-name x "~/.spacemacs.d")))
            :caller 'fengqi/ivy-rg-find-file-in-project))

(defun fengqi/magit-status-add-dotfiles-config ()
  (interactive)
  (require 'magit)
  (let ((git-dir (concat "--git-dir=" (expand-file-name "~/dotfiles")))
        (worktree (concat "--work-tree=" (expand-file-name "~"))))
    (add-to-list 'magit-git-global-arguments worktree)
    (add-to-list 'magit-git-global-arguments git-dir)
    (call-interactively 'magit-status)))

(defun fengqi/magit-status-remove-dotfiles-config ()
  (interactive)
  (require 'magit)
  (let ((git-dir (concat "--git-dir=" (expand-file-name "~/dotfiles")))
        (worktree (concat "--work-tree=" (expand-file-name "~"))))
    (setq magit-git-global-arguments (remove worktree (remove git-dir magit-git-global-arguments)))
    (message "magit dotfiles configuration removed")))

(defun fengqi/show-hide-async-shell-command-window ()
  "Show/Hide the window containing the Async Shell Command buffer."
  (interactive)
  (when-let ((buffer (get-buffer "*Async Shell Command*")))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (pop-to-buffer buffer))))

(defun fengqi/search-notes ()
  (interactive)
  (require 'counsel)
  (let ((initial-input (when (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    (counsel-rg initial-input notes-default-directory nil
                (format "rg from [%s]: " notes-default-directory))))

(defun fengqi/swiper ()
  (interactive)
  (let ((initial-input (when (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    (swiper initial-input)))
