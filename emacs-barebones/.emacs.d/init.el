;;; init.el --- Basic Emacs configuration for Emacs 24.2.1

;; Basic settings
(setq make-backup-files nil)  ; Disable backups for old filesystem compatibility
(setq auto-save-default nil)
(show-paren-mode 1)           ; Show matching parentheses
(global-font-lock-mode 1)     ; Syntax highlighting

;; Basic CUA-like keybindings
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; Simplified key bindings
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)

;; Basic window management
(global-set-key (kbd "C-x 2") 'split-window-vertically)
(global-set-key (kbd "C-x 3") 'split-window-horizontally)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)

;; Basic font configuration
(when (member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :family "Monaco" :height 120))
;; Simple text editing setup
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sentence-end-double-space nil)

;; Basic dired configuration
(require 'dired)
(setq dired-listing-switches "-alh")

;; require recentf
(recentf-mode)

;; Simple theme setup
(load-theme 'tsdh-dark) ;;built-in theme that should be available

;; Line numbers
(global-linum-mode 1)

;; Easy way to close emacs window using the mouse

(defun my/close-clicked-window (event)
  "Close the window that was clicked on."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (delete-window window))))

(defvar my/close-window-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/close-clicked-window)
    map)
  "Keymap for the close window button in the mode line.")

(defun my/close-window-modeline-construct ()
  "Construct a clickable [×] for the mode line to close windows.
Only appears when there are multiple windows in the frame."
  (if (> (count-windows) 1)
      (propertize "[ x ]"
                  'local-map my/close-window-modeline-map
                  'mouse-face 'mode-line-highlight
                  'face '(:foreground "red" :height 150)
                  'help-echo "Click to close this window")
    ""))

;; Insert at the beginning of the mode-line-format
(setq-default mode-line-format
              (cons '(:eval (my/close-window-modeline-construct))
                    mode-line-format))

;; Force update of all mode lines
(force-mode-line-update t)


;; some org settings
(setq org-src-fontify-natively t
      org-pretty-entities t
      org-src-preserve-indentation nil
      org-fontify-whole-heading-line t)


;; TODO - not working for now - search buffer interactive

(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.
If a selection is active and multi-line, perform a search restricted to that
region.
If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (if (and (featurep 'vertico)
               (fboundp 'consult-line))
          (if (and start end (not multiline-p))
              (consult-line
               (replace-regexp-in-string
                " " "\\\\ "
                (rxt-quote-pcre
                 (buffer-substring-no-properties start end))))
            (call-interactively #'consult-line))))))


;; Define a prefix key map for "M-a"
(define-prefix-command 'my-prefix-map)
(global-set-key (kbd "M-a") 'my-prefix-map)

;; Top-level bindings
(define-key my-prefix-map (kbd "d") 'dired)
(define-key my-prefix-map (kbd "b") 'ido-switch-buffer)

;; File submenu
(define-prefix-command 'my-file-map)
(define-key my-prefix-map (kbd "f") 'my-file-map)
(define-key my-file-map (kbd "f") 'find-file)
(define-key my-file-map (kbd "r") 'recentf-open-files)

;; Notes submenu
(define-prefix-command 'my-notes-map)
(define-key my-prefix-map (kbd "n") 'my-notes-map)
(define-key my-notes-map (kbd "s") 'org-edit-src-code)
(define-key my-notes-map (kbd "d") 'org-deadline)
(define-key my-notes-map (kbd "e") 'org-emphasize)
(define-key my-notes-map (kbd "l") 'org-insert-link)

;; Restart submenu
(define-prefix-command 'my-restart-map)
(define-key my-prefix-map (kbd "r") 'my-restart-map)
(define-key my-restart-map (kbd "r") 'restart-emacs)

;; Window submenu
(define-prefix-command 'my-window-map)
(define-key my-prefix-map (kbd "w") 'my-window-map)
(define-key my-window-map (kbd "d") 'delete-window)
(define-key my-window-map (kbd "a") 'split-window-right)
(define-key my-window-map (kbd "s") 'split-window-below)
(define-key my-window-map (kbd "w") 'other-window)
