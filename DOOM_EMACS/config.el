;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 17))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;;
(custom-theme-set-faces
     'user
     `(org-level-3 ((t (:size 17))))
     `(org-level-2 ((t (:height 1.05))))
     `(org-level-1 ((t (:height 1.1)))))

;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 18))
;; (set-face-attribute 'fixed-pitch nil :family "IBM 3270" :height 160)
;; (set-face-attribute 'variable-pitch nil :family "Avenir" :height 160)
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default
;;
;;
;; (setq doom-theme 'doom-sourcerer)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;
;

(defun my/mac-handle-application-effective-appearance-change (_event)
  (interactive "e")
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    (cond ((equal appearance "NSAppearanceNameAqua")
           (load-theme light-mode-theme))
          ((equal appearance "NSAppearanceNameDarkAqua")
           (load-theme dark-mode-theme)))))

;; Key mapping definition
(define-key mac-apple-event-map [application-kvo effectiveAppearance]
  'my/mac-handle-application-effective-appearance-change)

;; Call the function after init to set the theme at startup
(add-hook 'after-init-hook
          (lambda ()
            (my/mac-handle-application-effective-appearance-change nil)))

;; (setq doom-theme 'doom-salmon)
;; (use-package ultra-scroll-mac
;;   :if (eq window-system 'mac)
;;   :init
;;   (setq scroll-conservatively 101) ; important for jumbo images
;;   :config
;;   (ultra-scroll-mac-mode 1))
(setq fancy-splash-image (concat doom-user-dir "emacs.svg"))


;; set word count
(setq doom-modeline-enable-word-count t)

;; disable hl-line mode
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(setq org-modern-label-border nil)

(setq ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-modern-todo-faces '(("PROJ" :background "gray" :foreground "black"))

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")
(global-org-modern-mode)



(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
(setq olivetti-body-width 130)
(dolist (f '(prog-mode-hook org-mode-hook text-mode-hook))
 (add-hook f #'olivetti-mode))

(use-package! org-fragtog
:after org
:hook (org-mode . org-fragtog-mode); this auto-enables it when you enter an org-buffer, remove if you do not want this
:config
;; whatever you want
)



(use-package svg-tag-mode
  :commands svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                  nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))
  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                  (svg-tag-make tag
                                                                :end -1
                                                                :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
           (,(format "\\(\\[%s\\]\\)" date-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
           (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
           (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))


;; (after! org
;; (setq org-ellipsis " ▾ ")
;;   (appendq! +ligatures-extra-symbols
;;           `(:checkbox      "☐"
;;             :pending       "◼"
;;             :checkedbox    "☑"
;;             :list_property "∷"
;;             :em_dash       "—"
;;             :ellipses      "…"
;;             :arrow_right   "→"
;;             :arrow_left    "←"
;;             :title        " "
;;             :subtitle      "𝙩"
;;             :author        "𝘼"
;;             :date          "𝘿"
;;             :property      "⏻"
;;             :options       "⌥"
;;             :startup       ""
;;             :macro         "𝓜"
;;             :html_head     "🅷"
;;             :html          "🅗"
;;             :latex_class   "🄻"
;;             :latex_header  "🅻"
;;             :beamer_header "🅑"
;;             :latex         "🅛"
;;             :attr_latex    "🄛"
;;             :attr_html     "🄗"
;;             :attr_org      "⒪"
;;             :begin_quote   "❝"
;;             :end_quote     "❞"
;;             :caption       "☰"
;;             :header        "›"
;;             :results       "🠶"
;;             :begin_export  "⏩"
;;             :end_export    "⏪"
;;             :properties    "⏻"
;;             :end           "∎"
;;             :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
;;             :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
;;             :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
;;             :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
;;             :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
;;             :roam_tags nil
;;             :filetags nil))
;; (set-ligatures! 'org-mode
;;   :merge t
;;   :checkbox      "[ ]"
;;   :pending       "[-]"
;;   :checkedbox    "[X]"
;;   :list_property "::"
;;   :em_dash       "---"
;;   :ellipsis      "..."
;;   :arrow_right   "->"
;;   :arrow_left    "<-"
;;   :title         "#+title:"
;;   :subtitle      "#+subtitle:"
;;   :author        "#+author:"
;;   :date          "#+date:"
;;   :property      "#+property:"
;;   :options       "#+options:"
;;   :startup       "#+startup:"
;;   :macro         "#+macro:"
;;   :html_head     "#+html_head:"
;;   :html          "#+html:"
;;   :latex_class   "#+latex_class:"
;;   :latex_header  "#+latex_header:"
;;   :beamer_header "#+beamer_header:"
;;   :latex         "#+latex:"
;;   :attr_latex    "#+attr_latex:"
;;   :attr_html     "#+attr_html:"
;;   :attr_org      "#+attr_org:"
;;   :begin_quote   "#+begin_quote"
;;   :end_quote     "#+end_quote"
;;   :caption       "#+caption:"
;;   :header        "#+header:"
;;   :begin_export  "#+begin_export"
;;   :end_export    "#+end_export"
;;   :results       "#+RESULTS:"
;;   :property      ":PROPERTIES:"
;;   :end           ":END:"
;;   :priority_a    "[#A]"
;;   :priority_b    "[#B]"
;;   :priority_c    "[#C]"
;;   :priority_d    "[#D]"
;;   :priority_e    "[#E]"
;;   :roam_tags     "#+roam_tags:"
;;   :filetags      "#+filetags:")
;; (plist-put +ligatures-extra-symbols :name "⁍")
;; )

(tool-bar-mode 1)
(vertico-mouse-mode 1)


(setq org-download-image-org-width 400)

(setq org-download-image-html-width 400)



;; for latex editing
(setq +latex-viewers '(pdf-tools))
(defun my/resize-org-latex-overlays ()
  (cl-loop for o in (car (overlay-lists))
     if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
     do (plist-put (cdr (overlay-get o 'display))
           :scale (expt text-scale-mode-step
                text-scale-mode-amount))))
(use-package org
:hook (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t))))

(require 'ef-themes)

(require 'org-download)

;; org-roam package and settings:
(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "jethro/org-capture-slipbox" "<tab>" #'jethro/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (defun jethro/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))


(after! org
  (map! :leader
        :prefix "e"
        :desc "org-edit-latex-fragment" "l" #'org-edit-latex-fragment))


(setq centaur-tabs-height 16)
