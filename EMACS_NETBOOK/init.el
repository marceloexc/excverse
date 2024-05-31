;; Emacs 29? EWS leverages functionality from the latest Emacs version.

(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires Emacs version 29 or later"))

;; Custom settings in a separate file and load the custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Load EWS functions

(load-file (concat (file-name-as-directory user-emacs-directory) "ews.el"))

;; Check for missing external software

(ews-missing-executables
 '("soffice" "zip" "pdftotext" "ddjvu"
   "curl"
   "dvipng"
   "dot"
   ("convert" "gm")
   "latex"
   "hunspell"
   ("grep" "ripgrep")
   ("gs" "mutool")
   "pdftotext"
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc")))

;;; LOOK AND FEEL
;; Keyboard-centric user interface removing tool, menu and scroll bars

;;(tool-bar-mode -1)
;;(menu-bar-mode -1)
;;(scroll-bar-mode -1)

;; Short answers only please

(setq use-short-answers t)

;; Spacious padding

(use-package spacious-padding
  :custom
  (line-spacing 3)
  :init
  (spacious-padding-mode 1))

;; Modus Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings '((1 . (1.2))
                           (2 . (1.1))
                           (t . (1.0))))
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  :init
  (load-theme 'modus-operandi-tinted :no-confirm)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t s" . modus-themes-select)))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; MINIBUFFER COMPLETION

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))
  
  (vertico-mouse-mode)

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

;;; Text mode settings

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

;; Check spelling with flyspell and hunspell

(use-package flyspell
  :custom
  (ispell-silently-savep t)
  (ispell-program-name "hunspell")
  (flyspell-default-dictionary "en_AU")
  (flyspell-case-fold-duplications t)
  (flyspell-issue-message-flag nil)
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

;;; Ricing Org mode

(use-package org
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}"))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))
   
   
   (setq org-preview-latex-default-process 'dvipng)
   (setq org-preview-latex-default-process 'dvipng)

;; Org modern: Most features disables for beginnng users

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil)
  (org-modern-keyword nil)
  (org-modern-timestamp nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-block-name nil)
  (org-modern-keyword nil)
  (org-modern-footnote nil)
  (org-modern-internal-target nil)
  (org-modern-radio-target nil)
  (org-modern-statistics nil)
  (org-modern-progress nil))

;; INSPIRATION

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Reading LibreOffice files
;; Fixing a bug in Org Mode pre 9.7
;; Org mode clobbers associations with office documents

(use-package ox-odt
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:OD[CFIGPST]\\|od[cfigpst]\\)\\'"
                 . doc-view-mode-maybe)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

(use-package citar
  :custom
  (org-cite-global-bibliography ews-bibtex-files)
  (citar-bibliography ews-bibtex-files)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (("C-c w b o" . citar-open)))

;; Use EWW
;; (setq browse-url-browser-function 'eww-browse-url)

;; Configure Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory
              (getenv "HOME"))
                 "Documents/elfeed.org"))))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System

(use-package emms
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("C-c w m b" . emms-browser)
   ("C-c w m e" . emms)
   ("C-c w m p" . emms-play-playlist )
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

;; Fleeting notes

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-default-notes-file
   (concat (file-name-as-directory ews-home-directory)
         "Documents/inbox.org"))
  (org-capture-bookmark nil)

;; Capture templates

(org-capture-templates
 '(("f" "Fleeting note"
    item
    (file+headline org-default-notes-file "Notes")
    "- %?")
   ("p" "Permanent note" plain
    (file denote-last-path)
    #'denote-org-capture
    :no-save t
    :immediate-finish nil
    :kill-buffer t
    :jump-to-captured t)
   ("t" "New task" entry
    (file+headline org-default-notes-file "Tasks")
    "* TODO %i%?"))))

;; Denote

(use-package denote
  :custom
  (denote-sort-keywords t)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c w d b" . denote-find-backlink)
   ("C-c w d d" . denote-date)
   ("C-c w d f" . denote-find-link)
   ("C-c w d i" . denote-link-or-create)
   ("C-c w d I" . denote-org-extras-dblock-insert-links)
   ("C-c w d k" . denote-keywords-add)
   ("C-c w d K" . denote-keywords-remove)
   ("C-c w d n" . denote)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

;; Consult-Notes for easy access

(use-package consult-notes
  :custom
  (consult-narrow-key ":")
  (consult-notes-file-dir-sources
   `(("Denote Notes"  ?n ,denote-directory)))
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-c w f" . consult-notes)
   ("C-c w g" . consult-notes-search-in-all-notes)))

;; Citar-Denote to manage literature notes

(use-package citar-denote
  :demand t
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)))

;; Explore and manage your Denote collection

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-keywords-barchart)
   ("C-c w x x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x r" . denote-explore-rename-keywords)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-degree-barchart)))

;; Set some Org mode shortcuts

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Distraction-free writing

(use-package olivetti
  :bind
  (("C-c w o" . ews-olivetti)))

;; Undo Tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualize)))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-csl-styles-dir ews-bibtex-directory
      org-cite-export-processors
      '((latex natbib "apalike2" "authoryear")
        (t     csl    "apa6.csl")))

;; Lookup words in online dictionary

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

;; Writegood-Mode for buzzwords, passive writing and repeated word word detection

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

;; ediff

(setq ediff-keep-variants nil
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Generic Org Export Settings

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; LaTeX templates

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("crc"
     "\\documentclass[krantz2]{krantz}
        \\usepackage{lmodern}
        \\usepackage[authoryear]{natbib}
        \\usepackage{nicefrac}
        \\usepackage[bf,singlelinecheck=off]{caption}
        \\captionsetup[table]{labelsep=space}
        \\captionsetup[figure]{labelsep=space}
        \\usepackage{Alegreya}
        \\usepackage[scale=.8]{sourcecodepro}
        \\usepackage[breaklines=true]{minted}
        \\usepackage{rotating}
        \\usepackage[notbib, nottoc,notlot,notlof]{tocbibind}
        \\usepackage{amsfonts, tikz, tikz-layers}
        \\usetikzlibrary{fadings, quotes, shapes, calc, decorations.markings}
        \\usetikzlibrary{patterns, shadows.blur}
        \\usetikzlibrary{shapes,shapes.geometric,positioning}
        \\usetikzlibrary{arrows, arrows.meta, backgrounds}
        \\usepackage{imakeidx} \\makeindex[intoc]
        \\renewcommand{\\textfraction}{0.05}
        \\renewcommand{\\topfraction}{0.8}
        \\renewcommand{\\bottomfraction}{0.8}
        \\renewcommand{\\floatpagefraction}{0.75}
        \\renewcommand{\\eqref}[1]{(Equation \\ref{#1})}
        \\renewcommand{\\LaTeX}{LaTeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\paragraph*{%s}"))))

(use-package ox-epub
  :demand t)

;; ADVANCED NDOCUMENTED EXPORT SETTINGS FOR EWS

;; Use GraphViz for flow diagrams
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

;;; ADMINISTRATION

;; Bind org agenda command

;; (use-package org
;;  :bind
;;  (("SPC o A" . org-agenda)))

;; FILE MANAGEMENT

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide hidden files

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      version-control t
      delete-old-versions t
      create-lockfiles nil)  ; No lock files

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  (run-at-time nil (* 5 60)
               (lambda () (let ((save-silently t))
                            (recentf-save-list))))
  :custom
  (recentf-max-saved-items 50))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r D" . bookmark-delete))
  
;; evil

(use-package evil :ensure t
  :config
  (evil-mode 1))


(use-package evil-leader :ensure t
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>") 
  (evil-leader/set-key "b b" 'buffer-list)
  (evil-leader/set-key "v t" 'transpose-lines)
  ;; Recent Files
  (evil-leader/set-key "f r" 'recentf-open) ; You might need to install helm package
                                        ;
                                          ;; (("SPC f r" . recentf-open)))

  ;; Find Files
  (evil-leader/set-key "f f" 'find-file)
  ;; Save File
  (evil-leader/set-key "f s" 'save-buffer))


(global-display-line-numbers-mode)



(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(setq centaur-tabs-height 24)

(setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)




;; general keybindings
;;
(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
  ;; doomesque hotkeys using spacebar as prefix
  (my-leader-def
    :states '(motion normal visual)
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    "C-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "S-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    ">" '(find-file-other-frame :which-key "Find file (new frame)")
    ;":" '(execute-extended-command :which-key "M-x")
    "x" '(open-scratch-buffer :which-key "Open scratch buffer")
    "d" '(dired-jump :which-key "dired-jump")
    "/" '(+consult/ripgrep :which-key "+consult/ripgrep")
    "?" '(consult-ripgrep :which-key "consult-ripgrep")
    ;"[" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
    ;"]" '(+tab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
    "v" '(vterm-toggle :which-key "+term/here")


    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(vundo :which-key "vundo")
    "ev" '(vundo :which-key "vundo")
    "er" '(query-replace :which-key "query-replace")
    ;"ec" '(consult-theme :which-key "consult-theme")
    "ep" '(point-to-register :which-key "point-to-register")
    "es" '(consult-register-store :which-key "consult-register-store")
    "ej" '(jump-to-register :which-key "jump-to-register")
    "ef" '(:ignore t :which-key "Fold")
    "efh" '(hs-hide-block :which-key "hs-hide-block")
    "efs" '(hs-show-block :which-key "hs-show-block")
    "efa" '(hs-show-all :which-key "hs-show-all")

    ;; consult
    "c" '(:ignore t :which-key "consult")
    ;"cf" '(consult-flycheck :which-key "consult-flycheck")
    "cf" '(consult-flymake :which-key "consult-flymake")

;; file
;;
    "f" '(:ignore t :which-key "File")
    "ff" '(find-file :which-key "find-file")
    "fr" '(recentf-open :which-key "recentf-open")


    ;; buffer
    ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "consult-buffer")
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "br" '(revert-buffer-no-confirm :which-key "Revert buffer")
    "bK" '(kill-all-buffers :which-key "Kill all buffers")

    ;; open
    "o" '(:ignore t :which-key "Open")
    "oc" '(open-init-file :which-key "Open init.el")

    ;; project
    "p" '(:ignore t :which-key "Project")
    "pp" '(projectile-switch-project :which-key "Switch Project")
    "po" '(projectile-find-other-file :which-key "projectile-find-other-file")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(helpful-callable :which-key "describe-function")
    "hk" '(helpful-key :which-key "describe-key")
    "hv" '(helpful-variable :which-key "describe-variable")
    "ho" '(helpful-symbol :which-key "describe-symbol")
    "hm" '(describe-mode :which-key "describe-mode")
    "hF" '(describe-face :which-key "describe-face")
    "hw" '(where-is :which-key "where-is")
    "h." '(display-local-help :which-key "display-local-help")

    ;; zoom
    ;; the hydra is nice but the rest is kind of janky, need to play around with this more
    "=" '(text-scale-increase :which-key "text-scale-increase")
    "-" '(text-scale-decrease :which-key "text-scale-decrease")
    "z" '(:ignore t :which-key "zoom")
    "z=" '(zoom-in :which-key "zoom-in")
    "z-" '(zoom-out :which-key "zoom-out")
    "zz" '(hydra-zoom/body :which-key "hydra-zoom")

    ;; window
    "w" '(:ignore t :which-key "Window")
    "wd" '(evil-window-delete :which-key "Delete Window ")

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ta" '(corfu-mode :which-key "corfu-mode") ;; 'a' for autocomplete
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    "tf" '(flyspell-mode :which-key "flyspell-mode")
    ;"tc" '(flycheck-mode :which-key "flycheck-mode")
    "tc" '(flymake-mode :which-key "flymake-mode")
    "tm" '(minimap-mode :which-key "minimap-mode")
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

;; org
;;
    "o" ('ignore t :which-key "Org")
    "oA" (org-agenda :which-key "org-agenda")


    ;; notes/org
    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "find-node")
    "ni" '(org-roam-node-insert :which-key "insert-node")
    "nt" '(org-roam-dailies-goto-today :which-key "org-roam-dailies-goto-today")
    "n/" '(+consult/org-roam-ripgrep :which-key "+consult/org-roam-ripgrep")
    "na" '(org-agenda :which-key "org-agenda")

    ;; narrow
    "N" '(:ignore t :which-key "Narrow")
    "Nr" '(narrow-to-region :which-key "narrow-to-region")
    "Nw" '(widen :which-key "widen")

    ;; tabs
    ;; "TAB" '(:ignore t :which-key "Tabs")
    ;; "TAB TAB" '(tab-bar-switch-to-tab :which-key "tab-bar-switch-to-tab")
    ;; "TAB [" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
    ;; "TAB ]" '(t+ab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
    ;; "TAB n" '(+tab-bar/add-new :which-key "+tab-bar/add-new")
    ;; "TAB k" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    ;; "TAB d" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    ;; "TAB K" '(+tab-bar/close-all-tabs-except-current :which-key "+tab-bar/close-all-tabs-except-current")
    ;; "TAB r" '(tab-rename :which-key "tab-rename")
