;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers '(
     emacs-lisp
     git github version-control
     latex markdown
     spell-checking syntax-checking
     erc
     dash
     extra-langs
     spotify
     elm agda racket rust purescript
     themes-megapack


     (org :variables
          org-enable-github-support t)

     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t
                 evil-snipe-scope 'visible)

     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)

     (haskell :variables
              haskell-enable-hindent-style "johan-tibell"
              haskell-process-type 'stack-ghci
              haskell-tags-on-save t
              haskell-process-use-presentation-mode t
              haskell-process-suggest-haskell-docs-imports nil)

     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-snippets-in-popup t))

   dotspacemacs-additional-packages '(helm-pages
                                      focus
                                      cdlatex
                                      typing
                                      annotate
                                      ox-reveal
                                      2048-game
                                      dired-narrow
                                      outshine
                                      deferred
                                      hl-sentence
                                      visual-regexp-steroids
                                      processing-mode
                                      togetherly)
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs
   initialization before layers configuration. You should not put
   any user code in there besides modifying the variable values."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists nil
   dotspacemacs-themes '(spacemacs-light spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 21
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   )
  )

(defun dotspacemacs/user-init ()
  (setq-default
   avy-all-windows 'all-frames)

  ;; This is important to prevent spacemacs freezing at startup
  ;; Check out spacemacs FAQ on github
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (defvar outline-minor-mode-prefix "\M-#")

  )

(defun dotspacemacs/user-config ()

  ;; Initial scratch buffer should be org-mode
  (setq dotspacemacs-scratch-mode 'org-mode)

  ;; Haskell configuration
  (setq-default flycheck-disabled-checkers '(haskell-ghc haskell-stack-ghc haskell-hlint))
  (push 'haskell-mode spacemacs-indent-sensitive-modes)

  (setq haskell-process-suggest-remove-import-lines nil
        haskell-process-suggest-hoogle-imports      nil
        haskell-interactive-popup-errors            nil
        ;; ghc-report-errors                           nil
        flycheck-indication-mode                    nil
        flycheck-ghc-args (quote ("-fno-warn-type-defaults")))

  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
      (delete-region (line-beginning-position) (point))))

  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice)

  (defun meditans/compile-and-switch ()
    (interactive)
    (haskell-interactive-mode-clear)
    (haskell-process-load-file)
    (haskell-interactive-bring)
    (evil-goto-first-line))

  (defun meditans/switch-and-maximize ()
    (interactive)
    (bury-buffer)
    (spacemacs/alternate-buffer)
    (spacemacs/toggle-maximize-buffer))

  (global-set-key (kbd "<f7>") 'meditans/compile-and-switch)
  (global-set-key (kbd "<f8>") 'meditans/switch-and-maximize)


  ;; Togetherly configuration
  (require 'togetherly)

  ;; Page Break Mode configuration
  (push 'haskell-mode page-break-lines-modes)
  (global-page-break-lines-mode)

  ;; Golden Ratio configuration
  (spacemacs/toggle-golden-ratio-on)

  ;; Eshell configuration
  (defun eshell/clear () "Clear the eshell buffer."
    (let ((inhibit-read-only t)) (erase-buffer)))

  (defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
    (setq command (file-truename command)))


  ;; Erc configuration
  (setq erc-hide-list '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
  (setq erc-nick "meditans")
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-autojoin-channels-alist
        '(("freenode.net"
           "#haskell.it"
           "##narrative-ai"
           "#haskell"
           "#haskell-lens"
           "#haskell-stack"
           "#haskell-ide-engine"
           "#org-mode"
           "##typetheory")))

  ;; LaTeX configuration
  (setq TeX-save-query         nil
        preview-scale-function 2.0)

  ;; Babel configuration
  (org-babel-do-load-languages
    (quote org-babel-load-languages)
    (quote ((emacs-lisp . t)
            (calc . t)
            (sh . t)
            (lilypond . t))))

  ;; helm-dash configuration
  (defun meditans/helm-dash-delete-docsets ()
    "Cancella il contenuto della variabile helm-dash-common-docsets"
         (interactive)
         (setq helm-dash-common-docsets ()))

  ;; My keybinding for closing parenthesis, and enabling automatic $$ completion
  (define-key evil-insert-state-map "\C-l" 'sp-forward-sexp)
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\[" "\]")

  ;; Org mode config
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'smartparens-mode)
  ;; (setq org-babel-load-languages '((emacs-lisp . t) (calc . t) (haskell . t) (lilypond . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (calc . t)
                                                           (haskell . t)
                                                           (lilypond . t)))
  (setq org-src-fontify-natively t)

  ;; Org mode config for latex code snippets highlighting.
  ;; Comes from http://praveen.kumar.in/2012/03/10/org-mode-latex-and-minted-syntax-highlighting/

  (require 'ox-latex)
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Outshine config
  (require 'outshine)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (setq outshine-use-speed-commands t
        outshine-preserve-delimiter-whitespace t)
  (add-hook 'prog-mode-hook 'outline-minor-mode)

  (defun meditans/outshine-edit ()
    (interactive)
    (outline-previous-heading)
    (outorg-edit-as-org)
    (spacemacs/toggle-maximize-buffer)
    (outorg-save-edits-to-tmp-file)
    (org-toggle-latex-fragment '(16)))

  (global-set-key (kbd "<f10>") 'meditans/outshine-edit)

  ;; Habitica config

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'load-path "/home/carlo/.emacs.d/private/habitrpg.el")
  (require 'habitrpg)
  (setq habitrpg-api-user "9835d4c7-7d78-49f0-8f3e-b3dd7036488b")
  (setq habitrpg-api-token "4f816fc3-0d02-44cb-837d-fc7f25eb943c")

  ;; Global visual-line mode
  (global-visual-line-mode)
  ;; Voglio che, essendo attivo ~global-visual-line-mode~, i tasti di movimento di vim si spostino seguendo le linee visuali, anziche' quelle logiche.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Highlight sentence configuration
  ;; (require 'hl-sentence)
  ;; (set-face-attribute 'hl-sentence-face nil
  ;;                     :foreground "#444")

  ;; Qualche riga di configurazione per processing, presa da
  ;; https://github.com/ptrv/processing2-emacs
  (setq processing-location "/home/carlo/code/arduino/engine/processing/processing-java")
  (setq processing-application-dir "/home/carlo/code/arduino/engine/processing/processing")
  (setq processing-sketchbook-dir "/home/carlo/code/arduino/processing")
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cdlatex-math-modify-alist (quote ((66 "\\pmb" nil t nil nil))))
 '(cdlatex-math-symbol-alist
   (quote
    ((46
      ("\\cdot" "\\circ"))
     (40
      ("\\langle" "\\llbracket"))
     (41
      ("\\rangle" "\\rrbracket")))))
 '(org-agenda-files (quote ("~/doc/agenda.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-latex-listings-langs
   (quote
    ((emacs-lisp "Lisp")
     (lisp "Lisp")
     (clojure "Lisp")
     (c "C")
     (cc "C++")
     (fortran "fortran")
     (perl "Perl")
     (cperl "Perl")
     (python "Python")
     (ruby "Ruby")
     (html "HTML")
     (xml "XML")
     (tex "TeX")
     (latex "[LaTeX]TeX")
     (shell-script "bash")
     (gnuplot "Gnuplot")
     (ocaml "Caml")
     (caml "Caml")
     (sql "SQL")
     (sqlite "sql")
     (makefile "make")
     (haskell "Haskell"))))
 '(org-latex-packages-alist
   (quote
    (("" "amscd" t)
     ("" "stmaryrd" t)
     ("" "bussproofs" t))))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
