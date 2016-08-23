;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers '(
     emacs-lisp
     git github version-control
     latex markdown
     erc gnus
     dash
     extra-langs
     spotify
     elm agda racket rust purescript yaml
     themes-megapack
     syntax-checking
     (spell-checking :variables spell-checking-enable-auto-dictionary t)

     ;; intero

     (org :variables
          org-enable-github-support t)

     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t
                 evil-snipe-scope 'visible)

     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)

     (haskell :variables haskell-completion-backend 'intero)

     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-snippets-in-popup t)
     )

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
                                      evil-vimish-fold
                                      yankpad
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
   dotspacemacs-themes '(sanityinc-solarized-light spacemacs-light spacemacs-dark)
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
   dotspacemacs-folding-method 'origami
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

  ;; Frame maximisation configuration
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  ;; BUT one should remember to recompile org.el like in http://permalink.gmane.org/gmane.emacs.orgmode/50536
  ;; otherwise loses dvi preview!

  (require 'ox-latex)
  (setq org-latex-listings 'minted)

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
    (spacemacs/toggle-spelling-checking-off)
    (spacemacs/toggle-auto-fill-mode-on)
    (outorg-save-edits-to-tmp-file)
    (org-toggle-latex-fragment '(16)))

  (global-set-key (kbd "<f10>") 'meditans/outshine-edit)

  ;; Defining avy-jump with two characters, on s
  ;; (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)

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

  ;; Filtra i messaggi nell'echo area
  ;; presa da https://www.emacswiki.org/emacs/EchoArea
  (defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                       "^Ispell process killed$"
                                       "^Wrote.*$")
    "filter formatted message string to remove noisy messages")
  (defadvice message (around message-filter-by-regexp activate)
    (if (not (ad-get-arg 0))
        ad-do-it
      (let ((formatted-string (apply 'format (ad-get-args 0))))
        (if (and (stringp formatted-string)
                 (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
            (save-excursion
              (set-buffer "*Messages*")
              (goto-char (point-max))
              (insert formatted-string "\n"))
          (progn
            (ad-set-args 0 `("%s" ,formatted-string))
            ad-do-it)))))

  ;; Configurazione di gnus con emacs, presa dallo spacemacs help, layer gnus
  ;; Get email, and store in nnml
  (setq gnus-secondary-select-methods
        '(
          (nnimap "gmail"
                  (nnimap-address
                   "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          ))

  ;; Send email via Gmail:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com")

  ;; Archive outgoing email in Sent folder on imap.gmail.com:
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")

  ;; store email in ~/gmail directory
  (setq nnml-directory "~/mail/gmail")
  (setq message-directory "~/mail/gmail")

  ;; Set org-mode mu4e support, from https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
  (setq org-mu4e-convert-to-html t)
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
    (:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.3 :matchers
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
     ("" "bussproofs" t)
     ("" "tikz" t)
     ("" "tikz-cd" t)
     ("" "minted" t))))
 '(paradox-github-token t)
 '(user-mail-address "meditans@gmail.com")
 '(yas-indent-line (quote fixed)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-fringe-warning ((t (:background "orange" :foreground "#b58900")))))
