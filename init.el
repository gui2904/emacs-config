;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-packages on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7) ;; interval to update in days
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t) ;; hide updated packages, nil to show
;;   :config
;;   (auto-package-update-maybe) ;; will check wether it needs an update at startup time
;;   (auto-package-update-at-time "09:00"))

(use-package gcmh
  :config
  (gcmh-mode 1)
  (setq gcmh-cons-threshold 100000000))

(defvar clover/default-font-size 120)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)	;Disable visible scrollbar
(tool-bar-mode -1)	;Disable the toolbar
(tooltip-mode -1)	;Disable tooltips
(set-fringe-mode 10)	;Give some breathing room

(menu-bar-mode -1)	;Disable the menu bar

(setq visual-line-mode t)

;; Set up the visible bell
(setq visible-bell t) 

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; INDENTATION (global defaults)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(electric-indent-mode 1)

;; AUTO PAIRS
(electric-pair-mode 1)

;; (add-hook 'nix-mode-hook (lambda () (setq tab-width 2)))

(set-face-attribute 'default nil :family "Fira Code" :height clover/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :family "Cantarell" :height 100 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package embrace)
(use-package evil-embrace
  :after evil
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;; (unless (package-installed-p 'evil)
;;   (package-install 'evil))

;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :init
;;   (evil-collection-init))

;; ;; Enable Evil
;; (setq evil-want-keybinding nil)
;; (require 'evil)
;; (evil-mode 1)
;; (when (require 'evil-collection nil t)
;;   (evil-collection-init))

;; Makes "jk" quit insert mode
(defun my-jk ()
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(define-key evil-insert-state-map (kbd "j") 'my-jk)

(use-package doom-themes ;; counsel-load-theme
  :init (load-theme 'doom-tomorrow-night t)) ;; doom-peacock before

(use-package all-the-icons)

;; bar at the of the buffer
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; which key mini buffer
(require 'which-key)
(which-key-mode)

;; Ivy config ----
(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Little description next to the command 
(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; replace some commands for better ones
(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t) ;; Enable cycling for next and previous
  (corfu-preselect 'prompt)
  (corful-preselected 'directory) ;; Select first candidate, except for dirs
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (keymap-unset corfu-map "RET")) ;; Unbind RET completely

(setq corfu-auto t
      corfu-auto-prefix 1
      corfu-on-exact-match 'quit
      corfu-auto-delay 0.1
      corfu-preview-current 1
      corfu-popupinfo-delay '(0.0 . 0.2))

(use-package emacs
  :custom
  (tab-always-indent 'complete))

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "TAB") #'corfu-insert)
  (define-key corfu-map (kbd "<tab>") #'corfu-insert)
  (define-key corfu-map (kbd "RET") #'corfu-insert))

(add-hook 'python-ts-mode-hook
        (lambda ()
          (setq-local completion-at-point-functions
                      (list #'yasnippet-capf #'eglot-completion-at-point #'cape-file #'cape-dabbrev))))

(add-hook 'org-mode-hook
        (lambda ()
          (setq-local corfu-auto nil)))

;; (with-eval-after-load 'corfu
;;   (define-key corfu-map (kbd "TAB") #'corfu-insert)
;;   (define-key corfu-map (kbd "<tab>") #'corfu-insert))

(use-package cape
  :bind ("M-p" . cape-prefix-map) ;; keymap providing all cape commands
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-no-prompt))
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function) ;; C-h f
  ([remap describe-command] . helpful-command) ;; C-h c
  ([remap describe-variable] . counsel-describe-variable) ;; C-h v
  ([remap describe-key] . helpful-key)) ;; C-h k

(require 'org)
;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :family "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun clover/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . clover/org-mode-setup)
  :config
  (setq org-ellipsis " ↴")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/notes/OrgFiles/tasks.org"
          "~/notes/OrgFiles/habits.org"
          "~/notes/OrgFiles/birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets ;; Will move DONE tasks
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling! will save all org buffers after refiling basically
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
           ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/notes/OrgFiles/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/notes/OrgFiles/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/notes/OrgFiles/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/notes/OrgFiles/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/notes/OrgFiles/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j") ;; Thats how you define global keys, this one will take us to org-capture jj, just by pressing C-c j
              (lambda () (interactive) (org-capture nil "jj")))

  (setq org-hide-emphasis-markers t));; Hides the wrap characters, like the * for bold

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun clover/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . clover/org-mode-visual-fill))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

 (require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;; (add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun clover/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'clover/org-babel-tangle-config)))

(require 'ox-md)

  (defun org-to-readme ()
    "Export the current Org file to README.md."
    (interactive)
    (let ((output-file "README.md"))
      (org-export-to-file 'md output-file)
      (message "Exported to %s" output-file)))

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
;;         (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         ;; (nix "https://github.com/nix-community/tree-sitter-nix")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package treesit-auto
  :defer 2
  :config
  (setq treesit-auto-install t
        treesit-font-lock-level 4)
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(setq lsp-mode nil)
(fset 'lsp 'ignore)
(fset 'lsp-deferred 'ignore)

(use-package eglot
  :ensure nil
  :hook ((rust-ts-mode . eglot-ensure)
         (go-ts-mode   . eglot-ensure)
         (nix-ts-mode  . eglot-ensure)
         (zig-ts-mode  . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
  ;; Optional: format on save for eglot buffers only
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(with-eval-after-load 'eglot
  (setq completion-category-overrides
        '((eglot (styles orderless))
          (eglot-capf (styles orderless)))))


(use-package eldoc-box
  :config
  (require 'eldoc-box))

(defvar clover/indent-width 2
  "Indentation width used everywhere.")

(use-package tagedit
  :hook (html-mode . tagedit-mode)
  :config
  ;; optional but recommended
  (tagedit-add-paredit-like-keybindings)

  ;; experimental auto-insert/close behavior
  (tagedit-add-experimental-features))

;; (use-package rustic
;;   :ensure
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;                                         ;(setq rustic-format-on-save t)
;;   (setq rustic-format-trigger 'on-save))

;; (defun rustic-mode-auto-save-hook ()
;;   "Enable auto-saving in rustic-mode buffers."
;;   (when buffer-file-name
;;     (setq-local compilation-ask-about-save nil)))
;; (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

(setq-default rust-indent-offset 2)

(setq-default python-indent-offset 2)

(setq-default css-indent-offset 2)



(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))


;; C-c p f to find file, then M-o for more things to do
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(unless (file-exists-p "~/.emacs-temp")
  (mkdir "~/.emacs-temp"))

(let ((backup-dir "~/.emacs-temp/")
      (auto-saves-dir "~/.emacs-temp/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir
        ))

(setq backup-by-copying t    ; Don't delink hardlinks                           
      delete-old-versions t  ; Clean up the backups                             
      version-control t      ; Use version numbers on backups,                  
      kept-new-versions 5    ; keep some new versions                           
      kept-old-versions 2)   ; and some old ones, too                           

(setq lock-file-name-transforms
    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun clover/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . clover/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)
(autoload 'dired-single-magic-buffer "dired-single" "" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "" t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")) ;; puts the directories first and files after in the dired view
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "n" 'dired-create-empty-file
    "N" 'dired-create-directory))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(setq delete-by-moving-to-trash t)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Install fonts if not already installed
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))) ;; `t` skips confirmation




