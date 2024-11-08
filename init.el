
(setq inhibit-startup-message t)

(scroll-bar-mode -1)	;Disable visible scrollbar
(tool-bar-mode -1)	;Disable the toolbar
(tooltip-mode -1)	;Disable tooltips
(set-fringe-mode 10)	;Give some breathing room

(menu-bar-mode -1)	;Disable the menu bar

;; Set up the visible bell
(setq visible-bell t) 

;; Font Configuration ----------------------------------------------------------

(set-face-attribute 'default nil :family "Fira Code Retina" :height 150)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :family"Fira Code Retina" :height 260)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :family "Cantarell" :height 260 :weight 'regular)

;; Make ESC quit prompt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Wrap lines
(global-visual-line-mode t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-packages on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes 
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; OLD
;;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; Define key example
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) ;; this would bind the function that loads theme

;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; bar at the of the buffer
(require 'doom-modeline)
(doom-modeline-mode 1)

(use-package doom-themes ;; counsel-load-theme
  :init (load-theme 'doom-laserwave t))


;; which key mini buffer
(require 'which-key)
(which-key-mode)

;; Little description next to the command 
(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; replace some commands for better ones
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function) ;; C-h f
  ([remap describe-command] . helpful-command) ;; C-h c
  ([remap describe-variable] . counsel-describe-variable) ;; C-h v
  ([remap describe-key] . helpful-key)) ;; C-h k

;; Package to define keys
;(use-package general
  ;:config
  ;general-create-definer clover/leader-keys
    ;:keymaps '(normal insert visual emacs)
    ;:prefix "SPC"
    ;:prefix "C-SPC")

  ;(clover/leader-keys
   ;"t" '(:ignore t :which-key "toggles")
   ;"tt" '(counsel-load-theme :which-key "choose theme")))

;;
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

;(defun clover/evil-hook ()
;  (dolist (mode '(custom-mode
;		  eshell-mode
;		  git-rebase-mode
;		  erc-mode
;		  circe-server-mode
;		  circe-chat-mode
;		  circe-query-mode
;		  sauron-mode
;		  term-mode))
;    (add-to-list 'evil-emacs-state-modes mode)))

;(use-package evil
;  :init
;  (setq evil-want-integration t)
;  (setq evil-want-keybinding nil)
;  (setq evil-want-C-u-scroll t)
;  (setq evil-want-C-i-jump nil)
;  :hook (evil-mode . clover/evil-hook)
;  :config
;  (evil-mode 1)
;  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
;  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;  (evil-set-initial-state 'messages-buffer-mode 'normal)
;  (evil-set-initial-state 'dashboard-mode 'normal))

(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; C-c p f to find file, then M-o for more things to do
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun clover/org-mode-setup()
  (variable-pitch-mode)
  (setq evil-auto-indent nil))

;; Org Mode

(defun clover/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . clover/org-mode-setup)
  :config
  (setq org-ellipsis " ↴"))

;; Hides the wrap characters, like the * for bold
(setq org-hide-emphasis-markers t)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
