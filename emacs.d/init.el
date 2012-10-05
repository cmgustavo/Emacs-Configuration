;; EMACS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(add-to-list 'load-path "~/.emacs.d/packages")

(add-to-list 'load-path "~/.emacs.d/emms/lisp/")
;(require 'emms-setup)
;(emms-standard)
;(emms-default-players)
;(emms-player-for '(*track* (type . file) (name . "foo.mp3")))
;(executable-find "afplay")

;(setq exec-path (append exec-path '("/usr/local/bin")))
;; (require 'emms-setup)
;; (require 'emms-player-mplayer)
;; (emms-standard)
;; (emms-default-players)
;; (define-emms-simple-player mplayer '(file url)
;;       (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
;;                     ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
;;                     ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
;;       "afplay" "-slave" "-quiet" "-really-quiet" "-fullscreen")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emms/lisp"))

(require 'emms-setup)
;(emms-standard)
;(emms-devel)
(require 'emms-player-mplayer)
(emms-standard)
(emms-default-players)

(setq emms-source-file-default-directory "~/Music/")

(define-emms-simple-player mplayer-mp3 '(file url)
  "\\.[mM][pP][23]$" "afplay")

(define-emms-simple-player mplayer-ogg '(file)
  (regexp-opt '(".ogg" ".OGG" ".FLAC" ".flac" )) "mplayer")

(define-emms-simple-player mplayer-playlist '(streamlist)
   "http://" "afplay" "-playlist")

(define-emms-simple-player mplayer-list '(file url)
   (regexp-opt '(".m3u" ".pls")) "afplay" "-playlist")

(define-emms-simple-player mplayer-video '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv"
                ".wma" ".mov" ".avi" ".divx" ".ogm" ".asf"
                ".mkv" "http://")) "afplay")

(setq emms-player-list '(emms-player-mplayer-mp3
                         emms-player-mplayer-ogg
                         emms-player-mplayer-playlist
                         emms-player-mplayer-video
                         emms-player-mplayer-list
                         ))

(setq emms-playlist-buffer-name "*EMMS*")

(setq emms-info-asynchronously t)

(setq emms-stream-default-action "play")

(defun emms-add-universe-synchronously ()
  (interactive)
  (let ((emms-info-asynchronously nil))
    (emms-add-directory-tree emms-source-file-default-directory)
     (message "Thud!")))

;debug players
; (emms-player-for '(*track* (type . file)
;                           (name . "myfile.pls")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup NONE

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABS 

;; Dont use mouse wheel
(setq mouse-wheel-mode nil)

;; Include tabbar
(require 'tabbar)
 (tabbar-mode)
 (setq tabbar-buffer-groups-function
           (lambda ()
             (list "All Buffers")))

(setq tabbar-buffer-list-function
     	(lambda ()
     	  (remove-if
     	   (lambda(buffer)
     	     (find (aref (buffer-name buffer) 0) " *"))
     	   (buffer-list))))

(defun tabbar-buffer-groups (buffer)
   "Return the list of group names BUFFER belongs to.
 Return only one group for each buffer."
   (with-current-buffer (get-buffer buffer)
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       '("Emacs Buffer"))
      ((eq major-mode 'dired-mode)
       '("Dired"))
      (t
       '("User Buffer"))
      )))

;; C-S-<tab> ;; C-S-<win>-<tab>
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-s-iso-lefttab>") 'tabbar-backward-tab)
;; C-x C-<left> ;; C-x C-<right>
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
     
    (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
      `(defun ,name (arg)
         (interactive "P")
         ,do-always
         (if (equal nil arg)
             ,on-no-prefix
           ,on-prefix)))
     
    (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
    (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
     
    (global-set-key [(control tab)] 'shk-tabbar-next)
    (global-set-key [(control shift tab)] 'shk-tabbar-prev)

 ;; add a buffer modification state indicator in the tab label,
 ;; and place a space around the label to make it looks less crowd
 (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
		(if (and (buffer-modified-p (tabbar-tab-value tab))
				 (buffer-file-name (tabbar-tab-value tab)))
			(concat " + " (concat ad-return-value " "))
			(concat " " (concat ad-return-value " ")))))

 ;; called each time the modification state of the buffer changed
 (defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))
 ;; first-change-hook is called BEFORE the change is made
 (defun ztl-on-buffer-modification ()
   (set-buffer-modified-p t)
   (ztl-modification-state-change))
 (add-hook 'after-save-hook 'ztl-modification-state-change)
 ;; this doesn't work for revert, I don't know
 ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
 (add-hook 'first-change-hook 'ztl-on-buffer-modification)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalidades

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Los archivos .bb son templates de SQL
(setq auto-mode-alist (cons '("bb$" . sql-mode) auto-mode-alist))
;; La tecla 'Supr' borra la parte seleccionada
(pending-delete-mode 1)
;; Abre archivos comprimidos con gzip y bzip2
(auto-compression-mode 1)
;; Algunas combinaciones de teclas
(global-set-key "\C-g" 'goto-line)


;; Entra en modo de coloreado por sintaxis
(global-font-lock-mode t)
;; make it easy on eyes first ...
(set-foreground-color "gray")
(set-background-color "black")

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)

;; Muestra pares de parentesis/corchetes/llaves       
(require 'paren)
(show-paren-mode)
(setq show-paren-mismatch t)
;; Mostrar los bloques marcados mientras los estamos marcando
(setq transient-mark-mode t)
; Hacer scroll por 10 lineas              
(setq scroll-step 15)
;; No seguir agregando lineas en blanco al final
(setq next-line-add-newlines nil)
;; Agregar automaticamente fin de linea a los archivos
(setq require-final-newline t)
; Recordamos nuestra posicion en el buffer               
(setq save-place t)
;; Acepta 'y' o 'n' cuando pide 'yes' o 'no'
(fset 'yes-or-no-p 'y-or-n-p)
;; Resalta la linea que esta el cursor
;(global-hl-line-mode 1)
;; Indenta por default en 4 tabs/espacios 
(setq standard-indent 4)
;; Deshabilita los tab para indent
;(setq-default indent-tabs-mode nil)
;; Autocompletado de filas      
(setq auto-fill-mode 1)
;; Muestra el numero de linea               
(line-number-mode 1)

(autoload 'tt-mode "tt-mode")
;(setq auto-mode-alist
;      (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
                  (tt-mode "\\[%" "%\\]")
                  ))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "tt"))
(multi-web-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Development Tools
;(load-file "~/.emacs.d/cedet/common/cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEST PERL MODE
(add-to-list 'load-path "~/.emacs.d/pde")
(load "pde-load")

;(ido-mode 1)
;(require 'template-simple)

;(load-file "~/.emacs.d/packages/ffap.el")
;(require 'ffap)
;(ffap-bindings)

;; M-SPC not available, window manager take it away
;; (global-set-key (kbd "M-'") 'just-one-space)
;; (global-set-key (kbd "C-M-=") 'pde-indent-dwim)
;; ;; nearest key to dabbrev-expand
;; (global-set-key (kbd "M-;") 'hippie-expand)
;; (global-set-key (kbd "C-;") 'comment-dwim)
;; (global-set-key (kbd "C-c f") 'comint-dynamic-complete)

;; (setq hippie-expand-try-functions-list
;;       '(try-expand-line
;;         try-expand-dabbrev
;;         try-expand-line-all-buffers
;;         try-expand-list
;;         try-expand-list-all-buffers
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-file-name
;;         try-complete-file-name-partially
;;         try-complete-lisp-symbol
;;         try-complete-lisp-symbol-partially
;;         try-expand-whole-kill))
;; (autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
;; (setq comint-completion-addsuffix '("/" . ""))

;; (setq-default indent-tabs-mode nil)

;; (defalias 'perl-mode 'cperl-mode)
;; (defun pde-perl-mode-hook ()
;;   (abbrev-mode t)
;;   (add-to-list 'cperl-style-alist
;;                '("PDE"
;;                  (cperl-auto-newline                          t)
;;                  (cperl-brace-offset                          0)
;;                  (cperl-close-paren-offset                    -4)
;;                  (cperl-continued-brace-offset                0)
;;                  (cperl-continued-statement-offset            4)
;;                  (cperl-extra-newline-before-brace            nil)
;;                  (cperl-extra-newline-before-brace-multiline  nil)
;;                  (cperl-indent-level                          4)
;;                  (cperl-indent-parens-as-block                t)
;;                  (cperl-label-offset                          -4)
;;                  (cperl-merge-trailing-else                   t)
;;                  (cperl-tab-always-indent                     t)))
;;   (cperl-set-style "PDE"))

;; (global-set-key (kbd "C-c s") 'compile-dwim-compile)
;; (global-set-key (kbd "C-c r") 'compile-dwim-run)
;; (setq compilation-buffer-name-function 'pde-compilation-buffer-name)
;; (autoload 'compile-dwim-run "compile-dwim" "Build and run" t)
;; (autoload 'compile-dwim-compile "compile-dwim" "Compile or check syntax" t)
;; (autoload 'executable-chmod "executable"
;;   "Make sure the file is executable.")

;; (defun pde-perl-mode-hook ()
;;   ;; chmod when saving
;;   (when (and buffer-file-name
;;              (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
;;     (add-hook 'after-save-hook 'executable-chmod nil t))
;;   (set (make-local-variable 'compile-dwim-check-tools) nil))

;; (global-set-key (kbd "C-c i") 'imenu)
;; (global-set-key (kbd "C-c v") 'imenu-tree)
;; ;;(global-set-key (kbd "C-c j") 'ffap)

;(setq tags-table-list '("~/TAGS"))
;; (autoload 'imenu-tree "imenu-tree" "Show imenu tree" t)
;; (setq imenu-tree-auto-update t)
;; (eval-after-load "imenu"
;;   '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
;; (autoload 'tags-tree "tags-tree" "Show TAGS tree" t)
;; A wonderful minibuffer completion mode

;; (partial-completion-mode 1)
;; (setq PC-include-file-path
;;       (delete-dups (append PC-include-file-path pde-perl-inc)))

;; (setq ffap-url-regexp
;;       (concat
;;        "\\`\\("
;;        "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
;;        "\\|"
;;        "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
;;        "\\)[^:]" ; fix for perl module, require one more character that not ":"
;;        ))
;; (add-to-list 'ffap-alist  '(cperl-mode . pde-ffap-locate))

;; (defun perl-module-path (module-name)
;;   (let* ((file-name
;;           (concat (replace-regexp-in-string "::" "~/Documents/Github/Cinemaki/perl/CK/" module-name)
;;                   ".pm"))
;;          (command-line
;;           (concat "perl -M'"
;;                   module-name
;;                   "' -e'print $INC{q{"
;;                   file-name
;;                   "}}'"))
;;          (path (shell-command-to-string command-line))
;;          (cant-locate (string-match "^Can't locate " path)))
;;     (if cant-locate
;;         nil
;;       path)))
;; (defun find-perl-module (module-name)
;;   (interactive "sPerl module name: ")
;;   (let ((path (perl-module-path module-name)))
;;     (if path
;;         (find-file path)
;;       (error "Module '%s' not found" module-name))))

;; Rebinding keys for hideshow
;; (require 'hideshow)
;; (define-key hs-minor-mode-map "\C-c\C-o"
;;   (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
;;     ;; C-h is help to remind me key binding
;;     (define-key map "\C-h" 'describe-prefix-bindings)
;;     (define-key map "\C-q" 'hs-toggle-hiding)
;;     ;; compatible with outline
;;     (define-key map "\C-c" 'hs-hide-block)
;;     (define-key map "\C-e" 'hs-show-block)
;;     map))

;; (global-set-key (kbd "C-c h") 'help-dwim)
;; (setq cperl-lazy-help-time 2)
;; (defun pde-perl-mode-hook ()
;;   (cperl-lazy-install))

;; (autoload 'run-perl "inf-perl" "Start perl interactive shell" t)
;; (autoload 'perldb-ui "perldb-ui" "perl debugger" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet (Autocomplete)
(add-to-list 'load-path
             "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart tabs (Autocomplete with TAB)
(require 'smart-tab)
;(global-smart-tab-mode 1)
(define-key read-expression-map [(tab)] 'hippie-expand)
(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoanadir parentesis o llaves de cierre
(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "'" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "<" 'skeleton-pair-insert-maybe)

;; Match HTML tags
(add-hook 'html-mode-hook
	  (lambda ()
	    (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
	    (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
	    )
	  )


