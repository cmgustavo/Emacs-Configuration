;; EMACS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;(require 'package)
;(add-to-list 'package-archives 
;    '("marmalade" .
;      "http://marmalade-repo.org/packages/"))
;(package-initialize)

(add-to-list 'load-path "~/.emacs.d/packages")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup NONE

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABS 

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

;; Dont use mouse wheel
;(setq mouse-wheel-mode nil)
;(setq mac-right-option-modifier nil)

;; La tecla 'Supr' borra la parte seleccionada
(pending-delete-mode 1)

;; Algunas combinaciones de teclas
(global-set-key "\C-g" 'goto-line)
;; Indentar
(global-set-key "\C-i" 'indent-region)

;; Entra en modo de coloreado por sintaxis
;(global-font-lock-mode t)

;; Muestra pares de parentesis/corchetes/llaves       
(require 'paren)
(show-paren-mode)
(setq show-paren-mismatch t)

;; Mostrar los bloques marcados mientras los estamos marcando
(setq transient-mark-mode t)
; Hacer scroll por 10 lineas              
;(setq scroll-step 25)
;; No seguir agregando lineas en blanco al final
(setq next-line-add-newlines nil)
;; Agregar automaticamente fin de linea a los archivos
(setq require-final-newline t)
; Recordamos nuestra posicion en el buffer               
(setq save-place t)
;; Acepta 'y' o 'n' cuando pide 'yes' o 'no'
(fset 'yes-or-no-p 'y-or-n-p)
;; Resalta la linea que esta el cursor
(global-hl-line-mode 1)
;; Indenta por default en 4 tabs/espacios 
(setq standard-indent 2)
;; Deshabilita los tab para indent
(setq-default indent-tabs-mode nil)
;; Autocompletado de filas      
(setq auto-fill-mode 1)
;; Muestra el numero de linea               
(line-number-mode 1)
;; No mostrar toolbar
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom files
;; Los archivos .bb son templates de SQL
(setq auto-mode-alist (cons '("bb$" . sql-mode) auto-mode-alist))
(autoload 'tt-mode "tt-mode")
(setq auto-mode-alist
      (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi web mode
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
;;                   (tt-mode "\\[%" "%\\]")
;;                   ))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "tt"))
;; (multi-web-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template Jade
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Development Tools
(load-file "~/.emacs.d/cedet/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB
(add-to-list 'load-path
             "~/.emacs.d/ecb")
(load-file "~/.emacs.d/ecb/ecb.el")
(require 'ecb)
(require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart tabs (Autocomplete with TAB)
(require 'smart-tab)
; comment for use autocomplete by type of file
;(global-smart-tab-mode 1)
(define-key read-expression-map [(tab)] 'hippie-expand)
(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet (Autocomplete-code)
;(add-to-list 'load-path
;             "~/.emacs.d/yasnippet")
;(require 'yasnippet)
;(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEST PERL MODE
;(add-to-list 'load-path "~/.emacs.d/pde")
;(load "pde-load")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoanadir parentesis o llaves de cierre
(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "'" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "<" 'skeleton-pair-insert-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match HTML tags
(add-hook 'html-mode-hook
	    (lambda ()
	          (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
		      (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
		          )
	      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;; (add-to-list 'load-path "~/.emacs.d/lintnode")
;; (require 'flymake-jslint)
;; (setq lintnode-location "~/.emacs.d/lintnode")
;; (setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (lintnode-hook)))

;; (require 'flymake-cursor)

;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (imenu-add-menubar-index)
;;             (hs-minor-mode t)))

;; Show-hide
;(global-set-key (kbd "") 'hs-show-block)
;(global-set-key (kbd "") 'hs-show-all)
;(global-set-key (kbd "") 'hs-hide-block)
;(global-set-key (kbd "") 'hs-hide-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB key fix!
;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding
;; point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AngularJS
;(require 'angular-snippets)
;(eval-after-load "sgml-mode"
;  '(define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS (Music Player)

;; (add-to-list 'load-path "~/.emacs.d/emms/lisp/")
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emms/lisp"))

;; (require 'emms-setup)
;; ;(require 'emms-player-mplayer)
;; (emms-standard)
;; (emms-default-players)

;; (setq emms-source-file-default-directory "~/Music/")
;; (define-emms-simple-player mplayer-mp3 '(file url)
;;   "\\.[mM][pP][23]$" "afplay")
;; (define-emms-simple-player mplayer-ogg '(file)
;;   (regexp-opt '(".ogg" ".OGG" ".FLAC" ".flac" )) "mplayer")
;; (define-emms-simple-player mplayer-playlist '(streamlist)
;;    "http://" "afplay" "-playlist")
;; (define-emms-simple-player mplayer-list '(file url)
;;    (regexp-opt '(".m3u" ".pls")) "afplay" "-playlist")
;; (define-emms-simple-player mplayer-video '(file url)
;;   (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv"
;;                 ".wma" ".mov" ".avi" ".divx" ".ogm" ".asf"
;;                 ".mkv" "http://")) "afplay")
;; (setq emms-player-list '(emms-player-mplayer-mp3
;;                          emms-player-mplayer-ogg
;;                          emms-player-mplayer-playlist
;;                          emms-player-mplayer-video
;;                          emms-player-mplayer-list
;;                          ))
;; (setq emms-playlist-buffer-name "*Music*")
;; (setq emms-info-asynchronously t)
;; (setq emms-stream-default-action "play")
;; (defun emms-add-universe-synchronously ()
;;   (interactive)
;;   (let ((emms-info-asynchronously nil))
;;     (emms-add-directory-tree emms-source-file-default-directory)
;;      (message "Thud!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Color
(setq default-frame-alist
 '((width . 150) 
   (height . 50)
   (cursor-color . "Magenta")
   (cursor-type . box)
   (foreground-color . "Wheat")
   (face-background . "SteelBlue4")
   (background-color . "Black")))
