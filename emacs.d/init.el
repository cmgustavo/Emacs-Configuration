;; EMACS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(add-to-list 'load-path "~/.emacs.d/packages")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup NONE

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalidades

;; Dont use mouse wheel
;(setq mouse-wheel-mode nil)
;(setq mac-right-option-modifier nil)

;; Los archivos .bb son templates de SQL
(setq auto-mode-alist (cons '("bb$" . sql-mode) auto-mode-alist))
;; La tecla 'Supr' borra la parte seleccionada
(pending-delete-mode 1)

;; Algunas combinaciones de teclas
(global-set-key "\C-g" 'goto-line)

;; Entra en modo de coloreado por sintaxis
(global-font-lock-mode t)

;; Muestra pares de parentesis/corchetes/llaves       
(require 'paren)
(show-paren-mode)
(setq show-paren-mismatch t)

;; Mostrar los bloques marcados mientras los estamos marcando
(setq transient-mark-mode t)
; Hacer scroll por 10 lineas              
(setq scroll-step 25)
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
(setq standard-indent 4)
;; Deshabilita los tab para indent
(setq-default indent-tabs-mode nil)
;; Autocompletado de filas      
(setq auto-fill-mode 1)
;; Muestra el numero de linea               
;(line-number-mode 1)

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
;; Smart tabs (Autocomplete with TAB)
(require 'smart-tab)
; comment for use autocomplete by type of file
(global-smart-tab-mode 1)
(define-key read-expression-map [(tab)] 'hippie-expand)
(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet (Autocomplete)
(add-to-list 'load-path
             "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEST PERL MODE
(add-to-list 'load-path "~/.emacs.d/pde")
(load "pde-load")

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
 '((width . 140) 
   (height . 50)
   (cursor-color . "Magenta")
   (cursor-type . box)
   (foreground-color . "Wheat")
   (face-background . "SteelBlue4")
   (background-color . "Black")))
 
