;; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    smartparens
    undo-tree
    auto-complete
    flycheck
    yatex
    ruby-end
    web-mode
    haml-mode
    go-mode
    markdown-mode
    stan-mode
    yaml-mode
    julia-mode
    recentf-ext
    yasnippet
    yasnippet-snippets
    helm
   ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; テーマ
(load-theme 'misterioso t)

;; 改行でインデント
(global-set-key "\C-m" 'newline-and-indent)

;; 行番号を表示
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-linum-mode t)
 '(package-selected-packages
   (quote
    (scala-mode haskell-mode rust-mode yatex web-mode undo-tree smartparens ruby-end php-mode markdown-mode haml-mode go-mode flycheck auto-complete))))

;; 変更のあったファイルの自動再読み込み
(global-auto-revert-mode t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; smartparens
(smartparens-global-mode t)

;; 対応する括弧をハイライト
(show-paren-mode t)

;; undo-tree
(global-undo-tree-mode)

;; auto-complete
(ac-config-default)

;; 一行で指定文字数を超えるとハイライト
(add-hook 'c-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'ruby-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))


;; 新規ファイルを開いた時テンプレートを挿入
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/insert/")
(setq auto-insert-alist
      (append '(
		("\\.py" . "template.py")
		("\\.sh" . "template.sh")
		) auto-insert-alist))

;;
;; yasnippet
;;
(require 'yasnippet)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

;; recentf
;; ref: http://keisanbutsuriya.hateblo.jp/entry/2015/02/15/174758

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
   (with-suppressed-message (recentf-save-list))))
(require 'recentf-ext)
(global-set-key (kbd "C-c o") 'recentf-open-files)

;; helm
(require 'helm-config)
(helm-mode 1)

;;;
;;;
;;;

;; Golang
(require 'go-mode)
(add-hook 'go-mode-hook
	  (lambda()
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    (setq indent-tabs-mode t)
	    (setq c-basic-offset 4)
	    (setq tab-width 4)))

;; Javascript
(setq js-indent-level 2)

;; web-mode
;; TODO smartparens-modeとconflict起こさない設定はしてある？
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; indent
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset 2)
  ;; (local-set-key (kbd "C-m") 'newline-and-indent)
  ;; auto tag closing
  ;0=no auto-closing
  ;1=auto-close with </
  ;2=auto-close with > and </
  (setq web-mode-tag-auto-close-style 2)
)
(add-hook 'web-mode-hook 'web-mode-hook)

;; Ruby
;; マジックコメントを入れない
(setq ruby-insert-encoding-magic-comment nil)
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

;; ;; YaTeX
;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.tex$" . yatex-mode)
;;                 ("\\.ltx$" . yatex-mode)
;;                 ("\\.cls$" . yatex-mode)
;;                 ("\\.sty$" . yatex-mode)
;;                 ("\\.clo$" . yatex-mode)
;;                 ("\\.bbl$" . yatex-mode)) auto-mode-alist))
;; (setq YaTeX-inhibit-prefix-letter t)
;; (setq YaTeX-kanji-code nil)
;; (setq YaTeX-latex-message-code 'utf-8)
;; (setq YaTeX-use-LaTeX2e t)
;; (setq YaTeX-use-AMS-LaTeX t)
;; (setq YaTeX-dvi2-command-ext-alist
;;       '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
;; (setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
;; 					;(setq tex-command "platex-ng -synctex=1")
;; 					;(setq tex-command "pdflatex -synctex=1")
;; 					;(setq tex-command "lualatex -synctex=1")
;; 					;(setq tex-command "luajitlatex -synctex=1")
;; 					;(setq tex-command "xelatex -synctex=1")
;; 					;(setq tex-command "latexmk")
;; 					;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; 					;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
;; 					;(setq tex-command "latexmk -e '$pdflatex=q/platex-ng %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;; 					;(setq tex-command "latexmk -e '$pdflatex=q/pdflatex %O -synctex=1 %S/' -e '$bibtex=q/bibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/makeindex %O -o %D %S/' -norc -gg -pdf")
;; 					;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
;; 					;(setq tex-command "latexmk -e '$lualatex=q/luajitlatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
;; 					;(setq tex-command "latexmk -e '$xelatex=q/xelatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdfxe")
;; (setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; (setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; 					;(setq dvi2-command "xdg-open")
;; (setq dvi2-command "evince")
;; 					;(setq dvi2-command "atril")
;; 					;(setq dvi2-command "okular --unique")
;; 					;(setq dvi2-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;; 					;(setq dvi2-command "qpdfview --unique")
;; 					;(setq dvi2-command "texworks")
;; 					;(setq dvi2-command "texstudio --pdf-viewer-only")
;; 					;(setq tex-pdfview-command "xdg-open")
;; (setq tex-pdfview-command "evince")
;; 					;(setq tex-pdfview-command "atril")
;; 					;(setq tex-pdfview-command "okular --unique")
;; 					;(setq tex-pdfview-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;; 					;(setq tex-pdfview-command "qpdfview --unique")
;; 					;(setq tex-pdfview-command "texworks")
;; 					;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
;; (setq dviprint-command-format "wine cmd /c start AcroRd32.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")

;; (require 'dbus)

;; (defun un-urlify (fname-or-url)
;;   "A trivial function that replaces a prefix of file:/// with just /."
;;   (if (string= (substring fname-or-url 0 8) "file:///")
;;       (substring fname-or-url 7)
;;     fname-or-url))

;; (defun evince-inverse-search (file linecol &rest ignored)
;;   (let* ((fname (decode-coding-string (url-unhex-string (un-urlify file)) 'utf-8))
;;          (buf (find-file fname))
;;          (line (car linecol))
;;          (col (cadr linecol)))
;;     (if (null buf)
;;         (message "[Synctex]: %s is not opened..." fname)
;;       (switch-to-buffer buf)
;;       (goto-line (car linecol))
;;       (unless (= col -1)
;;         (move-to-column col))
;;       (x-focus-frame (selected-frame)))))

;; (dbus-register-signal
;;  :session nil "/org/gnome/evince/Window/0"
;;  "org.gnome.evince.Window" "SyncSource"
;;  'evince-inverse-search)

;; (with-eval-after-load 'yatexprc
;;   (defun YaTeX-preview-jump-line ()
;;     "Call jump-line function of various previewer on current main file"
;;     (interactive)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (let*((pf (or YaTeX-parent-file
;;                       (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
;;               (pdir (file-name-directory pf))
;;               (bnr (substring pf 0 (string-match "\\....$" pf)))
;; 					;(cf (file-relative-name (buffer-file-name) pdir))
;;               (cf (buffer-file-name)) ;2016-01-08
;;               (buffer (get-buffer-create " *preview-jump-line*"))
;;               (line (count-lines (point-min) (point-end-of-line)))
;;               (previewer (YaTeX-preview-default-previewer))
;;               (cmd (cond
;;                     ((string-match "Skim" previewer)
;;                      (format "%s %d '%s.pdf' '%s'"
;;                              YaTeX-cmd-displayline line bnr cf))
;;                     ((string-match "evince" previewer)
;;                      (format "%s '%s.pdf' %d '%s'"
;;                              "fwdevince" bnr line cf))
;;                     ((string-match "sumatra" previewer)
;;                      (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
;;                              previewer bnr cf line))
;;                     ((string-match "zathura" previewer)
;;                      (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
;;                              previewer line cf bnr))
;;                     ((string-match "qpdfview" previewer)
;;                      (format "%s '%s.pdf#src:%s:%d:0'"
;;                              previewer bnr cf line))
;;                     ((string-match "okular" previewer)
;;                      (format "%s '%s.pdf#src:%d %s'"
;;                              previewer bnr line (expand-file-name cf)))
;;                     )))
;;           (YaTeX-system cmd "jump-line" 'noask pdir))))))

;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (auto-fill-mode -1)))

;; ;;
;; ;; RefTeX with YaTeX
;; ;;
;; 					;(add-hook 'yatex-mode-hook 'turn-on-reftex)
;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (reftex-mode 1)
;;              (define-key reftex
;; 	       -mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
;;              (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;; Gauche
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
