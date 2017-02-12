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
(custom-set-variables '(global-linum-mode t))

;; 変更のあったファイルの自動再読み込み
(global-auto-revert-mode t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; smartparens
(smartparens-global-mode t)

;; undo-tree
(global-undo-tree-mode)

;; auto-complete
(ac-config-default)

;; Javascript
(setq js-indent-level 2)
