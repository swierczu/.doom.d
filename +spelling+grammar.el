;;; +spelling+grammar.el -*- lexical-binding: t; -*-

(use-package! ispell
  :if (string-equal system-type "darwin")
  :defer t
  :config
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,pl_PL")
  (setq ispell-personal-dictionary "~/.doom.d/hunspell_personal")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pl_PL"))

;; TODO jinx:
;; https://github.com/doomemacs/doomemacs/issues/7617#issuecomment-1952479210

(use-package! jinx
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-languages "pl en_US")
  (map! (:leader :desc "Spell checker" :n "t s" #'jinx-correct)
        :m "] s" #'jinx-next
        :m "[ s" #'jinx-previous))
