;;; +spelling+grammar.el -*- lexical-binding: t; -*-

(use-package! ispell
  :defer t
  :config
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,pl_PL")
  (setq ispell-personal-dictionary "~/.doom.d/hunspell_personal")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pl_PL"))

(after! flyspell
  (setq flyspell-lazy-idle-seconds 2))

(use-package! languagetool
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar"
        languagetool-server-command "/usr/local/opt/languagetool/libexec/languagetool-server.jar"))

;; TODO jinx:
;; https://github.com/doomemacs/doomemacs/issues/7617#issuecomment-1952479210
