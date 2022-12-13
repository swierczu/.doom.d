;;; +lang+web.el -*- lexical-binding: t; -*-

(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        js-indent-level 2
        typescript-indent-level 2
        json-reformat:indent-width 2
        css-indent-offset 2
        prettier-js-args '("--single-quote")))
