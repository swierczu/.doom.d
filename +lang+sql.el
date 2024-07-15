;;; +lang+sql.el -*- lexical-binding: t; -*-

(let ((file-path (expand-file-name "priv/sql-connection.el" user-emacs-directory)))
  (when (file-exists-p file-path)
    (load file-path)))
