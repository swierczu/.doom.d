;;; +lang+sql.el -*- lexical-binding: t; -*-

;; Load personal sql connections list:
(let ((file-path (expand-file-name "priv/sql-connection.el" doom-user-dir)))
  (when (file-exists-p file-path)
    (load file-path)))
