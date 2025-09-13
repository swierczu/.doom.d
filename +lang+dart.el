;;; +lang+web.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :defer t
  :hook (dart-mode . lsp)
  :config
  (setq flutter-sdk-path "~/sdk/flutter")
  (setq lsp-dart-flutter-sdk flutter-sdk-path)
  (setq lsp-dart-sdk-dir (concat flutter-sdk-path "/bin/cache/dart-sdk"))
  (setq lsp-dart-project-root-discovery-strategies '(closest-pubspec lsp-root))
  (setq +sw/android-emulator-path "~/sdk/android/emulator/emulator")
  (map! (:localleader
         :map dart-mode-map
         :desc "show flutter outline" "o" #'lsp-dart-show-flutter-outline
         :desc "run Android emulator" "e" #'+sw/run-android-emulator)))

(defun +sw/android-emulators()
  (let* ((emulators (shell-command-to-string
                     (format "%s -list-avds" +sw/android-emulator-path)))
         (emulators-list (split-string emulators "\n")))
    emulators-list))

(defun +sw/run-android-emulator(emulator-id)
  (interactive
   (list (let* ((emulators (+sw/android-emulators))
                (choice (completing-read "Emulator: " emulators)))
           choice)))
  (message "Running avd: %s" emulator-id)
  (let ((name emulator-id)
        (buffer (concat "avd: " emulator-id))
        (program (format "%s -avd %s"
                         +sw/android-emulator-path
                         emulator-id)))
    (start-process-shell-command name buffer program)))
