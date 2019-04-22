;;; watch-game --- Run and watch a game of Entelect Challenge 2019 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)

(defvar watch-buffer-name "*Watch*"
  "The name of the buffer in which to render the game.")

(defvar watch-output-buffer-name "*Watch-Output*"
  "The name of the buffer in which to continually spit the output of the game.")

(defvar watch-process-name "*Watch-Process*"
  "The name of the process in which to render the game.")

(defvar watch-process nil
  "The process which watches for output.")

(require 'hi-lock)

(defface watch-hi-brown-b
    '((((min-colors 88)) (:weight bold :foreground "saddle brown"))
      (t (:weight bold :foreground "saddle brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface watch-hi-black-b
    '((((min-colors 88)) (:weight bold :foreground "black"))
      (t (:weight bold :foreground "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun watch (project-dir)
  "Watch the game currently specified in game-runner of PROJECT-DIR.

To watch two different bots play against each other, change the
  configuration in that directory."
  (interactive "D(defaults to project in current dir): ")
  (progn
    (let ((default-directory (format "%sgame-runner"
                                     (or project-dir
                                         (car (project-roots (project-current)))))))
      (setq watch-process
            (start-process watch-process-name
                           watch-output-buffer-name
                           "make"
                           "run"))
      (switch-to-buffer watch-buffer-name)
      (read-only-mode -1)
      (hi-lock-mode 1)
      (font-lock-mode 1)
      (font-lock-add-keywords nil
                              '(("█"  . 'watch-hi-black-b)
                                ("▓"  . 'watch-hi-brown-b)
                                ("╠╣" . 'hi-red-b)))
      (kill-region (point-min) (point-max))
      (read-only-mode 1)
      (watch--read-print-loop))))

(defconst watch--start-round-token "=======================================
Starting round:"
  "The token to scan for to determine the start of a round.")

(defconst watch--end-round-token "=======================================
Completed round:"
  "The token to scan for to determine the end of a round.")

(defun watch--read-print-loop ()
  "Watch the watch process and print whenever a new round starts."
  (with-current-buffer watch-output-buffer-name
    (save-window-excursion
      (goto-char (point-min))
      (let ((start (save-excursion (search-forward watch--start-round-token nil t)))
            (end   (save-excursion (search-forward watch--end-round-token nil t))))
        (when (or (process-live-p watch-process)
                  start)
          (when (and start end)
            (let ((next-output (buffer-substring (- start (length watch--start-round-token)) end)))
              (delete-region (- start (length watch--start-round-token)) end)
              (switch-to-buffer watch-buffer-name)
              (read-only-mode -1)
              (delete-region (point-min) (point-max))
              (insert next-output)
              (font-lock-ensure (point-min) (point-max))
              (read-only-mode 1)))
          (run-at-time 0.1 nil #'watch--read-print-loop))))))

(provide 'watch-game)
;;; watch-game ends here
