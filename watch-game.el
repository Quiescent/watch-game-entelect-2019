;;; watch-game --- Run and watch a game of Entelect Challenge 2019 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)
(require 'hi-lock)

(defvar watch-buffer-name "*Watch*"
  "The name of the buffer in which to render the game.")

(defvar watch--output-buffer-name "*Watch-Output*"
  "The name of the buffer in which to continually spit the output of the game.")

(defvar watch--process-name "*Watch-Process*"
  "The name of the process in which to render the game.")

(defvar watch--process nil
  "The process which watches for output.")

(defvar watch--previous-game-states nil
  "The previous states of the game.")

(defvar watch--searching-through-history nil
  "Whether we're currently looking through history.")

(defvar watch--current-history-element-index nil
  "What point we're at in the history of outputs.")

(defvar watch-game-mode-map
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "p") #'watch-previous-state)
      (define-key keymap (kbd "n") #'watch-next-state)
      (define-key keymap (kbd "r") #'watch-resume-live)
      (define-key keymap (kbd "q") #'watch-quit)
      keymap)
  "The keymap for watch.")

(define-minor-mode watch-game
    "A minor mode for viewing Entelect Challenge replays."
  nil nil watch-game-mode-map)

(defun watch--insert-into-watch-buffer (text)
  "Insert TEXT into the viewing buffer."
  (save-excursion
    (switch-to-buffer watch-buffer-name)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert text)
    (font-lock-ensure (point-min) (point-max))
    (read-only-mode 1)))

(defun watch--current-output ()
  "Proudce the output which should be rendered."
  (cdr (assoc watch--current-history-element-index watch--previous-game-states)))

(defun watch--prevent-over/underflow (i)
  "Produce an index I such that it's not below zero or greater than history length."
  (if (< i 1) 1 (if (>= i (length watch--previous-game-states)) (1- i) i)))

(defun watch-quit ()
  "Quit this watch session.

If it's running from the console then quit Emacs too."
  (interactive)
  (progn
    (kill-buffer watch--output-buffer-name)
    (kill-buffer watch-buffer-name)
    (when (null window-system)
      (kill-emacs t))))

(defun watch-previous-state ()
  "Move to the previous state."
  (interactive)
  (progn
    (setq watch--searching-through-history     t
          watch--current-history-element-index (watch--prevent-over/underflow
                                                (1- watch--current-history-element-index)))
    (watch--insert-into-watch-buffer (watch--current-output))))

(defun watch-next-state ()
  "Move to the next state."
  (interactive)
  (progn
    (setq watch--searching-through-history     t
          watch--current-history-element-index (watch--prevent-over/underflow
                                                (1+ watch--current-history-element-index)))
    (watch--insert-into-watch-buffer (watch--current-output))))

(defun watch-resume-live ()
  "Resume watching the live match."
  (interactive)
  (setq watch--searching-through-history     nil
        watch--current-history-element-index (1- (length watch--previous-game-states))))

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
      (setq watch--process
            (start-process watch--process-name
                           watch--output-buffer-name
                           "make"
                           "run"))
      (switch-to-buffer watch-buffer-name)
      (read-only-mode -1)
      (watch-game 1)
      (hi-lock-mode 1)
      (font-lock-mode 1)
      (font-lock-add-keywords nil
                              '(("█"  . 'watch-hi-black-b)
                                ("▓"  . 'watch-hi-brown-b)
                                ("╠╣" . 'hi-red-b)))
      (kill-region (point-min) (point-max))
      (setq watch--previous-game-states nil)
      (setq watch--searching-through-history nil)
      (setq watch--current-history-element-index 0)
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
  (with-current-buffer watch--output-buffer-name
    (save-window-excursion
      (goto-char (point-min))
      (let ((start (save-excursion (search-forward watch--start-round-token nil t)))
            (end   (save-excursion (search-forward watch--end-round-token nil t))))
        (when (or (process-live-p watch--process)
                  start)
          (when (and start end)
            (let ((next-output (buffer-substring (- start (length watch--start-round-token)) end)))
              (delete-region (- start (length watch--start-round-token)) end)
              (push (cons (1+ (length watch--previous-game-states)) next-output) watch--previous-game-states)
              (when (not watch--searching-through-history)
                (watch--insert-into-watch-buffer next-output)
                (setq watch--current-history-element-index (caar watch--previous-game-states)))))
          (run-at-time 0.1 nil #'watch--read-print-loop))))))

(provide 'watch-game)
;;; watch-game ends here
