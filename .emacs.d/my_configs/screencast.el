;; Config for running a screencast

(defvar screenkey-timeout 4
  "Number of seconds to allow screenkey to run normally")

(defun screenkey-timed (&optional timeout)
  "Run an external command and then asynchronously kill it after timeout"
  (interactive)
  (start-process "screenkey" "*screenkey*" "screenkey" "--no-detach")
  (run-at-time (format "%d sec" (or timeout screenkey-timeout)) nil
             '(lambda () (delete-process "*screenkey*"))))

(global-set-key (kbd "C-M-k") 'screenkey-timed)
