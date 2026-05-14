(in-package #:reality-engine-lsp)

(defun main ()
  (let ((mode (or (first uiop:*command-line-arguments*) "both")))
    (cond
      ((string= mode "reality")
       (start-reality-from-environment)
       (loop (sleep 3600)))
      ((string= mode "perception")
       (start-perception-from-environment)
       (loop (sleep 3600)))
      (t
       (start-reality-from-environment)
       (start-perception-from-environment)
       (loop (sleep 3600))))))

