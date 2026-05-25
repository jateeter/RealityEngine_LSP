(in-package #:reality-engine-lsp)

(defun main ()
  (let* ((args (rest sb-ext:*posix-argv*))
         (mode (or (first args) "both")))
    (cond
      ;; CLI shim: pe <command> [flags] — mirrors reality_engine_cli pe in _CPP.
      ((string= mode "pe")
       (cli-main (rest args)))
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

