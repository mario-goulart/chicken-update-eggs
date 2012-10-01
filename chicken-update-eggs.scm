(use posix utils srfi-1 irregex)

(define *deploy-dir* (get-environment-variable "CUE_DEPLOY_DIR"))


(define (chicken-tool prefix tool)
  (let ((tool-file (make-pathname (if *deploy-dir*
                                      #f ;; Get chicken tools from path
                                      (list prefix "bin"))
                                  tool
                                  (and (eq? (software-type) 'windows) "exe"))))
    (when (and (not *deploy-dir*) (not (file-exists? tool-file)))
      (print "Could not find " tool-file ". Aborting"))
    tool-file))


(define (list-eggs prefix)
  (let ((setup-infos
         (glob (make-pathname (list prefix
                                    "lib"
                                    "chicken"
                                    (number->string (##sys#fudge 42)))
                              "*" "setup-info")))
        (eggs '()))
    (for-each (lambda (setup-info)
                (and-let* ((setup-info-data (with-input-from-file setup-info read))
                           (egg (alist-ref 'egg-name setup-info-data))
                           (egg (string->symbol (car egg))))
                  (unless (memq egg eggs)
                    (set! eggs (cons egg eggs)))))
              setup-infos)
    eggs))


(define (install-eggs! to-prefix eggs #!key dry-run?)
  (let ((chicken-install (chicken-tool to-prefix "chicken-install")))
    (handle-exceptions exn
      (begin
        (print "Error installing eggs.  Aborting.")
        (exit 1))
      ((if dry-run?
           print
           system*)
       (string-append chicken-install
                      (if *deploy-dir*
                          (string-append " -deploy -p " *deploy-dir*)
                          "")
                      " "
                      (string-intersperse
                       (map symbol->string eggs)))))))



(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))


(define (usage #!optional exit-code)
  (print "Usage: " (pathname-strip-directory (program-name))
         " [ <options> ] <from prefix> <to prefix>\n\n"
         "<from prefix> is the chicken installation prefix where "
         "to get the egg list to install into <to prefix>.\n\n"
         "<options> are:\n"
         "  --dry-run           only shows what is to be executed without actually executing it\n"
         "  --skip-eggs=<eggs>  don't attempt to install <eggs> (a comma-sparated list og eggs)\n\n"
         "Example:\n\n"
         "    $ chicken-update-eggs /usr/local/chicken-4.7.0 /usr/local/chicken-4.7.4\n\n"
         "would install all eggs from /usr/local/chicken-4.7.0 into /usr/local/chicken-4.7.4\n"
         "This program can also be used to update all eggs if you give it the same values for "
         "<from prefix> and <to prefix>.")
  (when exit-code (exit exit-code)))



(let ((args (command-line-arguments)))
  (when (or (null? args)
            (null? (cdr args)))
    (usage 1))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (let* ((from-prefix (last (butlast args)))
         (to-prefix (last args))
         (skip (and-let* ((skip (cmd-line-arg '--skip-eggs args)))
                 (map string->symbol (string-split skip ","))))
         (dry-run? (and (member "--dry-run" args) #t)))
    (install-eggs! to-prefix
                   (if skip
                       (remove (lambda (egg)
                                 (memq egg skip))
                               (list-eggs from-prefix))
                       (list-eggs from-prefix))
                   dry-run?: dry-run?)))
