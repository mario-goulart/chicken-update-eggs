(use extras posix utils srfi-1 irregex)

(define *deploy-dir* (get-environment-variable "CUE_DEPLOY_DIR"))

(define (die! fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (exit 1))

(define list-remote-eggs
  (let ((eggs #f)
        (with-input-from-request #f))
    (lambda ()
      (unless eggs
        (handle-exceptions exn
          (die!
           (string-append
            "--skip-local-eggs requires the http-client egg. "
            "You don't seem to have it installed.  To install it, run:\n\n"
            "    chicken-install http-client"))
          (set! with-input-from-request
            (eval '(let() (use http-client) with-input-from-request))))
        (handle-exceptions exn
          (die! "Error fetching egg list.")
          (set! eggs
            (remove
             string-null?
             (with-input-from-request
              "http://code.call-cc.org/cgi-bin/henrietta.cgi?list=1"
              #f
              read-lines)))))
      (map string->symbol eggs))))

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
      (die! "Error installing eggs.  Aborting.")
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
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #this [<options>] <from prefix> <to prefix>

<from prefix> is the chicken installation prefix where to obtain the
egg list to install into <to prefix>

<options> are:

--dry-run
  only shows what is to be executed without actually executing it.

--skip-eggs=<eggs>
  don't attempt to install <eggs> (a comma-sparated list of eggs).

--skip-local-eggs
  don't attempt to install eggs which are not served by the egg
  server.

Example:

    $ chicken-update-eggs /usr/local/chicken-4.7.0 /usr/local/chicken-4.7.4

would install all eggs from /usr/local/chicken-4.7.0 into
/usr/local/chicken-4.7.4

This program can also be used to update all eggs if you give it the
same values for <from prefix> and <to prefix>.

EOF
    port)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (when (or (null? args)
            (null? (cdr args)))
    (usage 1))
  (let* ((from-prefix (last (butlast args)))
         (to-prefix (last args))
         (skip (and-let* ((skip (cmd-line-arg '--skip-eggs args)))
                 (map string->symbol (string-split skip ","))))
         (skip-local-eggs? (member "--skip-local-eggs" args))
         (dry-run? (and (member "--dry-run" args) #t))
         (eggs-without-skipped
          (if skip
              (remove (lambda (egg)
                        (memq egg skip))
                      (list-eggs from-prefix))
              (list-eggs from-prefix)))
         (eggs (if skip-local-eggs?
                   (filter (lambda (egg)
                             (memq egg (list-remote-eggs)))
                           eggs-without-skipped)
                   eggs-without-skipped)))
    (install-eggs! to-prefix eggs dry-run?: dry-run?)
    (when skip-local-eggs?
      (let ((local-eggs (remove (lambda (egg)
                                  (memq egg (list-remote-eggs)))
                                eggs-without-skipped)))
        (unless (null? local-eggs)
          (print "\nThe following local eggs have been skipped:")
          (for-each (lambda (local-egg)
                      (print "  * " local-egg))
                    local-eggs))))))
