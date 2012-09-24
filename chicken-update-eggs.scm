(use posix utils srfi-1 http-client)

(define henrietta-uri
  (make-parameter "http://code.call-cc.org/cgi-bin/henrietta.cgi"))


(define (list-remote-eggs)
  (with-input-from-request
   (string-append (henrietta-uri) "?list=1")
   #f
   read-file))


(define (chicken-tool prefix tool)
  (let ((tool-file (make-pathname (list prefix "bin")
                                  tool
                                  (and (eq? (software-type) 'windows) "exe"))))
    (unless (file-exists? tool-file)
      (print "Could not find " tool-file ". Aborting"))
    tool-file))


(define (list-local-eggs prefix)
  (map (compose string->symbol pathname-file)
       (glob (make-pathname (list prefix
                                  "lib"
                                  "chicken"
                                  (number->string (##sys#fudge 42)))
                            "*" "setup-info"))))


(define egg?
  (let ((remote-eggs #f))
    (lambda (module)
      (unless remote-eggs
        (set! remote-eggs (list-remote-eggs)))
      (and (memq module remote-eggs) #t))))


(define (install-eggs! to-prefix eggs #!key dry-run?)
  (let ((chicken-install (chicken-tool to-prefix "chicken-install")))
    (handle-exceptions exn
      (begin
        (print "Error installing eggs.  Aborting.")
        (exit 1))
      ((if dry-run?
           print
           system*)
       (string-append chicken-install " "
                      (string-intersperse
                       (map symbol->string eggs)))))))


(define (eggs-to-install installed-stuff)
  (filter egg? installed-stuff))


(define (usage #!optional exit-code)
  (print "Usage: " (pathname-strip-directory (program-name))
         " [ --dry-run ] <from prefix> <to prefix>\n\n"
         "<from prefix> is the chicken installation prefix where "
         "to get the egg list to install into <to prefix>.\n\n"
         "Example:\n\n"
         "    $ chicken-update-eggs /usr/local/chicken-4.7.0 /usr/local/chicken-4.7.4\n\n"
         "would install all eggs from /usr/local/chicken-4.7.0 into /usr/local/chicken-4.7.4\n\n"
         "--dry-run (optional) only shows what is to be executed without actually executing it.\n\n"
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
  (let ((from-prefix (last (butlast args)))
        (to-prefix (last args))
        (dry-run? (and (member "--dry-run" args) #t)))
    (install-eggs! to-prefix
                   (eggs-to-install (list-local-eggs from-prefix))
                   dry-run?: dry-run?)))
