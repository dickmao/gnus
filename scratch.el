;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(cl-loop with result
         for x below 3
         for y below 2 and z = 1
         collect x)

(cl-loop with result
         for x below 3
         and y below 2
         collect x)

(cl-loop with result
         for x below 3
         for y = (progn (push x result) x) and z = 1
         append (list x y) into result
         finally return result)

(cl-loop with result
         for x below 3
         for y below 2
         and z = (progn (push x result) nil)
         finally return result)

(cl-loop with result
         for x below 3
         and y = (progn (push x result) x) and z = 1
         append (list x y) into result
         finally return result)

(cl-loop with result
         for x below 3
         for y = (progn (push x result))
         finally return result)

(cl-loop with result
         for x below 3
         and y = (progn (push x result))
         finally return result)

(cl-loop with result
         for x below 3
         and y = (progn (push x result)) then (progn (push (1+ x) result))
         finally return result)

(cl-loop with result
         for x below 3
         for y = (progn (push x result) x) then (progn (push (1+ x) result) (1+ x))
         and z = 1
         collect y into result1
         finally return result)

(cl-loop with result
         for x below 3
         for y = (progn (push x result) x) then (progn (push (1+ x) result) (1+ x))
         and z = 1
         collect y into result1
         finally return (equal (nreverse result) result1))

(cl-loop for i from 1 upto 100 and j = 1 then (1+ j)
         do (cl-assert (= i j) t)
         until (> j 10))

(cl-loop for i from 1 upto 100 and j = 2 then (1+ j)
                   if (not (= i j))
                   return nil
                   end
                   until (> j 10)
                   finally return t)
(let* ((size 7)
       (arr (make-vector size 0)))
  (cl-loop for k below size
           for x = (* 2 k) and y = (1+ (elt arr k))
           collect (list k x y)))

(require 'anaphora)
(defvar-local thread-tests-counter nil)
(defvar-local nntp-server-buffer nil)

(defmacro gnus-maybe-thread (mtx fn &rest args)
  "Depending on `gnus-threaded-get-unread-articles', make a thread.

MTX, if non-nil, is the mutex for the new thread.  Wrap FN ARGS in `make-thread'."
  (declare (indent 0))
  (if gnus-threaded-get-unread-articles
      (list 'lexical-let (mapcar (lambda (x) `(,x ,x)) `(,@args))
            (list 'make-thread
                  (list 'lambda nil
                        (append (if (eval mtx)
                                    (list 'with-mutex mtx)
                                  (list 'prog1))
                                (list `(apply ,fn (list ,@args)))))))
    `(apply ,fn (list ,@args))))

(defun gnus-instantiate-server-buffer (name)
  (let ((buffer (generate-new-buffer (format " *gnus-thread %s*" name))))
    (nnheader-prep-server-buffer buffer)
    buffer))

(defsubst gnus-with-mutex (mtx body)
  "Dynamic scope disallows referencing mtx in `gnus-maybe-thread' in the natural way.

Also, `lexical-let' appears to be obsolesced in `cl-lib'."
  (with-mutex mtx
    (funcall body)))

(defsubst gnus-with-server-buffer (working-buffer body)
  (let ((nntp-server-buffer working-buffer))
    (funcall body)))

(defun gnus-maybe-thread (mtx working &rest fns)
  "Depending on `gnus-threaded-get-unread-articles', make a thread.

MTX, if non-nil, is the mutex for the new thread.  Wrap each of FNS in `make-thread'."
  (when fns
    (when (stringp working)
      (setq working (gnus-instantiate-server-buffer working)))
    (if gnus-threaded-get-unread-articles
        (let* ((continuation (if (cdr fns)
                                 (apply #'apply-partially
                                        #'gnus-maybe-thread mtx working
                                        (cdr fns))
                               (apply-partially #'kill-buffer working)))
               (thread-name (format "%s" (car fns)))
               (thread-start (apply-partially #'message "Start! %s" thread-name))
               (thread-done (apply-partially #'message "Done! %s" thread-name))
               (catch-error (apply-partially
                             (lambda (name f)
                               (condition-case err (funcall f)
                                 (error (gnus-message 5 "gnus-maybe-thread: '%s' at %s"
                                                      (error-message-string err)
                                                      name))))
                             thread-name))
               (body0 (apply-partially #'gnus-with-server-buffer working (car fns)))
               (body1 (if mtx (apply-partially #'gnus-with-mutex mtx body0) body0))
               (body2 (apply-partially catch-error body1))
               (body3 (apply-partially #'mapc #'funcall
                                       `(,thread-start ,body2 ,continuation ,thread-done))))
          (make-thread body3))
      (mapc #'funcall fns))))

(defvar gnus-mutex-get-unread-articles (make-mutex "gnus-mutex-get-unread-articles")
  "Updating or displaying state of unread articles are critical sections.")

(setq gnus-threaded-get-unread-articles t)
(setq gnus-threaded-get-unread-articles nil)

(setq gnus-mutex-get-unread-articles (make-mutex "gnus-mutex-get-unread-articles"))
(setq gnus-mutex-get-unread-articles nil)

(defun thread-function (whoami)
  (message "got here %s working-buffer=%s nntp-server-buffer=%s"
           whoami (current-buffer) nntp-server-buffer)
  (error "injun")
  (sleep-for 0 5000)
  (setq thread-tests-counter (1- thread-tests-counter)))

(let ((nntp-server-buffer (current-buffer))) ;; by the time thread runs, let is off the stack!
  (setq thread-tests-counter 0)
  (gnus-maybe-thread gnus-mutex-get-unread-articles "get-unread-articles"
                     (apply-partially #'thread-function 1)
                     (apply-partially #'thread-function 2)))

(mapc #'kill-buffer (seq-filter (lambda (b) (search "gnus-thread" (buffer-name b))) (buffer-list)))

(seq-filter (lambda (b) (string-match "nnimap.*gnus-thread" (buffer-name b))) (buffer-list))

(nthcdr 2 (cl-fourth gnus-secondary-select-methods))
(nnimap-server-opened (nth 1 (cl-fourth gnus-secondary-select-methods)))
(nnimap-server-opened "sbu")
(gnus-open-server (cl-fifth gnus-secondary-select-methods))
(gnus-server-opened (cl-fifth gnus-secondary-select-methods))
(nnoo-current-server 'nnimap)

(mapcar (apply-partially #'eq (current-thread)) (all-threads))

(funcall (apply-partially (lambda (f) (condition-case err (funcall f) (error (error-message-string err)))) (apply-partially #'error "foo")))

(defvar bigdaddy nil)
(setq bigdaddy t)
(setq bigdaddy nil)
(make-thread (lambda () (let ((bigdaddy bigdaddy))
                          (setq bigdaddy t)
                          (sleep-for 0 10000)
                          (message "my bigdaddy = %s (%s)" bigdaddy
                                   (current-buffer)))))
