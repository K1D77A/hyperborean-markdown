;;;; hyperborean-markdown.lisp

(in-package #:hyperborean-markdown)


(defparameter *syntax* ())

(defun s-conc (&rest args)
  (apply #'concatenate 'string args))

(defun new-syntax (key fun)
  (let ((list (getf *syntax* key)))
    (when list
      (remf *syntax* key))
    (setf (getf *syntax* key) fun)))

(defmacro def-syntax ((key arg-name environment-name) &body body)
  `(new-syntax ,key
               (lambda (stream ,arg-name ,environment-name)
                 (declare (ignorable stream ,arg-name ,environment-name))
                 (locally ,@body))))

(defun execute (key stream list-or-string environment)
  (let ((fun (getf *syntax* key)))
    (if fun
        (funcall fun stream list-or-string environment)
        (error "no such function"))))

(defun add-spacing (stream env)
  (let ((current-space (getf env :current-depth)))
    (unless (zerop current-space))
    (format stream "~A" (make-string (getf env :current-depth) :initial-element #\Space))))

(defun indent (env)
  (incf (getf env :current-depth) 4))

(defun remove-indent (env)
  (unless (zerop (getf env :current-depth))
    (decf (getf env :current-depth) 4)))

(defmacro with-indenting (env &body body)
  `(unwind-protect (progn (indent ,env)
                          (locally ,@body))
     (remove-indent ,env)))

(def-syntax (:%default string env)
  (format stream "~A" string))

(def-syntax (:h1 string env)
  (format stream "# ~A" string))

(def-syntax (:h2 string env)
  (format stream "## ~A" string))

(def-syntax (:h3 string env)
  (format stream "### ~A" string))

(def-syntax (:h4 string env)
  (format stream "#### ~A" string))

(def-syntax (:h5 string env)
  (format stream "##### ~A" string))

(def-syntax (:h6 string env)
  (format stream "###### ~A" string))

(def-syntax (:br string env)
  (format stream "  ~A" string))

(def-syntax (:bold string env)
  (format stream "**~A**" string))

(def-syntax (:italic string env)
  (format stream "*~A*" string))

(def-syntax (:bold-and-italic string env)
  (format stream "***~A***" string))

(def-syntax (:blockquote string env)
  (format stream ">~A" string))

(def-syntax (:blockquote1 string env)
  (format stream ">>~A" string))

(def-syntax (:ordered-list string env)
  (let ((item-n (getf env :item-n)))
    (format stream "~D. ~A" item-n string)
    (incf (getf env :item-n))))

(def-syntax (:unordered-list string env)
  (format stream "- ~A" string))

(def-syntax (:code-block string env)
  (with-indenting env
    (add-spacing stream env)
    (format stream "~A" string)))

(def-syntax (:image string env)
  (let ((options (getf env :options)))
    (destructuring-bind (&key url &allow-other-keys)
        options
      (format stream "![~A](~A)" string url))))

(def-syntax (:link string env)
  (let ((options (getf env :options)))
    (destructuring-bind (&key url &allow-other-keys)
        options
      (format stream "[~A](~A)" string url))))

(def-syntax (:hrule string env)
  (format stream "***"))

(def-syntax (:linked-image string env)
  (let ((options (getf env :options)))
    (destructuring-bind (&key url title &allow-other-keys)
        options
      (format stream "![~A](~A ~A)" string url (if title title "")))))

;;;last one we need to do is references but this requires special processing rules
;;;as references have to be collected and added onto the end.

(defparameter *md*
  '((:h1 "abc")
    "oof"
    (:h2 "abcd")
    (:unordered-list "def")
    (:unordered-list "abc")
    (:ordered-list 
     "oof"
     "boof")
    (:ordered-list "boof"
     (:unordered-list "coof")
     (:unordered-list
      (:h2 "omega based")))
    "lu lu lu I want some apples"
    (:bold "bold")
    (:italic "italic")
    (:blockquote
     "abc"
     "def")))

(defmacro with-resetting-keys (environment keys &body body)
  "Stores the values associated with KEYS found in the plist ENVIRONMENT, 
stores them before the execution of body then resets those keys after 
the execution of body back to the before values."
  (let* ((vals-keys (loop :for key :in keys
                          :collect (list (gensym) key)))
         (genned-setf
           (loop :for (sym key) :in vals-keys
                 :collect `(setf (getf ,environment ,(intern (string key) :keyword))
                                 ,sym))))
    `(destructuring-bind (&key ,@keys &allow-other-keys)
         ,environment
       (let (,@vals-keys)
         (unwind-protect 
              (locally ,@body))
         ,@genned-setf))))

(defun parse-to-md (stream list)
  (let ((environment (list :current-depth 0
                           :options nil
                           :item-n 0
                           :current-syntax (getf *syntax* :%default)
                           :references nil)))        
    (mapc (lambda (element)
            ;;(print element)
            (typecase element
              (string (process-list stream 
                                    (list :%default element)
                                    environment))
              (list (process-list stream element environment))
              (otherwise (error "unknown"))))
          list)))

(defun process-list (stream list environment)
  (labels ((process-string (stream list environment)
             (let ((syn (first list))
                   (ele (second list)))
               (setf (getf environment :current-syntax) syn)
               (add-spacing stream environment) 
               (execute syn stream ele environment)
               (format stream "~%")))
           (process-list (stream list environment)
             (let ((syn (first list)))
               (with-resetting-keys environment (item-n)
                 (mapc (lambda (arg)
                         (typecase arg
                           (string (process-string stream (list syn arg) environment))
                           (list (with-indenting environment
                                   (process-list stream arg environment)))))
                       list)))))
    (process-list stream list environment)))

(with-markdown
    (:h1 "abc")
  "oof"
  (:h2 "abcd")
  (:unordered-list "def")
  (:unordered-list "abc")
  (:ordered-list "oof")
  (:ordered-list "boof"
                 (:unordered-list "coof")
                 (:unordered-list
                  (:h2 "omega based")))
  "lu lu lu I want some apples"
  (:bold "bold")
  (:italic "italic")
  (:blockquote
   "abc"
   "def")))

