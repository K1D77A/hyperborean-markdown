;;;; hyperborean-markdown.lisp

(in-package #:hyperborean-markdown)

(defparameter *default-spacing* 1
  "The default spacing applies after executing a syntax function.")

(defparameter *syntax* ()
  "A plist of plists associating information regarding how to parse lisp to md.")

(defparameter *specials* (make-hash-table :test #'equal)
  "A hash table of lambdas associated with strings, these strings are special strings 
that have their own execution rules.")

(define-condition hyper-condition ()
  ((message
    :accessor message
    :initarg :message
    :type string
    :documentation "A message sent with each condition"))
  (:documentation "the toplevel conditions for all of hyperborean-markdowns conditions"))

(define-condition key-missing-fun (hyper-condition)
  ((key
    :accessor key
    :initarg :key
    :type keyword
    :documentation "The key used for lookup.")
   (where
    :accessor where
    :initarg :where
    :type string
    :documentation "The name of the location where lookup happened"))
  (:documentation "Signalled when a lookup was performed for a key that doesn't exist.")
  (:report
   (lambda (obj stream)
     (format stream "Key: ~A. Where: ~A~%Message: ~A"
             (key obj)
             (where obj)
             (message obj)))))


(defun new-special (string fun)
  "Associated FUN with STRING in the hash-table *specials*"
  (setf (gethash string *specials*) fun))

(defmacro def-special ((special environment-name) &body body)
  "Defines a new special string SPECIAL, these strings are used to invoke special 
behaviour when they are seen within the list used to parse MD. The most basic example 
is the \"\" special which is used to insert a carriage return into the stream."
  `(new-special ,special
                (lambda (stream ,environment-name)
                  (declare (ignorable stream ,environment-name))
                  (locally ,@body))))

(defun new-syntax (key options fun)
  "Associate the list (list :fun FUN :options OPTIONS) with KEY within the plist 
*syntax*."
  (setf (getf *syntax* key) (list :fun fun :options options)))

(defmacro def-syntax ((key arg-name environment-name &optional (options ())) &body body)
  "Define a new 'syntax' this is an association between a KEY something like :h1 and 
the function that is going to be executed when that key is found while iterating through 
the list data structure used for converting lists to markdown. OPTIONS is a list of 
keywords that is used to provide special data to the parser, some markdown options like 
images require a url, so the key :href would be provided within the OPTIONS list so that
special processing can take place. When OPTIONS is non nil then the keys within OPTIONS 
are automatically destructured and can be referenced as arguments within the body of 
def-syntax."
  `(new-syntax ,key ',options
               (lambda (stream ,arg-name ,environment-name)
                 (declare (ignorable stream ,arg-name ,environment-name))
                 ,(if options
                      `(destructuring-bind (&key
                                              ,@(mapcar (lambda (key)
                                                          (intern (string-upcase key)))
                                                        options)
                                            &allow-other-keys)
                           (getf ,environment-name :options)
                         (locally ,@body))
                      `(locally ,@body)))))

(defun execute (key stream list-or-string environment)
  "Uses KEY to find a plist within *syntax*. If the plist is found then calls the function 
associated with STREAM LIST-OR-STRING and ENVIRONMENT as arguments. If no plist is found 
then signals the condition 'key-missing-fun."
  (let ((fun (getf (getf *syntax* key) :fun)))
    (if fun
        (funcall fun stream list-or-string environment)
        (error
         'key-missing-fun :where "syntax" :key key
                          :message "couldn't find key. Perhaps your List is invalid."))))

;; (defun add-spacing (stream env)
;;   (let ((current-space (getf env :current-depth)))
;;     (unless (zerop current-space))
;;     (format stream "~A" (make-string (getf env :current-depth) :initial-element #\Space))))

(defun indent (env)
  "Increment :current-depth by 4."
  (incf (getf env :current-depth) 4))

(defun remove-indent (env)
  "Unless :current-depth is already 0, remove 4."
  (unless (zerop (getf env :current-depth))
    (decf (getf env :current-depth) 4)))

(defmacro with-indenting (env &body body)
  "Evaluates BODY after incrementing :current-depth by 4, after evaluation decrements 
:current-depth by 4."
  `(unwind-protect (progn (indent ,env)
                          (locally ,@body))
     (remove-indent ,env)))

;;;a list of all the syntax rules that I'm aware of so far.

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
    (format stream "```~A```" string)))

(def-syntax (:image string env (:href))
  (format stream "![~A](~A)" string href))

(def-syntax (:link string env (:href))
  (format stream "[~A](~A)" string href))

(def-syntax (:hrule string env)
  (format stream "***"))

(def-syntax (:linked-image string env (:path :description :href))
  (format stream "[![~A](~A \"~A\")](~A)" string path description href))

(def-syntax (:%newline string env ())
  (format stream "~%"))

(def-syntax (:%special string env ())
  (funcall (gethash string *specials*) stream env))

(def-special ("" env)
  (format stream "~%"))

;;;last one we need to do is references but this requires special processing rules
;;;as references have to be collected and added onto the end.

(defparameter *md*
  '((:h1 "abc")
    "oof"
    ""
    (:h3 "Who are we?")
    (:blockquote     
     "oof"
     ""
     (:code-block "coof")
     "oof")
    ""
    (:code-block "oof")
    (:h2 "abcd")
    (:link :href "https://oof.com" "ooga")
    (:linked-image
     :path "~/oof.jpg"
     :description "an oof image"
     :href "imgur.com/oof"
     "an oof image")    
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

(defun contains-options-p (list)
  "Checks if the key at (first LIST) has options associated with it, and if it does 
then searches for those options within LIST. If it has no options returns nil."
  (destructuring-bind (&key fun options)
      (getf *syntax* (first list))
    (declare (ignore fun))
    (when options
      (some (lambda (ele)
              (some (lambda (option)
                      (eql ele option))
                    options))
            list))))

(defun extract-options (list)
  "Attempts to extract the options keys and their values from LIST. For example a list 
like  (:link :href \"https://oof.com\" \"ooga\") should eval to a list like: 
'(:href \"https://oof.com\")."
  (loop :for (ele . rest) :on (rest list) :by #'cddr
        :while rest
        :appending (list ele (first rest))))

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

(defgeneric initiate-environment (list)
  (:documentation "This method is used to generate the initial environment plist that is passed to every syntax function within the parser. If you wish to modify the environment for 
your own syntax or specials, or you want to modify some of the default values then 
you can just create your own version of this that is specialized on a list, perform the 
modifications with (setf (getf ...)..) and finally return the list."))

(defmethod initiate-environment :around ((list list))
  "Sets up the default environment with the keys :current-depth :options :item-n 
:current-syntax and :references."
  (let ((environment
          (list :current-depth 0
                :options ()
                :item-n 1
                :current-syntax (getf *syntax* :%default)
                :references ())))
    (call-next-method environment)))

(defmethod initiate-environment ((list list))
  list)

(defgeneric initiate-spacing (list)
  (:documentation "Sets up the association between a key and the spacing applied after
its execution. The default values are (:h1 1) and (:blockquote 1). If you wish to 
customize this then you can change the defaults by specializing your own version 
with ((list list)) and making sure you return the list."))

(defmethod initiate-spacing :around ((list list))
  (let ((spacing ()))
    (call-next-method spacing)))

(defmethod initiate-spacing ((list list))
  (setf (getf list :h1) 1
        (getf list :blockquote) 1)
  list)

(defun parse-to-md (stream list)
  (let ((environment (initiate-environment ()))
        (*spacing* (initiate-spacing ())))
    (declare (special *spacing*))
    (mapc (lambda (element)
            (typecase element
              (string (process-list stream
                                    (list (if (gethash element *specials*)
                                              :%special
                                              :%default)
                                          element)
                                    environment))
              (list (process-list stream element environment))
              (otherwise (error "unknown"))))
          list)))

(defun process-list (stream list environment)
  (labels ((process-string (stream list environment)
             (let* ((syn (first list))
                    (ele (second list)))
               (setf (getf environment :current-syntax) syn)
               (add-spacing stream environment) 
               (execute syn stream ele environment)
               (format stream "~A" (make-string *default-spacing*
                                                :initial-element #\Newline))))
           (process-list (stream list environment)
             (declare (special *spacing*))
             (let ((sym (first list))
                   (options? (if (contains-options-p list)
                                 (extract-options list)
                                 nil)))
               (with-resetting-keys environment (item-n options)
                 (when options? 
                   (setf (getf environment :options) options?))
                 (mapc (lambda (arg)
                         (typecase arg
                           (string (process-string stream (list
                                                           (if (gethash arg *specials*)
                                                               :%special
                                                               sym)
                                                           arg)
                                                   environment))
                           (list (with-indenting environment
                                   (process-list stream arg environment)))))
                       (if options?
                           (list (first list) (car (last list)))
                           list))))))
    (process-list stream list environment)))


