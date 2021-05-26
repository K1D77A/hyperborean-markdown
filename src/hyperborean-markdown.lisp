;;;; hyperborean-markdown.lisp

(in-package #:hyperborean-markdown)

(defparameter *default-spacing* 1
  "The default spacing applies after executing a syntax function.")

(defparameter *default-tab-size* 4
  "Inserted when adding spacing for lists")

(defparameter *syntax* ()
  "A plist of plists associating information regarding how to parse lisp to md.")

(defparameter *specials* (make-hash-table :test #'equal)
  "A hash table of lambdas associated with strings, these strings are special strings 
that have their own execution rules.")

(defparameter *inline-syntax* ()
  "A plist of plists associating before and after functions used to perform calls that 
wrap around other transformation. The plists look like '(:syntax <key> :before <before> 
:after <after>")

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
  (:documentation "Signalled when a lookup was performed for a key that does't exist.")
  (:report
   (lambda (obj stream)
     (format stream "Key: ~A. Where: ~A~%Message: ~A"
             (key obj)
             (where obj)
             (message obj)))))

(define-condition unknown-option (hyper-condition)
  ((key
    :accessor key
    :initarg :key
    :type keyword
    :documentation "A keyword that is supposed to be an option, but isn't.")
   (syntax
    :accessor syntax
    :initarg :syntax
    :type keyword
    :documentation "The syntax you tried to find key for.")
   (options
    :accessor options
    :initarg :options
    :type list
    :documentation "The list of valid options."))
  (:documentation "This is signalled when the user has provided an option that doesnt
exist for that markdown")
  (:report
   (lambda (obj stream)
     (format stream "That option doesn't exist for that syntax.~%Syntax: ~A. Key: ~A. ~
                     Valid options: ~A.~%Message: ~A."
             (syntax obj)
             (key obj)
             (options obj)
             (message obj)))))

(defun new-inline-syntax (key before-fun after-fun)
  "Associate a before and after function with the KEY within *inline-syntax*. If KEY is not
first found within *syntax* then signals 'key-missing-fun."
  (if (getf *syntax* key)
      (setf (getf *inline-syntax* key)
            (list :syntax key :before before-fun :after after-fun))
      (error 'key-missing-fun
             :where "*syntax*" :key key
             :message "Trying to create a new inline-syntax for a key that doesn't exist.")))

(defun get-inline (key)
  (let ((inline (getf *inline-syntax* key)))
    (if inline
        inline
        (getf *inline-syntax* :%default))))

(defmacro def-inline-syntax ((key arg-name environment-name)
                             before-fun
                             after-fun)
  `(new-inline-syntax ,key
                      (lambda (stream ,arg-name ,environment-name)
                        (declare (ignorable stream ,arg-name ,environment-name))
                        (locally ,before-fun))
                      (lambda (stream ,arg-name ,environment-name)
                        (declare (ignorable stream ,arg-name ,environment-name))
                        (locally ,after-fun))))
                        

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

(defun add-spacing (stream env)
  (let ((current-depth (getf env :current-depth)))
    (unless (<= current-depth 1)
      (format stream "~A" (make-string *default-tab-size*
                                       :initial-element #\Space)))))

(defun increase-depth (env)
  "Increment :current-depth by 1."
  (incf (getf env :current-depth)))

(defun decrease-depth (env)
  "Unless :current-depth is already 0, remove 1."
  (unless (zerop (getf env :current-depth))
    (decf (getf env :current-depth))))

(defmacro with-increased-depth (env &body body)
  "Evaluates BODY after incrementing :current-depth by 1, after evaluation decrements 
:current-depth by ."
  `(unwind-protect (progn (increase-depth ,env)
                          (locally ,@body))
     (decrease-depth ,env)))

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
  ;;  (setf (getf env :disable-before-p) t)
  (let ((item-n (getf env :item-n)))
    (format stream "~D. ~A" item-n string)
    (incf (getf env :item-n))))

(def-syntax (:unordered-list string env)
  ;;(setf (getf env :disable-before-p) t)
  (format stream "- ~A" string))

(def-syntax (:code-block string env (:lang))
  (if lang
      (format stream "```~A~% ~A~%```" lang string)
      (format stream "```~%~A~%```" string)))

(def-syntax (:image string env (:href))
  (format stream "![~A](~A)" string href))

(def-syntax (:link string env (:href :title))
  (if title 
      (format stream "[~A](~A \"~A\")" string href title)
      (format stream "[~A](~A)" string href)))

(def-syntax (:hrule string env)
  (format stream "***"))

(def-syntax (:linked-image string env (:path :description :href))
  (print path *debug-io*)
  (format stream "[![~A](~A \"~A\")](~A)" string path description href))

(def-syntax (:%special string env ())
  (funcall (gethash string *specials*) stream env))

(def-special ("" env)
  (format stream "~%"))

(def-special ("**" env)
  (format stream "**"))

(def-inline-syntax (:bold form env)
                   (format stream "**")
                   (format stream "**"))

(def-inline-syntax (:italic form env)
                   (format stream "*")
                   (format stream "*"))

(def-inline-syntax (:ordered-list form env)
                   (add-spacing stream env)
                   (format stream ""))

(def-inline-syntax (:unordered-list form env)
                   (progn (add-spacing stream env))
                   ;;(format stream "- "))
                   (format stream ""))

(def-inline-syntax (:bold-and-italic form env)
                   (format stream "***")
                   (format stream "***"))

(def-inline-syntax (:blockquote form env)
                   (format stream "> ")
                   (format stream ""))

(def-inline-syntax (:blockquote1 form env)
                   (format stream ">> ")
                   (format stream ""))

(def-inline-syntax (:code-block form env)
                   (format stream "``` ")
                   (format stream " ```"))

(def-inline-syntax (:%default form env)
                   nil
                   (format stream "~A" (make-string *default-spacing*
                                                    :initial-element #\Newline)))

;;;last one we need to do is references but this requires special processing rules
;;;as references have to be collected and added onto the end.


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
  (let ((keys (getf (getf *syntax* (first list)) :options)))
    (loop :for (key val) :on (rest list) :by #'cddr
          :if (and (keywordp key)
                   (find key keys))
            :appending (list key val) :into options
          :else :if (stringp key)
                  :collect key :into remainder
          :else :do (error 'unknown-option :syntax (first list) :key key 
                                           :options keys :message "Bad key")
          :finally (return (values options remainder)))))

(defmacro with-resetting-keys (environment keys &body body)
  "Stores the values associated with KEYS found in the plist ENVIRONMENT, 
stores them before the evaluation of BODY then resets the keys back to their previous 
values."
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
  (:documentation "This method is used to generate the initial environment
 plist that is passed to every syntax function within the parser.
 If you wish to modify the environment for 
your own syntax or specials, or you want to modify some of the default values then 
you can just create your own version of this that is specialized on a list, perform the 
modifications with (setf (getf ...)..) and finally return the list."))

(defmethod initiate-environment :around ((list list))
  "Sets up the default environment with the keys :current-depth :options :item-n 
:current-syntax and :references."
  (let ((environment
          (list :current-depth 0
                :options ()
                :current-inline (list (get-inline :%default))
                :item-n 1
                :add-spacing-p t
                :add-newline-p t
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

(defmacro destructuring-environment (environment &body body)
  `(destructuring-bind (&key current-depth options current-inline item-n
                          add-spacing-p add-newline-p current-syntax references
                        &allow-other-keys)
       ,environment
     (declare (ignorable current-depth options current-inline item-n
                         add-spacing-p add-newline-p current-syntax references))
     (locally ,@body)))

(defun parse-to-md (stream list)
  (let ((environment (initiate-environment ()))
        (*spacing* (initiate-spacing ())))
    (declare (special *spacing*))
    (%process-list stream list environment)))

(defun %process-list (stream list environment)
  (destructuring-environment environment
    (let ((item (first list))
          (remainder (rest list)))
      (typecase item
        (null
         (pop (getf environment :current-inline))
         nil)
        (string         
         (%execute-inlines stream item environment (or (rest current-inline)
                                                       current-inline))
         (%process-list stream remainder environment))
        (list
         (if (or (eql (first item) :ordered-list)
                 (eql (first item) :unordered-list))
             (with-resetting-keys environment (item-n current-depth)
               (when (eql (first item) :ordered-list)
                 (setf (getf environment :item-n) 1));reset when inside a new ordered-list
               (increase-depth environment)
               (%process-list stream item environment))
             (progn 
               (%process-list stream item environment)))
         (%process-list stream remainder environment))
        (keyword
         (with-resetting-keys environment (current-syntax options item-n)
           (push (get-inline item) (getf environment :current-inline))
           (let ((syn (getf *syntax* item)))
             (unless syn
               (error 'key-missing-fun :where "syntax" :key item
                                       :message "No function found for that key"))
             (setf (getf environment :current-syntax) syn))
           (if (contains-options-p list)
               (multiple-value-bind (options remainder)
                   (extract-options list)
                 (setf (getf environment :options) options)
                 (%process-list stream remainder environment))
               (%process-list stream remainder environment))))))))

(defun clean-inlines (list)
  list)
  ;; (loop :for inline :in list
  ;;       :unless (or (eql (second inline) :ordered-list)
  ;;                   (eql (second inline) :unordered-list))
  ;;         :collect inline))

(defun %execute-inlines (stream item environment inlines)
  (destructuring-bind (&key fun options &allow-other-keys)
      (getf environment :current-syntax)
    (declare (ignorable options))
    (mapc (lambda (inline)
            (destructuring-bind (&key before &allow-other-keys)
                inline
              (unless (getf environment :disable-before-p)
                (funcall before stream item environment))))
          (reverse (clean-inlines inlines)))
    (funcall fun stream item environment)
    (mapc (lambda (inline)
            (destructuring-bind (&key after &allow-other-keys)
                inline
              (unless (getf environment :disable-after-p)
                (funcall after stream item environment))))
          (clean-inlines inlines))))

(defmacro with-markdown ((stream) &body body)
  `(parse-to-md ,stream ',@body))

(defmacro with-markdown-to-string (&body body)
  (alexandria:with-gensyms (stream)
    `(with-output-to-string (,stream)
       (parse-to-md ,stream ',@body))))

(defmacro with-markdown-to-file ((pathname &rest keys &key &allow-other-keys)
                                 &body body)
  `(alexandria:write-string-into-file (with-markdown-to-string ,@body) ,pathname ,@keys))

(defparameter *md*
  '("oof"
    (:h1 "abc")
    "oof"
    (:h3 "Who are we?")    
    (:blockquote     
     "1"
     (:bold "2"
      "3")
     (:code-block 
      (:blockquote
       (:bold 
        "bbb"
        (:bold "10"))))
     "5"
     "6")
    (:bold
     "oof"
     "doof"
     (:italic "stucked")
     (:italic "oof"))    
    (:unordered-list
     "link1"
     "oof"
     ;;     (:bold-and-italic (:link :href "https://oof.com" "ooga"))
     (:unordered-list
      "abc"
      "def"
      (:unordered-list
       "oof")))
    (:code-block "oof")
    (:h2 "abcd")
    (:link :href "https://oof.com" "ooga")
    ""
    (:linked-image
     :path "~/oof.jpg"
     :description "an oof image"
     :href "imgur.com/oof"
     "an oof image")
    (:ordered-list "next list"
     (:ordered-list "def" "abc"))
    (:ordered-list 
     "oof"
     "boof")
    (:ordered-list "next list"
     (:unordered-list "1" (:h1 "2") "3"
      (:ordered-list "next-list")))
    "lu lu lu I want some apples"
    (:bold "bold")
    (:italic "italic")
    (:ordered-list "1" "2" "3")
    (:blockquote
     "abc"
     "def")))
