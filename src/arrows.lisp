(in-package :cl-swiss-arrows)


(defun diamond-1 (input form &optional default-to-last-position)
  (if (consp form)
      (let ((c (count '<> form)))
        (cond
         ((= c 0)
          (if default-to-last-position
              (append form (list input))
              (list* (car form) input (cdr form))))
         ((= c 1)
          (substitute input '<> form))
         (:otherwise
          (let ((v (gensym)))
            `(let ((,v ,input))
               ,(substitute v '<> form))))))

      (list form input)))


(defun diamond (x forms &optional default-to-last-position)
  (reduce (lambda (acc form)
            (diamond-1 acc form default-to-last-position))
          forms
          :initial-value x))


(defun some-diamond (x forms &optional default-to-last-position)
  (reduce (lambda (acc form)
            (let ((v (gensym)))
              `(let ((,v ,acc))
                 (when ,v ,(diamond-1 v form default-to-last-position)))))
          forms
          :initial-value x))


(defmacro -<> (x &rest forms)
  "the 'diamond wand': top-level insertion of x in place of '<>' symbol
   within the threaded form if present, otherwise
   mostly behave as the thread-first macro."
  (diamond x forms))


(defmacro -<>> (x &rest forms)
  "the 'diamond spear': top-level insertion of x in place of '<>' symbol
   within the threaded form if present, otherwise
   mostly behave as the thread-last macro."
  (diamond x forms t))


(defmacro some-<> (x &rest forms)
  "the diamond wand version of some->"
  (some-diamond x forms))


(defmacro some-<>> (x &rest forms)
  "the diamond spear version of some->"
  (some-diamond x forms t))


(defmacro -!<> (x &rest forms)
  "non-updating -<> for unobtrusive side-effects"
  (let ((y (gensym)))
    `(let ((,y ,x))
       (-<> ,y ,@forms)
       ,y)))


(defmacro -!<>> (x &rest forms)
  "non-updating -<>> for unobtrusive side-effects"
  (let ((y (gensym)))
    `(let ((,y ,x))
       (-<>> ,y ,@forms)
       ,y)))

