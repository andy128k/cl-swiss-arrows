(in-package :cl-swiss-arrows-test)


(defun subst-gensyms (form &rest symbols)
  (let ((map (make-hash-table :test #'eql)))
    (labels ((traverse(form)
               (cond
                ((consp form)
                 (mapcar #'traverse form))
                ((and (symbolp form)
                      (not (symbol-package form)))
                 (change form))
                (t
                 form)))
             
             (change (form)
               (unless (gethash form map)
                 (setf (gethash form map) (pop symbols)))
               (gethash form map)))
      
      (traverse form))))


(defmacro expands-to (expected expression)
  `(is (equal (quote ,expected)
              (subst-gensyms (macroexpand-1 (quote ,expression))
                             '{1} '{2} '{3}))))


(test |-<> and -<>>|
  (expands-to value
              (-<> value))

  (expands-to (first '(1))
              (-<> (first '(1))))

  (expands-to (func param)
              (-<> param func))

  (expands-to (func param)
              (-<> param (func)))

  (expands-to (func param 1 2 3)
              (-<> param (func 1 2 3)))

  (expands-to (func 1 2 3 param)
              (-<>> param (func 1 2 3)))

  (expands-to (func param)
              (-<> param (func <>)))

  (expands-to (car '(1 2 3))
              (-<> '(1 2 3)
                   car))

  (expands-to (let (({1} "a la guerre"))
                (concatenate 'string {1} " comme " {1}))
              (-<> "a la guerre"
                   (concatenate 'string <> " comme " <>)))

  (expands-to (let (({1}
                     (let (({2} param))
                       (+ {2} 2 {2}))))
                (* {1} {1}))
              (-<> param
                   (+ <> 2 <>)
                   (* <> <>)))

  (is (equalp #(1 2 0 3 4)
              (-<> 0
                   (* <> 5)
                   (vector 1 2 <> 3 4))))
      
  (is (equalp #(-1 0 1 2 3 4 5 6 7 8 9 10 11 12)
              (-<> #(1 2 3)
                   (concatenate 'vector #(-1 0) <> #(4 5)
                                (-<> 10
                                     (list 7 8 9 <> 11 12)
                                     (cons 6 <>))))))

  (is (equal '(0 1 2 3)
             (-<> 0 (list 1 2 3))))
  
  (is (equal '(1 2 3 0)
             (-<>> 0 (list 1 2 3))))
  
  (is (equalp #(0 1 2 3)
              (-<> 0 (vector 1 2 3))))
  
  (is (equalp #(1 2 3 0)
              (-<>> 0 (vector 1 2 3))))
  
  (is (equalp #(1 2 3)
              (-<> '(:a 1 :b 2) (member :a <>) cadr 1+ (vector 1 <> 3))))
  
  (is (equalp #(1 2 3)
              (-<> '((:a . 1) (:b . 2)) (assoc :a <>) cdr 1+ (vector 1 <> 3))))
  
  (is (equalp #(1 0 0)
              (-<> 0 (vector 1 <> <>))))

)

(defun just (v) v)
(defun just-nil () nil)
(defun just-t () t)

(test |some-<> and some-<>>|
  (is (null (some-<> (just-nil)
                     (concatenate 'string <> " + more"))))

  (is (null (some-<> (just-t)
                     not
                     (concatenate 'string <> " + more"))))

  (is (equal "asd + more"
             (some-<> (just "asd")
                      (concatenate 'string <> " + more"))))

  (is (equal " + moreasd"
             (some-<>> (just "asd")
                       (concatenate 'string " + more")))))


(defvar *out*)

(defmacro with-side-effects (expression)
  (let ((result (gensym)))
    `(let (*out*)
       (let ((,result ,expression))
         (values ,result
                 (reverse *out*))))))


(test |-!<> and -!<>>|
  (is (equal (values '(:foo "bar")
                     '("bar"))
             (with-side-effects (-!<> '(:foo "bar")
                                      (member :foo <>)
                                      cadr
                                      (push *out*)))))

  (is (equal (values '(:foo "bar")
                     '("bar"))
             (with-side-effects (-!<>> '(:foo "bar")
                                       (member :foo)
                                       cadr
                                       (push <> *out*)))))

  (is (equal (values '(:bar "foo" :baz ("quux" "you") :foo "bar")
                     '(("you" "got here")
                       ("got here" "you")
                       ("got" "you" "here")))
             (with-side-effects (-<> '(:foo "bar")
                                     (list* :baz '("quux" "you") <>)
                                     (-!<>  (member :baz <>) cadr second (list "got here") (push *out*))
                                     (-!<>> (member :baz)    cadr second (list "got here") (push <> *out*))
                                     (-!<>  (member :baz <>) cadr second (list "got" <> "here") (push *out*))
                                     (list* :bar "foo" <>))))))

