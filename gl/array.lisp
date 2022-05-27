
(in-package #:cl-opengl)


; gl-array
(defstruct (gl-array (:copier nil))
  "Pointer to C array with size and type information attached."
  (pointer (null-pointer))
  (size 0 :type unsigned-byte)
  (type nil :type symbol))


(defmacro alloc-gl-array (type &rest foreign-alloc-args)
  "Allocates a new gl-array. Must be freed by free-gl-array."
  `(make-gl-array :pointer (foreign-alloc ,type ,@foreign-alloc-args)
                  :size (or ,(getf foreign-alloc-args :count) (max (length ,(getf foreign-alloc-args :initial-contents)) 1))
                  :type ,type))

#|
(defstruct (gl-vertex-array (:copier nil) (:include gl-array))
  "Like GL-ARRAY, but with an aditional vertex array binder."
  (binder #'identity :type function))
|#

#|
(defun alloc-gl-array (type count)
  (if (get type 'vertex-array-binder)
      (make-gl-vertex-array
       :pointer (foreign-alloc type :count count)
       :size count :type type :binder (get type 'vertex-array-binder))
      (make-gl-array :pointer (foreign-alloc type :count count)
                     :size count :type type)))
|#

#|
(declaim (inline make-gl-array-from-pointer))
(defun make-gl-array-from-pointer (ptr type count)
  "Same as ALLOC-GL-ARRAY but uses a supplied pointer instead of
allocating new memory."
  (let ((binder (find-vertex-array-binder type nil)))
    (if binder
        (make-gl-vertex-array :pointer ptr :size count
                              :type type :binder binder)
        (make-gl-array :pointer ptr :size count :type type))))
|#

(defun free-gl-array (array)
  "Frees an array allocated by ALLOC-GL-ARRAY."
  (foreign-free (gl-array-pointer array)))

(defun make-null-gl-array (type)
  "Returns a GL-ARRAY with a size of 0, a null pointer and of type TYPE."
  (make-gl-array :pointer (null-pointer) :size 0 :type type))


(declaim (inline gl-aref))
(defun gl-aref (array index &optional (component nil c-p))
  "Returns the INDEX-th component of ARRAY. If COMPONENT is
  supplied and ARRAY is of a compound type the component named
  COMPONENT is returned."
  (if c-p
    (foreign-slot-value (mem-aref (gl-array-pointer array)
                                  (gl-array-type array)
                                  index)
                        (gl-array-type array)
                        component)
    (mem-aref (gl-array-pointer array) (gl-array-type array) index)))


(declaim (inline (setf gl-aref)))
(defun (setf gl-aref) (value array index &optional (component nil c-p))
  "Sets the place (GL-AREF ARRAY INDEX [COMPONENT]) to VALUE."
  (if c-p
    (setf (foreign-slot-value (mem-aref (gl-array-pointer array)
                                        (gl-array-type array)
                                        index)
                              (gl-array-type array)
                              component)
          value)
    (setf (mem-aref (gl-array-pointer array) (gl-array-type array) index)
          value)))


(declaim (inline gl-array-pointer-offset))
(defun gl-array-pointer-offset (array offset)
  "Returns a pointer to the OFFSET-th element in ARRAY."
  (inc-pointer (gl-array-pointer array)
               (* (foreign-type-size (gl-array-type array)) offset)))


(declaim (inline gl-array-byte-size))
(defun gl-array-byte-size (array)
  "Returns the number of bytes in the array."
  (* (gl-array-size array) (foreign-type-size (gl-array-type array))))


(defmacro with-gl-array ((var type &rest gl-array-args) &body body)
  "Allocates a fresh GL-ARRAY of type TYPE and COUNT elements.
  The array will be bound to VAR and is freed when execution moves
  outside WITH-GL-ARRAY."
  `(let ((,var (alloc-gl-array ,type ,@gl-array-args)))
     (declare (dynamic-extent ,var))
     ,@body
     (free-gl-array ,var)))
