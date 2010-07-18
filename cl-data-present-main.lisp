;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash tables reader
(defun |#M-reader| (stream char arg)
  (declare (ignore char arg))
  (cons 'hashmap (read stream nil)))

  
(defun hashmap (&rest keys-and-vals)
  (let ((hash-table (make-hash-table)))
    (loop for (key value . rest) on keys-and-vals by #'cddr	 
	 do (setf (gethash key hash-table) value)
	 finally (return hash-table))))

(set-dispatch-macro-character #\# #\M #'|#M-reader|)

;;; Проверка
(setq ht #M(:key1 'val1 :key2 'val2))
(maphash #'(lambda (k v) (print (list k v))) ht)

(defun hashmap-test(&aux test-data)
  (let* ((test-data '(:key1 val1 :key2 val2 :key3 val3))
	 (hash-table (hashmap test-data)))
    (equal (princ (loop for key being the hash-keys in hash-table using (hash-value value)
	      collect key
	      collect value))
	   test-data)))
(hashmap-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String with fill pointer reader
(defparameter *initial-size-for-str-with-fp* 100)
(defun |#"-reader| (stream char arg)
  (declare (ignore char))
  (loop
     with result = (make-array (if arg arg *initial-size-for-str-with-fp*)
			       :element-type 'character 
			       :fill-pointer 0)
     with flag-not-end
     for char = (read-char stream)
     while (or flag-not-end (char/= char #\"))
     if (not 
	 (setq flag-not-end (char= char #\\)))
       do (format result (string char))
     finally (return result)))

(defun make-reader-error-fun (char)
  (lambda (stream char arg)
    (declare (ignore stream arg))
    #+sbcl (error 'sb-int:simple-reader-error :stream *error-output*
                  :format-control "no dispatch function defined for #\~c"
                  :format-arguments (list char))
    #-(or sbcl)
    (error 'simple-error :stream *error-output*
           :format-control "no dispatch function defined for #\~c"
           :format-arguments (list char))))

(defmethod enable-literal-syntax ((which (eql :str-with-fill-pointer)))
  (set-dispatch-macro-character #\# #\" #'|#"-reader|))

(defmethod disable-literal-syntax ((which (eql :str-with-fill-pointer)))
  (set-dispatch-macro-character #\# #\" (make-reader-error-fun #\{)))

(enable-literal-syntax :str-with-fill-pointer)

(type-of #"adfg sdf \"ascdf asd")
(type-of #50"sadfg sdf \"asdf asd")
(list #"as\"df")

(disable-literal-syntax :str-with-fill-pointer)
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object reader
(defun |#t-reader| (stream char arg)
  (declare (ignore char arg))
  (destructuring-bind (class &rest slots)
      (read stream nil)
    (loop 
       with new-obj = (make-instance class) 
       for (slot-name slot-value) in slots
       do (setf (slot-value new-obj slot-name) slot-value)
       finally (return new-obj))))

(defmethod enable-literal-syntax ((which (eql :object-reader)))
  (set-dispatch-macro-character #\# #\t #'|#t-reader|))

(defmethod disable-literal-syntax ((which (eql :object-reader)))
  (set-dispatch-macro-character #\# #\t (make-reader-error-fun #\{)))

(enable-literal-syntax :object-reader)

;;;check
(defclass myclass ()
  (x y z))

(with-slots (x y z)
    #t(myclass (x 4) (y 5) (z 6))
    (list x y z))