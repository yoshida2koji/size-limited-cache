(defpackage :size-limited-cache
  (:use :cl)
  (:export
   :create-cache-manager
   :add-cache
   :get-cache
   :clear-cache
   :clear-all-cache
   :all-cache-info))

(in-package :size-limited-cache)

(defun make-cache-table ()
  (make-hash-table :test #'equal :size 1024))


(defstruct cache-container
  (value)
  (size 0 :type (integer 0))
  (last-used-at 0 :type (integer 0))
  (on-cache-removed nil :type (or null function)))


(defstruct cache-manager
  (cache-table (make-cache-table) :type hash-table)
  (max-size 0 :type (integer 0))
  (size 0 :type (integer 0))
  (rate-of-max-size-to-clear 0 :type (real 0.0 1.0)))

(defun %clear-cache (manager key container)
  (decf (cache-manager-size manager) (cache-container-size container))
  (remhash key (cache-manager-cache-table manager))
  (when (functionp (cache-container-on-cache-removed container))
    (funcall (cache-container-on-cache-removed container)))
  (values))

(defun clear-cache (manager key)
  (let ((container (gethash key (cache-manager-cache-table manager))))
    (when container
      (%clear-cache manager key container)))
  (values))

(defun hash-table-entries (table)
  (let (entries)
    (maphash (lambda (k v) (push (cons k v) entries)) table)
    entries))

(defun cache-entry-sort-predicate (a b)
  (< (cache-container-last-used-at (cdr a))
     (cache-container-last-used-at (cdr b))))

(defun clear-old-cache (manager)
  (let* ((target-size (* (cache-manager-max-size manager) (cache-manager-rate-of-max-size-to-clear manager)))
         (cache-table (cache-manager-cache-table manager))
         (entries (sort (hash-table-entries cache-table) #'cache-entry-sort-predicate)))
    (labels ((clear (ls)
               (cond ((null ls) (values))
                     ((<= (cache-manager-size manager) target-size) (values))
                     (t (let ((entry (car ls)))
                          (%clear-cache manager (car entry) (cdr entry)))
                        (clear (cdr ls))))))
      (clear entries))))

(defun clear-all-cache (manager)
  (setf (cache-manager-cache-table manager) (make-cache-table)
        (cache-manager-size manager) 0)
  (values))



(defun create-cache-manager (&key (max-size (* 1024 1024 100))
                               (rate-of-max-size-to-clear 0.5))
  (make-cache-manager :max-size max-size
                      :rate-of-max-size-to-clear rate-of-max-size-to-clear))

(defun get-cache (manager key)
  (let ((container (gethash key (cache-manager-cache-table manager))))
    (cond ((null container) nil)
          (t (setf (cache-container-last-used-at container) (get-internal-real-time))
             (cache-container-value container)))))

(defun add-cache (manager key value size &optional on-cache-removed)
  (clear-cache manager key)
  (when (> (cache-manager-size manager) (cache-manager-max-size manager))
    (clear-old-cache manager))
  (setf (gethash key (cache-manager-cache-table manager))
        (make-cache-container :value value
                              :size size
                              :last-used-at (get-internal-real-time)
                              :on-cache-removed on-cache-removed))
  (incf (cache-manager-size manager) size)
  value)

(defun all-cache-info (manager)
  (mapcar (lambda (entry)
            (destructuring-bind (key . container) entry
              (list :key key
                    :size (cache-container-size container)
                    :last-used-at (cache-container-last-used-at container))))
          (sort (hash-table-entries (cache-manager-cache-table manager)) #'cache-entry-sort-predicate)))
