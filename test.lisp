(defpackage :size-limited-cache-test
  (:use :cl :size-limited-cache))

(in-package :size-limited-cache-test)


(defun basic ()
  (let ((manager (create-cache-manager :max-size 100)))
    (assert (null (get-cache manager (list "aaa" 10))))
    (assert (equal "xxx" (add-cache manager (list "aaa" 10) "xxx" 10)))
    (assert (equal "xxx" (get-cache manager (list "aaa" 10))))
    (add-cache manager (list "aaa" 100) "yyy" 10)
    (assert (equal "xxx" (get-cache manager (list "aaa" 10))))
    (assert (equal "yyy" (get-cache manager (list "aaa" 100))))
    (clear-cache manager (list "aaa" 10))
    (assert (null (get-cache manager (list "aaa" 10))))
    (clear-all-cache manager)
    (assert (null (get-cache manager (list "aaa" 100))))))

(defun auto-clear-cache ()
  (let ((manager (create-cache-manager :max-size 100)))
    (add-cache manager (list "a1" 10) "x1" 20)
    (add-cache manager (list "a2" 10) "x2" 20)
    (add-cache manager (list "a3" 10) "x3" 20)
    (add-cache manager (list "a4" 10) "x4" 20)
    (add-cache manager (list "a5" 10) "x5" 21)
    (sleep 0.01)
    (assert (equal "x1" (get-cache manager (list "a1" 10))))
    (assert (equal "x5" (get-cache manager (list "a5" 10))))
    (add-cache manager (list "a6" 10) "x6" 1)
    (assert (equal "x1" (get-cache manager (list "a1" 10))))
    (assert (equal "x5" (get-cache manager (list "a5" 10))))
    (assert (null (get-cache manager (list "a2" 10))))
    (assert (null (get-cache manager (list "a3" 10))))
    (assert (null (get-cache manager (list "a4" 10))))
    (assert (equal "x6" (get-cache manager (list "a6" 10))))))

;; (basic)

;; (auto-clear-cache)
