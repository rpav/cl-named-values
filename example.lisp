(defpackage :named-values.test
  (:use #:cl #:named-values))

(in-package :named-values.test)

(defun get-x-y ()
  (named-values get-x-y :x 1 :y 2))

(nbind (y)
    (get-x-y)
  y) ;; => 2

(defun get-point (&optional 3d-p)
  (if 3d-p
      (named-values point-3d :x 0 :y 0 :z 1)
      (named-values point-2d :x 0 :y 0)))

(get-point)   ;; => 0, 0
(get-point t) ;; => 0, 0, 1

(ncase (get-point t)
  (point-2d (x y)
    (format t "A 2D point at ~A,~A~%" x y))
  (point-3d (x y z)
    (format t "A 3D point at ~A,~A,~A~%" x y z)))

;; "A 3D point at 0,0,1"

(ncase (get-point t)
  (t (type &rest values)
    (format t "Got ~A: ~A" type values)))

;; "Got POINT-3D: (0 0 1)"

(defun plain-values ()
  (values 1 2 3))

(defun map-names-to-values ()
  (values-map-names point-3d (x y z)
    (plain-values)))

(plain-values)         ;; => 1, 2, 3
(map-names-to-values)  ;; => 1, 2, 3

(nbind point-3d (x y z)
    (plain-values)
  (list x y z)) ;; => ERROR

(nbind point-3d (x y z)
    (map-names-to-values)
  (list x y z)) ;; => (1 2 3)
