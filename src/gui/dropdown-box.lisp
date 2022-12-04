(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-dropdown-box (gui-object text-label pressable editable)
    ((%active :initarg :active
              :type integer  ; TODO: pointer
              :accessor active))
    (:documentation "Dropdown Box control, sets PRESSED when clicked")))

(defun-pt-bool gui-dropdown-box claylib/ll:gui-dropdown-box
  "Dropdown Box control, returns selected item"
  (bounds rl-rectangle)
  (text string)
  (active integer)
  (edit-mode boolean))

(defmethod draw-object ((obj gui-dropdown-box))
  (setf (slot-value obj '%pressed)
        (gui-dropdown-box (bounds obj)
                          (text obj)
                          (active obj)
                          (edit-mode obj))))

(defun make-gui-dropdown-box (x y width height &rest args &key text active edit-mode)
  (declare (ignorable text active edit-mode))
  (apply #'make-instance 'gui-dropdown-box
         :bounds (make-simple-rec x y width height)
         args))
