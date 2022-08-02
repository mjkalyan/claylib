(in-package #:cl-user)
(defpackage claylib/examples/models-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/models-1)

(defun pathname-from-relative-path (path)
  "Return an absolute pathname from a PATH relative to the claylib project root."
  (asdf:system-relative-pathname :claylib path))

(defparameter *assets*
  (list (make-model-asset (pathname-from-relative-path
                           "examples/models/resources/models/iqm/guyanim.iqm"))
        (make-texture-asset (pathname-from-relative-path
                             "examples/models/resources/models/iqm/guytex.png"))
        (make-animation-asset (pathname-from-relative-path
                               "examples/models/resources/models/iqm/guyanim.iqm"))))

(defparameter *scene*
  (make-scene ((model-asset (car *assets*))
               (model-texture (cadr *assets*))
               (anims (caddr *assets*)))
              ((camera (make-camera-3d 10 10 10
                                       0 0 0
                                       0 1 0
                                       :mode +camera-free+))
               (model (make-model model-asset
                                  0 0 0
                                  :rot-axis (make-vector3 1 0 0)
                                  :rot-angle -90
                                  :tint +white+))
               (grid (make-grid 10 1))
               (instructions (make-text "PRESS SPACE to PLAY MODEL ANIMATION"
                                        10 10
                                        :size 20
                                        :color +maroon+))
               (copyright (make-text "(c) Guy IQM 3D model by @culacant"
                                     (- (get-screen-width) 200) (- (get-screen-height) 20)
                                     :size 10
                                     :color +gray+)))))

(defun main ()
  (with-window (:title "raylib [models example - model animation")
    (with-scenes *scene*
      (with-scene-objects (camera model instructions copyright anims grid) *scene*
        (do-game-loop (:livesupport t
                       :vars ((anim-frame-counter 0)))
          (update-camera camera)
          ;; Play animation when spacebar is held down
          ;; (when (is-key-down-p +key-space+)
          ;;   (incf anim-frame-counter)
          ;;   (claylib/ll:update-model-animation model (first anims) anim-frame-counter)
          ;;   (when (>= anim-frame-counter (frame-count (first *anims*)))
          ;;     (setf anim-frame-counter 0)))

          (with-drawing
            (with-3d-mode camera
              (draw-object model)
              ;; TODO draw cubes
              (draw-object grid))
            (draw-object instructions)
            (draw-object copyright)))))))
