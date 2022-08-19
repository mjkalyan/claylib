(in-package #:cl-user)
(defpackage claylib/examples/text-3
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/text-3)

(defparameter *assets* (list (make-font-asset (claylib/examples:claylib-path
                                               "examples/text/resources/KAISG.ttf")
                                              :size 96
                                              :glyph-count 0)))

(defparameter *scene*
  (make-scene ((kaisg-ass (car *assets*)))
              ((text-1 (make-text "Use mouse wheel to change font size"
                                  20 20))
               (text-2 (make-text "Use KEY_RIGHT and KEY_LEFT to move text"
                                  20 40))
               (text-3 (make-text "Use 1, 2, 3 to change texture filter"
                                  20 60))
               (text-4 (make-text "Drop a new TTF font for dynamic loading"
                                  20 80
                                  :color +darkgray+))
               (dynamic-font (make-text "Loaded Font"
                                        40 (- (/ (get-screen-height) 2) 80)
                                        :font (asset kaisg-ass)
                                        :size (size kaisg-ass)
                                        :spacing 0
                                        :color +black+))
               (rec (make-rectangle 0 (- (get-screen-height) 80) (get-screen-width) 80 +lightgray+))
               (text-fsize (make-text ""
                                      20 (- (get-screen-height) 50)
                                      :color +darkgray+))
               (text-tsize (make-text ""
                                      20 (- (get-screen-height) 50)
                                      :color +darkgray+))
               (text-filter (make-text "CURRENT TEXT FILTER:" 250 400 :size 20)))))

(defun main ()
  (with-window (:title "raylib [text] example - font filters")
    (with-scenes *scene*
      (with-scene-objects (dynamic-font text-fsize text-tsize) *scene*
        ;; FIXME should be address
        (claylib/ll:gen-texture-mipmaps (c-ref (claylib::c-struct (font dynamic-font))
                                               texture-t :texture &))
        (claylib/ll:set-texture-filter (claylib::c-struct (texture (font dynamic-font)))
                                       claylib/ll:+texture-filter-point+)
        (do-game-loop (:livesupport t
                       :vars ((text-size (make-vector2 0 0))
                              (current-font-filter 'text-point)))
          (incf (size dynamic-font) (* (get-mouse-wheel-move) 4))

          (cond
            ((is-key-pressed-p +key-one+)
             (progn (claylib/ll:set-texture-filter (claylib::c-struct (texture (font dynamic-font)))
                                                   claylib/ll:+texture-filter-point+)
                    (setf current-font-filter 'text-point)))
            ((is-key-pressed-p +key-two+)
             (progn (claylib/ll:set-texture-filter (claylib::c-struct (texture (font dynamic-font)))
                                                   claylib/ll:+texture-filter-bilinear+)
                    (setf current-font-filter 'text-bilinear)))
            ((is-key-pressed-p +key-three+)
             (progn (claylib/ll:set-texture-filter (claylib::c-struct (texture (font dynamic-font)))
                                                   claylib/ll:+texture-filter-trilinear+)
                    (setf current-font-filter 'text-trilinear))))

          (measure-text-ex dynamic-font :vector text-size)

          (cond ((is-key-down-p +key-left+) (decf (x dynamic-font) 10))
                ((is-key-down-p +key-right+) (incf (x dynamic-font) 10)))

          (setf (text text-fsize) (format t "Font size: ~2,2$" (size dynamic-font))
                (text text-tsize) (format t "Text size: [~2,2$, ~2,2$]" (x text-size) (y text-size)))

          (with-drawing ()
            (draw-scene-except *scene* 'text-point 'text-bilinear 'text-trilinear)
            (draw-scene *scene* current-font-filter)))))))
