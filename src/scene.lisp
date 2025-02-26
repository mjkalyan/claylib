(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass game-scene ()
    ((%parameters :initarg :params
                  :type hash-table
                  :initform (make-hash-table :test #'equalp)
                  :accessor params)
     (%game-objects :initarg :objects
                    :type hash-table
                    :initform (make-hash-table :test #'equalp)
                    :accessor objects)
     (%game-assets :initarg :assets
                   :type hash-table
                   :initform (make-hash-table :test #'equalp)
                   :accessor assets)
     (%gc :initarg :gc
          :type boolean
          :accessor gc))))

(defun load-scene (scene &rest names)
  (dolist (asset names)
    (load-asset (gethash asset (assets scene)))))

(defun load-scene-all (scene)
  (dolist (asset (alexandria:hash-table-values (assets scene)))
    (load-asset asset)))

(defun load-scene-except (scene &rest names)
  (dolist (asset (remove-if (lambda (kv)
                              (member (car kv) names))
                            (alexandria:hash-table-alist (assets scene))))
    (load-asset (cdr asset))))

(defun draw-scene (scene &rest names)
  "Draw the objects in SCENE referred to by the symbols in NAMES."
  (dolist (obj names)
    (draw-object (scene-object scene obj))))

(defun draw-objects (&rest objects)
  "Draw the given OBJECTS without having to specify a scene.

This is handy when the objects are in scope already, for example via WITH-SCENE-OBJECTS."
  (dolist (obj objects)
    (draw-object obj)))

(defun draw-scene-all (scene)
  "Draw all the objects in the given SCENE."
  (dolist (obj (nreverse (alexandria:hash-table-values (objects scene))))
    (draw-object obj)))

(defun draw-scene-except (scene &rest names)
  "Draw all the objects in the given SCENE except those specified as one of NAMES."
  (dolist (obj (remove-if (lambda (kv)
                            (member (car kv) names))
                          (nreverse (alexandria:hash-table-alist (objects scene)))))
    (draw-object (cdr obj))))

(defun draw-scene-regex (scene regex)
  "Draw the objects in the SCENE whose name matches the given Perl REGEX."
  (loop for kv in (nreverse (alexandria:hash-table-alist (objects scene)))
        when (cl-ppcre:scan regex (symbol-name (car kv)))
          do (draw-object (cdr kv))))

(defmacro make-scene (assets objects &key (gc t) (defer-init t))
  "Make a GAME-SCENE.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually via WITH-SCENES or
SET-UP-SCENE directly). This is useful when your scene contains objects like TEXTURES which require
an OpenGL context before being loaded into the GPU.

GC expresses a preference for whether garbage collection should be run when the scene closes.
This can be overridden in WITH-SCENES.

ASSETS and OBJECTS are lists of items of the form (BINDING VALUE). In a nutshell: assets are
things that get loaded, and objects are things that get drawn. Values can reference previous
bindings but will be initialized in the order they are declared."
  (let ((scene (gensym))
        (syms (when defer-init
                (alexandria:make-gensym-list (length objects)))))
    `(let ((,scene (make-instance 'game-scene :gc ,gc))
           ,@syms)
       (declare (ignorable ,@syms))
       (symbol-macrolet (,@assets)
         ,@(loop for (binding val) in assets
                 collect `(setf (gethash ',binding (assets ,scene)) ,binding))
         ,(if defer-init
              `(symbol-macrolet (,@(loop for sym in syms
                                         for obj in objects
                                         collect `(,(car obj) (eager-future2:touch ,sym))))
                 (let* (,@(loop for sym in syms
                                for obj in objects
                                collect `(,sym (eager-future2:pcall
                                                (lambda () ,(cadr obj))
                                                :lazy))))
                   ,@(loop for sym in syms
                           for obj in objects
                           collect `(setf (gethash ',(car obj) (objects ,scene))
                                          ,sym))))
              `(symbol-macrolet (,@objects)
                 ,@(loop for (binding val) in objects
                         collect `(setf (gethash ',binding (objects ,scene)) ,binding)))))
       ,scene)))

(defmacro make-scene-pro ((&rest components) &key (gc t) (defer-init t))
  "Make a GAME-SCENE.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually via WITH-SCENES or
SET-UP-SCENE directly). This is useful when your scene contains objects like TEXTURES which require
an OpenGL context before being loaded into the GPU.

GC expresses a preference for whether garbage collection should be run when the scene closes.
This can be overridden in WITH-SCENES.

Each COMPONENT is a list whose head is either :PARAMS, :ASSETS, or :OBJECTS. In a nutshell: assets
are things that get loaded, objects are things that get drawn, and parameters are generic bindings
which are neither loaded nor drawn. Items after the head look like (BINDING VALUE). Values can
reference previous bindings but will be initialized in the order they are declared."
  (let* ((scene (gensym))
         (items (reduce #'append
                        (mapcar #'cdr components)))
         (syms (when defer-init
                 (alexandria:make-gensym-list (length items)))))
    (flet ((group-case (group)
             (ecase (car group)
               (:params `(params ,scene))
               (:assets `(assets ,scene))
               (:objects `(objects ,scene)))))
      `(let ((,scene (make-instance 'game-scene :gc ,gc))
             ,@syms)
         (declare (ignorable ,@syms))
         ,(if defer-init
              `(symbol-macrolet (,@(loop for sym in syms
                                         for item in items
                                         collect `(,(car item) (eager-future2:touch ,sym))))
                 (let* (,@(loop for sym in syms
                                for item in items
                                collect `(,sym (eager-future2:pcall
                                                (lambda () ,(cadr item))
                                                :lazy))))
                   ,@(loop for group in components
                           with i = -1
                           append (mapcar #'(lambda (item)
                                              (incf i)
                                              `(setf (gethash ',(car item) ,(group-case group))
                                                     ,(elt syms i)))
                                          (cdr group)))))
              `(symbol-macrolet (,@items)
                 ,@(loop for group in components
                         append (mapcar #'(lambda (item)
                                            `(setf (gethash ',(car item) ,(group-case group))
                                                   ,(car item)))
                                        (cdr group)))))
         ,scene))))

(defun scene-object (scene object)
  (gethash object (objects scene)))

(defun scene-asset (scene asset)
  (gethash asset (assets scene)))

(defun scene-param (scene param)
  (gethash param (params scene)))

(defmacro with-scene-objects (objects scene &body body)
  `(symbol-macrolet ,(loop for obj in objects
                           collect (if (listp obj)
                                       `(,(car obj) (gethash ,(cadr obj) (objects ,scene)))
                                       `(,obj (gethash ',obj (objects ,scene)))))
     ,@body))

(defmacro with-scenes (scenes (&key (gc nil gc-supplied-p)) &body body)
  "Execute BODY after loading & initializing SCENES, tearing them down afterwards.
Pass :GC (T or NIL) to force/unforce garbage collection, overriding what the scenes request.

Note: additional scenes can be loaded/GC'd at any point using {SET-UP,TEAR-DOWN}-SCENE."
  (unless (listp scenes) (setf scenes `(list ,scenes)))
  `(progn
     (mapcar #'set-up-scene ,scenes)
     ,@body
     ,(cond
        ((and gc-supplied-p gc) `(tg:gc :full t))
        ((not gc-supplied-p) `(mapcar #'tear-down-scene ,scenes))
        (t nil))))

(defmethod set-up-scene ((scene game-scene))
  (flet ((yield-things (ht)
           (maphash (lambda (binding val)
                      "Yield the futures in the hash table in place."
                      (when (typep val 'eager-future2:future)
                        (setf (gethash binding ht) (eager-future2:yield val))))
                    ht)))
    (yield-things (assets scene))
    (load-scene-all scene)
    (yield-things (params scene))
    (yield-things (objects scene))))

(defmethod set-up-scene ((scene null)) ())

(defmethod tear-down-scene ((scene game-scene))
  (when (gc scene) (tg:gc :full t)))

(defmethod tear-down-scene ((scene null)) ())
