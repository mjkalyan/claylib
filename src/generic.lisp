(in-package #:claylib)

(defgeneric x (vec)
  (:documentation "Get the X value of a vector, or another object with a slot backed by a vector."))
(defgeneric y (vec)
  (:documentation "Get the Y value of a vector, or another object with a slot backed by a vector."))
(defgeneric z (vec)
  (:documentation "Get the Z value of a vector3/4, or another object with a slot backed by a vector3/4."))
(defgeneric w (vec)
  (:documentation "Get the W value of a vector4, or another object with a slot backed by a vector4."))

(defgeneric (setf x) (value vec)
  (:documentation "Set the X value of a vector, or another object with a slot backed by a vector."))
(defgeneric (setf y) (value vec)
  (:documentation "Set the Y value of a vector, or another object with a slot backed by a vector."))
(defgeneric (setf z) (value vec)
  (:documentation "Set the Z value of a vector3/4, or another object with a slot backed by a vector3/4."))
(defgeneric (setf w) (value vec)
  (:documentation "Set the W value of a vector4, or another object with a slot backed by a vector4."))

(defgeneric x1 (line)
  (:documentation "Get the X value of the start point of a line."))
(defgeneric y1 (line)
  (:documentation "Get the Y value of the start point of a line."))
(defgeneric z1 (line)
  (:documentation "Get the Z value of the start point of a 3D line."))
(defgeneric x2 (line)
  (:documentation "Get the X value of the end point of a line."))
(defgeneric y2 (line)
  (:documentation "Get the Y value of the end point of a line."))
(defgeneric z2 (line)
  (:documentation "Get the Z value of the end point of a 3D line."))

(defgeneric (setf x1) (value line)
  (:documentation "Set the X value of the start point of a line."))
(defgeneric (setf y1) (value line)
  (:documentation "Set the Y value of the start point of a line."))
(defgeneric (setf z1) (value line)
  (:documentation "Set the Z value of the start point of a 3D line."))
(defgeneric (setf x2) (value line)
  (:documentation "Set the X value of the end point of a line."))
(defgeneric (setf y2) (value line)
  (:documentation "Set the Y value of the end point of a line."))
(defgeneric (setf z2) (value line)
  (:documentation "Set the Z value of the end point of a 3D line."))

(defgeneric width (shape)
  (:documentation "Get the width (X dimension) of any shape."))
(defgeneric height (shape)
  (:documentation "Get the height (Y dimension) of any shape."))
(defgeneric len (shape)
  (:documentation "Get the length (Z dimension) of any 3D shape."))

(defgeneric (setf width) (value shape)
  (:documentation "Set the width (X dimension) of any shape."))
(defgeneric (setf height) (value shape)
  (:documentation "Set the height (Y dimension) of any shape."))
(defgeneric (setf len) (value shape)
  (:documentation "Set the length (Z dimension) of any 3D shape."))

(defgeneric draw-object (obj)
  (:documentation "Draw a game object. Should be used within a draw loop."))

(defgeneric fovy (cam)
  (:documentation "Get a 3D camera's field-of-view in Y."))
(defgeneric projection (cam)
  (:documentation "Get a 3D camera's projection type."))

(defgeneric (setf pos) (value cam)
  (:documentation "Set the position of a 3D camera. VALUE is a vector3."))
(defgeneric (setf target) (value cam)
  (:documentation "Set the target of a 2D or 3D camera. VALUE is a vector2/3."))
(defgeneric (setf up) (value cam)
  (:documentation "Set a 3D camera's 'up' vector (rotation over its axis). VALUE is a vector3."))
(defgeneric (setf fovy) (value cam)
  (:documentation "Set a 3D camera's field-of-view in Y."))
(defgeneric (setf projection) (value cam)
  (:documentation "Set a 3D camera's projection type."))

(defgeneric rot (cam)
  (:documentation "Get a 2D camera's rotation, in degrees."))
(defgeneric zoom (cam)
  (:documentation "Get a 2D camera's zoom value."))

(defgeneric (setf offset) (value cam)
  (:documentation "Set the offset of a 2D camera. VALUE is a vector2."))
(defgeneric (setf rot) (value cam)
  (:documentation "Set a 2D camera's rotation, in degrees."))
(defgeneric (setf zoom) (value cam)
  (:documentation "Set a 2D camera's zoom value."))

(defgeneric r (color)
  (:documentation "Get the red value of a color."))
(defgeneric g (color)
  (:documentation "Get the green value of a color."))
(defgeneric b (color)
  (:documentation "Get the blue value of a color."))
(defgeneric a (color)
  (:documentation "Get the alpha value of a color."))

(defgeneric (setf r) (value color)
  (:documentation "Set the red value of a color."))
(defgeneric (setf g) (value color)
  (:documentation "Set the green value of a color."))
(defgeneric (setf b) (value color)
  (:documentation "Set the blue value of a color."))
(defgeneric (setf a) (value color)
  (:documentation "Set the alpha value of a color."))

(defgeneric (setf radius) (value circle)
  (:documentation "Set the radius of a circle or sphere."))

(defgeneric size (obj)
  (:documentation "Get the size value or vector of some object."))

(defgeneric (setf size) (value obj)
  (:documentation "Set the size value or vector of some object."))

(defgeneric set-slot (slot obj value)
  (:documentation "Copy the slot values (of object VALUE) to the slots in child object SLOT of parent OBJ.
Don't use this if SLOT contains an atom or string -- use normal SETF instead."))

(defgeneric load-asset (asset &key force-reload)
  (:documentation "Load a game-asset's backing C object from a file.
Force a reload & free old memory when FORCE-RELOAD is T."))

(defgeneric copy-asset-to-object (asset)
  (:documentation "Return a copy of the object in the given ASSET's %ASSET slot."))

(defgeneric image-draw (image obj)
  (:documentation "Draw an object OBJ onto the the rl-image IMAGE."))

(defgeneric set-up-scene (scene)
  (:documentation "Load a SCENE's assets and initialize its objects."))

(defgeneric tear-down-scene (scene)
  (:documentation "Free a SCENE's assets and objects."))

(defgeneric switch-scene (scene)
  (:documentation "Switch *SCENE* to SCENE, loading the new & unloading the old scene."))

(defgeneric make-rl-*-array (c-struct num)
  (:documentation "Make an array of appropriate rl-* objects using NUM elements of the C array
referenced by C-STRUCT.

Warning: if NUM is greater than or equal to the number of elements in the C array, you will get
bogus data."))
