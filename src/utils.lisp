(in-package #:mun)

(defparameter *init-pat* nil)
(progn
  (export '(vec2-x vec2-y))
  (defclass vec2 ()
    ((x
      :initarg :x
      :initform 0
      :accessor vec2-x
      :type integer)
     (y
      :initarg :y
      :initform 0
      :accessor vec2-y
      :type integer))))

(defmethod vec-is-empty((v vec2))
  (and (eql (vec2-x v) 0) (eql (vec2-y v) 0)))

(defun init()
  (unless *init-pat*
    (sdl2:init :everything)
    (sdl2-image:init '(:jpg :png))
    (sdl2-mixer:init :mp3)
    (sdl2-mixer:open-audio 44100 :s16sys 2 1024)))

(defun deinit()
  (when *init-pat*
    (sdl2-mixer:close-audio)
    (sdl2-mixer:quit)
    (sdl2-image:quit)
    (sdl2:quit)))

(defun load-texture (renderer pathname)
  (let ((surface (sdl2-image:load-image pathname)))
    (prog1 (sdl2:create-texture-from-surface renderer surface)
      (sdl2:free-surface surface))))

