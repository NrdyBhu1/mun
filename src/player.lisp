(in-package #:mun)

(deftype movement () '(member :up :down :left :right))

(defun load-player-textures (renderer)
  (list
   :move-down (load-texture renderer #P"./assets/_down walk.png")
   :move-up (load-texture renderer #P"./assets/_up walk.png")
   :move-side (load-texture renderer #P"./assets/_side walk.png")
   :idle-down (load-texture renderer #P"./assets/_down idle.png")
   :idle-up (load-texture renderer #P"./assets/_up idle.png")
   :idle-side (load-texture renderer #P"./assets/_side idle.png")
   :attack-down (load-texture renderer #P"./assets/_down attack.png")
   :attack-up (load-texture renderer #P"./assets/_up attack.png")
   :attack-side (load-texture renderer #P"./assets/_side attack.png")))

(progn
  (export '(player-pos))
  (defclass player()
    ((pos
      :initarg :pos
      :initform (make-instance 'vec2)
      :accessor player-pos
      :type vec2)
     (vel
      :initarg :vel
      :initform (make-instance 'vec2)
      :accessor player-vel
      :type vec2)
     (movement
      :initform :down
      :accessor player-movement
      :type movement)
     (textures
      :accessor player-textures)
     (counter
      :initform 0
      :accessor anim-counter)
     (frame
      :initform 0
      :accessor anim-frame)
     (speed
      :initarg :speed
      :accessor player-speed
      :type integer))))

(defmethod reset-anim ((p player))
  (setf (anim-counter p) 0)
  (setf (anim-frame p) 0))

;; init
(defmethod player-start ((p player) renderer)
  (setf (player-textures p) (load-player-textures renderer)))

;; deinit
(defmethod player-stop ((p player))
  (sdl2:destroy-texture (getf (player-textures p) :move-down))
  (sdl2:destroy-texture (getf (player-textures p) :move-up))
  (sdl2:destroy-texture (getf (player-textures p) :move-side))
  (sdl2:destroy-texture (getf (player-textures p) :idle-down))
  (sdl2:destroy-texture (getf (player-textures p) :idle-up))
  (sdl2:destroy-texture (getf (player-textures p) :idle-side))
  (sdl2:destroy-texture (getf (player-textures p) :attack-down))
  (sdl2:destroy-texture (getf (player-textures p) :attack-up))
  (sdl2:destroy-texture (getf (player-textures p) :attack-side)))

(defmethod player-attack ((p player))
  ())

;; player movement methods
(defmethod player-move-left ((p player))
  "Set horizontal motion to left"
  (setf (player-movement p) :left)
  (setf (vec2-x (player-vel p)) -1))

(defmethod player-move-right ((p player))
  "Set horizontal motion to right"
  (setf (player-movement p) :right)
  (setf (vec2-x (player-vel p)) 1))

(defmethod player-move-up ((p player))
  "Set vertical motion to up"
  (setf (player-movement p) :up)
  (setf (vec2-y (player-vel p)) -1))

(defmethod player-move-down ((p player))
  "Set vertical motion to down"
  (setf (player-movement p) :down)
  (setf (vec2-y (player-vel p)) 1))

(defmethod player-stop-vertical ((p player))
  "Stop vertical motion"
  (reset-anim p)
  (setf (vec2-y (player-vel p)) 0))

(defmethod player-stop-horizontal ((p player))
  "Stop horizontal motion"
  (reset-anim p)
  (setf (vec2-x (player-vel p)) 0))

(defmethod player-update-motion ((p player))
  "Updating the player's position according to the velocity"
  (unless (vec-is-empty (player-vel p))
    (if (= (anim-counter p) 30)
        (if (= (anim-frame p) 5)
            (setf (anim-frame p) 0)
            (setf (anim-frame p) (+ (anim-frame p) 1))))
    (if (= (anim-counter p) 30)
        (setf (anim-counter p) 0)
        (setf (anim-counter p) (+ (anim-counter p) 1))))
  (when (vec-is-empty (player-vel p))
    (if (= (anim-counter p) 30)
        (if (= (anim-frame p) 4)
            (setf (anim-frame p) 0)
            (setf (anim-frame p) (+ (anim-frame p) 1))))
    (if (= (anim-counter p) 30)
        (setf (anim-counter p) 0)
        (setf (anim-counter p) (+ (anim-counter p) 1))))
  (setf (vec2-x (player-pos p)) (+ (vec2-x (player-pos p)) (* (vec2-x (player-vel p)) (player-speed p))))
  (setf (vec2-y (player-pos p)) (+ (vec2-y (player-pos p)) (* (vec2-y (player-vel p)) (player-speed p))))
  (if (< (vec2-x (player-pos p)) -48)
      (setf (vec2-x (player-pos p)) 752))
  (if (< (vec2-y (player-pos p)) -48)
      (setf (vec2-y (player-pos p)) 452))
  (if (> (vec2-x (player-pos p)) 752)
      (setf (vec2-x (player-pos p)) -48))
  (if (> (vec2-y (player-pos p)) 452)
      (setf (vec2-y (player-pos p)) -48)))

;; player rendering methods
(defmethod player-render ((p player) renderer frames)
  "Rendering the player"
  (unless (vec-is-empty (player-vel p))
    (case (player-movement p)
      ((:up) (sdl2:render-copy renderer (getf (player-textures p) :move-up)
                               :source-rect (nth (anim-frame p) frames)
                               :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:down) (sdl2:render-copy renderer (getf (player-textures p) :move-down)
                                 :source-rect (nth (anim-frame p) frames)
                                 :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:left) (sdl2:render-copy renderer (getf (player-textures p) :move-side)
                                 :source-rect (nth (anim-frame p) frames)
                                 :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:right) (sdl2:render-copy-ex renderer (getf (player-textures p) :move-side)
                                     :source-rect (nth (anim-frame p) frames)
                                     :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)
                                     :angle 0
                                     :center nil
                                     :flip '(:horizontal)))))
  (when (vec-is-empty (player-vel p))
    (case (player-movement p)
      ((:up) (sdl2:render-copy renderer (getf (player-textures p) :idle-up)
                               :source-rect (nth (anim-frame p) frames)
                               :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:down) (sdl2:render-copy renderer (getf (player-textures p) :idle-down)
                                 :source-rect (nth (anim-frame p) frames)
                                 :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:left) (sdl2:render-copy renderer (getf (player-textures p) :idle-side)
                                 :source-rect (nth (anim-frame p) frames)
                                 :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)))
      ((:right) (sdl2:render-copy-ex renderer (getf (player-textures p) :idle-side)
                                     :source-rect (nth (anim-frame p) frames)
                                     :dest-rect (sdl2:make-rect (vec2-x (player-pos p)) (vec2-y (player-pos p)) 128 128)
                                     :angle 0
                                     :center nil
                                     :flip '(:horizontal))))))

