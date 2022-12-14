(ql:quickload '("trivial-gamekit" 
                "trivial-arguments"))

(defconstant rad (/ pi 180) "Degrees to Radians") 

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *red*   (gamekit:vec4 1 0 0 1))

(gamekit:define-font :brick-font "assets/NotoSans-Regular.ttf")
(defvar *font* nil)

(gamekit:defgame bullet-sim () ()
  (:viewport-width 800)
  (:viewport-height 800)
  (:viewport-title "Bullet hell pattern simulation"))

(defmacro loop-array (arr el &body body)
  `(loop :for i :from 0 :below (length ,arr) :do
         (let ((,el (aref ,arr i))) ,@body)))

(defmacro do-array (arr &body body)
  `(dotimes (i (length ,arr))
         ,@body))

(defgeneric draw (object) 
  (:documentation "Draw an object in the game"))

(defgeneric update (object) 
  (:documentation "Update an object in the game"))

(defclass 2d-object () 
  ((origin 
     :accessor origin
     :initarg :origin
     :initform (gamekit:vec2 0 0)))
  (:documentation "A 2d object in the game"))

(defmethod draw ((object 2d-object)) ())
(defmethod update ((object 2d-object)) ())

(defclass line (2d-object)
  ((end
     :accessor end
     :initarg :end
     :initform (gamekit:vec2 0 0))
   (color
     :accessor color
     :initarg :color
     :initform *black*)
   (thickness
     :accessor thickness
     :initarg :thickness
     :initform 1))
  (:documentation "A line that goes from origin to end"))

(defmethod draw ((object line))
  (gamekit:draw-line (origin object) (end object) (color object)
                     :thickness (thickness object)))

(defclass translatable (2d-object)
  ((target
     :accessor target
     :initarg :target
     :initform (gamekit:vec2 0 0))
   (vvec
     :accessor vvec
     :initform (gamekit:vec2 0 0))
   (old-origin
     :accessor old-origin
     :initform (gamekit:vec2 0 0))
   (dist
     :accessor dist
     :initform 0)
   (moving
     :accessor moving
     :initform nil)
   (target-speed
     :accessor target-speed
     :initarg :target-speed
     :initform 1))
  (:documentation "A 2d object that will move towards a target each frame"))

(defun clone-vec2 (vec) (gamekit:vec2 (gamekit:x vec) (gamekit:y vec)))

;; Can probably rewrite all of these to use mult, add, div & subt
(defun calc-1-dist (target current) (expt (- target current) 2))
(defun calc-dist (target current) 
  (+ (calc-1-dist (gamekit:x target) (gamekit:x current))
     (calc-1-dist (gamekit:y target) (gamekit:y current))))
(defun calc-1-vvec (target current target-speed sqrdist)
  (* (/ (- target current) sqrdist) target-speed))
(defun calc-vvec (target current target-speed sqrdist)
  (gamekit:vec2 
    (calc-1-vvec (gamekit:x target) (gamekit:x current) target-speed sqrdist) 
    (calc-1-vvec (gamekit:y target) (gamekit:y current) target-speed sqrdist)))

(defmethod set-target (new-target (object translatable))
  (setf (old-origin object) (clone-vec2 (origin object))
        (target object) new-target
        (dist object) (calc-dist (target object) (origin object)))
  (when (= (dist object) 0)
    (setf (vvec object) (gamekit:vec2 0 0)
          (moving object) nil)
    (return-from set-target))
  (setf (vvec object) (calc-vvec 
                        (target object) 
                        (origin object) 
                        (target-speed object) 
                        (sqrt (dist object)))
        (moving object) t))

(defmethod update ((object translatable))
  (when (moving object)
    (setf (origin object) (gamekit:add (origin object) (vvec object)))
    (when (>= (calc-dist (old-origin object) (origin object)) (dist object))
      (setf (origin object) (clone-vec2 (target object))
            (vvec object) (gamekit:vec2 0 0)
            (moving object) nil))))

(defmethod initialize-instance :after ((object translatable) &rest args)
  (declare (ignore args))
  (set-target (target object) object))

(defclass circle (2d-object) 
  ((radius
     :accessor radius
     :initarg :radius
     :initform 2)
   (fill-paint
     :accessor fill-paint
     :initarg :fill-paint
     :initform *red*))
  (:documentation "A 2d circle in the game"))

(defmethod draw ((object circle))
  (gamekit:draw-circle (origin object) (radius object) :fill-paint (fill-paint object)))


(defclass bullet (circle translatable) ()
  (:documentation "A single bullet"))

(defun make-adj-arr () (make-array '(0) :adjustable t :fill-pointer t))
(defun make-empty-arr (len) (make-array len :initial-element 'none))
(defclass brick (2d-object)
  ((children
     :accessor children
     :initarg :children
     :initform (make-adj-arr))
   (text
     :accessor text
     :initarg :text
     :allocation :class
     :initform "default-brick")
   (w
     :accessor w
     :initarg :w
     :allocation :class
     :initform 20)
   (h
     :accessor h
     :initarg :h
     :allocation :class
     :initform 20)
   (color
     :accessor color
     :initarg :color
     :allocation :class
     :initform (gamekit:vec4 0 1 1 1))
   (input
     :accessor input
     :initarg :input
     :initform (make-empty-arr 2))
   (input-color
     :accessor input-color
     :initarg :input-color
     :allocation :class
     :initform (gamekit:vec4 0 0 0 1))
   (output-color
     :accessor output-color
     :initarg :output-color
     :allocation :class
     :initform (gamekit:vec4 0 0 0 1))
   (input-hover-color
     :accessor input-hover-color
     :initarg :input-hover-color
     :allocation :class
     :initform (gamekit:vec4 1 0 0 1))
   (output-hover-color
     :accessor output-hover-color
     :initarg :output-hover-color
     :allocation :class
     :initform (gamekit:vec4 1 0 0 1))  
   (input-w
     :accessor input-w
     :initarg :input-w
     :allocation :class
     :initform 5)
   (input-h
     :accessor input-h
     :initarg :input-h
     :allocation :class
     :initform 5)
   (output-w
     :accessor output-w
     :initarg :output-w
     :allocation :class
     :initform 5)
   (output-h
     :accessor output-h
     :initarg :output-h
     :allocation :class
     :initform 5)
   (output
     :accessor output
     :initarg :output
     :initform (make-empty-arr 2)))
  (:documentation "A generic brick"))

(defgeneric run-brick (object)
  (:documentation "Executes code for a brick"))

(defmethod set-input (i new-input (object brick))
  (setf (aref (input object) i) new-input))

(defmethod set-output (i new-output (object brick))
  (setf (aref (output object) i) new-output))

(defun calc-input-origin (i object)
  (gamekit:add (origin object)
               (gamekit:vec2 (* i (* (input-w object) 2)) 0)))

(defun calc-output-origin (i object)
  (gamekit:add (gamekit:subt 
                 (origin object)
                 (gamekit:vec2 0 (- (output-h object) (h object))))
               (gamekit:vec2 (* i (* (output-w object) 2)) 0)))

(defclass menu-element ()
  ((text
     :accessor text
     :initarg :text
     :initform "----")
   (run
     :accessor run
     :initarg :run
     :initform (lambda ())))
   (:documentation "A menu element"))

(defun make-menu-element (text run)
  (make-instance 'menu-element :text text :run run))

(defclass context-menu (2d-object)
  ((elements
     :accessor elements
     :initarg :elements
     :initform (make-adj-arr))
   (element-w
     :accessor element-w
     :initarg :element-w
     :initform 200)
   (element-h
     :accessor element-h
     :initarg :element-h
     :initform 30)
   (element-color
     :accessor element-color
     :initarg :element-color
     :initform (gamekit:vec4 0 0 0 1))
   (element-font-color
     :accessor element-font-color
     :initarg :element-font-color
     :initform (gamekit:vec4 1 1 1 1)))
  (:documentation "A context menu with elements"))

(defun calc-menu-origin (i context-menu)
  (gamekit:vec2 0
                (+ (* (element-h context-menu) (+ i 1)) -10)))
(defun calc-menu-text-origin (context-menu)
  (gamekit:vec2 5
                (- (/ (element-h context-menu) 2) 6)))
(defun calc-ctx-menu-origin (object i) 
  (gamekit:subt 
    (origin object) (calc-menu-origin i object)))

(defmethod draw ((object context-menu))
  (let ((text-origin (calc-menu-text-origin object)))
  (loop-array (elements object) element
    (let ((menu-origin (calc-ctx-menu-origin object i)))
    (gamekit:draw-rect menu-origin
                       (element-w object)
                       (element-h object)
                       :fill-paint (element-color object))
    (gamekit:draw-text (text element)
                       (gamekit:add menu-origin text-origin)
                       :font *font*
                       :fill-color (element-font-color object))))))

(defmethod push-menu-element (text run (object context-menu))
  (vector-push-extend (make-menu-element text run) (elements object)))

(defvar *bullets* (make-adj-arr))
(defvar *bricks* (make-adj-arr))
(defvar *ui* (make-adj-arr))
(defvar *other* (make-adj-arr))
(defvar *dragging* nil)
(defvar *draggingbrick* nil)
(defvar *mouse-x* 10000)
(defvar *mouse-y* 10000)
(defvar *mouse-w* 1)
(defvar *mouse-h* 1)
(defvar *mousebrick* nil)
(defvar *dragginginput* nil)
(defvar *draggingoutput* nil)

(defclass mouse-line (line) () (:documentation "A line that goes from origin to the mouse position"))

(defmethod update ((object mouse-line))
  (setf (end object) (gamekit:vec2 *mouse-x* *mouse-y*)))

(defun push-brick (&optional (brick 'brick))
  (aref *bricks* (vector-push-extend (make-instance brick) *bricks*)))

(defun push-mouse-line (origin &optional (color *black*) (thickness 1))
  (aref *other* (vector-push-extend 
                  (make-instance 
                    'mouse-line 
                    :origin origin
                    :end origin
                    :color color
                    :thickness thickness)
                  *other*)))

(defun push-context-menu (origin)
  (aref *ui* (vector-push-extend (make-instance 'context-menu :origin origin) *ui*)))

(defun check-less (s a b) (< a (+ b s)))
(defun check-greater (s a b) (> (+ a s) b))
(defun is-in-rect-bounds (w h origin x2 y2 w2 h2)
  (let ((x1 (+ (gamekit:x origin) w)) (y1 (+ (gamekit:y origin) h)))
  (return-from is-in-rect-bounds 
               (and (check-less w x1 x2) (check-greater w2 x1 x2)
                    (check-less h y1 y2) (check-greater h2 y1 y2)))))
(defun mouse-on (object) 
  (is-in-rect-bounds 
    (w object) 
    (h object) 
    (origin object) 
    *mouse-x* 
    *mouse-y*
    *mouse-w*
    *mouse-h*))
(defun mouse-on-input (i object) 
  (is-in-rect-bounds 
    (input-w object) 
    (input-h object) 
    (calc-input-origin i object) 
    *mouse-x* 
    *mouse-y*
    *mouse-w*
    *mouse-h*))
(defun mouse-on-output (i object) 
  (is-in-rect-bounds 
    (output-w object) 
    (output-h object) 
    (calc-output-origin i object) 
    *mouse-x* 
    *mouse-y*
    *mouse-w*
    *mouse-h*))
(defun mouse-on-menu (ctx-menu i) 
  (is-in-rect-bounds 
    (element-w ctx-menu) 
    (element-h ctx-menu) 
    (calc-ctx-menu-origin ctx-menu i) 
    *mouse-x* 
    *mouse-y*
    (element-w ctx-menu)
    *mouse-h*))

(defun find-mouse-input (object)
  (do-array (input object)
    (when (mouse-on-input i object)
      (return-from find-mouse-input i))))
(defun find-mouse-output (object)
  (do-array (output object)
    (when (mouse-on-output i object)
      (return-from find-mouse-output i))))
(defun find-mouse-brick ()
  (loop-array *bricks* brick
    (when (mouse-on brick)
      (return-from 
        find-mouse-brick 
        (values 
          brick 
          (find-mouse-input brick)
          (find-mouse-output brick))))))

(defun get-input-color (i object)
  (if 
    (mouse-on-input i object)
    (input-hover-color object)
    (input-color object)))
(defun get-output-color (i object)
  (if 
    (mouse-on-output i object)
    (output-hover-color object)
    (output-color object)))
(defun draw-output-line (i object output)
  (when (not (eq output 'none))
    (gamekit:draw-line 
      (calc-output-origin i object) 
      (calc-input-origin (car output) (cdr output)) 
      *black*)))
(defmethod draw ((object brick))
  (gamekit:draw-rect (origin object) (w object) (h object)
                     :fill-paint (color object))
  (do-array (input object)
    (gamekit:draw-rect (calc-input-origin i object)
      (input-w object) (input-h object) 
      :fill-paint (get-input-color i object)))
  (loop-array (output object) output
    (draw-output-line i object output)
    (gamekit:draw-rect (calc-output-origin i object) 
      (output-w object) (output-h object)
      :fill-paint (get-output-color i object)))
  (gamekit:draw-text (text object) (gamekit:add 
                                     (origin object) 
                                     (gamekit:vec2 (w object) (h object))) 
                                     :font *font*))
(defun ctx-menu-input () 
  (format t "Input~%"))

(defun ctx-menu-new (object)
  (setf (origin (push-brick)) (origin object)))

(defun gamekit-cursor-bind ()
  (gamekit:bind-cursor 
    (lambda (x y)
      (setf *mouse-x* x
            *mouse-y* y)
      (when *draggingbrick*
        (setf (origin *draggingbrick*) (gamekit:vec2 x y)))))

  (gamekit:bind-button :mouse-left :pressed
    (lambda () 
      (block mouse-left
      (loop-array *ui* ui
        (loop-array (elements ui) menu
            (when (mouse-on-menu ui i)
              (format t "~A~%" i)
              (funcall (run menu))
              (setf *ui* (make-adj-arr))
              (return-from mouse-left))))

      (multiple-value-bind 
            (brick input output) 
            (find-mouse-brick)
        (when input
          (setf *other* (make-adj-arr))
          (when *draggingoutput*
            (set-output *draggingoutput* (list input brick) *mousebrick*)
            (setf *draggingoutput* nil)
            (setf *dragginginput* nil)
            (return-from mouse-left))
          (setf *dragginginput* input)
          (setf *mousebrick* brick)
          (push-mouse-line (calc-input-origin input brick))
          (return-from mouse-left))
        (when output
          (setf *other* (make-adj-arr))
          (when *dragginginput*
            (set-input *dragginginput* (list output brick) *mousebrick*)
            (setf *dragginginput* nil)
            (setf *draggingoutput* nil)
            (return-from mouse-left))
          (setf *draggingoutput* output)
          (setf *mousebrick* brick)
          (push-mouse-line (calc-output-origin output brick))
          (return-from mouse-left))
        (setf *draggingbrick* brick)))))

  (gamekit:bind-button :mouse-right :pressed
    (lambda ()
      (let ((ctx-menu (push-context-menu (gamekit:vec2 *mouse-x* *mouse-y*))))
        (push-menu-element "Input" #'ctx-menu-input ctx-menu)
        (push-menu-element "Output" (lambda () (format t "Output")) ctx-menu)
        (push-menu-element "New" 
          (lambda () (funcall #'ctx-menu-new ctx-menu)) 
          ctx-menu)
        (push-menu-element "Edit" (lambda () (format t "Edit")) ctx-menu)
        (push-menu-element "Delete" (lambda () (format t "delete")) ctx-menu)
    )))


  (gamekit:bind-button :mouse-left :released
                       (lambda () (setf *dragging* nil
                                        *draggingbrick* nil))))

(defun calc-angle-step (amount startAngle endAngle) 
  (/ (- startAngle endAngle) amount))

(defun fire-bullet (fire-origin target-origin bullet-speed)
  (vector-push-extend (make-instance 'bullet 
                                     :origin fire-origin
                                     :target-speed bullet-speed
                                     :target target-origin)
                      *bullets*))

(defun deg-to-rad (angle) (* angle rad))

(defun calc-circ-target-vec (origin target-rad w h)
  (gamekit:add origin (gamekit:vec2 
                        (* (sin target-rad) w) 
                        (* (cos target-rad) h))))

(defun circ-fire-bullets (amount fire-origin startAngle endAngle bullet-speed) 
  (loop :for angle = startAngle 
        :then (+ angle (calc-angle-step amount startAngle endAngle))
        :for i :from 0 :to amount
        :do (fire-bullet 
              fire-origin 
              (calc-circ-target-vec 
                fire-origin (deg-to-rad angle) (gamekit:viewport-width) (gamekit:viewport-height))
              bullet-speed)))

(defun set-target-all (new-target new-speed) 
  (loop :for i :from 0 :below (length *bullets*) :do 
        (let ((bullet (aref *bullets* i))) 
          (setf (target-speed bullet) new-speed) 
          (set-target new-target bullet))))

(defmethod gamekit:post-initialize ((app bullet-sim))
  (gamekit-cursor-bind)
  (setf *font* (gamekit:make-font :brick-font 24)))

(defun viewport-center-vec2 () 
  (gamekit:vec2 (/ (gamekit:viewport-width) 2)
                (/ (gamekit:viewport-height) 2)))

(defvar *ticks* 0)

(defmacro loop-update (arr)
  `(loop-array ,arr el (update el)))
(defmacro loop-draw (arr)
  `(loop-array ,arr el (draw el)))

(defmethod gamekit:act ((app bullet-sim))
  (incf *ticks*)
  (loop-update *bullets*)
  (loop-update *bricks*)
  (loop-update *ui*)
  (loop-update *other*))

(defmethod gamekit:draw ((app bullet-sim))
  (loop-draw *bullets*)
  (loop-draw *bricks*)
  (loop-draw *ui*)
  (loop-draw *other*))

(gamekit:start 'bullet-sim)
