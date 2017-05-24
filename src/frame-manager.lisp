(in-package :gamebox-frame-manager)

(defclass frame-manager ()
  ((%now :initform (local-time:now))
   (%before :initform 0)
   (%delta-render :reader delta-render
                  :initform 0)
   (%delta-physics :reader delta-physics
                   :initarg :delta-physics
                   :initform (/ 30.0))
   (%delta-buffer :initform 0)
   (%accumulator :initform 0)
   (%alpha :reader alpha
           :initform 0.0)
   (%debug-interval :initarg :debug-interval
                    :initform 5)
   (%debug-time :initform 0)
   (%debug-count :initform 0)))

(defmethod initialize-instance :after ((object frame-manager) &key)
  (reinitialize-instance object :delta-physics (float (delta-physics object) 1.0)))

(defun smooth-delta-time (frame-manager refresh-rate)
  "Smooth the delta time based on the monitor's refresh rate. This improves the rendering quality."
  (with-slots (%delta-render %delta-physics %delta-buffer) frame-manager
    (slog:emit :frame-manager.smooth.before %delta-physics %delta-render %delta-buffer)
    (incf %delta-render %delta-buffer)
    (let ((frame-count (truncate (1+ (* %delta-render refresh-rate))))
          (previous %delta-render))
      (setf frame-count (if (> frame-count 0) frame-count 1)
            %delta-render (/ frame-count refresh-rate)
            %delta-buffer (- previous %delta-render))
      (slog:emit :frame-manager.smooth.after %delta-physics %delta-render %delta-buffer frame-count))))

(defun calculate-frame-rate (frame-manager)
  "Calculate the frames-per-second and milliseconds-per-frame, and emits them as a log message."
  (with-slots (%debug-interval %debug-time %debug-count) frame-manager
    (let* ((now (get-internal-real-time))
           (elapsed-seconds (/ (- now %debug-time) internal-time-units-per-second))
           (fps (/ %debug-count %debug-interval)))
      (when (and (>= elapsed-seconds %debug-interval)
                 (plusp fps))
        (slog:emit :frame-manager.rate fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun update (frame-manager step-func)
  "A single physics update which calls the user-supplied STEP-FUNC when necessary.
The ALPHA reader method of the frame manager stores the interpolation coefficient to be used for blending frames in the
STEP-FUNC."
  (with-slots (%delta-physics %accumulator %alpha) frame-manager
    (loop :while (>= %accumulator %delta-physics)
          :for count = 0 :then (incf count)
          :do (funcall step-func)
              (decf %accumulator %delta-physics)
          :finally (setf %alpha (/ %accumulator %delta-physics)))))

(defun tick (frame-manager refresh-rate step-func)
  "This is designed to be called each iteration of a main game loop, which calls STEP-FUNC to update the physics when
necessary, based on the DELTA-PHYSICS of the frame manager."
  (with-slots (%now %before %delta-render %accumulator) frame-manager
    (setf %before %now
          %now (local-time:now)
          %delta-render (local-time:timestamp-difference %now %before))
    (smooth-delta-time frame-manager refresh-rate)
    (incf %accumulator %delta-render)
    (calculate-frame-rate frame-manager)
    (update frame-manager step-func)))
