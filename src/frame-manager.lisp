(in-package :gamebox-frame-manager)

(defclass frame-manager ()
  ((now :initform (local-time:now))
   (before :initform 0)
   (dt-target :initarg :dt-target
              :initform (/ 1 30.0))
   (dt-buffer :initform 0)
   (dt :initform 0)
   (accumulator :initform 0)
   (alpha :reader alpha
          :initform 0.0)
   (debug-time :initform (local-time:now))
   (debug-count :initform 0)))


(defun smooth-delta-time (frame-manager refresh-rate)
  "Smooth the delta time based on the monitor's refresh rate. This improves the rendering quality."
  (with-slots (hz dt dt-target dt-buffer) frame-manager
    (slog:emit :frame-manager.smooth.before dt-target dt dt-buffer)
    (incf dt dt-buffer)
    (let ((frame-count (truncate (1+ (* dt refresh-rate))))
          (previous-dt dt))
      (setf frame-count (if (> frame-count 0) frame-count 1)
            dt (/ frame-count refresh-rate)
            dt-buffer (- previous-dt dt))
      (slog:emit :frame-manager.smooth.after dt-target dt dt-buffer frame-count))))


(defun calculate-frame-rate (frame-manager)
  "Calculate the frames-per-second and milliseconds-per-frame, and emits them as a log message."
  (with-slots (now debug-time debug-count) frame-manager
    (let* ((interval 5)
           (elapsed (local-time:timestamp-difference now debug-time))
           (fps (/ debug-count interval)))
      (incf debug-count)
      (when (and (>= elapsed interval)
                 (plusp fps))
        (slog:emit :frame-manager.rate fps (/ 1000 fps))
        (setf debug-count 0
              debug-time now)))))

(defun update (frame-manager step-func)
  "A single physics update which calls the user-supplied STEP-FUNC when necessary.
The ALPHA reader method of the frame-manager stores the interpolation coefficient to be used for
blending frames in the STEP-FUNC."
  (with-slots (dt-target accumulator alpha) frame-manager
    (loop :while (>= accumulator dt-target)
          :for count = 0 :then (incf count)
          :do (funcall step-func)
              (decf accumulator dt-target)
          :finally (setf alpha (/ accumulator dt-target)))))

(defun tick (frame-manager refresh-rate step-func)
  "This is designed to be called each iteration of a main game loop, which calls STEP-FUNC to update
the physics when necessary, based on the TARGET-DT of the frame manager."
  (with-slots (now before dt accumulator) frame-manager
    (setf before now
          now (local-time:now)
          dt (local-time:timestamp-difference now before))
    (smooth-delta-time frame-manager refresh-rate)
    (incf accumulator dt)
    (calculate-frame-rate frame-manager)
    (update frame-manager step-func)))
