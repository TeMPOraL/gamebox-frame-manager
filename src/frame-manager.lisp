(in-package :gamebox-frame-manager)

(defclass frame-manager ()
  ((%now :initform (local-time:now))
   (%before :initform 0)
   (%delta :reader delta
           :initarg :delta
           :initform (/ 30.0))
   (%delta-buffer :initform 0)
   (%frame-time :reader frame-time
                :initform 0)
   (%accumulator :initform 0)
   (%alpha :reader alpha
           :initform 0.0)
   (%period-elapsed :initform (local-time:now))
   (%period-interval :initarg :period
            :initform nil)
   (%debug-interval :initarg :debug-interval
                    :initform 5)
   (%debug-time :initform 0)
   (%debug-count :initform 0)))

(defmethod initialize-instance :after ((object frame-manager) &key)
  (reinitialize-instance object :delta (float (delta object) 1.0)))

(defun smooth-delta-time (frame-manager refresh-rate)
  "Smooth the delta time based on the monitor's refresh rate. This improves the
rendering quality."
  (with-slots (%delta %delta-buffer %frame-time) frame-manager
    (incf %frame-time %delta-buffer)
    (let ((frame-count (truncate (1+ (* %frame-time refresh-rate))))
          (previous %frame-time))
      (setf frame-count (if (plusp frame-count) frame-count 1)
            %frame-time (/ frame-count refresh-rate)
            %delta-buffer (- previous %frame-time)))))

(defun calculate-frame-rate (frame-manager)
  "Calculate the frames-per-second and milliseconds-per-frame, and emits them as
a log message."
  (with-slots (%debug-interval %debug-time %debug-count) frame-manager
    (let* ((now (get-internal-real-time))
           (elapsed-seconds (/ (- now %debug-time)
                               internal-time-units-per-second))
           (fps (/ %debug-count %debug-interval)))
      (when (and (>= elapsed-seconds %debug-interval)
                 (plusp fps))
        (slog:emit :frame-manager.rate fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun update (frame-manager step-func)
  "A single physics update which calls the user-supplied STEP-FUNC when
necessary. The ALPHA reader method of the frame manager stores the interpolation
coefficient to be used for blending frames in the STEP-FUNC."
  (with-slots (%delta %frame-time %accumulator %alpha) frame-manager
    (incf %accumulator %frame-time)
    (loop :while (>= %accumulator %delta)
          :do (funcall step-func)
              (decf %accumulator %delta)
          :finally (setf %alpha (/ %accumulator %delta)))))

(defun periodic-update (frame-manager func)
  "A periodic physics update. If the frame manager class is instantiated with
a :period argument, call FUNC every PERIOD-INTERVAL seconds. This is useful when
you need to call an expensive operation or perform operations periodically,
rather than every game tick."
  (with-slots (%period-elapsed %period-interval) frame-manager
    (let ((now (local-time:now)))
      (when (and %period-interval
                 func
                 (>= (local-time:timestamp-difference now %period-elapsed)
                    %period-interval))
        (funcall func)
        (slog:emit :frame-manager.periodic-update %period-interval)
        (setf %period-elapsed now)))))

(defun tick (frame-manager refresh-rate step-func &key periodic-func)
  "This is designed to be called each iteration of a main game loop, which calls
STEP-FUNC to update the physics when necessary, based on the DELTA-PHYSICS of
the frame manager."
  (with-slots (%init %now %before %frame-time %period) frame-manager
    (setf %before %now
          %now (local-time:now)
          %frame-time (local-time:timestamp-difference %now %before))
    (smooth-delta-time frame-manager refresh-rate)
    (update frame-manager step-func)
    (when periodic-func
      (periodic-update frame-manager periodic-func))
    (calculate-frame-rate frame-manager)))
