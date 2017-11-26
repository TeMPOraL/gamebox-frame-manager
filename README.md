# gamebox-frame-manager

A manager for frames within a game loop.

## Overview

This is a simple manager for frame updates within a main game loop. After
creating a simple frame manager, one can just call a function every iteration of
the main game loop, and have it handle the desired physics updates at the
correct times. It also provides a logging feature to show the game's current
frame rate, and other useful information.

## Install

``` lisp
(ql:quickload :gamebox-frame-manager)
```

## Usage

First, create a `FRAME-MANAGER` object with your desired delta time:

``` lisp
(defparameter *frame-manager* (make-instance 'frame-manager
                                             :delta (/ 1 30.0)))
```

Instead, you can also sub-class `FRAME-MANAGER`:

``` lisp
(defclass my-game (frame-manager)
  ((delta :initform (/ 1 30.0))))

(defparameter *my-game* (make-instance 'my-game))
```

You will likely want the frame rate logged to the REPL during development of
your game. In that case, set the current logging level to `:DEBUG`:

``` lisp
(setf slog:*current-level* :debug)
```

Then call the `TICK` method every step of the main game loop, with the correct
value for your monitor's refresh rate, and a custom physics update function to
apply.

``` lisp
(tick *frame-manager* 60 #'step-func)
```

This will call `STEP-FUNC` only when needed - that is, enough time has passed
between the last physics update based on the frame manager's target delta time.
Within your `STEP-FUNC`, you have access to the reader method `(ALPHA
FRAME-MANAGER)` to obtain the interpolation coefficient that can be used to
blend the previous and current frames.

With the current log message level set to `:DEBUG`, you should see the frame
rate printed every 5 seconds.


## License

Copyright © 2014 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
