(in-package :cl-user)

(asdf:defsystem #:gamebox-frame-manager
  :description "A manager for frames within a game loop."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-frame-manager"
  :bug-tracker "https://github.com/mfiano/gamebox-frame-manager/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-frame-manager.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:local-time
               #:simple-logger)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "log")
   (:file "frame-manager")))
