(in-package :gamebox-frame-manager)

(slog:define-message :trace :frame-manager.smooth.before
  "Before smoothing: Delta: ~F, Buffer: ~F, Frame Time: ~F")

(slog:define-message :trace :frame-manager.smooth.after
  "After smoothing: Delta: ~F, Buffer: ~F, Frame Time: ~F, Frames: ~A")

(slog:define-message :debug :frame-manager.rate
  "Frame rate: ~,2f fps (~,3f ms/f)")

(slog:define-message :debug :frame-manager.periodic-update
  "Periodic update performed (every ~D seconds).")
