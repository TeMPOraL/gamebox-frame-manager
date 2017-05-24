(in-package :gamebox-frame-manager)

(slog:define-message :trace :frame-manager.smooth.before
  "Before smoothing: Target: ~F, Actual: ~F, Buffer: ~F")

(slog:define-message :trace :frame-manager.smooth.after
  "After smoothing: Target: ~F, Actual: ~F, Buffer: ~F, Frames: ~A")

(slog:define-message :debug :frame-manager.rate
  "Frame rate: ~,2f fps (~,3f ms/f)")
