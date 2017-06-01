(in-package :gamebox-frame-manager)

(slog:define-message :debug :frame-manager.rate
  "Frame rate: ~,2f fps (~,3f ms/f)")

(slog:define-message :debug :frame-manager.periodic-update
  "Periodic update performed (every ~D seconds).")
