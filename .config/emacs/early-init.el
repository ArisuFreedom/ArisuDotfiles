;; early-init.el — Startup optimizations

;; Disable built-in package.el because we use straight.el
(setq package-enable-at-startup nil)

;; Temporarily raise GC limits during startup for performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; UI optimizations (disable unnecessary elements early)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Avoid expensive implied resize during frame creation
(setq frame-inhibit-implied-resize t)
