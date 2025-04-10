;;; perso/rust/keymaps.el -*- lexical-binding: t; -*-

(defun +rustic-cargo-run-release ()
  (interactive)
  (rustic-cargo-run-command "--release"))

(map!
 :mode 'rustic-mode
 :map rustic-mode-map
 :leader
 :prefix "b"
 :desc "cargo run --release" :n "R" #'+rustic-cargo-run-release)
