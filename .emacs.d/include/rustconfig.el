;;; rustconfig.el --- Configuration for Rust.
;;; Commentary:
;;    Configuration for Rust;
;;; Code:

(setq lsp-rust-analyzer-cargo-watch-enable t)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;; Inlay hints
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-inlay-hints-mode t)
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-parameter-hints t)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)

;;; rustconfig.el ends here
