(add-to-list 'auto-mode-alist '("\\.ino\\'" .
                                (lambda ()
                                  (c-or-c++-mode)
                                  (setq lsp-clients-clangd-args
                                        `(
                                          "-j=2"
                                          "--background-index"
                                          "--clang-tidy"
                                          "--completion-style=detailed"
                                          (concat "--query-driver=" (getenv-internal "HOME") "/.platformio/packages/toolchain-atmelavr/bin/avr-g++"))))))
(provide 'lm-arduino)
