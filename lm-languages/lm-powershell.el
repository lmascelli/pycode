(use-package powershell    
  :ensure t
  :defer t
  :init
  (setq compile-command "pwsh -c ./project.ps1 ")
  :commands powershell)

(use-package ob-powershell
  :ensure t
  :after org)
