;;; web.el --- web module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides web functionality
;;; Code:
(require 'badliveware-projectile-util)
(require 'badliveware-company-lib)

(require 'javascript)
(require 'html)
(require 'css)


(def-project-mode! +web-angularjs-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'angular))

(def-project-mode! +web-react-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'react))

(provide 'web)
;;; web.el ends here
