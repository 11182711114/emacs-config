;;; web-lang.el --- web module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides web functionality
;;; Code:
(require 'badliveware-projectile-util)
(require 'badliveware-company-lib)

(require 'javascript-lang)
(require 'html-lang)
(require 'css-lang)


(def-project-mode! +web-angularjs-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'angular))

(def-project-mode! +web-react-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'react))

(provide 'web-lang)
;;; web-lang.el ends here
