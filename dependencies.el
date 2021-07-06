(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(setq package-check-signature nil)

(unless (package-installed-p 'request)
  (package-install 'request))
(require 'request)
(require 'json)
