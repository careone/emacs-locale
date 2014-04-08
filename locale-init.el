;;; locale-init.el --- init file for emacs-locale -*- coding: utf-8 -*-

;; Copyright
;; 2006 Ye Wenbin <wenbinye@163.com>
;; 2014 Wei-Lun Chao <bluebat@member.fsf.org>

(when (or (string-match "zh_CN" (getenv "LANG"))
       (string-match "zh_SG" (getenv "LANG")))
(require 'locale-zh-cn))

(when (or (string-match "zh_TW" (getenv "LANG"))
       (string-match "zh_HK" (getenv "LANG")))
(require 'locale-zh-tw))
