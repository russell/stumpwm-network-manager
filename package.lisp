;;; Network Manager interface for StumpWM.
;;;
;;; Copyright 2014 Russell Sim <russell.sim@gmail.com>
;;;
;;; This module is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This module is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:stumpwm.contrib.network-manager
  (:use #:cl)
  (:import-from #:dbus
                #:interface-name
                #:list-object-interfaces
                #:object-path
                #:message-signature
                #:object-connection
                #:system-server-addresses)
  (:import-from #:flexi-streams
                #:octets-to-string)
  (:import-from #:alexandria
                #:compose
                #:with-gensyms
                #:assoc-value)
  (:import-from #:stumpwm
                #:defcommand
                #:select-from-menu
                #:current-screen
                #:add-screen-mode-line-formatter)
  (:import-from #:stumpwm.contrib.dbus
                #:make-object-from-introspection
                #:object-invoke
                #:with-introspected-object
                #:make-future
                #:attach
                #:future-values
                #:future-finished-p
                #:futurep
                #:list-object-interfaces
                #:interface-name
                #:alet
                #:finish
                #:with-futures
                ))
