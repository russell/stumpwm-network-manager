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
                #:system-server-addresses
                #:with-open-bus
                #:object-invoke
                #:object-path
                #:make-object-from-introspection
                #:with-introspected-object
                #:bus-connection
                #:list-object-interfaces
                #:interface-name)
  (:import-from #:flexi-streams
                #:octets-to-string)
  (:import-from #:alexandria
                #:compose
                #:assoc-value)
  (:import-from #:stumpwm
                #:defcommand
                #:select-from-menu
                #:current-screen
                #:add-screen-mode-line-formatter))
