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

;;;; stumpwm.contrib.network-manager.asd

(asdf:defsystem #:stumpwm.contrib.network-manager
  :serial t
  :description "Network Manager interface for StumpWM."
  :author "Russell Sim <russell.sim@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm #:dbus #:flexi-streams)
  :components ((:file "package")
               (:file "network-manager")))
