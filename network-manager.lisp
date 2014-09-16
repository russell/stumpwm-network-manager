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

;;;; network-manager.lisp

(in-package #:stumpwm.contrib.network-manager)


(defvar nm-state
  (list
   ;; Networking state is unknown.
   0 :unknown
   ;; Networking is inactive and all devices are disabled.
   10 :asleep
   ;; There is no active network connection.
   20 :disconnected
   ;; Network connections are being cleaned up.
   30 :disconnecting
   ;; A network device is connecting to a network and there is no
   ;; other available network connection.
   40 :connecting

   ;; A network device is connected, but there is only link-local
   ;; connectivity.
   50 :connected-local
   ;; A network device is connected, but there is only site-local
   ;; connectivity.
   60 :connected-site
   ;; A network device is connected, with global network connectivity.
   70 :connected-global))

(defvar device-state
  (list
   ;; The device is in an unknown state.
   0 :unknown
   ;; The device is recognized but not managed by NetworkManager.
   10 :unmanaged
   ;; The device cannot be used (carrier off, rfkill, etc).
   20 :unavailable
   ;; The device is not connected.
   30 :disconnected
   ;; The device is preparing to connect.
   40 :prepare
   ;; The device is being configured.
   50 :config
   ;; The device is awaiting secrets necessary to continue connection.
   60 :need-auth
   ;; The IP settings of the device are being requested and
   ;; configured.
   70 :ip-config
   ;; The device's IP connectivity ability is being determined.
   80 :ip-check
   ;; The device is waiting for secondary connections to be activated.
   90 :secondaries
   ;; The device is active.
   100 :activated
   ;; The device's network connection is being torn down.
   110 :deactivating
   ;; The device is in a failure state following an attempt to
   ;; activate it.
   120 :failed))


(defvar *dbus-connection* nil)

(defvar *access-point-menu* nil)

(defun string-to-symbol (name)
  (intern (string-upcase name) (find-package 'keyword)))

(defun collapse-prefix (l special-words)
  (unless (null l)
    (multiple-value-bind (newpre skip) (check-prefix l special-words)
      (cons newpre (collapse-prefix (nthcdr skip l) special-words)))))

(defun check-prefix (l special-words)
  (let ((pl (loop for i from (1- (length l)) downto 0
                  collect (apply #'concatenate 'simple-string (butlast l i)))))
    (loop for w in special-words
          for p = (position-if #'(lambda (s) (string= s w)) pl)
          when p do (return-from check-prefix (values (nth p pl) (1+ p))))
    (values (first l) 1)))

(defun split-if (test seq &optional (dir :before))
  (remove-if #'(lambda (x) (equal x (subseq seq 0 0)))
             (loop for start fixnum = 0
                     then (if (eq dir :before)
                              stop
                    (the fixnum (1+ (the fixnum stop))))
                   while (< start (length seq))
                   for stop = (position-if test seq
                                           :start (if (eq dir :elide)
                                                      start
                                                      (the fixnum (1+ start))))
                   collect (subseq seq start
                                   (if (and stop (eq dir :after))
                                       (the fixnum (1+ (the fixnum stop)))
                                       stop))
                   while stop)))

(defgeneric translate-camelcase-name (name &key upper-initial-p special-words)
  (:method ((name string) &key upper-initial-p special-words)
    (declare (ignore upper-initial-p))
    (values (intern (reduce #'(lambda (s1 s2)
                                (concatenate 'simple-string s1 "-" s2))
                            (mapcar #'string-upcase
                                    (collapse-prefix
                                     (split-if #'(lambda (ch)
                                                   (upper-case-p ch))
                                               name)
                                     special-words)))
                    'keyword)))
  (:method ((name symbol) &key upper-initial-p special-words)
    (apply #'concatenate
           'string
           (loop for str in (split-if #'(lambda (ch) (eq ch #\-))
                                          (string name)
                                      :elide)
                 for first-word-p = t then nil
                 for e = (member str special-words
                                 :test #'equal :key #'string-upcase)
                 collect (cond
                           ((and first-word-p (not upper-initial-p))
                            (string-downcase str))
                           (e (first e))
                           (t (string-capitalize str)))))))

(defun convert-key-to-keyword (element)
  (setf (car element) (translate-camelcase-name (car element)))
  element)


(defparameter *object-paths* (make-hash-table))

(defparameter *device-classes*
  '(("org.freedesktop.NetworkManager.Device.Wireless" . device-wifi)))

(defclass network-manager-object ()
  ((dbus-object
    :accessor dbus-object
    :initarg :dbus-object)
   (dbus-path
    :accessor dbus-path
    :initarg :dbus-path)
   (dbus-primary-interface
    :accessor dbus-primary-interface
    :initarg :dbus-primary-interface)))

(defclass device (network-manager-object)
  ((dbus-primary-interface
    :accessor dbus-primary-interface
    :initform "org.freedesktop.NetworkManager.Device"
    :initarg :dbus-primary-interface)))

(defclass device-wifi (device)
  ((dbus-primary-interface
    :accessor dbus-primary-interface
    :initform "org.freedesktop.NetworkManager.Device.Wireless"
    :initarg :dbus-primary-interface)))

(defmethod object-path (object)
  (object-path (dbus-object object)))

(defmethod initialize-instance :around ((instance network-manager-object)
                                        &key
                                        dbus-path
                                        dbus-object
                                        &allow-other-keys)
  (cond
    (dbus-path
     (prog1
         (call-next-method)
       (setf (dbus-object instance)
             (make-object-from-introspection
              dbus-path
              "org.freedesktop.NetworkManager"))))
    (t
     (let ((future (make-future))
           (object (call-next-method)))
       (when dbus-object
        (setf (slot-value object 'dbus-path) (object-path dbus-object)))
       (finish future object)
       (print future)
       (attach future (lambda (a) "called me"))
       future))))




(defmethod details (dbus-object)
  (with-introspected-object (nm (object-path dbus-object)
                                "org.freedesktop.NetworkManager")
    (mapcar #'convert-key-to-keyword
     (nm "org.freedesktop.DBus.Properties" "GetAll"
         (dbus-primary-interface dbus-object)))))


(defun current-state ()
  (with-introspected-object (nm
                                "/org/freedesktop/NetworkManager"
                                "org.freedesktop.NetworkManager")
    (getf nm-state
          (nm "org.freedesktop.NetworkManager" "state"))))


(defun futures-mapcar (function list cb)
  "Map across a list of item with a function that returns a future.
Then wait unit they are all resolved before calling CB."
  (let (futures-list)
   (flet ((call-back (result)
            (declare (ignore result))
            (when (every #'future-finished-p futures-list)
              (funcall cb (mapcar #'future-values futures-list)))))
     (if list
         (dolist (item list)
           (attach (car (push (funcall function item) futures-list))
                   #'call-back))
         (funcall cb nil)))))


(defun list-devices ()
  (let ((future (make-future)))
    (with-introspected-object (nm "/org/freedesktop/NetworkManager"
                                  "org.freedesktop.NetworkManager")
      (attach (nm "org.freedesktop.NetworkManager" "GetDevices")
              (lambda (devices)
                (print devices)
                (flet ((introspect (path)
                         (make-object-from-introspection path "org.freedesktop.NetworkManager")))
                 (with-futures (devices (mapcar #'introspect devices))
                   (with-futures (devices (mapcar (compose #'get-device #'car) devices))
                     (finish future devices))))
                )))
    future))


(defun get-device (device)
  "Return a device"
  (let* ((interface (find "org.freedesktop.NetworkManager.Device."
                          (mapcar #'interface-name (list-object-interfaces device))
                          :test (lambda (e a) (equal (subseq a 0 (length e))
                                                     e))))
         (device-class (or (assoc-value *device-classes* interface
                                        :test #'equal)
                           'device)))
    (make-instance device-class :dbus-object device)))


(defmethod device-active-access-point ((device device-wifi))
  (get-access-point
   (object-invoke (dbus-object device)
                  "org.freedesktop.DBus.Properties" "Get"
                  (dbus-primary-interface device)
                  "ActiveAccessPoint")))


(defun device-details (device)
  (with-introspected-object (nm device
                                "org.freedesktop.NetworkManager")
    (nm "org.freedesktop.DBus.Properties" "GetAll"
        "org.freedesktop.NetworkManager.Device")))


(defclass connection (network-manager-object)
  ((dbus-primary-interface
    :accessor dbus-primary-interface
    :initform "org.freedesktop.NetworkManager.Settings.Connection"
    :initarg :dbus-primary-interface)
   (settings
    :accessor connection-settings)))

(defun get-connection (connection-path)
  "Return a connection"
  (make-instance 'connection :dbus-path connection-path))

(defmethod connection-ssid ((connection connection))
  (cadr (assoc :ssid
               (cadr (assoc :802-11-wireless
                                  (connection-settings connection))))))

(defmethod connection-id ((connection connection))
  (cadr (assoc :id
               (cadr (assoc :connection
                                  (connection-settings connection))))))

(defmethod connection-settings ((connection connection))
  (labels ((translate-connection-settings (element)
             (convert-key-to-keyword element)
             (case (car element)
               (:ssid
                (setf (cadr element) (octets-to-string (cadr element)))))
             (when (and (listp (cadr element)) (listp (caadr element)))
               (mapcar #'translate-connection-settings (cadr element)))
             element))
    (if (slot-boundp connection 'settings)
        (slot-value connection 'settings)
        (setf (connection-settings connection)
              (mapcar #'translate-connection-settings
                      (object-invoke (dbus-object connection)
                                     (dbus-primary-interface connection)
                                     "GetSettings"))))))

(defun list-connections ()
  (let ((future (make-future)))
          (with-introspected-object (nm
                                     "/org/freedesktop/NetworkManager/Settings"
                                     "org.freedesktop.NetworkManager")
            (alet ((connections (nm "org.freedesktop.NetworkManager.Settings"
                                     "ListConnections")))
              (with-futures (connections (mapcar #'get-connection connections))
                (finish future connections))))
    future))

(defun list-wifi-access-points ()
  (loop :for device :in (list-devices-with-interface
                          "org.freedesktop.NetworkManager.Device.Wireless")
        :append (list-access-points device)))

(defun list-devices-with-interface (interface)
  "List all the connections with a particular interface."
  (remove-if #'null
             (mapcar
              (has-interface interface)
              (list-devices))))

(defun has-interface (name)
  (lambda (object)
    (if (find t
           (mapcar
            (interface-p name)
            (list-object-interfaces (dbus-object object))))
        object
        nil)))

(defun interface-p (name)
  (lambda (interface)
    (equal name (interface-name interface))))

(defmethod device-state ((device device))
  (getf device-state
        (object-invoke (dbus-object device)
                       "org.freedesktop.DBus.Properties" "Get"
                       "org.freedesktop.NetworkManager.Device" "State")))

(defclass access-point (network-manager-object)
  ((dbus-primary-interface
    :accessor dbus-primary-interface
    :initform "org.freedesktop.NetworkManager.AccessPoint"
    :initarg :dbus-primary-interface)
   (devices
    :accessor access-point-devices
    :initarg :devices)))

(defmethod access-point-device ((device access-point))
  (car (access-point-devices device)))

(defmethod list-access-points ((device device-wifi))
  (mapcar (lambda (ap) (get-access-point ap :device device))
   (object-invoke (dbus-object device)
                  "org.freedesktop.NetworkManager.Device.Wireless"
                  "GetAccessPoints")))

(defmethod access-point-ssid ((access-point access-point))
  (octets-to-string
   (object-invoke (dbus-object access-point)
                  "org.freedesktop.DBus.Properties" "Get"
                  "org.freedesktop.NetworkManager.AccessPoint" "Ssid")))

(defmethod access-point-strength ((access-point access-point))
  (object-invoke (dbus-object access-point)
                 "org.freedesktop.DBus.Properties" "Get"
                 "org.freedesktop.NetworkManager.AccessPoint" "Strength"))

(defmethod details ((dbus-object access-point))
  (with-introspected-object (nm (object-path dbus-object)
                                "org.freedesktop.NetworkManager")
    (flet ((translate-access-point (element)
             (convert-key-to-keyword element)
             (case (car element)
               (:ssid
                (setf (cadr element) (octets-to-string (cadr element)))))
             element))
      (mapcar #'translate-access-point
              (nm "org.freedesktop.DBus.Properties" "GetAll"
                  (dbus-primary-interface dbus-object))))))


(defun get-access-point (access-point &key device (connection *dbus-connection*))
  (make-instance 'access-point :dbus-path access-point :devices (list device)))


(defmethod activate-connection ((connection connection) (device device)
                                specific-object)
  (with-introspected-object (nm
                                "/org/freedesktop/NetworkManager"
                                "org.freedesktop.NetworkManager")
    (nm "org.freedesktop.NetworkManager" "ActivateConnection"
        (object-path connection) (object-path device) specific-object)))


(defun list-access-points-in-range ()
  "Return the ACCESS-POINTS in range that are known to network
manager and the DEVICE they were found on."
  (flet ((access-point-to-alist (element)
           (list (access-point-ssid element) element)))
    (let ((connections (list-connections))
          (access-points (mapcar #'access-point-to-alist
                                 (list-wifi-access-points))))
      (loop :for connection :in connections
            :for access-point = (assoc (connection-ssid connection)
                                       access-points
                                       :test #'equal)
            :when access-point
              :collect (list connection (cadr access-point))))))


(defclass active-connection (network-manager-object)
  ((dbus-primary-interface
    :accessor dbus-primary-interface
    :initform "org.freedesktop.NetworkManager.Connection.Active"
    :initarg :dbus-primary-interface)
   (device
    :accessor active-connection-devices
    :initarg :device)
   (connection
    :accessor active-connection-connection
    :initarg :connection)))

(defun get-active-connection (active-connection)
  "Return an active connection"
  (make-instance 'active-connection :dbus-path active-connection))

(defun active-connections ()
  (let ((future (make-future)))
   (with-introspected-object (nm "/org/freedesktop/NetworkManager"
                                 "org.freedesktop.NetworkManager")
     (alet ((connections (nm "org.freedesktop.DBus.Properties" "Get"
                            "org.freedesktop.NetworkManager" "ActiveConnections")))
       (futures-mapcar #'get-active-connection connections
                       (lambda (results)
                         (finish future (mapcar #'car results))))))
    future))

(defmethod active-connection-devices ((active-connection active-connection))
  (if (slot-boundp active-connection 'device)
      (slot-value active-connection 'device)
      (setf (active-connection-devices active-connection)
            (mapcar #'get-device
             (object-invoke (dbus-object active-connection)
                            "org.freedesktop.DBus.Properties"
                            "Get"
                            (dbus-primary-interface active-connection)
                            "Devices")))))


(defmethod active-connection-connection ((active-connection active-connection))
  (if (slot-boundp active-connection 'connection)
      (slot-value active-connection 'connection)
      (setf (active-connection-connection active-connection)
            (get-connection
             (object-invoke (dbus-object active-connection)
                            "org.freedesktop.DBus.Properties"
                            "Get"
                            (dbus-primary-interface active-connection)
                            "Connection")))))


(defun current-connection-ids ()
  (with-open-bus (*dbus-connection* (system-server-addresses))
    (let ((connections (active-connections)))
      (mapcar #'connection-id
       (mapcar #'active-connection-connection connections)))))


(defun current-connection-signal ()
  (with-open-bus (*dbus-connection* (system-server-addresses))
    (loop
      :for connection :in (active-connections)
      :for device = (car (active-connection-devices connection))
      :collect
      (case (type-of device)
        ('device-wifi
         (access-point-strength (device-active-access-point device)))))))


(defcommand nm-select-access-point () ()
    "List known access points in range."
  (labels ((pick (options)
             (let ((selection (select-from-menu (current-screen)
                                                options "")))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 (t
                  (second selection))))))
    (with-open-bus (*dbus-connection* (system-server-addresses))
      (let* ((*access-point-menu*
               (mapcar (lambda (e)
                         (list (connection-id (car e))
                               (list (car e)
                                     (access-point-device (cadr e))
                                     (cadr e))))  ; access-point
                       (list-access-points-in-range)))
             (choice (pick *access-point-menu*)))
        (apply #'activate-connection (car choice) (cdr choice))))))
