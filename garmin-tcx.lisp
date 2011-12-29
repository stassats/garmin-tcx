;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:garmin-tcx)

(defun load-tcx (file-or-url)
  (cxml:parse (if (eql 0 (search "http" file-or-url))
                  (drakma:http-request file-or-url)
                  (pathname file-or-url))
              (stp:make-builder)))

(defun elementp (x)
  (typep x 'stp:element))

(defun local-name (x)
  (and (elementp x)
       (stp:local-name x)))

(defun find-child (local-name xml)
  (stp:find-child local-name xml
                  :key #'local-name
                  :test #'equal))

(defun find-child-value (local-name xml)
  (let ((child (find-child local-name xml)))
    (when child
      (stp:string-value child))))

(defun find-children (local-name xml)
  (stp:filter-children (lambda (child)
                         (equal (local-name child)
                                local-name)) xml))

(defun find-child-path (xml &rest local-names)
  (loop with (name . rest) = local-names
        for child in (find-children name xml)
        thereis (if rest
                    (apply #'find-child-path child rest)
                    (and (equal (local-name child) name)
                         child))))

(defvar *sport-types*
  '(("Running" . :run)
    ("Biking" .  :bike-ride)))

(defun indoor-cycling-p (xml)
  (not (stp:find-recursively "Position" xml
                             :key #'local-name :test #'equal)))

(defun get-sport-type (xml)
  (let ((type (cdr (assoc (stp:attribute-value xml "Sport") *sport-types*
                          :test #'equal))))
    (if (and (eql type :bike-ride)
             (indoor-cycling-p xml))
        :indoor-bike-ride
        type)))

(defun parse-float (string)
  (let (*read-eval*
        (*read-default-float-format* 'double-float))
    (read-from-string string)))

(defun get-time (xml)
  (let ((value (find-child-value "TotalTimeSeconds" xml)))
    (when value
      (parse-float value))))

(defun integer-value (node)
  (when (and (elementp node)
             (stringp (stp:string-value node)))
    (parse-integer (stp:string-value node) :junk-allowed t)))

(defun get-avg-cadence (xml)
  (integer-value
   (or (find-child-path xml "Extensions"
                        "LX" "AvgRunCadence")
       (find-child-path xml "Cadence"))))

(defun get-max-cadence (xml)
  (integer-value
   (or (find-child-path xml "Extensions"
                        "LX" "MaxRunCadence")
       (find-child-path xml "Extensions"
                        "LX" "MaxBikeCadence"))))

(defun get-start-time (xml)
  (let ((value (stp:attribute-value xml "StartTime")))
    (when value
      (local-time:parse-timestring value))))

(defun get-distance (xml)
  (let ((value (find-child-value "DistanceMeters" xml)))
    (when value
      (parse-float value))))

(defun get-max-speed (xml)
  (let ((value (find-child-value "MaximumSpeed" xml)))
    (when value
      (parse-float value))))

(defun get-avg-speed (xml)
  (parse-float
   (stp:string-value
    (find-child-path xml "Extensions"
                     "LX" "AvgSpeed"))))

(defun get-avg-hr (xml)
  (integer-value
   (find-child-path xml "AverageHeartRateBpm" "Value")))

(defun get-max-hr (xml)
  (integer-value
   (find-child-path xml "MaximumHeartRateBpm" "Value")))

(defun get-speed (xml)
  (let ((child (find-child-path xml "Extensions"
                                "TPX" "Speed")))
    (when child
      (parse-float
       (stp:string-value child)))))

(defun trackpoint-time (xml)
  (let ((value (find-child-value "Time" xml)))
    (when value
      (local-time:parse-timestring value))))

(defvar *moving-speed-threshold* 1
  "In m/s")

(defun get-moving-time (xml)
  (let (previous-time
        (total-time 0))
    (stp:do-children (trackpoint (find-child "Track" xml))
      (when (equal (local-name trackpoint) "Trackpoint")
        (let ((time (trackpoint-time trackpoint))
              (speed (get-speed trackpoint)))
          (when (and previous-time
                     speed
                     (> speed *moving-speed-threshold*))
            (incf total-time
                  (local-time:timestamp-difference time previous-time)))
          (setf previous-time time))))
    total-time))

(defun parse-lap (xml)
  (list :start-time (get-start-time xml)
        :time (get-time xml)
        :distance (get-distance xml)
        :avg-speed (get-avg-speed xml)
        :max-speed (get-max-speed xml)
        :avg-hr (get-avg-hr xml)
        :max-hr (get-max-hr xml)
        :avg-cadence (get-avg-cadence xml)
        :max-cadence (get-max-cadence xml)
        :moving-time (get-moving-time xml)))

(defun parse-laps (xml)
  (let ((laps (find-children "Lap" xml)))
    (loop for lap in laps
          collect (remove-nil-plist (parse-lap lap)))))

(defun besides-zero (value)
  (unless (zerop value)
    value))

(defun sum-data (laps indicator)
  (besides-zero
   (loop for lap in laps
         sum (or (getf lap indicator)
                 0))))

(defun maximize-data (laps indicator)
  (besides-zero
   (loop for lap in laps
         maximize (or (getf lap indicator)
                      0))))

(defun average-data (laps indicator)
  (loop for lap in laps
        for weight = (getf lap :time)
        sum (* (or (getf lap indicator)
                   0)
               weight)
        into weighted
        sum weight into total-weight
        finally (return
                  (unless (or (zerop weighted)
                              (zerop total-weight))
                    (/ weighted total-weight)))))

(defun round-non-nil (x)
  (and x (round x)))

(defun remove-nil-plist (plist)
  (loop for (key value) on plist by #'cddr
        when value
        collect key
        and
        collect value))

(defun combine-laps (laps)
  (remove-nil-plist
   (list :start-time (getf (car laps) :start-time)
         :time (sum-data laps :time)
         :distance (sum-data laps :distance)
         :avg-speed (average-data laps :avg-speed)
         :max-speed (maximize-data laps :max-speed)
         :avg-hr (round-non-nil (average-data laps :avg-hr))
         :max-hr (maximize-data laps :max-hr)
         :avg-cadence (round-non-nil (average-data laps :avg-cadence))
         :max-cadence (maximize-data laps :max-cadence)
         :moving-time (sum-data laps :moving-time))))

(defun get-data (xml)
  (let ((laps (parse-laps xml)))
   `(:type ,(get-sport-type xml)
     ,@(combine-laps laps)
     :laps ,laps)))

(defun parse (xml)
  (let ((activities (find-children
                     "Activity"
                     (find-child-path xml
                                      "TrainingCenterDatabase"
                                      "Activities"))))
    (loop for activity in activities
          collect (get-data activity))))

(defun parse-file (file-or-url)
  (parse (load-tcx file-or-url)))
