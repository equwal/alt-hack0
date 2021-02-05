(defpackage :all-fonts
  (:use :cl)
  (:import-from :alexandria :map-combinations)
  (:import-from :uiop :run-program))
(in-package :all-fonts)

(defparameter *groups*
  '(curve round backslash diamond dot forwardslash noslab1 flattop3 widerknife slabi)
  "Things you actually want to permute together.")

(defparameter *exclusions*
  '((curve round)
    (backslash diamond dot forwardslash))
  "Things which you don't want specified at the same time.")

(defun verify (spec)
  "Ensure that the spec is not bogus."
  (let ((group (not (set-difference spec *groups*)))
        (exclusion (not (some #'identity
                              (mapcar #'(lambda (ex)
                                          (<= 2 (length (intersection spec ex))))
                                      *exclusions*)))))
    (values (and group exclusion)
            (if group (if exclusion 'group-and-exclusion 'group)
                (if exclusion 'exclusion 'unknown)))))
(defun gen-all-fonts ()
  (loop for x from 1 to (length *groups*)
        do (map-combinations
             (lambda (f)
               (when (verify f)
                 (run-program (format nil "./generator.ros --ttf --woff --woff2 ~(~{ ~A~}~)" f) :output t)
                 ;; KLUDGE: Weird bug where everything ends up in the ttf folder
                 (run-program (format nil "tar -vcf - Hack/build/ttf/* | xz -12 >'~(~{~A~}~).tar.xz'" f) :output t)
                 (run-program "rm -Rf Hack/build")
                 (run-program "mkdir Hack/build")))
            *groups*
             :length x)))
