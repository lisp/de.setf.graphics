;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document
  "This file implements quicktime contexts for the 'de.setf.graphics' library."

  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
   "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20060809 "janderson@ravenpack.com")))

;;; nb.
;;; this was last used under osx 10.2 and mcl 5.0. !

(defclass quicktime-context (projection-context )
  ((pathname :initarg :pathname
             :accessor context-pathname)
   (generator :initarg :generator
              :accessor context-generator)
   ))

(defmethod context-view-size ((context quicktime-context))
  (view-size (context-view (context-generator context))))

(defmacro errcheck-movie (form)
  `(let ((result ,form)
         (error (#_GetMoviesError)))
     (assert (zerop error) () "~&~A had an error: ~A" ',(car form) error)
     result))

(defparameter %movie% nil)
(defparameter %media% nil)
(defparameter %movie-resrefnum% nil)
(defparameter %movie-resource-id% nil)

(defmethod call-with-projection-context ((function t) (context quicktime-context)
                                                  &key (pathname (context-pathname context))
                                                  &allow-other-keys)
  "open the context's quicktime medium for output, prepare it to accept frames, and invoke the function. at conclusion, close the quicktime medium."
  (rlet ((%fsspec :fsspec))
    (flet ((get-fsspec ()             ; from qt-objects
             (let ((resolved-path (ccl::mac-namestring pathname)))
               (ccl:with-pstrs ((%filename resolved-path))
                 (let ((result-code (#_FSmakeFSSpec 0 0 %filename %fsspec)))
                   (if (or (zerop result-code) (= result-code #$fnfErr))
                     %fsspec
                     nil)))))
           (create-movie-file ()      ; from qt-objects
             (rlet ((%ResRefNum :word) 
                    (%mptr :pointer))
               ;; no check as it was just made
               ;; (unless (valid-fsspec-p %fsspec) (error "Invalid file specification."))
               (errcheck-movie 
                (#_CreateMovieFile %fsSpec
                 :|TVOD| ; #$MovieFileType
                 0
                 #$createmoviefileDeleteCurFile
                 %ResRefNum
                 %mptr))
               (values (%get-ptr %mptr) 
                       (ccl:%get-signed-word %ResRefNum)
                       (rref %mptr :resourcespec.resid))))
           (save-movie-file ()
             (let ((resolved-path (ccl::mac-namestring pathname)))
               (ccl:with-pstrs ((%filename resolved-path))
                 (rlet ((%res-id :word))
                   (print :saving)
                   (errcheck-movie
                    (#_AddMovieResource %movie% %movie-resrefnum% %res-id %filename))
                   (print :closing)
                   (errcheck-movie
                    (#_CloseMovieFile %movie-resrefnum%))
                   (%get-word %res-id))))))
      (unwind-protect
        (progn ;; open medium
          (if (get-fsspec)
            (multiple-value-bind (%movie% %movie-resrefnum% %movie-resource-id%)
                                 (create-movie-file)
              (let ((%track nil)
                    (%media% nil)
                    (size (make-point 320 240))
                    (status :started))
                (unwind-protect
                  (progn (print %movie%)
                         (errcheck-movie
                          (setf %track (#_NewMovieTrack %movie%
                                        (#_FixRatio (point-h size) 1)
                                        (#_FixRatio (point-v size) 1)
                                        #$kNoVolume)))
                         (print (list :trace %track))
                         (errcheck-movie 
                          (setf %media% (#_NewTrackMedia %track #$VideoMediaType 600
                                         (%null-ptr) 0)))
                         (print (list :media %media%))
                         (errcheck-movie (#_BeginMediaEdits %media%))
                         (print-record (%get-ptr %media%) :mediarecord)
                         (call-next-method)
                         (setf status :finished))
                  (unwind-protect
                    (when (and %media% (eq status :finished))
                      (errcheck-movie (#_EndMediaEdits %media%))
                      (errcheck-movie 
                       (#_InsertMediaIntoTrack %track 0 0 (#_GetMediaDuration  %media%) #$fixed1)))
                    ;; save and dispose of medium
                    (when %movie-resrefnum% 
                      (print (list :res-id (save-movie-file))))))))
            ;; fsspec failed
            (error "can't get fsspec: ~s." pathname))
          )))))


(defmethod project ((context quicktime-context))
  (let* ((w (context-view (context-generator context)))
         (compressed-length nil)
         (max-compressed-length 0)
         (&data nil)
         (%data nil)
         (&image-description nil)
         (%image-description nil))
    (rlet ((%frame-rect :rect :top 0 :left 0
                   :bottom (point-v (view-size w)) :right (point-h (view-size w)))
           (%max-length :long))
      (let* ((%port (#_GetWindowPort (wptr w)))
             (&pixmap-to-lock (#_GetPortBitMapForCopyBits %port)))
        (#_LockPixels &pixmap-to-lock)
        (unwind-protect
          (progn (assert
                  (zerop (#_GetMaxCompressionSize (#_GetPortPixmap %port)
                          %frame-rect
                          0
                          #$codecNormalQuality 
                          :|rle |
                          (%null-ptr)
                          %max-length)))
                 (setf max-compressed-length (ccl:%get-long %max-length))
                 (setf &data (#_NewHandle max-compressed-length))
                 (#_HLock &data)
                 (setf %data (%get-ptr &data))
                 (setf &image-description (#_NewHandle 4))
                 (#_OffsetRect %frame-rect (point-h (view-position w))
                  (- (point-v (view-position w)) 22))
                 (flet ((generate-frames (qd-context)
                          (clear-view)
                          (dotimes (x 100)
                            (with-projection-variables ()
                              (let ((size (view-size (context-view qd-context))))
                                (transform :view
                                           :translate (/ (point-h size) 2) (/ (point-v size) 2) 0.0
                                           :scale 100.0 -100.0 100.0
                                           :projection
                                           :rotate (* x .01) 0.0 0.0))
                              (sampler )

                              (errcheck-movie
                               (#_CompressImage
                                (#_GetPortPixmap (#_GetWindowPort (wptr w)))
                                ; crashes &pixmap-to-lock
                                %frame-rect
                                #$codecNormalQuality 
                                :|rle |
                                &image-description
                                %data))
                              (setf %image-description (%get-ptr &image-description))
                              (setf compressed-length (%get-signed-long %image-description 44))
                              (errcheck-movie 
                               (#_AddMediaSample
                                %media%
                                &data
                                0           ; offset
                                compressed-length     ; data size
                                50                 ;(context-duration-per-sample context)
                                &image-description
                                1                  ; sample count
                                0       
                                (%null-ptr)))
                              (sleep .1)))))
                   (call-with-projection-context #'generate-frames (context-generator context))))
          (when &data (#_disposeHandle &data))
          (when &image-description (#_disposeHandle &image-description))
          (#_UnlockPixels &pixmap-to-lock))))))

#|

(defparameter *qt-context*
  (make-instance 'quicktime-context
    :pathname #p"og:code;tests;qd2qt-1.mov"
    :generator *qc*))

(qt:initialize-quicktime)
(setf (context-pathname *qt-context*) #p"og:code;tests;qdc.mov")

(let ((view (context-make-window *qc* :view-position #@(100 100))))
  (setf (context-view *qc*) view)
  (call-with-projection-context #'(lambda (qc)
                                    (setf (context-generator *qt-context*) qc)
                                    (call-with-projection-context
                                     #'(lambda (qtc) (project qtc))
                                     *qt-context*
                                     :pathname #p"og:code;tests;qd2qt.mov"))
                                *qc*)
  (print view))

|#

:de.setf.graphics

