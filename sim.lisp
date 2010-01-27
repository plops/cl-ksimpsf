(progn 
  (require :asdf)
  (require :cffi)
  (require :cl-opengl)
  (require :cl-glu)
  (require :cl-glut))

(defpackage :mkgl
  (:use :cl :gl))
(in-package :mkgl)

(declaim (optimize (speed 2) (safety 1) (debug 3)))

(defclass mk-window (glut:window)
  ((tex :accessor tex))
  (:default-initargs :width 512 :height 512 :pos-y 10 :mode '(:double :rgb :depth)))

(defun sim-psf (&key
		(na 1.4)
		(ri 1.5)
		(lambda-ex 488)
		(lambda-em 488)
		(size '(128 128 128))
		(scale '(80 80 160)))
  (sb-ext:run-program "/usr/local/bin/khorosBin/SimPSF"
		      (list "-o" "/dev/shm/psf"
			    "-sX" (format nil "~d" (first size))
			    "-sY" (format nil "~d" (second size))
			    "-sZ" (format nil "~d" (third size))
			    "-scaleX" (format nil "~d" (first scale))
			    "-scaleY" (format nil "~d" (second scale))
			    "-scaleZ" (format nil "~d" (third scale))
			    "-lambdaEm" (format nil "~f" lambda-em)
			    "-lambdaEx" (format nil "~f" lambda-ex)
			    "-na" (format nil "~f" na)
			    "-ri" (format nil "~f" ri)
			    "-scalarTheory"))
  (let* ((num (reduce #'* size))
	 (bytes (* num 4))
	 (store (make-array bytes :element-type '(unsigned-byte 8)))
	 (volume (make-array size :element-type '(complex single-float))))
    (with-open-file (str "/dev/shm/psf" :element-type '(unsigned-byte 8))
      (read-sequence store str))
    (cffi:with-pointer-to-vector-data (p (sb-ext:array-storage-vector volume))
      (dotimes (i bytes)
	(setf (cffi:mem-aref p :unsigned-char i)
	      (aref store i))))
    volume))

(defun mk-log (x)
  (if (<= x 0f0)
      0f0
      (log x)))

(defun clamp (x)
  (cond ((< x 0) 0)
	((<= x 255) x)
	(t 255)))

(defmethod glut:display-window :before ((w mk-window))
  (enable :depth-test :blend :line-smooth)
  (line-width 2)
  (blend-func :src-alpha :one-minus-src-alpha)
;;   (tex-gen :s :texture-gen-mode :object-linear)
;;   (tex-gen :t :texture-gen-mode :object-linear)
;;   (tex-gen :r :texture-gen-mode :object-linear)
;;   (enable :texture-gen-s
;; 	  :texture-gen-t
;; 	  :texture-gen-r)
  (let* ((target :texture-3d))  
    (setf (tex w) (first (gen-textures 1)))
    (bind-texture target (tex w))
    (tex-parameter target :texture-mag-filter :linear)
    (tex-parameter target :texture-min-filter :linear)
    (let* (#+nil (psf (sim-psf :size '(128 128 128)))
		 (n 64)
		 (f (/ 256 n))
		 (buf (make-array (list n n n 4) :element-type '(unsigned-byte 8)))
		 #+nil (scale (/ 255 (loop for i below (* 128 128 128)
					   maximize (abs (aref (sb-ext:array-storage-vector psf) i))))))
      
      (dotimes (i n)
	(dotimes (j n)
	  (dotimes (k n)
	    (let* ((h (/ n 2))
		   (s (/ 1d0 n))
		   (x (* s (- i h)))
		   (y (* s (- j h)))
		   (z (* s (- k h)))
		   (r (+ (* x x) (* y y) (* z z))))
	      (declare (fixnum i j k n h)
		       (double-float s x y z r))
	      (setf (aref buf i j k 0) (* f i)
		    (aref buf i j k 1) (* f j)
		    (aref buf i j k 2) (* f k)
		    (aref buf i j k 3) 
		    (cond ((< r (* .2 .2)) 128)
			  ((< r (* .4 .4)) (if (< k h) 
					       54
					       5))
			  (t 0)))))))
      #+nil  (dotimes (i (* 128 128 128))
	       (setf (aref buf i)
		     (floor (* scale (abs (aref (sb-ext:array-storage-vector psf) i))))))
      
      (cffi:with-pointer-to-vector-data (p (sb-ext:array-storage-vector buf))
	(tex-image-3d target 0 :rgba n n n 0 :rgba :unsigned-byte p)))))

(defmethod glut:reshape ((win mk-window) w h)
  (setf (glut:width win) w
        (glut:height win) h)
  (format t "reshape ~a~%" (list w h))
  (load-identity)
  (viewport 0 0 w h)
  (matrix-mode :projection)
  (load-identity)
  ;;  (ortho 0 w 0 h -1 1)
  (glu:perspective 30 (/ w h) .1 1000)
  (glu:look-at 2 3 5
	       .5 .5 .5
	       0 0 1)
  (matrix-mode :modelview)
  (load-identity)
  (glut:post-redisplay))


(defun draw-cube ()
  (disable :texture-3d)
  (with-primitive :line-strip

    (color 1 0 0)
    (vertex 0 0 0)
    (vertex 1 0 0)
    (color 1 1 1)

    (vertex 1 0 0)
    (vertex 1 0 1)
    (vertex 0 0 1)
    
    (color 0 0 1)
    (vertex 0 0 1)
    (vertex 0 0 0)
    (color 1 1 1))
  (with-primitive :line-strip
    (vertex 0 1 0)
    (vertex 1 1 0)
    (vertex 1 1 1)
    (vertex 0 1 1)
    (vertex 0 1 0))
  (with-primitive :lines
    (vertex 0 .1 1)
    (vertex 0 .9 1)
      
    (vertex 1 .1 1)
    (vertex 1 .9 1)

    (vertex 1 .1 0)
    (vertex 1 .9 0)
    
    (color 0 1 0)
    (vertex 0 .1 0)
    (vertex 0 .9 0)
    (color 1 1 1)))

(defparameter *rot* 0)

(defun draw ()
  (clear-color 0 0 0 1)
  (clear :color-buffer-bit :depth-buffer-bit)
  (enable :texture-3d)
  (with-pushed-matrix 
    (rotate *rot* 0 0 1)
    (translate -.5 -.5 -.5)
    (setf *rot* (if (> *rot* 359)
		    0
		    (+ 3 *rot*)))
    (loop for z upto 1.001 by .02 do
     (with-primitive :quads
       (tex-coord 0 0 z)
       (vertex 0 0 z)
       (tex-coord 1 0 z)
       (vertex 1 0 z)
       (tex-coord 1 1 z)
       (vertex 1 1 z)
       (tex-coord 0 1 z)
       (vertex 0 1 z)))
    (draw-cube))
  (glut:swap-buffers)
  (sleep (/ 34))
  (glut:post-redisplay))

(defmethod glut:display ((w mk-window))
  (format t "display~%")
  (draw))
(defmethod glut:keyboard ((w mk-window) key x y)
  (format t "key~%")
  (case key
    (#\Esc (glut:destroy-current-window))))

(defun run ()
  (glut:display-window (make-instance 'mk-window)))
;#+nil
(run)


