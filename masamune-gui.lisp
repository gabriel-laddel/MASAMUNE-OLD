(in-package #:mmg)

(defparameter *min-x* -7)
(defparameter *max-x* 7)
(defparameter *min-y* -7)
(defparameter *max-y* 7)
(defparameter *graph-size* 600)
(defparameter *graph-width* nil)
(defparameter *graph-height* nil)
(defparameter *graph-ink* +black+)

(defun draw-thin-bar-graph-1 (medium function scale min max dx)
  (loop for i from 0 below (floor (- max min) dx)
        for x = min then (+ x dx)
        do (draw-line* medium i 0 i (* scale (funcall function x)))))

(defun draw-vector-bar-graph 
    (vector &key (stream *standard-output*) (scale-y 1) (ink +black+)
     (key 'identity) (start 0) (end nil))
  (let ((range (- (reduce 'max vector :start start :end end :key key)
                  0 #+NIL (reduce 'min vector :start start :end end :key key)))) ; totally wrong

    (with-room-for-graphics (stream :first-quadrant t)
      (with-new-output-record (stream)
        (with-drawing-options (stream :ink ink)
          (unless (zerop range)
            (when (eql t scale-y)
              (setf scale-y (/ 250 range)))
            (draw-thin-bar-graph-1 
             stream 
             (lambda (i) (funcall key (aref vector i)))
             scale-y start (or end (length vector)) 1)))))))

(defun draw-function-filled-graph 
    (function &key (stream *standard-output*)
     (min-x *min-x*) (max-x *max-x*)
     (min-y *min-y*) (max-y *max-y*)
     size
     (width  (or size *graph-width* *graph-size*))
     (height (or size *graph-height* *graph-size*))
     (ink *graph-ink*))
  (with-room-for-graphics (stream :first-quadrant t)
    (with-new-output-record (stream)
      (with-drawing-options (stream :ink ink)
        (draw-thin-bar-graph-1 stream function
                               (float (/ height (- max-y min-y)) 0.0f0)
                               min-x max-x
                               (/ (- max-x min-x) width))))))

(defun interaction-pane ()
  (find-pane-named *application-frame* 'interaction-pane))
