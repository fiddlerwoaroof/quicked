(load #p"~/quicklisp/setup.lisp")
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload :cl-charms)
(ql:quickload :cells)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(defpackage :lisp-edit
  (:use :cl :cl-charms :cells :alexandria :anaphora))
(in-package :lisp-edit)

(defun apply-cursor-action (action)
  (case action
    ((:up)    (cons  0 -1))
    ((:down)  (cons  0  1))
    ((:left)  (cons -1  0))
    ((:right) (cons  1  0))))

(defun dispatch-char (c)
  (case c
    ((nil) nil)
    ((#\j) :down )
    ((#\h) :left )
    ((#\k) :up   )
    ((#\l) :right)
    ((#\f) :paint)
    ((#\q #\Q) :quit)))

(defmodel input-handler ()
          ((input-key :cell :ephemeral :accessor input-key :initform (c-in nil))
           (action :cell :ephemeral :accessor action :initform (c? (dispatch-char (^input-key))))))

(defmodel cursor ()
          ((action :cell :ephemeral :initarg :action :accessor action :initform (c-in nil))
           (current-char :initarg :current-char :accessor current-char :initform (c-in nil))
           (delta :accessor cursor-delta :initform
                  (c? (apply-cursor-action (^action))))
           (x :accessor cursor-x :initarg :x :initform (c... (0) (car (^cursor-delta))))
           (y :accessor cursor-y :initarg :y :initform (c... (0) (cdr (^cursor-delta))))))

(defun map-actions (current-char action)
  (case action
    ((:paint) (case current-char
                ((#\#) #\Space)
                ((#\Space) #\*)
                ((#\*) #\#)))))

(defmodel painter ()
          ((action :cell :ephemeral :initarg :action :accessor action :initform (c-in nil))
           (current-char :initarg :current-char :accessor current-char :initform (c-in nil))
           (output :cell :ephemeral :accessor output :initform (c? (map-actions (^current-char)
                                                                                (^action))))))

(defvar *input-handler*)
(defvar *painter*)
(defvar *cursor*)

(defun paint ()
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window*
                          (if (char/= #\Space (char-at-cursor *standard-window*))
                            #\Space
                            #\*))))

(defun constrain-pos (elem pos delta window-dimensions)
  (let* ((accessor (case elem
                     ((:x) #'car)
                     ((:y) #'cdr)))
         (window-elem (funcall accessor window-dimensions))
         (delta-elem (funcall accessor delta))
         (pos-elem (funcall accessor pos))
         (naive-delta (mod (+ pos-elem delta-elem)
                           window-elem))
         (real-delta (- naive-delta pos-elem)))
    real-delta))


(defun main ()
  (cells-reset)
  (handler-case
    (with-curses ()
      (disable-echoing)
      (enable-raw-input :interpret-control-characters t)
      (enable-non-blocking-mode *standard-window*)

      (setf *input-handler* (make-instance 'input-handler) 
            *cursor* (make-instance 'cursor :action (c? (action *input-handler*)))
            *painter* (make-instance 'painter
                                     :action (c? (action *input-handler*))
                                     :current-char (c? (current-char *cursor*))))

      (let* ((screen-dimensions (multiple-value-bind (x y) (window-dimensions *standard-window*)
                                  (cons x y))))
        (labels
          ((set-current-char (cursor)
             (with-restored-cursor *standard-window*
               (setf (current-char cursor) (char-at-cursor *standard-window*))))
           (mvc (x y)
             (move-cursor *standard-window* x y)
             (set-current-char *cursor*))
           (constrain-coordinate (side coord)
             (mod coord (funcall
                          (ecase side
                            ((:x) #'car)
                            ((:y) #'cdr))
                          screen-dimensions))))

          (defobserver output ((self painter))
                       (with-restored-cursor *standard-window*
                         (write-char-at-cursor *standard-window* new-value))
                       (with-integrity (:change)
                         (set-current-char *cursor*)))

          (defobserver y ((self cursor))
                       (with-integrity (:change)
                         (let ((adjusted-n-v (constrain-coordinate :y new-value)))
                           (if (/= new-value adjusted-n-v)
                             (setf (^cursor-y) (- adjusted-n-v new-value))
                             (mvc (^cursor-x) adjusted-n-v)))))

          (defobserver x ((self cursor))
                       (with-integrity (:change)
                         (let ((adjusted-n-v (constrain-coordinate :x new-value)))
                           (if (/= new-value adjusted-n-v)
                             (setf (^cursor-x) (- adjusted-n-v new-value))
                             (mvc adjusted-n-v (^cursor-y))))))

          (mvc 0 0)
          (loop :named driver-loop
                :for c := (get-char *standard-window* :ignore-error t)
                :do (progn
                      (refresh-window *standard-window*)
                      (setf (input-key *input-handler*) c))))))

    (sb-sys:interactive-interrupt (c) (declare (ignore c)))))

