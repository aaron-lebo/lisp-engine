(in-package :xet)

(require :cl-opengl)
(require :pngload)
(require :sdl2)

(defparameter *rgb* #(0.5 0.5 0.5))
(defvar *window*)

(defclass program ()
  ((id :initarg :id)))

(defun read-file (path)
  (with-open-file (stream path)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      str)))

(defun load-shader (program name type)
  (let ((src (read-file (format nil "shaders/~a_~a.glsl" name type)))
        (shader (gl:create-shader (if (eq type "vertex") :vertex-shader :fragment-shader))))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (gl:attach-shader program shader)
    shader))

(defun load-program (name &optional attribs uniforms)
  (let* ((prg (gl:create-program))
         (vert (load-shader prg name "vertex"))
         (frag (load-shader prg name "fragment"))
         (program (make-instance 'program :id prg)))
    (gl:link-program prg)
    (loop :for shader :in (list vert frag) :do
      (gl:detach-shader prg shader)
      (gl:delete-shader shader))
    (loop :for (slot str) :on attribs :by #'cddr :while str
          :do (setf (slot-value program slot) (gl:get-attrib-location prg str)))
    (loop :for (slot str) :on uniforms :by #'cddr :while str
          :do (setf (slot-value program slot) (gl:get-uniform-location prg str)))
    program))

(defun make-buffer (items &optional (type :array-buffer))
  (let* ((len (length items))
         (arr (gl:alloc-gl-array (if (eq type :array-buffer) :float :unsigned-short) len))
         (buf (gl:gen-buffer)))
    (gl:bind-buffer type buf)
    (dotimes (i len)
      (setf (gl:glaref arr i) (aref items i)))
    (gl:buffer-data type :static-draw arr)
    (gl:free-gl-array arr)
    (gl:bind-buffer type 0)
    buf))

(defun render-swap (vertex-buf index-buf program)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program program)
  (gl:uniformfv (gl:get-uniform-location program "color") *rgb*)

  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer vertex-buf)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  (gl:bind-buffer :element-array-buffer index-buf)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 3)

  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)

  (sdl2:gl-swap-window *window*))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:with-window (win :flags '(:opengl :shown))
      (setf *window* win)
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (gl:enable :cull-face)
        (gl:enable :depth-test)
        (gl:logic-op :invert)
        (gl:clear-color 0.0 0.0 0.0 1.0)

        (let ((vao (gl:gen-vertex-array))
              (vertex-buf (make-buffer #(-0.5 -0.5 0.0
                                         +0.5 -0.5 0.0
                                         +0.0  0.5 0.0)))
              (index-buf (make-buffer #(0 1 2) :element-array-buffer))
              (triangle-prg (load-program "triangle")))
          (gl:bind-vertex-array vao)
          (sdl2:with-event-loop (:method :poll)
            (:idle () (render-swap vertex-buf index-buf (slot-value triangle-prg 'id)))
            (:quit () t)))))))
