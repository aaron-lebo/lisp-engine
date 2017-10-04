(in-package :xet)

(require :cl-opengl)
(require :sdl2)

(defparameter *vertex-shader*
  "#version 330 core

   layout(location = 0) in vec3 xyz;

   void main() {
       gl_Position.xyz = xyz;
       gl_Position.w = 1.0;
   }")

(defparameter *fragment-shader*
  "#version 330 core

   uniform vec3 color;
   out vec3 colr;

   void main() {
       colr = color;
   }")

(defun make-buffer (verts)
  (let* ((len (length verts))
         (arr (gl:alloc-gl-array :float len))
         (buf (gl:gen-buffer)))
    (gl:bind-buffer :array-buffer buf)
    (dotimes (i len)
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)
    (gl:bind-buffer :array-buffer 0)
    buf))

(defun make-shader (type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    shader))

(defun make-program ()
  (let ((pro (gl:create-program))
        (vert (make-shader :vertex-shader *vertex-shader*))
        (frag (make-shader :fragment-shader *fragment-shader*)))
    (gl:attach-shader pro vert)
    (gl:attach-shader pro frag)
    (gl:link-program pro)
    pro))

(defparameter *rgb* #(0.5 0.5 0.5))

(defun render-swap (win buf program)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program program)
  (gl:uniformfv (gl:get-uniform-location program "color") *rgb*)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer buf)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (sdl2:gl-swap-window win))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:with-window (win :flags '(:opengl :shown))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (gl:clear-color 0.0 0.0 0.0 0.0)
        (let ((vao (gl:gen-vertex-array))
              (buf (make-buffer #(-0.5 -0.5 0.0
                                   0.5 -0.5 0.0
                                   0.0  0.5 0.0)))
              (pro (make-program)))
          (gl:bind-vertex-array vao)
          (sdl2:with-event-loop (:method :poll)
            (:idle () (render-swap win buf pro))
            (:quit () t)))))))
