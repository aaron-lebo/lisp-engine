(in-package :xet)

(require :cl-opengl)
(require :pngload)
(require :sdl2)

(defclass model ()
  ((window :accessor window)))

(defparameter *m* (make-instance 'model))

(defun load-texture (active-tex file &optional wrap)
  (pngload:with-png-in-static-vector (png file :flip-y t)
    (let ((tex (gl:gen-texture)))
      (gl:active-texture active-tex)
      (gl:bind-texture :texture-2d tex)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (when wrap
        (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
        (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge))
      (gl:tex-image-2d :texture-2d 0 :rgba (pngload:width png) (pngload:height png) 0 :rgba :unsigned-byte (pngload:data png)))))

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

(defun make-shader (program type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (gl:attach-shader program shader)))

(defun make-program ()
  (let ((pro (gl:create-program)))
    (make-shader pro :vertex-shader *vertex-shader*)
    (make-shader pro :fragment-shader *fragment-shader*)
    (gl:link-program pro)
    pro))

(defparameter *rgb* #(0.5 0.5 0.5))

(defun render-swap (buf program)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program program)
  (gl:uniformfv (gl:get-uniform-location program "color") *rgb*)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer buf)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (sdl2:gl-swap-window (window *m*)))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:with-window (win :flags '(:opengl :shown))
      (setf (window *m*) win)
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (gl:enable :cull-face)
        (gl:enable :depth-test)
        (gl:logic-op :invert)
        (gl:clear-color 0.0 0.0 0.0 1.0)

        (load-texture :texture0 "textures/texture.png")
        (load-texture :texture1 "textures/font.png")
        (load-texture :texture2 "textures/sky.png" t)
        (load-texture :texture3 "textures/sign.png")

        (let ((vao (gl:gen-vertex-array))
              (buf (make-buffer #(-0.5 -0.5 0.0
                                   0.5 -0.5 0.0
                                   0.0  0.5 0.0)))
              (pro (make-program)))
          (gl:bind-vertex-array vao)
          (sdl2:with-event-loop (:method :poll)
            (:idle () (render-swap buf pro))
            (:quit () t)))))))
