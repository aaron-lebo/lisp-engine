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

(defclass program ()
  ((id :initarg :id)
   position
   normal
   uv
   matrix
   sampler
   camera
   timer
   extra1
   extra2
   extra3
   extra4))

(defun read-file (file)
  (with-open-file (stream file)
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
    (gl:detach-shader prg vert)
    (gl:detach-shader prg frag)
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    (loop :for (slot str) :on attribs :by #'cddr :while str
          :do (setf (slot-value program slot) (gl:get-attrib-location prg str)))
    (loop :for (slot str) :on uniforms :by #'cddr :while str
          :do (setf (slot-value program slot) (gl:get-uniform-location prg str)))
    program))

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

        (let ((block-prg (load-program "block"
                                       '(position "position"
                                         normal "normal"
                                         uv "uv")
                                       '(matrix "matrix"
                                         sampler "sampler"
                                         camera "camera"
                                         timer "timer"
                                         extra1 "sky_sampler"
                                         extra2 "daylight"
                                         extra3 "fog_distance"
                                         extra4 "ortho")))
              (line-prg (load-program "line" '(position "position") '(matrix "matrix")))
              (text-prg (load-program "text"
                                      '(position "position"
                                        uv "uv")
                                      '(matrix "matrix"
                                        sampler "sampler"
                                        extra1 "is_sign")))
              (sky-prg (load-program "sky"
                                     '(position "position"
                                       normal "normal"
                                       uv "uv")
                                     '(matrix "matrix"
                                       sampler "sampler"
                                       timer "timer")))
              (triangle-prg (load-program "triangle"))
              (vao (gl:gen-vertex-array))
              (buf (make-buffer #(-0.5 -0.5 0.0
                                   0.5 -0.5 0.0
                                   0.0  0.5 0.0))))
          (gl:bind-vertex-array vao)
          (sdl2:with-event-loop (:method :poll)
            (:idle () (render-swap buf (slot-value triangle-prg 'id)))
            (:quit () t)))))))
