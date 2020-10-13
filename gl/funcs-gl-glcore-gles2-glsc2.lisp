;;; this file is automatically generated, do not edit
;;; generated from files with the following copyright:
;;;
;;; Copyright 2013-2020 The Khronos Group Inc.
;;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-opengl-bindings)

;;; generated 2020-10-13T02:36:23Z
;;; from gl.xml @ git sha d580a9cd9c22452cb2ec3a7caf4a506fe0557b8d, 2020-10-13T02:28:36Z

(defglextfun ("glBlendColor" blend-color) :void
  (red float)
  (green float)
  (blue float)
  (alpha float))

(defglextfun ("glBlendEquation" blend-equation) :void
  (mode blend-equation-mode-ext))

(defglextfun ("glBindRenderbuffer" bind-renderbuffer) :void
  (target renderbuffer-target)
  (renderbuffer uint))

(defglextfun ("glGenRenderbuffers" gen-renderbuffers) :void
  (n sizei)
  (renderbuffers (:pointer uint)))

(defglextfun ("glRenderbufferStorage" renderbuffer-storage) :void
  (target renderbuffer-target)
  (internalformat internal-format)
  (width sizei)
  (height sizei))

(defglextfun ("glGetRenderbufferParameteriv" get-renderbuffer-parameter-iv) :void
  (target renderbuffer-target)
  (pname renderbuffer-parameter-name)
  (params (:pointer int)))

(defglextfun ("glBindFramebuffer" bind-framebuffer) :void
  (target framebuffer-target)
  (framebuffer uint))

(defglextfun ("glGenFramebuffers" gen-framebuffers) :void
  (n sizei)
  (framebuffers (:pointer uint)))

(defglextfun ("glCheckFramebufferStatus" check-framebuffer-status) enum
  (target framebuffer-target))

(defglextfun ("glFramebufferTexture2D" framebuffer-texture-2d) :void
  (target framebuffer-target)
  (attachment framebuffer-attachment)
  (textarget texture-target)
  (texture uint)
  (level int))

(defglextfun ("glFramebufferRenderbuffer" framebuffer-renderbuffer) :void
  (target framebuffer-target)
  (attachment framebuffer-attachment)
  (renderbuffertarget renderbuffer-target)
  (renderbuffer uint))

(defglextfun ("glGetFramebufferAttachmentParameteriv" get-framebuffer-attachment-parameter-iv) :void
  (target framebuffer-target)
  (attachment framebuffer-attachment)
  (pname framebuffer-attachment-parameter-name)
  (params (:pointer int)))

(defglextfun ("glGenerateMipmap" generate-mipmap) :void
  (target texture-target))

(defglextfun ("glProgramBinary" program-binary) :void
  (program uint)
  (binaryFormat enum)
  (binary (:pointer :void))
  (length sizei))

(defglextfun ("glTexStorage2D" tex-storage-2d) :void
  (target texture-target)
  (levels sizei)
  (internalformat internal-format)
  (width sizei)
  (height sizei))

(defglextfun ("glGetGraphicsResetStatus" get-graphics-reset-status) enum)

(defglextfun ("glGetnUniformfv" getn-uniform-fv) :void
  (program uint)
  (location int)
  (bufSize sizei)
  (params (:pointer float)))

(defglextfun ("glGetnUniformiv" getn-uniform-iv) :void
  (program uint)
  (location int)
  (bufSize sizei)
  (params (:pointer int)))

(defglextfun ("glReadnPixels" readn-pixels) :void
  (x int)
  (y int)
  (width sizei)
  (height sizei)
  (format pixel-format)
  (type pixel-type)
  (bufSize sizei)
  (data (:pointer :void)))

