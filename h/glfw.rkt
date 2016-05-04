#lang racket/base
(require rmc
         rmc/h)

(define-rmc/header
  (H #:pre '("#define GLFW_INCLUDE_GLCOREARB")
     #:cflags (pkg-config-cflags "glfw3")
     #:ldflags (pkg-config-ldflags "glfw3")
     "<GLFW/glfw3.h>")
  (F glfwInit -> Bool)
  (F glfwTerminate -> Void)
  (F glfwGetVersionString -> String)
  (V GLFW_OPENGL_CORE_PROFILE S32)
  (E GLFWWindowHint S32
     ;; XXX
     GLFW_SAMPLES
     GLFW_CONTEXT_VERSION_MAJOR
     GLFW_CONTEXT_VERSION_MINOR
     GLFW_RESIZABLE
     GLFW_OPENGL_FORWARD_COMPAT
     GLFW_OPENGL_PROFILE)
  (F glfwWindowHint GLFWWindowHint S32 -> Void)
  (T GLFWmonitor)
  (F glfwGetPrimaryMonitor -> GLFWmonitor*)
  (S GLFWvidmode
     [S32 width] [S32 height]
     [S32 redBits] [S32 greenBits] [S32 blueBits]
     [S32 refreshRate])
  (F glfwGetVideoMode GLFWmonitor* -> GLFWvidmode*)
  (T GLFWwindow)
  (F glfwCreateWindow S32 S32 String GLFWmonitor* GLFWwindow* -> GLFWwindow*)
  (E GLFWInputMode S32
     GLFW_CURSOR GLFW_STICKY_KEYS GLFW_STICKY_MOUSE_BUTTONS)
  (Vs (GLFW_CURSOR_NORMAL GLFW_CURSOR_HIDDEN GLFW_CURSOR_DISABLED) S32)
  (F glfwSetInputMode GLFWwindow* GLFWInputMode S32 -> Void)
  (F glfwMakeContextCurrent GLFWwindow* -> Void)
  (F glfwSwapInterval S32 -> Void)
  (E GLFWKeyAction S32
     GLFW_PRESS GLFW_RELEASE GLFW_REPEAT)
  (Vs (GLFW_KEY_ESCAPE) S32)
  (Vs (GLFW_MOD_ALT GLFW_MOD_CONTROL GLFW_MOD_SHIFT GLFW_MOD_SUPER) S32)
  (define GLFWkeyfun (Fun (list GLFWwindow* S32 S32 GLFWKeyAction S32) Void))
  (F glfwSetKeyCallback GLFWwindow* GLFWkeyfun -> Void)
  (F glfwWindowShouldClose GLFWwindow* -> Bool)
  (F glfwPollEvents -> Void)
  (F glfwSetWindowShouldClose GLFWwindow* S32 -> Void)
  (F glfwSwapBuffers GLFWwindow* -> Void)
  ;; XXX more
  
  ;; OpenGL that comes from GLFW
  (Vs (GL_TRUE GL_FALSE) S32))
