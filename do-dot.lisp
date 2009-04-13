;; -*- mode: lisp; syntax: common-lisp; -*-

;; Copyright © 2008 Ben Hyde, Licensed under the Apache License, Version 2.0.

(in-package "COMMON-LISP-USER")

(defun generate-drawing ()
  "Emits to standard output a graphviz dot sketch of the currently installed packages"
  (let ((dependencies 
         (with-open-file (f "dependencies")
           (loop
             with result finally (return result)
             for ln = (read-line f nil nil)
             while ln 
             unless (eq #\# (char ln 0))
               do
            (push (cl-ppcre:split " +" ln) result)))))

    (let ((out *standard-output*)
          (noted ()))
      (labels ((f (x)
                 (cl-ppcre:regex-replace-all "[-+.]" x "_"))
               (arc (m1 m2)
                 (format out "~&~A -> ~A;" (f m1) (f m2)))
               (label-node (m)
                 (format out "~&~A [ label=\"~A\"];" (f m) m))
               (note-module (module)
                 (unless
                     (member module noted :test #'string=)
                   (push module noted)
                   (label-node module)
                   (loop for d in (rest (find module dependencies 
                                              :test #'string=
                                              :key #'first))
                         do
                      (arc module d)
                      (note-module d)))))
      (loop initially
            (format out "digraph G { concentrate=true; rankdir=LR; node [shape=plaintext]; edge [ arrowhead=open];")
            (label-node "Installed")

            finally (format out "~&}")

            for f in (directory "source/*")
            as module = (car (last (pathname-directory f)))
            do
         (arc "Installed" module)
         (note-module module))))))


