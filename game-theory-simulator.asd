
(in-package :cl-user)
(defpackage :game-theory-simulator-asd
  (:use :asdf :cl))
(in-package :game-theory-simulator-asd)


(defsystem game-theory-simulator
  :name "game-theory-simulator"
  :author "Makinori Ikegami"
  :licence "MIT"
  :description "2D expanded Prisoner's dilemma simulator"
  :depends-on (:lispbuilder-sdl )
               ;; :lispbuilder-sdl-gfx)
  :components 
  ((:file "packages")
   (:module "src"
            :components
            ((:file "util")
             (:file "field" :depends-on ("util"))
             (:file "rule")
             (:file "UI" :depends-on ("util" "field"))))))
