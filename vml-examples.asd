(defsystem "vml-examples"
  :name "VML-examples"
  :author "lambda_sakura <lambda.sakura@gmail.com>"
  :version "0.0.1"
  :components
  ((:module examples
            :components
            ((:file init-vml)
             (:file draw-blending)
             (:file draw-font)
             (:file draw-image)
             (:file draw-primitive)
             (:file input-example))))
  :serial t
  :depends-on (:vml))
