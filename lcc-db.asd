;;;; lcc-db.asd
;;;; Load using (asdf:load-system :lcc-db)

(asdf:defsystem #:lcc-db
  :serial t
  :description "A tool for organizing your personal library according to the Library of Congress Classification (LCC) Method."
  :author "nshan651"
  :license "AGPL V3.0"
  :depends-on (:drakma
               :cl-csv
               :cl-ppcre
               :cl-json
               :babel)
  :components ((:file "src/main")))
  ;; :components 
  ;; ((:module "src"
  ;;           :components
  ;;           ((:file "main")))))
