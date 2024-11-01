(defpackage :reblocks/paginated-list
  (:use :cl :reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:export #:paginated-list
           #:render-pagination
           #:paginated-list-source
           #:current-page
           #:page-size
           #:padding
           #:paginated-list-pagination
           #:current-page-items))

(in-package :reblocks/paginated-list)

(defwidget paginated-list ()
  ((current-page :initarg :current-page
                 :initform 1
                 :type integer
                 :accessor current-page)
   (page-size :initarg :page-size
              :type integer
              :initform (error "Provide page size")
              :accessor page-size)
   (padding :initarg :padding
            :type integer
            :initform 2
            :accessor padding)))

(defgeneric paginated-list-source (paginated-list)
  (:documentation "The MUPAGINATOR source for PAGINATED-LIST"))

(defwidget pluggable-paginated-list (paginated-list)
  ((list-renderer :initarg :list-renderer
                  :accessor list-renderer
                  :type trivial-types:function-designator)
   (source :initarg :source
           :accessor paginated-list-source
           :type (or trivial-types:function-designator sequence))))

(defun paginated-list-pagination (paginated-list)
  (mupaginator:make-pagination
   :current (current-page paginated-list)
   :page-size (page-size paginated-list)
   :source (paginated-list-source paginated-list)))

(defun current-page-items (paginated-list)
  (let ((pagination (paginated-list-pagination paginated-list)))
    (mupaginator:pagination-current-items pagination)))

(defun render-pagination (paginated-list)
  (mupaginator:print-pagination-html
   (mupaginator:make-pagination
    :current (current-page paginated-list)
    :page-size (page-size paginated-list)
    :source (paginated-list-source paginated-list))
   :on-click
   (lambda (page)
     (make-js-action (lambda (&rest args)
                       (declare (ignore args))
                       (setf (current-page paginated-list) page)
                       (update paginated-list))))
   :padding (padding paginated-list)
   :stream spinneret:*html*)
  "")

(defmethod reblocks/dependencies:get-dependencies ((widget paginated-list))
  (list*
   (reblocks-lass:make-dependency
    `(.pagination
      (a 
       :margin 5px
       :padding-left 3px
       :padding-right 3px
       :cursor pointer
       :border 1px solid black)
      (a.active
       :color white
       :background-color green)
      (a.disabled
       :color gray
       :border 1px solid gray
       ;;:display none
       )))
   (call-next-method)))
