(defpackage :reblocks-paginated-list-demo
  (:use :cl :reblocks/html)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget))

(in-package :reblocks-paginated-list-demo)

(defapp reblocks-paginated-list-demo
  :prefix "/")

(reblocks/debug:on)

(defvar *port* (find-port:find-port))

(defwidget packages-list (reblocks/paginated-list:paginated-list)
  ())

(defmethod reblocks/paginated-list:paginated-list-source ((paginated-list packages-list))
  (list-all-packages))

(defmethod render ((widget packages-list))
  (with-html
    (:ul
     (dolist (item (reblocks/paginated-list:current-page-items widget))
       (:li (:span (package-name item)))))
    (reblocks/paginated-list:render-pagination widget)))

(defmethod reblocks/page:init-page ((app reblocks-paginated-list-demo) url-path expire-at)
  (make-instance 'packages-list
                 :page-size 10
                 :padding 2))

;; (reblocks/server:start :port *port*)
;; (reblocks/debug:reset-latest-session)
