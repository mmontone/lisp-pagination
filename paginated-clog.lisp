(require :mupaginator)

(defpackage :clog-paginated-test
  (:use :cl)
  (:export #:start))

(in-package :clog-paginated-test)

;; CLOG HACK!!
;; Modified from clog:set-on-event
(defun make-clog-callback
    (obj event handler
     &key (call-back-script "")
       (eval-script "")
       (post-eval "")
       (cancel-event nil)
       (one-time nil))
  (let ((hook (format nil "~A:~A" (clog::html-id obj) event))
        (cd   (clog::connection-data obj)))
    (if cd
        (cond (handler
               (let ((callback (format nil "~Aws.send(\"E:~A \"~A)~A~@[~A~]~@[~A~]"
                                       eval-script
                                       hook
                                       call-back-script
                                       post-eval
                                       (when one-time
                                         (format nil "; ~A.off(\"~A\")"
                                                 (clog::jquery obj)
                                                 event))
                                       (when cancel-event "; return false"))))
                 (clog::bind-event-script obj event callback)
                 (setf (gethash hook cd) handler)
                 ;; we return the callback js script
                 callback))
              (t
               (clog::unbind-event-script obj event)
               (remhash hook cd)))
        (format t "Attempt to set event on non-existant connection.~%"))))

(defun create-paginated-list (clog-obj list)
  (let* ((pagination (mupaginator:make-pagination :source list))
         (paginated-list (clog:create-div clog-obj))
         (ul (clog:create-unordered-list paginated-list :class "w3-ul"))
         (pagination-buttons (clog:create-div paginated-list)))
    (labels ((update-view ()
               (setf (clog:text ul) "")
               (dolist (item (mupaginator:pagination-current-items pagination))
                 (clog:create-list-item ul :content (package-name item)))
               (setf (clog:text pagination-buttons) "")
               (clog:create-child pagination-buttons
                     (with-output-to-string (html)
                       (mupaginator:print-pagination-w3css
                        pagination
                        :on-click
                        (lambda (page)
                          (let ((a (clog:create-a clog-obj :auto-place nil)))
                            (make-clog-callback a "click"
                                                (lambda (&rest args)
                                                  (declare (ignore args))
                                                  (setf (mupaginator::pagination-current pagination) page)
                                                  (update-view)))))
                        :stream html)))))
      (update-view)
      paginated-list)))

(defun on-index (body)
  ;; Load css files
  (clog:load-css (clog:html-document body) "https://www.w3schools.com/w3css/4/w3.css")
  (clog:load-css (clog:html-document body) "https://www.w3schools.com/lib/w3-theme-teal.css")
  ;; Setup page
  (setf (clog:title (clog:html-document body)) "Paginated List Demo")
  (let* ((header (clog:create-section body :header :class "w3-container w3-card w3-theme"))
         (tmp    (clog:create-section header :h1 :content "List of packages"))
         ;; Main area of page
         (main (clog:create-div body :class "w3-container")))
    (declare (ignore tmp))
    (create-paginated-list main (list-all-packages))))

(defun start ()
  (clog:initialize 'on-index)
  (clog:open-browser))
