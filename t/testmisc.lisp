(in-package :json-test)
(in-suite json)

(test test-json-bind
  (json-bind (hello hi ciao) "{\"hello\":100,\"hi\":5}"
    (is (= hello 100))
    (is (= hi 5))
    (is-false ciao)))


(test test-json-bind-advanced
  (json-bind (hello-world
              sub-obj.property
              sub-obj.missing-property
              sub-obj.even-deeper-obj.some-stuff)
      "{\"helloWorld\":100,\"subObj\":{\"property\":20,\"evenDeeperObj\":{\"someStuff\":\"Guten Tag\"}}}"
    (is (= hello-world 100))
    (is (= sub-obj.property 20))
    (is-false sub-obj.missing-property)
    (is (string= sub-obj.even-deeper-obj.some-stuff "Guten Tag"))))

(test json-bind-in-bind-bug
  ;; A problem with json-bind. TODO: Fix it, but leave this testcase
  (let* ((input-json-rpc "{\"method\":\"rc\",\"id\":\"1\",\"params\":
[\"0\",{\"id\":\"pingId\",\"name\":\"ping\"},[],
[{\"name\":\"tableTennisGroupName\",\"id\":\"tableTennisGroupId\"}]]}")
         (input1 (copy-seq input-json-rpc))
         (input2 (copy-seq input-json-rpc))
         result1 result2 temp)
    (flet ((invoke-json-rpc (struct)
             (json:json-bind (method id params)
                 struct
               (format nil "{\"result\":\"~a\",\"error\":null,\"id\":\"1\"}"
                       params))))
      ;; This does not work correctly
      (json:json-bind (result error id)
          (invoke-json-rpc input1)
        (setf result1 result))
      ;; But this works
      (setf temp (invoke-json-rpc input2))
      (json:json-bind (result error id)
          temp
        (setf result2 result))
      ;; and to prove it:
      (is (string= input1 input2)) ;; We have same input
      (is (string= result1 result2))))) ;; so we should get same output


;;; Invalidated by the patch ``Re-implemented JSON-BIND (to illustrate
;;; dynamic customization).'' (Wed Jan 21 20:49:22 MSK 2009)
; 
; (test test-json-bind-with-alist
;   (with-decoder-simple-list-semantics
;     (let ((the-alist (decode-json-from-string "{\"hello\":100,\"hi\":5}")))
;       (json-bind (hello hi ciao) the-alist
;         (is (= hello 100))
;         (is (= hi 5))
;         (is-false ciao)))))
; 
; (test assoc-lookup
;   (is (equalp '(json::cdas widget-id (json::cdas parent data))
;               (macroexpand-1 '(json::assoc-lookup parent widget-id data)))))



;; JSON-RPC

;; Old syntax:
;;    (defun-json-rpc foo (x y)
;;        "Adds two numbers"
;;      (+ x y))

(defun-json-rpc foo-g :guessing (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       `((:digits . ,val)
         (:letters . ,(format nil "~R" val)))))

(defun-json-rpc foo-e :explicit (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       `(:object :digits  ,val 
                 :letters ,(format nil "~R" val))))

(defun-json-rpc foo-s :streaming (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       (format nil "{\"digits\":~a,\"letters\":\"~R\"}" val val)))

(defun test-json-rpc-helper (method-name)
  (with-decoder-simple-list-semantics
    (let (result)
      (setf result (json-rpc:invoke-rpc
                    (format nil "{\"method\":\"~a\",\"params\":[1,2],\"id\":999}" method-name)))
      (is (string= result "{\"result\":{\"digits\":3,\"letters\":\"three\"},\"error\":null,\"id\":999}")))))

(test test-json-rpc
  (test-json-rpc-helper "fooG"))

(test test-json-rpc-explicit
  (test-json-rpc-helper "fooE"))

(test test-json-rpc-streaming
  (test-json-rpc-helper "fooS"))

(test test-json-rpc-unknown-fn
  (with-decoder-simple-list-semantics
    (let ((*json-symbols-package* (find-package :json-test))
          result)
      (setf result (json-rpc:invoke-rpc "{\"method\":\"secretmethod\",\"params\":[1,2],\"id\":\"my id\"}"))
      (json-bind (result error id) result
        (is-false result)
        (is-true error)
        (is (string= id "my id"))))))
