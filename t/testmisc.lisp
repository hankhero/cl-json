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

(test test-json-bind-with-alist
  (let ((the-alist (decode-json-from-string "{\"hello\":100,\"hi\":5}")))
    (json-bind (hello hi ciao) the-alist
      (is (= hello 100))
      (is (= hi 5))
      (is-false ciao))))

(test assoc-lookup
  (is (equalp '(json::cdas widget-id (json::cdas parent data))
              (macroexpand-1 '(json::assoc-lookup parent widget-id data)))))


(defun-json-rpc foo (x y)
  "Adds two numbers"
  (+ x y))


(test test-json-rpc
  (let (result)
    (setf result (json-rpc:invoke-rpc "{\"method\":\"foo\",\"params\":[1,2],\"id\":999}"))
    (is (string= result "{\"result\":3,\"error\":null,\"id\":999}"))))

(test test-json-rpc-unknown-fn
  (let (result)
    (setf result (json-rpc:invoke-rpc "{\"method\":\"secretmethod\",\"params\":[1,2],\"id\":\"my id\"}"))
    (json-bind (result error id) result
      (is-false result)
      (is-true error)
      (is (string= id "my id")))))
