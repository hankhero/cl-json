(in-package :json-test)
(in-suite json)

(test test-json-bind
  (json-bind (hello hi ciao) "{\"hello\":100,\"hi\":5}"
    (is (= hello 100))
    (is (= hi 5))
    (is-false ciao)))


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
