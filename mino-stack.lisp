
(defgeneric check-stack (mino-stack)
    (:documentation "クリアー判定を行う"))

(defclass mino-stack ()
    ((stack
        :reader get-mino-stack
        :initarg :stack
        :initform nil
        :documentation "テトリミノスタック")
    (clear-num
        :reader get-clear-num
        :initarg :clear-num
        :initform 0
        :documentation "クリアー回数")))

;; 配列をずらす
(defun clear-stack (stack elem-x cor-y)
  (if (equal cor-y 0)
    (progn  (dotimes (i elem-x)
              (setf (aref stack i cor-y) 0)))
    (progn  (dotimes (i elem-x)
              (setf (aref stack i cor-y) (aref stack i (- cor-y 1))))
            (clear-stack stack elem-x (- cor-y 1)))))

;; クリアー判定を行い，クリアーする．
(defmethod check-stack ((mino-stack mino-stack))
  (with-slots (stack clear-num) mino-stack
    (let ((elem-x (array-dimension stack 0))
          (elem-y (array-dimension stack 1)))
      ;; 一行ごとに見ていく
      (dotimes (j elem-y)
        (dotimes (i elem-x)
          (unless (equal (aref stack i j) 1) (return))
          (when (equal i (- elem-x 1))
            (clear-stack stack elem-x j)
            (incf clear-num 1))
          )))))