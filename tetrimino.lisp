(defgeneric init-mino (tetrimino)
  (:documentation "テトリミノを初期化する"))
(defgeneric rotate-array (tetrimino vector)
  (:documentation "テトリミノを回転させる"))


(defclass tetrimino ()
	((x-array
		:reader mino-x-array
		:initarg :x-array
		:initform 5
		:documentation "xマス座標")
	 (y-array
	 	:reader mino-y-array
	 	:initarg :y-array
	 	:initform 5
	 	:documentation "yマス座標")
	 (form
	 	:reader mino-form
	 	:initarg :form
	 	:initform (make-array '(4 2) :initial-contents '((1 0) (1 0) (1 0) (1 0)))
	 	:documentation "テトリミノの形状")
	 (vector
	 	:initarg :vector
	 	:initform "up"
	 	:documentation "テトリミノの向き")
	 (falling-speed
	 	:reader get-falling-speed
	 	:initarg :falling-speed
	 	:initform 1
	 	:documentation "落下速度")
	 (falling-state
	 	:reader get-falling-state
	 	:initarg :falling-state
	 	:initform 1
	 	:documentation "テトリミノの落下状態")
	 (mino-time
	 	:reader get-mino-time
	 	:initarg :mino-time
	 	:initform 0
	 	:documentation "テトリミノの状態時間")
	 ; 1の場合操作可能
	 ; 操作可能でない場合，前回の操作情報が格納される
	 (enable-opt
	 	:reader get-opt
	 	:initform 1
	 	:documentation "操作状態")))

(defun rand-get (var num)
  (if (> num (length var))
    nil
    (if (equal num 0)
      (car var)
      (rand-get (cdr var) (- num 1)))))

;; テトリミノを初期化する
(defmethod init-mino ((tetrimino tetrimino))
  (with-slots (x-array y-array form vector falling-state) tetrimino
    (let ((minos
        '((list '(1 4) '((1 1 1 1)))
          (list '(2 2) '((1 1) (1 1)))
          (list '(2 3) '((1 1 0) (0 1 1)))
          (list '(2 3) '((0 1 1) (1 1 0)))
          (list '(2 3) '((1 1 1) (1 0 0)))
          (list '(2 3) '((1 1 1) (0 0 1)))
          (list '(2 3) '((1 1 1) (0 1 0)))
          )))
      (let ((elem (rand-get minos (random (length minos)))))
        (setq x-array 4)
        (setq y-array 0)
        (setq form (make-array (car (eval elem)) :initial-contents (car (cdr (eval elem)))))
        ))))

;; 配列を90度回転させる
(defmethod rotate-array ((tetrimino tetrimino) vector)
  (with-slots (x-array form) tetrimino
    (let ((elem-x (array-dimension form 0))
        (elem-y (array-dimension form 1)))
      (let ((tmp form))
        (setq form (make-array `(,elem-y ,elem-x) :initial-element 0))
        (dotimes (i elem-x)
          (dotimes (j elem-y)
            (cond ((string-equal vector `right)
              (setf (aref form (- elem-y 1 j) i) (aref tmp i j))))
            (cond ((string-equal vector `left)
              (setf (aref form j (- elem-x 1 i)) (aref tmp i j))))
            ))
        ))))