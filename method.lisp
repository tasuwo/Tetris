(defgeneric push-mino (tetrimino mino-stack)
    (:documentation "テトリミノをスタックにpushする"))
(defgeneric check-mino-collision (tetrimino mino-stack)
  (:documentation "テトリミノ同士の衝突判定を行う"))
(defgeneric fall-mino (tetrimino game-field mino-stack)
  (:documentation "テトリミノを高速落下する"))
(defgeneric check-collision (tetrimino game-field)
    (:documentation "ゲーム枠とテトリミノとの衝突判定を行う"))
(defgeneric opt-mino (tetrimino key-state game-field mino-stack)
    (:documentation "テトリミノを動かす"))
(defgeneric update-mino (tetrimino mino-stack)
    (:documentation "テトリミノの状態を更新する"))
(defgeneric check-gameover (tetrimino mino-stack game-manager)
    (:documentation "ゲームオーバー判定を行う"))
(defgeneric init-mino-stack (mino-stack game-field)
    (:documentation "テトリミノスタックを初期化する"))


;; スタックに積む
(defmethod push-mino ((tetrimino tetrimino)
                      (mino-stack mino-stack))
    (with-slots (stack) mino-stack
        (let ((x-cor (mino-x-array tetrimino))
              (y-cor (mino-y-array tetrimino))
              (form  (mino-form tetrimino))
              (form-x (array-dimension (mino-form tetrimino) 0))
              (form-y (array-dimension (mino-form tetrimino) 1)))
          (dotimes (i form-x)
            (dotimes (j form-y)
              (when (equal 1 (aref form i j))
                ;; テトリミノを格納する
                (let ((elem-x (+ i x-cor))
                      (elem-y (+ j y-cor)))
                  (setf (aref stack elem-x elem-y) 1))))))))


;; テトリミノ同士の衝突判定
(defmethod check-mino-collision ((tetrimino tetrimino)
                 (mino-stack mino-stack))
  (with-slots (x-array y-array falling-state) tetrimino
    (let ((stack  (get-mino-stack mino-stack))
        (form   (mino-form tetrimino))
        (form-x   (array-dimension (mino-form tetrimino) 0))
        (form-y   (array-dimension (mino-form tetrimino) 1))
        (vector   (get-opt tetrimino)))
      ;; テトリミノの存在する範囲内を探索する
      ;; スタックとかぶっている部分があれば補正する
      (unless (equal vector 1)
        (dotimes (i form-x)
          (dotimes (j form-y)
            ;; テトリミノの座標 + マス数
            (let ((elem-x (+ x-array i))
                (elem-y (+ y-array j)))
              (when (equal (aref form i j) 1)
                ; スタックが積まれていれば
                (when (equal (aref stack elem-x elem-y) 1)
                  ;; 補正
                  ;; どちらから進んできたか? を考慮して補正すると良い?
                  ;; 下補正を最初に行わないとおかしくなる
                  (cond ((string-equal vector `down)
                       (decf y-array 1)
                       (setq falling-state -1))
                      ((string-equal vector `right)
                       (decf x-array 1))
                      ((string-equal vector `left)
                       (incf x-array 1))))))))))))


;; テトリミノを落とす
(defmethod fall-mino ((tetrimino tetrimino)
                      (game-field game-field)
                      (mino-stack mino-stack))
  (with-slots (y-array enable-opt falling-state) tetrimino
    (unless (equal falling-state -1)
      (incf y-array 1)
      (setq enable-opt `down)
      (check-collision tetrimino game-field)
      (check-mino-collision tetrimino mino-stack)
      (fall-mino tetrimino game-field mino-stack)
    )))


;; ステージ枠との衝突判定
;; ステージの状況と, テトリミノの座標, テトリミノの操作から衝突判定を行う
(defmethod check-collision ((tetrimino tetrimino)
                            (game-field game-field))
    (with-slots (x-array y-array falling-state) tetrimino
        (let    ((mino-x        (mino-x-array       tetrimino))
                 (mino-y        (mino-y-array       tetrimino))
                 (mino-x-size   (array-dimension (mino-form tetrimino) 0))
                 (mino-y-size   (array-dimension (mino-form tetrimino) 1))
                 (field-x-size  (array-dimension (game-field-array game-field) 0))
                 (field-y-size  (array-dimension (game-field-array game-field) 1)))
            ;; 右衝突判定
            (if (>= (+ mino-x mino-x-size) field-x-size)
                (setq x-array (- field-x-size mino-x-size))
                nil
                )
            ;; 左衝突判定
            (if (<= mino-x 0)
                (setq x-array 0)
                nil
                )
            ;; 下衝突判定
            ;; 下に衝突したら，スタック状態にする
            (if (>= (+ mino-y mino-y-size) field-y-size)
                (progn  (setq y-array (- field-y-size mino-y-size))
                        (setq falling-state -1))
                nil
                ))))


;; テトリミノを操作する
;; コマンドを操作に変換する
(defmethod opt-mino ((tetrimino tetrimino)
                     (key-state key-state)
                     (game-field game-field)
                     (mino-stack mino-stack))
    (with-slots (x-array y-array enable-opt) tetrimino
        (with-slots (up down right left right-cycle left-cycle fall) key-state
            (if (equal enable-opt 1)
                (let ((chk 0))
                    (cond   (right  (if chk
                                        (progn (incf x-array 1) (setq enable-opt 'right))
                                        nil))
                            (left   (if chk
                                        (progn (decf x-array 1) (setq enable-opt 'left))
                                        nil))
                            (down   (if chk
                                        (progn (incf y-array 1) (setq enable-opt 'down))
                                        nil))
                            (up     (decf y-array 1)
                                    (setq enable-opt 'up))
                            (right-cycle    (if chk
                                            (progn (rotate-array tetrimino `right) (setq enable-opt 'right-cycle))
                                            nil))
                            (left-cycle     (if chk
                                            (progn (rotate-array tetrimino `left) (setq enable-opt 'left-cycle))
                                            nil))
                            (fall           (if chk
                                            (progn (fall-mino tetrimino game-field mino-stack) (setq enable-opt 'fall))
                                            nil))))
                ;; キーが全て離されたら操作可能にする
                (if (and    (equal right nil)
                            (equal left nil)
                            (equal up nil)
                            (equal down nil)
                            (equal right-cycle nil)
                            (equal left-cycle nil)
                            (equal fall nil))
                (setq enable-opt 1)
                nil)
                    ))))


;; テトリミノの状態を更新する
;; 落下の管理を行う
;; 落下状態, 停止状態, スタック状態の三つ
(defmethod update-mino ((tetrimino tetrimino)
                        (mino-stack mino-stack))
    (with-slots (mino-time falling-state y-array enable-opt) tetrimino
        (let ((speed    (get-falling-speed tetrimino)))
            (case falling-state
                ; 落下状態の場合
                (1 (progn   ; 停止状態にする
                        (setq falling-state 0)
                        ; 状態時間をセットする
                        (setq mino-time (get-universal-time))))
                ; 停止状態の場合
                (0 (progn   (let    ((elapsed-time 0)
                                     (univ-time (get-universal-time)))
                                ; 時刻を計測
                                (setq elapsed-time (- univ-time mino-time))
                                ; 落下速度に到達しているか? 到達したなら，一段落とし，落下状態にする
                                (if (>= elapsed-time speed)
                                    (progn  (setq falling-state 1)
                                            (setq enable-opt `down)
                                            (incf y-array 1))
                                    nil))))
                ; スタック状態の場合
                ; スタックにテトリミノをプッシュする
                ; テトリミノを初期化する
                (-1 (progn  (push-mino tetrimino mino-stack)
                            (init-mino tetrimino)))))))

;; ゲームオーバー判定
(defmethod check-gameover ((tetrimino tetrimino)
                           (mino-stack mino-stack)
                           (game-manager game-manager))
  (with-slots (form x-array y-array falling-state) tetrimino
      (with-slots (game-state) game-manager
        (with-slots (stack) mino-stack
            (let ((form-x    (array-dimension form 0))
                  (form-y    (array-dimension form 1)))
                (cond ((eq falling-state -1)
                       (dotimes (j form-y)
                           (dotimes (i form-x)
                            (if (and (eq (aref form i j) 1) (eq (aref stack (+ i x-array) (+ j y-array)) 1))
                                   (setq game-state 'gameover) nil)))
                       (setq falling-state 1))))))))

;; テトリミノスタックを初期化する
(defmethod init-mino-stack ((mino-stack mino-stack)
                            (game-field game-field))
    (with-slots (stack) mino-stack
        (let    ((x-size (array-dimension (game-field-array game-field) 0))
                 (y-size (array-dimension (game-field-array game-field) 1)))
            (setq stack (make-array `(,x-size ,y-size) :initial-element 0)))))