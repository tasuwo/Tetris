(defgeneric draw-init (game-field)
	(:documentation "テトリミノのサイズにあわせ, 画面をリサイズする"))
(defgeneric draw-array (xmax ymax margin mino-size)
	(:documentation "ゲームフィールドを描画する"))
(defgeneric draw-status-box (game-field)
	(:documentation "ステータスボックスを描画する"))
(defgeneric draw-tetrimino (game-field)
	(:documentation "テトリミノを描画する"))
(defgeneric draw-mino-stack (game-field)
	(:documentation "テトリミノスタックを描画する"))
(defgeneric update-field (game-field)
	(:documentation "画面を更新する"))


(defclass game-field ()
	((margin
	 	:reader get-field-margin
	 	:initarg :margin
	 	:initform 10
	 	:documentation "ゲーム画面のマージン")
	 (mino-array
	 	:reader game-field-array
	 	:initarg :mino-array
	 	:initform (make-array '(10 20))
	 	:documentation "テトリミノ格納用配列")
	 (mino-size
	 	:reader tetrimino-size
	 	:initarg :mino-size
	 	:initform 20
	 	:documentation "テトリミノ格納用配列の描画サイズ")
	 (mino
	 	:reader falling-tetrimino
	 	:initarg :mino
	 	:initform nil
	 	:documentation "落下中のテトリミノ")
	 (mino-stack
	 	:reader mino-stack
	 	:initarg :mino-stack
	 	:initform nil
	 	:documentation "テトリミノスタック")
	 (status
	 	:reader get-status-box
	 	:initarg :status
	 	:initform nil
	 	:documentation "ステータスボックス")
	 ))


;; 描画初期化
(defmethod draw-init ((game-field game-field))
	(let ((margin 			(get-field-margin game-field))
		  (array-width		(array-dimension (game-field-array game-field) 0))
		  (array-height		(array-dimension (game-field-array game-field) 1))
		  (mino-size 		(tetrimino-size game-field))
		  (status 			(get-status-box game-field)))
		(let ((status-width (get-status-box-w status)))
			(sdl:resize-window 	(+ (* mino-size array-width) (* margin 3) status-width)
								(+ (* mino-size array-height) (* margin 2))))))


;; ゲームフィールドを描画する
(defmethod draw-array (xmax ymax margin mino-size)
	(dotimes (x xmax)
		(dotimes (y ymax)
			;; 四角形描画
			(sdl:draw-box-* (+ margin (* x mino-size))			; 左上頂点のX座標
							(+ margin (* y mino-size))			; 左上頂点のY座標
							mino-size							; 幅
							mino-size							; 高さ
							:color sdl:*black*					; 中の色
							:stroke-color sdl:*white*))))		; 辺の色


;; ステータスボックスを描画する
(defmethod draw-status-box ((game-field game-field))
	(let 	((xmax  			(array-dimension (game-field-array game-field) 0))
			 (margin 			(get-field-margin game-field))
			 (mino-size 		(tetrimino-size game-field))
			 (status-box 		(get-status-box game-field)))
		(let 	((width  (get-status-box-w status-box))
				 (height (get-status-box-h status-box))
				 (time   (get-status-time  status-box))
				 (score  (get-status-score status-box)))
				;; 四角形描画
			(sdl:draw-box-* (+ (* margin 2) (* xmax mino-size))   	; 左上頂点のX座標
							margin 									; 左上頂点のY座標
							 width 									; 幅
							height 									; 高さ
							:color sdl:*black*						; 中の色
							:stroke-color sdl:*white*)  			; 辺の色
			(sdl:draw-string-solid-* (format nil "time  : ~D" time) 	(+ (+ (* margin 2) (* xmax mino-size)) 10) (+ margin 10))
			(sdl:draw-string-solid-* (format nil "score : ~D" score) 	(+ (+ (* margin 2) (* xmax mino-size)) 10) (+ margin (* 10 3))))))


;; テトリミノを描画する
(defmethod draw-tetrimino ((game-field game-field))
	(let ((tetrimino (falling-tetrimino game-field))
		  (margin 			(get-field-margin game-field))
		  (mino-size 		(tetrimino-size game-field)))
		(let ((x 		(mino-x-array tetrimino))
			  (y 		(mino-y-array tetrimino))
			  (form 	(mino-form tetrimino))
			  (form-x 	(array-dimension (mino-form tetrimino) 0))
			  (form-y 	(array-dimension (mino-form tetrimino) 1)))
		(dotimes (i form-x)
			(dotimes (j form-y)
				(when (< 0 (aref form i j))
				;; 四角形描画
				(sdl:draw-box-* (+ margin (* (+ i x) mino-size))		; 左上頂点のX座標
								(+ margin (* (+ j y) mino-size))		; 左上頂点のY座標
								mino-size								; 幅
								mino-size								; 高さ
								:color sdl:*blue*						; 中の色
								:stroke-color sdl:*white*)))))))		; 辺の色


;; テトリミノスタックを描画する
(defmethod draw-mino-stack ((game-field game-field))
	(let ((margin (get-field-margin game-field))
		  (mino-size (tetrimino-size game-field))
		  (mino-stack (mino-stack game-field)))
		(let ((elems-x 	(array-dimension (get-mino-stack mino-stack) 0))
			  (elems-y 	(array-dimension (get-mino-stack mino-stack) 1))
			  (stack 	(get-mino-stack mino-stack)))
			(dotimes (i elems-x)
				(dotimes (j elems-y)
					(when (< 0 (aref stack i j))
						;; 四角形描画
						(sdl:draw-box-* (+ margin (* i mino-size))		; 左上頂点のX座標
										(+ margin (* j mino-size))		; 左上頂点のY座標
										mino-size								; 幅
										mino-size								; 高さ
										:color sdl:*blue*					; 中の色
										:stroke-color sdl:*white*)))))))		; 辺の色


;; 画面を更新する
;; テトリミノ, ステータスボックス, ゲームフィールドを描画する
(defmethod update-field	((game-field game-field))
	(let ((margin 			(get-field-margin game-field))
		  (array-width		(array-dimension (game-field-array game-field) 0))
		  (array-height		(array-dimension (game-field-array game-field) 1))
		  (mino-size 		(tetrimino-size game-field)))
		(draw-array array-width array-height margin mino-size)
		(draw-status-box game-field)
		(draw-tetrimino game-field)
		(draw-mino-stack game-field)))