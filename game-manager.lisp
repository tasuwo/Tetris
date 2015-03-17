(defgeneric init-game-state (game-manager)
	(:documentation "ゲーム状態を初期化する"))
(defgeneric update-game (game-manager)
	(:documentation "ゲームの更新"))


(defclass game-manager ()
	((timer
		:reader get-timer
		:initarg :timer
		:initform nil
		:documentation "ゲームタイマー")
	(game-field
		:reader get-game-field
		:initarg :game-field
		:initform nil
		:documentation "ゲームフィールド")
	(game-state
		:reader get-game-state
		:initarg :game-state
		:initform nil
		:documentation "ゲーム状態")
	(key-state
		:reader get-key-state
		:initarg :key-state
		:initform nil
		:documentation "キー押下状態")))


;; ゲーム初期化
(defmethod init-game-state ((game-manager game-manager))
	(let ((game-field (get-game-field game-manager)))
		(let ((mino-stack (mino-stack game-field))
			  (tetrimino  (falling-tetrimino game-field)))
			;; 画面リサイズ
			(draw-init game-field)
			;; ミノスタック初期化
			(init-mino-stack mino-stack game-field)
			;; テトリミノ初期化
			(init-mino tetrimino)
			 ;; 画面更新
			(sdl:update-display))))


;; シンボルを結合する
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))


;; ゲーム更新のためのマクロ
(defmacro defupdategame (&rest screen)
	`(progn
		(defmethod update-game ((game-manager game-manager))
			(let ((game-field	(get-game-field game-manager))
				  (game-state (get-game-state game-manager)))
			(sdl:clear-display sdl:*black*)
			(cond
				,@(loop for s in screen
					collect `((eq game-state ',s)
							  (progn (,(symbol-append `update- `,s `-state) game-manager)
							  		 (,(symbol-append `update- `,s `-view) game-field)))))))
		,@(loop for s in screen collect
			`(progn
				(defgeneric ,(symbol-append `update- `,s `-state)  (game-manager))
				(defgeneric ,(symbol-append `update- `,s `-view)  (game-manager))))))


;; ゲーム画面が追加された場合はここに記述
(defupdategame
	start
	play
	gameover)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 状態更新 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod update-start-state ((game-manager	game-manager))
	(with-slots (game-state) game-manager
		(let ((key-state (get-key-state game-manager)))
			(with-slots (start) key-state
				(if start (setq game-state 'play) nil)))))

(defmethod update-play-state ((game-manager		game-manager))
	(let	((timer			  (get-timer		game-manager))
			 (game-field	  (get-game-field	game-manager))
			 (key-state		  (get-key-state	game-manager)))
		(let 	((tetrimino 	(falling-tetrimino	game-field))
				 (status-box 	(get-status-box		game-field))
				 (mino-stack 	(mino-stack 		game-field)))
			;; タイマー更新
			(update-game-timer timer)
			;; ステータスボックス(プレイタイム，スコア)更新
			(update-status-box status-box timer mino-stack)
			;; テトリミノ操作
			(opt-mino tetrimino key-state game-field mino-stack)
			;; テトリミノ更新
			(update-mino tetrimino mino-stack)
			;; ゲームオーバー判定
			(check-gameover tetrimino mino-stack game-manager)
			;; 衝突判定
			(check-collision tetrimino game-field)
			(check-mino-collision tetrimino mino-stack)
			;; クリアー判定
			(check-stack mino-stack)
			)))

(defmethod update-gameover-state ((game-manager game-manager))
	())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 描画更新 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod update-start-view ((game-field game-field))
	(sdl:draw-string-solid-* (format nil "PRESS ENTER TO PLAY!!")  85 100))

(defmethod update-play-view ((game-field game-field))
	(update-field game-field))

(defmethod update-gameover-view ((game-field game-field))
	(progn (update-field game-field) (sdl:draw-string-solid-* (format nil "GAME OVER")  220 120)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;