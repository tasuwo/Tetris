(defgeneric init-status-box (status-box game-timer)
	(:documentation "ステータスボックス初期化"))
(defgeneric update-status-box (status-box game-timer mino-stack)
	(:documentation "ステータスボックス更新"))


(defclass status-box ()
	((width
		:reader get-status-box-w
		:initarg :width
		:initform 100
		:documentation "ステータスボックス幅")
	(height
		:reader get-status-box-h
		:initarg :height
		:initform 100
		:documentation "ステータスボックス高さ")
	(init-time
		:reader get-init-status-time
		:initarg :init-time
		:initform 0
		:documentation "計測開始時間")
	(time
		:reader get-status-time
		:initarg :time
		:initform 0
		:documentation "プレイタイム")
	(score
		:reader get-status-score
		:initarg :score
		:initform 0
		:documentation "スコア")
	))

(defmethod init-status-box ((status-box status-box)
							(game-timer game-timer))
	(with-slots (time) status-box
		(let ((game-time (get-play-time game-timer)))
			(setq time game-time))))

(defmethod update-status-box 	((status-box status-box)
								 (game-timer game-timer)
								 (mino-stack mino-stack))
	(with-slots (time score) status-box
		(let 	((game-time (get-play-time game-timer))
				 (game-score (get-clear-num mino-stack)))
			(setq time game-time)
			(setq score game-score))))