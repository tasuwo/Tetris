;http://www.c3.club.kyutech.ac.jp/gamewiki/index.php?%A5%D5%A5%EC%A1%BC%A5%E0%C0%A9%B8%E6(2D%CA%D4)
;http://kaz.cyteen.nagoya-bunri.ac.jp/advprog2/lisp2.html
(defgeneric init-game-timer (game-timer)
	(:documentation "タイマーを初期化する"))
(defgeneric update-game-timer (game-timer)
	(:documentation "時刻を更新する"))

(defclass game-timer ()
	((play-time
		:reader get-play-time
		:initarg :play-time
		:initform 0
		:documentation "プレイ時間")
	 (start-time
		:reader get-start-time
		:initarg :start-time
		:initform (get-universal-time)
		:documentation "プレイ開始時刻")))

; タイマーの初期化
(defmethod init-game-timer ((game-timer game-timer))
	(with-slots (start-time) game-timer
		(setq start-time (get-universal-time))))

; タイマーを進める
; ゲーム全体のプレイタイム
(defmethod update-game-timer 	((game-timer game-timer))
	(with-slots (play-time) game-timer
		(let ((start-time (get-start-time game-timer))
			  (univ-time  (get-universal-time)))
			(setf play-time (- univ-time start-time))
			)))