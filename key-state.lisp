;; 総称関数
(defgeneric update-key-state (key key-press key-state)
	(:documentation "キーの状態を更新する"))

;; キー入力状態クラスを生成するマクロ
;; @param name     キー操作名
;; @param key-maps キー操作 複数指定可能
(defmacro defkeystate (name &rest key-maps)
	`(progn
		(defclass ,name ()
 			,(loop for k in key-maps collect `(,(car k) :initform nil))
 			)
		,(let ( (key (gensym)) (key-press (gensym)) (key-state (gensym)) )
			`(defmethod update-key-state (,key ,key-press (,key-state ,name))
				(with-slots ,(mapcar #'car key-maps) ,key-state
					(cond ,@(loop for k in key-maps
							collect `((sdl:key= ,key ,(cadr k))
									  (setf ,(car k) ,key-press )))))))))

;; キー入力の状態クラス
(defkeystate key-state
	(start 			:sdl-key-return)
	(up    			:sdl-key-up)
	(down  			:sdl-key-down)
	(right 			:sdl-key-right)
	(left  			:sdl-key-left)
	(left-cycle 	:sdl-key-lshift)
	(right-cycle 	:sdl-key-rshift)
	(fall 			:sdl-key-space))