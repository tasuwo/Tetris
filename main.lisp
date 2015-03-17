(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-image)

(load "key-state.lisp" :external-format :utf-8)
(load "tetrimino.lisp" :external-format :utf-8)
(load "mino-stack.lisp" :external-format :utf-8)
(load "game-timer.lisp" :external-format :utf-8)
(load "status-box.lisp" :external-format :utf-8)
(load "game-field.lisp" :external-format :utf-8)
(load "game-manager.lisp" :external-format :utf-8)
(load "method.lisp" :external-format :utf-8)

;; 画像読み込みのための関数
(defun load-png-image (source-file)
  (sdl:convert-to-display-format  :surface (sdl:load-image source-file)
                                  :enable-alpha t
                                  :pixel-alpha t))

(defun game-start ()
  (sdl:with-init ()
    (sdl:window 600 600 :title-caption "TETRIS")
    (setf (sdl:frame-rate) 60)
    (sdl:initialise-default-font sdl:*font-10x20*)

    ;; 各オブジェクト生成
    (let* ((current-key-state (make-instance 'key-state))
          (tetrimino (make-instance 'tetrimino
                                    :x-array 4
                                    :y-array 0
                                    :form (make-array '(2 3) :initial-contents '((1 1 0) (0 1 1)))
                                    :vector "up"
                                    :falling-speed 1
                                    :falling-state 1))
          (mino-stack (make-instance 'mino-stack
                                      :clear-num 0))
          (game-timer (make-instance 'game-timer
                                      :play-time 0
                                      :start-time (get-universal-time)))
          (status-box (make-instance 'status-box
                                      :width 150
                                      :height 100
                                      :time 0
                                      :score 0))
          (game-field (make-instance  'game-field
                                      :margin 10
                                      :mino-size 20
                                      :mino-array (make-array '(10 20))
                                      :mino tetrimino
                                      :mino-stack mino-stack
                                      :status status-box
                                      ))
          (game-manager (make-instance 'game-manager
                                        :timer game-timer
                                        :game-field game-field
                                        :game-state 'start
                                        :key-state current-key-state
                                        ))
          )

      ;; 初期化
      (init-game-state game-manager)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
            (if (sdl:key= key :sdl-key-escape)
                (sdl:push-quit-event)
                (update-key-state key t current-key-state)))
        (:key-up-event (:key key)
            (update-key-state key nil current-key-state))

        ;; ゲームループ
        (:idle ()
          (update-game game-manager)
          (sdl:update-display))))))