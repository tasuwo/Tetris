Tetris
======

A tetris game written in Common Lisp, using lispbuilder-sdl.  

Getting Started
===============

To play, complete the following steps:

1. Open the Common Lisp interpreter. (This has only been tested on SBCL. In Addition, we need lispbuilder-sdl and lispbuilder-sdl-image)
2. Run the following command to load the program:`(load "main.lisp" :external-format :utf-8)`
3. Run the `game-start` function by entering the following command:`(game-start)`

Controls
========

* cursor key(→↓←↑)
	* Move Tetrimino
* Space key
	* Increase Tetrimino drop speed
* Shift key(L/R)
	* Rotate Tetrimino left/right
* ESC key
    * Quit the Game

Basic Rules
===========

This is a common tetris game.  
Drop Tetriminos to form complete horizonal lines. When you form a line, it will be cleared away. Don't leave even a single open space or the line won't be cleared.  
If you get pushed all the way to the top, it's game over.
