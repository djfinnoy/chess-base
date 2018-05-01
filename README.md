# Chess game mechanics
This Scala package contains a programmatic representation of Chess.
It is limited to the game mechanics, meaning it's capable of representing a game, and identifying all valid moves within a given game state.
This is not (yet) a Chess "AI"; it does not make moves by itself, it only tells you (or a chess engine) what moves are available.
This functionality, whether it be hard-coded or taught via machine learning, forms the foundation of any chess engine.

### Demo
```
scala> import chess.base._

scala> val g = newGame()

g: chess.base.GameState =
===================
White to move
Valid moves: 20
-------------------
8 .♜.♞.♝.♛.♚.♝.♞.♜.
7 .♟.♟.♟.♟.♟.♟.♟.♟.
6 . . . . . . . . .
5 . . . . . . . . .
4 . . . . . . . . .
3 . . . . . . . . .
2 .♙.♙.♙.♙.♙.♙.♙.♙.
1 .♖.♘.♗.♕.♔.♗.♘.♖.
   a b c d e f g h
-------------------
Move #: 1
Check: false
No castling: Map(w -> (false,false), b -> (false,false))
Enpassant: None
50-rule counter: 0
===================

scala> g.Move("b1c3")

res1: chess.base.GameState =
===================
Black to move
Valid moves: 20
-------------------
8 .♜.♞.♝.♛.♚.♝.♞.♜.
7 .♟.♟.♟.♟.♟.♟.♟.♟.
6 . . . . . . . . .
5 . . . . . . . . .
4 . . . . . . . . .
3 . . .♘. . . . . .
2 .♙.♙.♙.♙.♙.♙.♙.♙.
1 .♖. .♗.♕.♔.♗.♘.♖.
   a b c d e f g h
-------------------
Move #: 2
Check: false
No castling: Map(w -> (false,false), b -> (false,false))
Enpassant: None
50-rule counter: 1
===================

scala>   def makeMoves(moves: List[String], gs: GameState): GameState = {
     |     moves match {
     |       case Nil => gs
     |       case _ => makeMoves(moves.tail, gs.Move(moves.head))
     |     }
     |   }
makeMoves: (moves: List[String], gs: chess.base.GameState)chess.base.GameState

scala> val foolsMate = List("f2f3", "e7e5", "g2g4", "d8h4")

foolsMate: List[String] = List(f2f3, e7e5, g2g4, d8h4)

scala> makeMoves(foolsMate, g)

res2: chess.base.GameState =
===================
White to move
Valid moves: 0
-------------------
8 .♜.♞.♝. .♚.♝.♞.♜.
7 .♟.♟.♟.♟. .♟.♟.♟.
6 . . . . . . . . .
5 . . . . .♟. . . .
4 . . . . . . .♙.♛.
3 . . . . . .♙. . .
2 .♙.♙.♙.♙.♙. . .♙.
1 .♖.♘.♗.♕.♔.♗.♘.♖.
   a b c d e f g h
-------------------
Move #: 5
Check: true
No castling: Map(w -> (false,false), b -> (false,false))
Enpassant: None
50-rule counter: 1
===================

```
