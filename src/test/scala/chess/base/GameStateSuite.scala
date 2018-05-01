package chess.base

import chess.base.GameState._
import org.scalatest.FunSuite


class GameStateSuite extends FunSuite {

  def makeMoves(moves: List[String], gs: GameState): GameState = {
    moves match {
      case Nil => gs
      case _ => makeMoves(moves.tail, gs.Move(moves.head))
    }
  }

  val g = newGame()

  // Moves
  test("20 valid opening moves") { assert(g.validMoves.length == 20) }

  // Checkmate
  val scholar = makeMoves(List("e2e4", "e7e5", "d1f3", "b8c6", "f1c4", "d7d6", "f3f7"), g)
  test("Scholar's mate") { assert(scholar.validMoves == Nil && scholar.attr.check) }

  val fool = makeMoves(List("f2f3", "e7e5", "g2g4", "d8h4"), g)
  test("Fools's mate") { assert(fool.validMoves == Nil && fool.attr.check) }

  // Castling
  val short = makeMoves(List("e2e4", "e7e5", "f1c4", "f8c5",
    "g1f3", "g8f6", "e1g1", "e8g8"), g)
  test("Short castling") { assert(short.board(Xy("f1")) == Some(Rook(w)) &&
                                  short.board(Xy("f8")) == Some(Rook(b)) &&
                                  short.board(Xy("g1")) == Some(King(w)) &&
                                  short.board(Xy("g8")) == Some(King(b)) &&
                                  short.attr.noCastling(w) == (true, true) &&
                                  short.attr.noCastling(b) == (true, true)) }

  val cast1 = intercept[InvalidMoveException] {
    makeMoves(List("e2e4", "e7e5", "f1c4", "f8c5", "e1g1"), g)
  }
  test("No short castling when path obstructed") {
    assert(cast1.getMessage == "Invalid move")
  }
  val cast2 = intercept[InvalidMoveException] {
    makeMoves(List("e2e4", "b7b6", "f1a6", "c8a6", "g1f3", "e7e6", "e1g1"), g)
  }
  test("No short castling when path threatened") {
    assert(cast2.getMessage == "Invalid move")
  }
  val cast3 = intercept[InvalidMoveException] {
    makeMoves(List("d2d4", "e7e5", "e2e3", "e5d4", "e3d4", "f8d6", "f2f3",
      "g8f6", "d1e2", "e8g8"), g)
  }
  test("No short castling when king threatened") {
    assert(cast3.getMessage == "Invalid move")
  }
  val cast4 = intercept[InvalidMoveException] {
    makeMoves(List("e2e4", "e7e5", "f1c4", "f8c5", "g1f3", "g8f6", "e1e2",
      "e8g8", "e2e1", "d7d6", "e1g1"), g)
  }
  test("No short castling if king has moved") {
    assert(cast4.getMessage == "Invalid move")
  }

  val long = makeMoves(List("d2d4", "d7d5", "c1e3", "c8e6", "b1c3", "b8c6",
    "d1d2", "d8d7", "e1c1", "e8c8"), g)
  test("Long castling") { assert(long.board(Xy("d1")) == Some(Rook(w)) &&
                                 long.board(Xy("d8")) == Some(Rook(b)) &&
                                 long.board(Xy("c1")) == Some(King(w)) &&
                                 long.board(Xy("c8")) == Some(King(b)) &&
                                 long.attr.noCastling(w) == (true, true) &&
                                 long.attr.noCastling(b) == (true, true)) }
  val cast5 = intercept[InvalidMoveException] {
    makeMoves(List("d2d4", "d7d5", "c1e3", "c8e6", "b1c3", "b8c6", "e1c1"), g)
  }
  test("No long castling when path obstructed") {
    assert(cast5.getMessage == "Invalid move")
  }
  val cast6 = intercept[InvalidMoveException] {
    makeMoves(List("d2d4", "e7e5", "c1g5", "d8g5", "b1c3", "e5d4",
      "d1d3", "d4c3", "e1c1"), g)
  }
  test("No long castling when path threatened") {
    assert(cast6.getMessage == "Invalid move")
  }
  val cast7 = intercept[InvalidMoveException] {
    makeMoves(List("e2e4", "d7d5", "e4d5", "d8d5", "d2d3", "e7e6",
      "c1f4", "f7f6", "b1c3", "g7g6", "d1f3", "d5e5", "e1c1"), g)
  }
  test("No long castling when king threatened") {
    assert(cast7.getMessage == "Invalid move")
  }
  val cast8 = intercept[InvalidMoveException] {
    makeMoves(List("e2e4", "d7d5", "e4d5", "d8d5", "d2d3", "e7e6",
      "c1f4", "f7f6", "b1c3", "g7g6", "d1f3", "h7h6", "e1d1",
      "a7a6", "d1e1", "b7b6", "e1c1"), g)
  }
  test("No long castling if king has moved") {
    assert(cast8.getMessage == "Invalid move")
  }

  // Enpassant
  val ep = makeMoves(List("e2e4", "a7a6", "e4e5", "f7f5", "e5f6"), g)
  test("Enpassant") { assert(ep.board(Xy("f6")) == Some(Pawn(w)) &&
                             ep.board(Xy("f5")) == None &&
                             ep.attr.enPassant == None) }

  // Promotion
  val prom = makeMoves(List("e2e4", "d7d5", "e4d5", "e7e6", "d5d6", "d8f6",
    "d6d7", "e8e7"), g)
  test("Queen promotion") {
    assert(prom.Move("d7d8q").board(Xy("d8")) == Some(Queen(w)))
  }
  test("Bishop promotion") {
    assert(prom.Move("d7d8b").board(Xy("d8")) == Some(Bishop(w)))
  }
  test("Knight promotion") {
    assert(prom.Move("d7d8n").board(Xy("d8")) == Some(Knight(w)))
  }
  test("Rook promotion") {
    assert(prom.Move("d7d8r").board(Xy("d8")) == Some(Rook(w)))
  }
  test("Promotion instructions required when pawn reaches destination") {
    val pmoves = prom.validMoves.filter(x => x.promotion)
    assert(pmoves.filter(x => x.promotionPiece == None) == Nil)
  }

  // 50 move rule
  val fif1 = makeMoves(List("e2e4", "e7e5", "d1f3", "b8c6", "f1c4"), g)
  test("Non-pawn / capture increments 50-move counter") {
    assert(fif1.attr.fiftyCount == 3)
  }
  val fif2 = fif1.Move("a7a6")
  test("Pawn move resets 50-move counter") {
    assert(fif2.attr.fiftyCount == 0)
  }
  test("Capture resets 50-move counter") {
    assert(fif2.Move("c4f7").attr.fiftyCount == 0)
  }
}
