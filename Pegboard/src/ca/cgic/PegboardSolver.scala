package ca.cgic

import scala.collection.mutable.Stack
import scala.collection.mutable.Map

object PegboardSolver extends App {
  type Board = MatrixBoard.Board

  val lookupBoard: Map[String, Board] = Map()
  val moveDepth: Stack[Int] = new Stack()

  var winner: Boolean = false
  var depth: Stack[Board] = new Stack()
  var moves: Long = 0

  var playedMap: Map[String, Boolean] = Map[String, Boolean]()

  override def main(args: Array[String]) {
    playedMap = Map[String, Boolean]()
    depth = new Stack()

    var board: Board = buildDefaultBoard()
    
    playBoard(board)
  }

  def playBoard(board: Board): Boolean = {
    var played: Boolean = false
    val alreadyPlayed = playedMap.keys.exists(_ == board.getUid)

    if (!alreadyPlayed && !winner) {
      depth.push(board)
      playedMap(board.getUid) = true
      for (board <- board.permutateBoard()) {
        winner = board.isWinner()
        if (!winner) {
          moves += 1
          played = playBoard(board)
        } else {
          println(board)
          println(depth)
          System.exit(0)
        }
      }
    }

    if (alreadyPlayed) {
      depth.pop()
      println(s"Played and skipping ${board.getUid} ${playedMap.size}{$moves}{${depth.size}...")
    }

    played
  }

  def buildDefaultBoard(): Board = {
    val boundsTuple = for (
      i <- List(0, 1, 5, 6);
      j <- List(0, 1, 5, 6)
    ) yield (i, j);

    val board = new Board(height = 7, width = 7, boundry = boundsTuple)

    for (
      i <- 0 until 7;
      j <- 2 until 5
    ) {
      if (i != j || i != 3) {
        board.setBit(row = i, col = j)
        board.setBit(row = j, col = i)
      }
    }

    board
  }

  def buildSuccessBoard(): Board =
    {
      val boundsTuple = for (
        i <- List(0, 1, 5, 6);
        j <- List(0, 1, 5, 6)
      ) yield (i, j);

      val board = new Board(height = 7, width = 7, boundry = boundsTuple, initValue = false)

      for (
        i <- 0 until 7;
        j <- 2 until 5
      ) {
        if (i != j || i != 3) {
          board.setBit(row = i, col = j, false)
          board.setBit(row = j, col = i, false)
        }
      }

      board.setBit(3, 3, true)

      board
    }

}