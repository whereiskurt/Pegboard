package ca.cgic

import scala.collection.mutable.ListBuffer

object MatrixBoard {

  type Row = ListBuffer[Boolean]
  type Matrix = ListBuffer[Row]
  type ProductMatrix = ListBuffer[Double]
  type TupleList = List[Tuple2[Int, Int]]

  class Board(
      val height: Int = 0,
      val width: Int = 0,
      val initValue: Boolean = false,
      val boundry: TupleList) {

    val matchCriteria = List(
      List(true, true, false),
      List(false, true, true))

    val md = java.security.MessageDigest.getInstance("SHA-1")

    //Our bitMatrix is organized MATRIX[ROWS][COLS]  
    val bitMatrix: Matrix = new Matrix()

    //Locations of BOUNDRY bits, true == boundry, false no boundrary
    val boundryMatrix: Matrix = new Matrix()
    val productMatrix: ProductMatrix = new ProductMatrix()

    //Initialize ListBuffers to proper sizes and zero/init values
    for (i <- 0 until height) {
      bitMatrix += new ListBuffer()
      boundryMatrix += new ListBuffer()
      for (j <- 0 until width) {
        bitMatrix(i) += initValue
        boundryMatrix(i) += false
      }
      productMatrix += 0L
    }
    boundryToMatrix()

    def getUid(): String =
      new sun.misc.BASE64Encoder().encode(
        md.digest(
          (bitMatrix.toString + boundryMatrix.toString).getBytes()))

    def this(newBoard: Board) = {
      this(
        height = newBoard.height,
        width = newBoard.width,
        boundry = newBoard.boundry)

      this.copy(newBoard)
    }

    def copy(newBoard: Board) {
      if (newBoard.height == this.height &&
        newBoard.width == this.width) {
        for (
          row <- 0 until height;
          col <- 0 until width
        ) {
          this.setBit(row, col, newBoard.getBit(row, col))
        }

      }
    }

    def boundryToMatrix() = {
      //Map Tuples[X,Y] to Matrix
      if (boundry != null) {
        for (t <- boundry) {
          boundryMatrix(t._2)(t._1) = true
        }
      }
    }

    def isInBounds(row: Int, col: Int): Boolean = !isOutOfBounds(row, col)

    def isOutOfBounds(row: Int, col: Int): Boolean = {
      if (row < height && col < width) boundryMatrix(row)(col)
      else true
    }

    def getBit(row: Int, col: Int): Boolean = bitMatrix(row)(col)
    def getRow(row: Int): Row = {
      if (row < height) {
        bitMatrix(row)
      } else
        new Row()
    }

    def setBit(row: Int, col: Int, bitval: Boolean = true) = {
      if (col <= width && row <= height) {
        //1. Set the bit
        bitMatrix(row)(col) = bitval
        //2. Calculate the 2^X value
        productMatrix(row) = 0
        for (i <- 0 until width) {
          if (bitMatrix(row)(i) == true)
            productMatrix(row) += scala.math.pow(2, i)
        }
      }
    }

    
    
    def flipBit(row: Int, col: Int) = setBit(row, col, !getBit(row, col))

    //We have to loop over every ROW which contains a COL.
    def getCol(col: Int): Row = {
      val columnAsRow = new Row()
      if (col < width) {
        for (row <- bitMatrix)
          columnAsRow += row(col)
      }
      columnAsRow
    }

    def hasXPermutation(row: Int, col: Int, rule: List[Boolean]): Boolean = {
      var allXMatched: Boolean = true
      for (offset <- 0 until rule.length) {
        allXMatched &= this.getBit(row, col + offset) == rule(offset)
      }
      allXMatched
    }

    def hasYPermutation(row: Int, col: Int, rule: List[Boolean]): Boolean = {
      var allYMatched: Boolean = true
      for (offset <- 0 until rule.length) {
        allYMatched &= this.getBit(row + offset, col) == rule(offset)
      }
      allYMatched
    }

    def permutateBoard(): List[Board] = {
      var newBoards: List[Board] = List()

      for (
        row <- 0 until height;
        col <- 0 until width;
        rule <- matchCriteria
      ) {
        var isBoundry = this.isOutOfBounds(row, col)
        var isXOutOfBounds = this.isOutOfBounds(row, col + rule.length - 1)
        var isYOutOfBounds = this.isOutOfBounds(row + rule.length - 1, col)

        if (!isBoundry && (!isXOutOfBounds || !isYOutOfBounds)) {
          var bit = this.getBit(row, col)
          var colList = this.getCol(col)
          var rowList = this.getRow(row)

          //X-axis lookups and transformations
          if (!isXOutOfBounds) {
            if (hasXPermutation(row, col, rule)) {
              val newXBoard = new Board(this)
              for (offset <- 0 until rule.length) {
                newXBoard.flipBit(row, col + offset)
              }
              //println(s"XFLIP: ${this.getUid} => ${newXBoard.getUid} => [$row,$col]($rule)")
              newBoards ::= newXBoard
            }
          }

          //Y-axis lookups and transformations
          if (!isYOutOfBounds) {
            if (hasYPermutation(row, col, rule)) {
              val newYBoard = new Board(this)
              for (offset <- 0 until rule.length) {
                newYBoard.flipBit(row + offset, col)
              }
              //println(s"YFLIP: ${this.getUid} => ${newYBoard.getUid} => [$row,$col]($rule)")
              newBoards ::= newYBoard
            }
          }
        }

      }
      newBoards
    }

    val BOARD_CENTER_PEG_ONLY : String = "FDyn53d8rMG8cicFjjv2ggeyyHI="
    val BOARD_EARLY_RANDOM : String = "lOTiDVhb0ofU89YzDcI8DBiMIm8="
    
    def isWinner(): Boolean = getUid == BOARD_EARLY_RANDOM 

    override def toString() = {
      val matrixOutput = new StringBuilder()

      matrixOutput.append(s"Board[${getUid}]: [\n")

      for (row <- 0 until height) {
        val cRow = for (col <- bitMatrix(row))
          yield if (col == true) "1" else "0"

        for (col <- 0 until width) {
          if (isOutOfBounds(row, col)) {
            cRow(col) = "X"
          }
        }

        matrixOutput.append(cRow.mkString("  [", ",", "]"))
        matrixOutput.append(s"[${productMatrix(row)}]")

        matrixOutput.append("\n")
      }
      matrixOutput.append(" ]\n")
      matrixOutput.toString
    }

  }
}