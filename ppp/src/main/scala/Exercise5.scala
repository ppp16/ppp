import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import scala.collection.mutable.ListBuffer
import scalaz.syntax.ToIdOps

object Exercise5 {
  val cols = 10
  val rows = cols
  var Mines = 0

  def main(args: Array[String]) {

    val difficultyPercent = 0.5 // defines the chance of a mine occurance on a tile
    // and a chance of 1-p of revealed tiles at gamestart

    val randomList = Array.fill(cols * rows)(If(Flip(difficultyPercent), 1, 2))
    val board = Array.tabulate(rows, cols)((i: Int, j: Int) =>
      If(Flip(difficultyPercent),
        If(Flip(difficultyPercent), 0, 1), // field not revealed
        If(Flip(1 - difficultyPercent), 2, 3)).generateValue()) // if field is revealed
    //    If(Flip(difficultyPercent),1,0).generateValue())
    println("Initial Board")
    var playboard = generatePlayBoard(board)

    printBoard(playboard)
    var mines = 0;

    Mines = totalMines(board)
    println("Mines total: " + Mines)
    print("Mines found: ")
    println(totalVisibleMines(board))

    var test = calculateChance(playboard)
    printBoard(test,playboard)




  }

  def printBoard(board: Array[Array[String]]) {
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        print(board(i)(j) + "\t")
      }
      println()
    }
  }

  def printBoard(board: Array[Array[Double]]) {
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        print(roundAt(2)(board(i)(j)) + "\t")
      }
      println()
    }
  }
  
  def printBoard(board: Array[Array[Double]], board2: Array[Array[String]]) {
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        if(board2(i)(j)=="#") print(roundAt(2)(board(i)(j)) + "\t")
        else 
        print(board2(i)(j) + "\t")
      }
      println()
    }
  }

  def generatePlayBoard(board: Array[Array[Int]]) = {
    val newBoard = Array.ofDim[String](rows, cols)
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        if (board(i)(j) == 0 || board(i)(j) == 1)
          newBoard(i)(j) = "#"
        else if (board(i)(j) == 2)
          newBoard(i)(j) = "M"
        else
          newBoard(i)(j) = (fieldsAround(board, i, j).count(_ == 2) + fieldsAround(board, i, j).count(_ == 0)).toString()
      }
    }
    newBoard
  }
  def minesAround(board: Array[Array[Int]], x: Int, y: Int) = {
    var mines: Int = 0
    for (i <- -1 to 1)
      for (j <- -1 to 1) {
        if ((x + i >= 0 && x + i < cols - 1) && // in col bounds
          (y + j >= 0 && y + j < rows - 1) && // in row bounds
          (i != j || i != 0) && // not the centerfield
          (board(x + i)(y + j) == 2 || board(x + i)(y + j) == 0)) // if bomb
          mines += 1;
      }
    mines
  }

  def totalMines(board: Array[Array[Int]]) = {
    var mines: Int = 0
    for (i <- 0 to rows - 1)
      for (j <- 0 to cols - 1)
        if (board(i)(j) == 0 || board(i)(j) == 2)
          mines += 1
    mines
  }

  def totalVisibleMines(board: Array[Array[Int]]) = {
    var mines: Int = 0
    for (i <- 0 to rows - 1)
      for (j <- 0 to cols - 1)
        if (board(i)(j) == 2)
          mines += 1
    mines
  }

  def totalSafeMines(board: Array[Array[Int]]) = {
    var mines: Int = 0
    for (i <- 0 to rows - 1)
      for (j <- 0 to cols - 1)
        if (board(i)(j) == 1)
          mines += 1
    mines
  }

  def calculateChance(board: Array[Array[String]]) = {
    var chance = 0.0
    var newBoard = Array.ofDim[Double](rows, cols)
    for (i <- 0 to rows - 1) // initializes a new board with 1 = mine
      // -1 = uncalculated , 0 = no mine
      for (j <- 0 to cols - 1)
        board(i)(j) match {
          case "M" => newBoard(i)(j) = 1
          case "#" => newBoard(i)(j) = -1
          case _ => newBoard(i)(j) = 0
        }

    var boardOfChances = newBoard.clone() // needed to not create a mess

    for (i <- 0 to rows - 1)
      for (j <- 0 to cols - 1)
        newBoard(i)(j) match {
          case 0 => { // all fields with a number on it
                              val around = fieldsAround(board, i, j)
                  val freeFields = around.count(_ == "#")
                  val minesAround = around.count(_ == "M")
                  val mines = board(i)(j).toInt
            for (i2 <- -1 to 1)
              for (j2 <- -1 to 1)
                if ((i + i2 >= 0 && i + i2 < cols ) && // in col bounds
                  (j + j2 >= 0 && j + j2 < rows ) && // in row bounds
                  (i2 != j2 || i2 != 0) && // not the centerfield
                  (board(i + i2)(j + j2) == "#")) { // only applying to unknown fields
                  if (mines == minesAround)
                    boardOfChances(i + i2)(j + j2) = 99 // value for definite NO mine here
                  else
                    boardOfChances(i + i2)(j + j2) = Math.max(boardOfChances(i + i2)(j + j2), (mines - minesAround).toDouble / freeFields)

                }
          }
          case _ =>
        }

    // 99 -> 0
    for (i <- 0 to rows - 1)
      for (j <- 0 to cols - 1)
        boardOfChances(i)(j) match {
          case 99 => boardOfChances(i)(j) = 0
          case _ =>
        }
    var changesWereMade = true
    var c = 0
    while (changesWereMade) {
      c+=1
      if(c==100) changesWereMade = false
      for (i <- 0 to rows - 1)
        for (j <- 0 to cols - 1)
          if(board(i)(j) != "#" && board(i)(j) != "M"){
                  val around = fieldsAround(board, i, j)
                  var freeFields = around.count(_ == "#")
                  var minesAround = around.count(_ == "M")
                  val mines = board(i)(j).toInt
              for (i2 <- -1 to 1)
                for (j2 <- -1 to 1)
                  if ((i + i2 >= 0 && i + i2 < cols ) && // in col bounds
                    (j + j2 >= 0 && j + j2 < rows ) && // in row bounds
                    (i2 != j2 || i2 != 0) && // not the centerfield
                    (board(i + i2)(j + j2) == "#")) { // only applying to unknown fields
                    if (boardOfChances(i + i2)(j + j2) == 0)// no mine
                       freeFields -= 1                      
                    else if (boardOfChances(i + i2)(j + j2) == 1)// surely mine
                       minesAround += 1      
                  }
                  // recalculates the chances
                  freeFields = Math.max(freeFields,0)
            for (i2 <- -1 to 1)
              for (j2 <- -1 to 1)
                if ((i + i2 >= 0 && i + i2 < cols ) && // in col bounds
                  (j + j2 >= 0 && j + j2 < rows ) && // in row bounds
                  (i2 != j2 || i2 != 0) && // not the centerfield
                  (board(i + i2)(j + j2) == "#") &&// only applying to unknown fields
                  (boardOfChances(i+i2)(j+j2) != 0)&&// only applying to 
                  (boardOfChances(i+i2)(j+j2) != 1)) { // fields with unknown chance
                  if (mines == minesAround)
                    boardOfChances(i + i2)(j + j2) = 0 // value for definite NO mine here
                  else
                    boardOfChances(i + i2)(j + j2) = Math.max(boardOfChances(i + i2)(j + j2), (mines - minesAround).toDouble / freeFields)

                }
          }
    }

    var minesReacheable:Double = 0
    var fieldsToFill = 0
    for (i <- 0 to rows - 1)
        for (j <- 0 to cols - 1){
         if(boardOfChances(i)(j)== -1)
            fieldsToFill+=1
         else 
            minesReacheable += boardOfChances(i)(j)
        }
            
    val minesToFind = Mines - minesReacheable
    // for the unknown -1 fields
        for (i <- 0 to rows - 1)
        for (j <- 0 to cols - 1)
          if(boardOfChances(i)(j)== -1){
            boardOfChances(i)(j) = minesToFind / fieldsToFill.toDouble 
          }
    boardOfChances
  }

  def fieldsAround(board: Array[Array[String]], x: Int, y: Int) = {
    var list = new ListBuffer[String]()
    for (i <- -1 to 1)
      for (j <- -1 to 1) {
        if ((x + i >= 0 && x + i < cols ) && // in col bounds
          (y + j >= 0 && y + j < rows ) && // in row bounds
          (i != j || i != 0)) // not the centerfield
          list += board(x + i)(y + j)
      }
    list.toList
  }

  def fieldsAround(board: Array[Array[Int]], x: Int, y: Int) = {
    var list = new ListBuffer[Int]()
    for (i <- -1 to 1)
      for (j <- -1 to 1) {
        if ((x + i >= 0 && x + i < cols ) && // in col bounds
          (y + j >= 0 && y + j < rows ) && // in row bounds
          (i != j || i != 0)) // not the centerfield
          list += board(x + i)(y + j)
      }
    list.toList
  }

  def count(board: Array[Array[String]], st: String) = {

    var count: Int = 0

    for (i <- 0 to rows - 1)
      count += board(i).count { _ == st }

    count

  }

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow (10, p)
    (math round n * s) / s
  }

}