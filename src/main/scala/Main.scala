import figure._
import position.Position
import state.ChessBoardState

import scala.util.Try

/**
  * Created by Piotr on 09.05.2018.
  */
object Main extends App{
  def initAvailablePositions() : Set[Position] = {
    Range(2,6).flatMap(x=> Range(0,8).map(y => Position(x,y,new EmptyField()))).toSet
  }
  def initPlayerOnePositions() : Set[Position] = {
    val pawns = Range(1,2).flatMap(x=> Range(0,8).map(y => Position(x,y,new Pawn()))).toSet
    val rook1 = Position(0,0,new Rook())
    val rook2 = Position(0,7,new Rook())
    val horse1 = Position(0,1,new Horse())
    val horse2 = Position(0,6,new Horse())
    val bishop1 = Position(0,2,new Bishop())
    val bishop2 = Position(0,5,new Bishop())
    val queen = Position(0,3,new Queen())
    val king = Position(0,4,new King())
    pawns + rook1 + rook2 + horse1 + horse2 + bishop1 + bishop2 + queen + king
  }
  def initPlayerTwoPositions() : Set[Position] = {
    val pawns = Range(6,7).flatMap(x=> Range(0,8).map(y => Position(x,y,new Pawn()))).toSet
    val rook1 = Position(7,0,new Rook())
    val rook2 = Position(7,7,new Rook())
    val horse1 = Position(7,1,new Horse())
    val horse2 = Position(7,6,new Horse())
    val bishop1 = Position(7,2,new Bishop())
    val bishop2 = Position(7,5,new Bishop())
    val queen = Position(7,3,new Queen())
    val king = Position(7,4,new King())
    pawns + rook1 + rook2 + horse1 + horse2 + bishop1 + bishop2 + queen + king
  }




  def play(chessBoardState: ChessBoardState, playedComputer: Boolean) : Boolean = {
    if(chessBoardState.isGameOver){
      chessBoardState.display(playedComputer)
      println("Player wins: " + !playedComputer)
      return true
    }
    if(playedComputer){
      chessBoardState.display(playedComputer)
      val nextState: ChessBoardState = userMakeMove(chessBoardState)
      play(nextState, playedComputer = false)
    }
    else{
      val reversedChessBoardState = chessBoardState.reverseChessBoardState()
      reversedChessBoardState.display(!playedComputer)
      play(chessBoardState.nextStateAlphaBeta(isMaximizingPlayer = true, 4), playedComputer = true)
    }
  }

  private def userMakeMove(chessBoardState: ChessBoardState) : ChessBoardState = {
    println("Enter row of your figure to move")
    val row1 = readInt()
    println("Enter column of your figure to move")
    val col1 = readInt()
    println("Enter row where you want to move")
    val row2 = readInt()
    println("Enter column where you want to move")
    val col2 = readInt()
    val isValidPosition = chessBoardState.playerOnePositions.map(p => p.col==col1 && p.row==row1)
    if(isValidPosition contains true){
      val position = chessBoardState.playerOnePositions.filter(p => p.col==col1 && p.row==row1).head
      val p1 = Position(row1, col1, position.field)
      val p2 = Position(row2, col2, position.field)
      if(chessBoardState.isValidMove(p1,p2)){
        chessBoardState.makeMove(p1,p2)
      }
      else{
        println("INVALID MOVE!!!!!")
        userMakeMove(chessBoardState)
      }
    }
    else{
      println("YOU DON'T HAVE A FIGURE AT THIS FIELD!!!!!")
      userMakeMove(chessBoardState)
    }
  }

  def readInt() : Int = {
    val input = scala.io.StdIn.readLine()
    if(Try(input.toInt).isSuccess && input.toInt>=0 && input.toInt<=7){
      input.toInt
    }
    else {
      println("Incorrect number")
      readInt()
    }
  }

  val initialState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
  play(initialState, playedComputer = true)


}


