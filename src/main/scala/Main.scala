/**
  * Created by Piotr on 09.05.2018.
  */
object Main extends App{
  def initAvailablePositions() : Set[Position] = {
    Range(2,6).flatMap(x=> Range(0,8).map(y => Position(x,y,0))).toSet
  }
  def initPlayerOnePositions() : Set[Position] = {
    val pawns = Range(1,2).flatMap(x=> Range(0,8).map(y => Position(x,y,1))).toSet
    val rook1 = Position(0,0,4)
    val rook2 = Position(0,7,4)
    val horse1 = Position(0,1,2)
    val horse2 = Position(0,6,2)
    val bishop1 = Position(0,2,3)
    val bishop2 = Position(0,5,3)
    val queen = Position(0,3,5)
    val king = Position(0,4,6)
    pawns + rook1 + rook2 + horse1 + horse2 + bishop1 + bishop2 + queen + king
  }

  def initPlayerTwoPositions() : Set[Position] = {
    val pawns = Range(6,7).flatMap(x=> Range(0,8).map(y => Position(x,y,1))).toSet
    val rook1 = Position(7,0,4)
    val rook2 = Position(7,7,4)
    val horse1 = Position(7,1,2)
    val horse2 = Position(7,6,2)
    val bishop1 = Position(7,2,3)
    val bishop2 = Position(7,5,3)
    val queen = Position(7,3,5)
    val king = Position(7,4,6)
    pawns + rook1 + rook2 + horse1 + horse2 + bishop1 + bishop2 + queen + king
  }
  val chessBoardState1 = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
//  val chessBoardState = chessBoardState1.makeMove(Position(1,1,1), Position(5,1,1))
//  val chessBoardState2 = chessBoardState.makeMove(Position(1,4,1), Position(2,4,1))
//  val chessBoardState3 = chessBoardState2.makeMove(Position(0,4,6), Position(7,4,6))
//  chessBoardState3.display()
//  print(chessBoardState3.isGameOver)

  val p = Position(0,1,6)

  print(chessBoardState1.minimax(true,5).display())

}


