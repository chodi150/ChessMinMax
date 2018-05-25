/**
  * Created by Piotr on 25.05.2018.
  */
trait Figure extends Field{
  val value:Int = 0

  def generatePositions(position: Position, chessBoardState: ChessBoardState):Set[Position]

  def tryToMoveInDirection(position: Position, colMove : Int, rowMove : Int, chessBoardState: ChessBoardState) : Set[Position] = {
    val newPosition = Position(position.row + colMove, position.col + rowMove, position.field)
    if (!newPosition.positionOnBoard())
      return Set()

    if (chessBoardState.playerOnePositions contains newPosition)
      Set()
    else if (chessBoardState.playerTwoPositions contains newPosition)
      Set(newPosition)
    else
      Set(newPosition) ++ tryToMoveInDirection(newPosition, colMove, rowMove, chessBoardState)
  }


}
