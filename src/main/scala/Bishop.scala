/**
  * Created by Piotr on 25.05.2018.
  */
class Bishop extends Figure{
  override val value: Int = 3
  override val displayValue: Int = 3

  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    tryToMoveInDirection(position, 1, 1, chessBoardState) ++ tryToMoveInDirection(position, 1, -1, chessBoardState) ++ tryToMoveInDirection(position, -1, 1, chessBoardState) ++ tryToMoveInDirection(position, -1, -1, chessBoardState)
  }


}
