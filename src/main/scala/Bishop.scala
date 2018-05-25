/**
  * Created by Piotr on 25.05.2018.
  */
class Bishop extends Figure{
  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    tryToMoveInDirection(position, 1, 1, chessBoardState) ++ tryToMoveInDirection(position, 1, -1, chessBoardState) ++ tryToMoveInDirection(position, -1, 1, chessBoardState) ++ tryToMoveInDirection(position, -1, -1, chessBoardState)
  }
}
