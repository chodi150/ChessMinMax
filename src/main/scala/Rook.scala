/**
  * Created by Piotr on 25.05.2018.
  */
class Rook extends Figure{
  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    tryToMoveInDirection(position, 0, 1,chessBoardState) ++ tryToMoveInDirection(position, 1, 0,chessBoardState) ++ tryToMoveInDirection(position, 0, -1,chessBoardState) ++ tryToMoveInDirection(position, -1, 0,chessBoardState)
  }
}
