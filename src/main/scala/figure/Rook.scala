package figure

import position.Position
import state.ChessBoardState

/**
  * Created by Piotr on 25.05.2018.
  */
class Rook extends Figure{

  override val value: Int = 5
  override val displayValue: Int = 4
  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    tryToMoveInDirection(position, 0, 1,chessBoardState) ++ tryToMoveInDirection(position, 1, 0,chessBoardState) ++ tryToMoveInDirection(position, 0, -1,chessBoardState) ++ tryToMoveInDirection(position, -1, 0,chessBoardState)
  }
}
