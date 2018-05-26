package figure

import position.Position
import state.ChessBoardState

/**
  * Created by Piotr on 25.05.2018.
  */
class Queen extends Figure{
  override val value: Int = 9
  override val displayValue: Int = 5

  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    val bishop = new Bishop()
    val rook = new Rook()
    rook.generatePositions(position, chessBoardState) ++ bishop.generatePositions(position, chessBoardState)
  }
}
