/**
  * Created by Piotr on 25.05.2018.
  */
class Pawn extends Figure{
  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    val emptyFields:Set[Position] = Set.empty ++ chessBoardState.availablePositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col, position.figure))).toSet ++ chessBoardState.playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col+1, position.figure))).toSet ++ chessBoardState.playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col-1, position.figure))).toSet
    emptyFields.map( x => Position(x.row, x.col, 1))
  }
}
