/**
  * Created by Piotr on 25.05.2018.
  */
class Horse extends Figure{

  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    val horseMove1 = Position(position.row + 2, position.col + 1, position.figure)
    val horseMove2 = Position(position.row + 2, position.col - 1, position.figure)
    val horseMove3 = Position(position.row - 2, position.col + 1, position.figure)
    val horseMove4 = Position(position.row - 2, position.col - 1, position.figure)
    val horseMove5 = Position(position.row + 1, position.col + 2, position.figure)
    val horseMove6 = Position(position.row + 1, position.col - 2, position.figure)
    val horseMove7 = Position(position.row - 1, position.col + 2, position.figure)
    val horseMove8 = Position(position.row - 1, position.col - 2, position.figure)
    val pseudoPossibleMoves = Set(horseMove1, horseMove2, horseMove3, horseMove4, horseMove5, horseMove6, horseMove6, horseMove7, horseMove8)
    val possibleMoves = pseudoPossibleMoves.toStream.filter(p => chessBoardState.availablePositions(p) || chessBoardState.playerTwoPositions(p)).toSet
    possibleMoves
  }
}
