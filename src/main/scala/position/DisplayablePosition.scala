package position

/**
  * Created by Piotr on 26.05.2018.
  */
case class DisplayablePosition(position: Position, computerFigure:Boolean) extends Ordered[DisplayablePosition] {
  def getDisplayedValue: Int = {
    if(computerFigure)
      position.field.displayValue*(-1)
    else
      position.field.displayValue*10
  }

  override def compare(that: DisplayablePosition): Int = position.compare(that.position)
}
