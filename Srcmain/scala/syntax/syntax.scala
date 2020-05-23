package syntax
import scala.util.parsing.input.Positional

trait SyntaxElement {
  var info: Info = NoInfo
  def setInfo(i: Info): Unit = this.info = i
