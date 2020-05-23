package syntax
import scala.util.parsing.input.Positional

trait SyntaxElement {
  var info: Info = NoInfo
  def setInfo(i: Info): Unit = this.info = i
  def pstr(names: Map[Name, String]): String
  def free: Set[Name]
}

abstract class Name extends SyntaxElement

case class NumName(val id: Int) extends Name {
  def next: NumName = new NumName(this.id + 1)
  def pstr(names: Map[Name, String]): String =
    names getOrElse (this, this.toString)
  def free: Set[Name] = Set(this)
  override def toString: String = s"<${this.id}>" // for debugging
}

abstract class PreDefName extends Name {
  def pstr(names: Map[Name, String]): String = this.toString
  def free: Set[Name] = Set(this)
}

case object StdOutName extends PreDefName {
  override def toString: String = "stdout" 
}

case object StdInName  extends PreDefName {
  override def toString: String = "stdin"
}

case object StdErrName extends PreDefName {
  override def toString: String = "stderr"
}

object findNextName extends Function1[Set[Name], NumName] {
