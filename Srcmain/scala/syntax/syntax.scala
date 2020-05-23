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
  def apply(names: Set[Name]): NumName =
      if ((names filter (_.isInstanceOf[NumName])).isEmpty) NumName ( 0 ) else
    NumName ( ( names.map ( _.asInstanceOf[NumName].id ).max ) + 1 )
}

abstract class Info {
  override def toString: String = this match {
    case NoInfo                                   => "<no data>"
    case SrcPosInfo ( ( ll , lc ) , ( rl , rc ) ) =>
      s"source position spanning line $ll, column $lc to line $rl, column $rc"
  }
}
case class SrcPosInfo(val lPos: (Int, Int), val rPos: (Int, Int)) extends Info
case object NoInfo extends Info

sealed abstract class Proc extends SyntaxElement {

  def pstr(names: Map[Name, String]): String = this match {
    case Send       ( c     , ts  , ms   , p      ) =>
      "send " + (c pstr names) + "; " +
      ((ts map (_ pstr names)) mkString ", ") +
