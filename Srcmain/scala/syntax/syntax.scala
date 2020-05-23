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
      "; " + ((ms map (_ pstr names)) mkString ", ") + ". " + (p pstr names)
    case Receive    ( r     , c   , qs   , as , p ) =>
      (if (r) "server" else "receive") + " " + (c pstr names) + "; " +
      ((qs map (_ pstr names)) mkString ", ") + "; " + ((as map {
        case (n, t) => s"${n pstr names}: ${t pstr names}"
      }) mkString ", ") + ". " + (p pstr names)
    case LetIn      ( bind  , t , exp , p         ) =>
      s"let ${bind pstr names}: ${t pstr names} = " +
      s"${exp pstr names} . ${p pstr names}"
    case IfThenElse ( exp   , tP  , fP            ) =>
      s"if ${exp pstr names} then ${tP pstr names} else ${fP pstr names} endif"
    case Parallel   ( p     , q                   ) =>
      s"[ ${p pstr names} | ${q pstr names} ]"
    case New        ( name  , t , p               ) =>
      s"new ${name pstr names}: ${t pstr names} . ${p pstr names}"
    case End                                       => "end"
  }

  def chanLiterals: Set[Name] = this match {
    case Send       ( c , _ , ms , p     ) =>
      c.chanLiterals union p.chanLiterals union
        ( ms map ( _.chanLiterals ) ).fold ( Set.empty ) ( _ union _ )
