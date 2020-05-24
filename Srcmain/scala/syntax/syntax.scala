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
    case Receive    ( _ , e , _  , _ , p ) =>
      e.chanLiterals union p.chanLiterals
    case LetIn      ( _ , _ , e  , p     ) =>
      e.chanLiterals union p.chanLiterals
    case IfThenElse ( e , p , q          ) =>
      e.chanLiterals union p.chanLiterals union q.chanLiterals
    case Parallel   ( p , q              ) =>
      p.chanLiterals union q.chanLiterals
    case New        ( _ , _ , p          ) =>
      p.chanLiterals
    case End                               =>
      Set.empty
  }

  def free: Set[Name] = this match {
    case IfThenElse ( e , p  , q           ) => e.free union p.free union q.free
    case Parallel   ( p , q                ) => p.free union q.free
    case New        ( n , t  , p           ) => (p.free - n) union t.free
    case End                                 => Set.empty
    case LetIn      ( n , t  , e  , p      ) =>
      e.free union (p.free - n) union t.free
    case Send       ( c , ts , ms , p      ) => c.free union p.free union
      ( ( ms map ( _.free ) ) ++ ( ts map ( _.free ) ) )
        .fold ( Set.empty ) ( _ union _ )
    case Receive    ( _ , c  , qs , as , p ) => {
      val freeInP: Set [ Name ] = p.free -- ( as map ( _._1 ) )
      val freeInAs: Set [ Name ] = ( ( ( as map ( _._2 ) ).map ( _.free ) )
        .fold ( Set.empty ) ( _ union _ ) ) -- qs
      c.free union freeInP union freeInAs
    }
  }

  /** Decompose top-level parallel compositions into a list of processes.
   */
  def listify: List[Proc] = this match {
    case Parallel ( p , q ) => p.listify ++ q.listify
    case End                => Nil
    case _                  => List(this)
  }
}

case class Send
  ( chan  : Exp
  , types : List [ SType ]
  , msgs  : List [ Exp   ]
  , p     : Proc
  ) extends Proc

case class Receive
  ( repl   : Boolean
  , chan   : Exp
  , tyArgs : List [ Name ]
  , args   : List [ ( Name , SType ) ]
  , p      : Proc
  ) extends Proc

case class LetIn
  ( name : Name
  , ty   : SType
  , exp  : Exp
  , p    : Proc
  ) extends Proc

case class IfThenElse
  ( cond   : Exp
  , trueP  : Proc
  , falseP : Proc
