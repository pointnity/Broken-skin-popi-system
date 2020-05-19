package interpreter

import syntax._

abstract class MachineState {
  def toProc: Proc
  def step: Option[MachineState]
  def someOf: Option[MachineState] = Some(this)
  def getNames: Map[Name, String]
  def getNext: NumName
}

sealed abstract class EvaluationException extends Exception
case class TypeError(message: String) extends EvaluationException {
  override def toString: String = s"TypeError: $message"
}
case class FreeVariableError(in: SyntaxElement) extends EvaluationException
case class ListBoundError(message: String, in: SyntaxElement)
  extends EvaluationException

object substituteProc extends Function3[Proc, Name, EvalExp, Proc] {

  /** Substitute the EvalExp 'to' for the Name 'from' within the Proc act, and
    *  obtain the resulting Proc.
    */
  def apply(p: Proc, from: Name, to: EvalExp): Proc = {
    val subP : Function[Proc, Proc] = q => substituteProc(q, from, to)
    val subE : Function[Exp, Exp] = e => substituteExp(e, from, to)
    p match {

      case Send(c, ts, ms, q) => Send(subE(c), ts, ms map subE, subP(q))

      case Receive(r, c, qs, as, q) => {
        val newQ = if ((as map (_._1)) contains from) q else subP(q)
        Receive(r, subE(c), qs, as, newQ)
      }

      case LetIn(name, ty, exp, q) => {
        val newQ = if (name == from) q else subP(q)
        LetIn(name, ty, subE(exp), newQ)
      }

      case IfThenElse(exp, tP, fP) =>
        IfThenElse(subE(exp), subP(tP), subP(fP))

      case Parallel(q, r) => Parallel(subP(q), subP(r))

      case New(name, ty, q) =>
        New(name, ty, if(name == from) q else subP(q))

      case End => End
    }
  }
}

object substituteProcFold extends Function2[Proc, List[(Name, EvalExp)], Proc] {

  // Repeatedly apply substituteProc for a list of names and expressions
  def apply(p: Proc, bindsEvalExps: List[(Name, EvalExp)]): Proc =
    (bindsEvalExps foldLeft p) { case (pCur, (bind, evalExp)) =>
      substituteProc(pCur, bind, evalExp)
    }
}

object substituteExp extends Function3[Exp, Name, EvalExp, Exp] {

  def apply(exp: Exp, from: Name, to: EvalExp): Exp = {
    val subE : Exp => Exp = e => substituteExp(e, from, to)
    exp match {
      case Variable    ( n ) if n == from => to.unEvalExp
      case Variable    ( n ) if n != from => exp
      case IntLiteral  ( x )              => exp
      case BoolLiteral ( x )              => exp
      case KharLiteral ( c )              => exp
      case ChanLiteral ( c )              => exp
      case Pair        ( l  , r         ) => Pair(subE(l), subE(r))
      case UnExp       ( ty , e         ) => UnExp(ty, subE(e))
      case BinExp      ( ty , lhs , rhs ) => BinExp(ty, subE(lhs), subE(rhs))
      case ListExp     ( es             ) => ListExp(es map subE)
    }
  }
