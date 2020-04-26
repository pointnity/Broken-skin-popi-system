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
