package main

import syntax._
import typeclass.desugar._
import parser._
import typecheck._
import interpreter._
import interpreter.turner.runWithTurnerMachine
import interpreter.concurrent._
import java.io.File
import java.io.IOException
import scala.io.Source

object Main extends App {

  if (args.length < 1) {
    println("Bad command, please supply a filename.")
    sys.exit(1)
  }

  val file: java.io.File = new java.io.File ( args ( 0 ) )

  if (!file.exists) {
    println(s"File '${file.getAbsolutePath}' does not exist.")
    sys.exit(1)
  }

  if (file.isDirectory) {
    println(s"File '${file.getAbsolutePath}' is a directory.")
    sys.exit(1)
  }

  try {

    lexAndParse ( Parser.tcElem , Source fromFile args(0) ) match {
      case Right ( ( names , nextName , program ) ) =>

        // 'flip' the name map from the lexer such that we can easily use it to
        // print terms.
        val namesF: Map [ Name , String ] = names.map ( _.swap )

        val checker: Typecheck = new Typecheck ( nextName )

        val cp: Option [ ( Proc , Map [ Name, ClassInfo ] ) ] =
          contextualProc ( program )

        val ( proc: Proc , tcInfo: Map [ Name, ClassInfo ] ) = cp.get

        // Generate typing constraints
        val ( _ , constr: ConstraintSet ) =
          checker.constraintsProc ( proc , Map.empty )

        // Try to solve constraints
        checker.unify ( constr , ConstraintSet.empty , tcInfo ) match {

          // If constraints are solved, run the program
          case Right ( _  ) =>
            runWithTurnerMachine(proc, namesF, nextName)

          case Left  ( cs ) => {
            println ( s"${cs.size} type errors found:" )
            cs.foreach ( {
              case TypeConstraint ( t1 , t2 , orig ) => {
                println (
                  s"\n  Cannot unify ${t1 pstr namesF} " +
                  s"with ${t2 pstr namesF}." )
                orig.foreach ( { o =>
                  println ( s"    At ${o.info}:" )
