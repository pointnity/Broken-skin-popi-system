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
