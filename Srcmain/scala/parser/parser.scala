package parser

/** Implements a parser for the nodes language using scala's parser combinators.
 *  Parsing is subdivided into separate lexing, postlexing and parsing phases.
 *  - Lexing converts a String of nodes code into a List [ PreToken ]. PreTokens
 *  are tokens where identifiers, ints and such carry the text that was lexed
 *  to produce them.
 *  - The postlexing step converts PreTokens into PostTokens, changing the
 *  representation of names from Strings to Integers, providing a map between
 *  string and integer names for printing purposes.
 *  - Parsing converts a List [ PostToken ] into a syntax.Proc.
 */

import syntax._
import typeclass.syntax._
import scala.io.Source
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.collection.immutable.PagedSeq

/** Responsible for converting a List [ PostToken ] into a syntax.Proc via the
 *  ... method.
 */

sealed abstract class LexerParserError ( row: Int , col: Int , msg: String )
case class LexerError  ( row: Int , col: Int , msg: String )
  extends LexerParserError ( row , col , msg )
case class ParserError ( row: Int , col: Int , msg: String )
  extends LexerParserError ( row , col , msg )

object lexAndParse {

  def apply [ T ] (
    production: Parser.Parser [ T ] ,
    input: Source
  ): Either [ LexerParserError , ( Map [ String , Name ] , NumName , T ) ] =
    for {
      lexed  <- Lexer  ( input                       ).right
      parsed <- Parser ( production , lexed._3       ).right
    } yield ( lexed._1 , lexed._2 , parsed )
}

object Parser extends Parsers {

  def apply [ T ] (
    production: Parser [ T ] ,
    input: List [ PostToken ]
  ): Either [ ParserError , T ] =
    production ( new TokenReader ( input ) ) match {
      case Success   ( prc , rest ) => Right ( prc                 )
