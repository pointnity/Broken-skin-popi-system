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
      case NoSuccess ( msg , rest ) =>
        Left ( ParserError ( rest.pos.line , rest.pos.column , msg ) )
    }


  /** Take a SyntaxElement and two PostTokens, assign the left source position
   *  of the SyntaxElement as the position of the first token, and the right
   *  source position of the SyntaxElement as the position of the second token.
   *  Return the SyntaxElement with reassigned positions.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: PostToken ,
  r: PostToken ): T = {
    elem.setInfo ( SrcPosInfo ( ( l.pos.line , l.pos.column ) ,
      ( r.pos.line, r.pos.column ) ) )
    elem
  }

  /** As above, but using positions taken from SyntaxElements as opposed to
   *  PostTokens.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: SyntaxElement ,
  r: SyntaxElement ): T = ( l.info , r.info ) match {
    case ( SrcPosInfo ( ll , lr ) , SrcPosInfo ( rl , rr ) ) =>
      elem.setInfo ( SrcPosInfo ( ll , rr ) ) ; elem
    case _ => elem.setInfo ( NoInfo ) ; elem
  }

  /** As above, where the first position is from a PostToken and the second from
   *  a SyntaxElement.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: PostToken ,
  r: SyntaxElement ): T = r.info match {
    case SrcPosInfo ( rl , rr ) =>
      elem.setInfo ( SrcPosInfo ( ( l.pos.line , l.pos.column ) , rr ) ) ; elem
    case _ => elem.setInfo ( NoInfo ) ; elem
  }

  override type Elem = PostToken

  def name: Parser [ Name ] = accept ( "POSTIDENT" , {
    case p @ POSTIDENT ( n ) => putPos ( n , p , p )
  } )

  def tcElem: Parser [ TypeClassElement ] = tcProc | tcClass | tcInst

  def tcClass: Parser [ TypeClassElement ] =
