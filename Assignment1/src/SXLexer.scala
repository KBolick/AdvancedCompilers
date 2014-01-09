/*
 SXLexer: A lexer for the programming language Scheme

 Author: Matthew Might
 Site:   http://matt.might.net/

 Modified by: Kevin Bolick
 */

class SXLexer extends NonblockingLexer[Char, Token] {
  
  import RegularLanguageImplicits._

  implicit def charsToString(l : List[Char]) : String = l mkString 

  // Abbreviations:
  private val che = (("#\\space") || ("#\\newline") || ("#\\tab"))
  private val ch = "#\\" ~ AnyChar
  private val id = (('A' thru 'Z') || ('a' thru 'z') || ('0' thru '9') || oneOf("-+/*_?%$&^=!@<>:"))+
  private val int = ("-"?) ~ ('0' thru '9')+
  private val ws = oneOf(" \r\t\n")+ // whitespace
  private val com = ";" ~ ((!oneOf("\r\n"))*) // single-line comment

  // States:
  protected val MAIN      = State()
  private val BANGCOMMENT = State(0)
  private val STRING      = State[List[Char]](List())

  // Rules:

  // State switching rules
  MAIN switchesOn ("#!") to { BANGCOMMENT(1) }
  MAIN switchesOn ("\"") to { STRING(List()) }

  // Regular tokens
  MAIN ("#!!#"){ }
  MAIN (",@")  { emit(PunctToken("(comma-unsplice)")) }
  MAIN (",")   { emit(PunctToken("(comma)")) }
  MAIN ("`")   { emit(PunctToken("(backtick)")) }
  MAIN ("'")   { emit(PunctToken("(tick)")) }
  MAIN ("#(")  { emit(PunctToken("#(")) }
  MAIN ("(")   { emit(PunctToken("left-parenthesis")) }
  MAIN (")")   { emit(PunctToken("right-parenthesis")) } 
  MAIN ("[")   { emit(PunctToken("left-bracket")) }
  MAIN ("]")   { emit(PunctToken("right-bracket")) } 
  MAIN ("{")   { emit(PunctToken("{")) }
  MAIN ("}")   { emit(PunctToken("}")) } 
  MAIN (".")   { emit(PunctToken(".")) }
  MAIN ("#t")  { emit(BooleanToken(true)) }
  MAIN ("#f")  { emit(BooleanToken(false)) }
  MAIN (END)   { terminate() }
  MAIN (ws)    { }
  MAIN (com)   { }
  MAIN (che)   over { string => emit(EscapedCharToken(string))}
  MAIN (ch)    over { chars => emit(CharToken(chars(2))) }
  MAIN (int)   over { chars => emit(IntToken(Integer.parseInt(chars))) }
  MAIN (id)    over { chars => emit(SymbolToken(chars)) }

  // Strings
  STRING ("\"")    = { (string,_)     => { emit(StringToken(string.reverse.mkString)) ; MAIN } }
  STRING ("\\\"")  = { (string,chars) => STRING('"' :: string) }
  STRING ("\\n")   = { (string,chars) => STRING('\n' :: string) }
  STRING ("\\\\")  = { (string,chars) => STRING('\\' :: string) }
  STRING (AnyChar) = { (string,chars) => STRING(chars.reverse ++ string) }

  // #! ... !# comments
  BANGCOMMENT ("#!")    = { (n,chars) => BANGCOMMENT(n+1) }
  BANGCOMMENT (AnyChar)   { }
  BANGCOMMENT ("!#")    = { case (1,chars) => MAIN 
                            case (n,chars) => BANGCOMMENT(n-1) }

}


/**
 Punctuation tokens.
 */
case class PunctToken(s : String) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case PunctToken(thatS) => s compare thatS
  }

  val tag = s

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(" + s + ")"
}

/**
 Symbol tokens.
 */
case class SymbolToken(s : String) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case SymbolToken(thatS) => s compare thatS
  }

  val tag = "Symbol"

  private def makeChar(s : String) : String = {
    var result = ""
    for(c <- s) {result = result + "(char " + c + ")"}
    result
  }

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(symbol (" + makeChar(s) + "))"
}

/**
 String literal tokens.
 */
case class StringToken(s : String) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case StringToken(thatS) => s compare thatS
  }

  val tag = "String"

  private def makeChar(s : String) : String = {
    var result = ""
    for(c <- s) {
      var temp = c match {
	case ' ' => "(char space) "
	case '\n' => "(char newline) "
	case '\t' => "(char tab) "
	case '(' => "(char left-parenthesis) "
	case ')' => "(char right-parenthesis) "
	case '[' => "(char left-bracket) "
	case ']' => "(char right-bracket) "
	case '`' => "(char backtick) "
	case '\'' => "(char tick) "
	case '"' => "(char doublequote) "
	case ',' => "(char comma) "
	case _ => "(char " + c + ") "
      }
      result = result + temp
    }
    if (result.length == 0)
      return ""
    return result.substring(0, result.length - 1)
  }
  override lazy val hashCode = s.hashCode
  override lazy val toString = "(text (" + makeChar(s) + "))"
}

/**
 Integer tokens.
 */
case class IntToken(n : Int) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case IntToken(thatN) => this.n compare thatN
  }

  val tag = "Int"

  override lazy val hashCode = n.hashCode
  override lazy val toString = "(integer " + n.toString + ")"
}

/**
 Boolean literal tokens.
 */
case class BooleanToken(b : Boolean) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case BooleanToken(thatB) => this.b compare thatB
  }

  val tag = "Boolean"

  override lazy val hashCode = b.hashCode
  override lazy val toString = if (b) "(boolean true)" else "(boolean false)"
}

/**
 Character tokens.
 */
case class CharToken(c : Char) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case CharToken(thatC) => this.c compare thatC
  }

  val tag = "Char"

  var result = c match {
    case ' ' => "space"
    case '\n' => "newline"
    case '\t' => "tab"
    case '(' => "left-parenthesis"
    case ')' => "right-parenthesis"
    case '[' => "left-bracket"
    case ']' => "right-bracket"
    case '`' => "backtick"
    case '\'' => "tick"
    case '"' => "doublequote"
    case ',' => "comma"
    case _ => c
  }
  override lazy val hashCode = c.hashCode
  override lazy val toString = "(char " + result + ")"
}

/**
 #\newline, #\tab, and #\space
 */
case class EscapedCharToken(s : String) extends Token {
  def isParsingMarker = false

  protected def localCompare(that : Token) = that match {
    case EscapedCharToken(thatS) => this.s compare thatS
  }

  val tag = "Char"

  var result = s match {
    case "#\\space" => "space"
    case "#\\newline" => "newline"
    case "#\\tab" => "tab"
  }
  override lazy val hashCode = s.hashCode
  override lazy val toString = "(char " + result + ")"
}

/**
 A simple program for testing the s-expression lexer.
 */
private object SXLexerTest {
  def main (args : Array[String]) {
    val in = args mkString " "
    val instream = LiveStream(in)
    val lexer = new SXLexer
    lexer.lex(instream)
    println(lexer.output) 
  }
}

/**
 This will take in a filename and produce a sequence of tokens
 */
private object Run {
  def main (args : Array[String]) {
    val source = scala.io.Source.stdin
    val instream = LiveStream(source.getLines mkString "\n")
    val lexer = new SXLexer
    lexer.lex(instream)
    println(lexer.output)
    source.close()
  }
}

