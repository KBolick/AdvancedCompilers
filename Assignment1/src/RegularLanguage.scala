abstract class RegularLanguage
{
  // Kleene star; zero or more repetitions:
  def * : RegularLanguage ;
  
  // Exactly n repetitions
  def ^ (n : Int) : RegularLanguage ;
  
  // Kleene plus: one or more repetitions
  def + : RegularLanguage ;
  
  // Option; zero or one instances:
  def ? : RegularLanguage ;
  
  // Concatenation:
  def ~ (suffix: RegularLanguage) : RegularLanguage;
  
  // Union/alternation:
  def || (that : RegularLanguage) : RegularLanguage;
  
  // For computing derivatives:
  def derive(c : Char) : RegularLanguage ;
  def deriveEND : RegularLanguage ;
  
  // Properties
  def acceptsEmptyString : Boolean ;
  def rejectsAll : Boolean ;
  def isEmptyString : Boolean ;
}

// Useful implicits for regular expressions:
object RegularLanguageImplicits {

  implicit def charToRegEx(c : Char) : RegularLanguage ;

  implicit def stringToRegEx(s : String) : RegularLanguage ;

  trait CharRangeable {
    def thru (end : Char) : RegularLanguage ;
  }

  implicit def charToCharRangeable(start : Char);
}

// Matches end of input:
case object END extends RegularLanguage

// Matches no strings at all:
case object EmptySet extends RegularLanguage

// Matches the length-zero string:
case object Epsilon extends RegularLanguage

// Matches any character:
case object AnyChar extends RegularLanguage
