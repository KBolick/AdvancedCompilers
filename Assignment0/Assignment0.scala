/**
  * Kevin Bolick
  * Assignment 0
 */

import java.io._
import SExp._
import Exp._

object Assignment0 { 
  def main(args: Array[String]) {
    
    //get the input
    val source = scala.io.Source.stdin
    val input = Exp.from(SExp.from(source.getLines mkString "\n"))
    source.close();
    
    // Void value
    val VOID = "(λ (void) void)";
    
    // ERROR'D UCOMBINATRIX (Whoo'PSH!)
    val ERROR = "(λ (_) ((λ (f) (f f)) (λ (f) (f f))))";
    
    // Booleans
    val TRUE = "(λ (t) (λ (f) (t " + VOID + ")))";
    val FALSE = "(λ (t) (λ (f) (f " + VOID + ")))";
    
    // Church numericals
    def churchNumeral(n : Int): String = {
      def applyN(f : String, n : Int, z : String): String = n match {
      	case 0 => z;
	case _ => "(" + f + " " + applyN(f, (n - 1), z) + ")";
      }
      n match {
        case 0 => "(λ (f) (λ (z) z))"
      	case _ => "(λ (f) (λ (z) " + applyN("f", n, "z") + "))"
      }
    }
    
    val ZEROhuh = "(λ (n) ((n (λ (_) " + FALSE + ")) " + TRUE + "))";
    val SUM = "(λ (n) (λ (m) (λ (f) (λ (z) ((m f) ((n f) z))))))";
    val MUL = "(λ (n) (λ (m) (λ (f) (λ (z) ((m (n f)) z)))))";
    val PRED = "(λ (n) (λ (f) (λ (z) (((n (λ (g) (λ (h) (h (g f))))) (λ (u) z)) (λ (u) u)))))";
    val SUB = "(λ (n) (λ (m) ((m " + PRED + ") n)))";
    
    // Lists
    val CONS = "(λ (car) (λ (cdr) (λ (on-cons) (λ (on-nil) ((on-cons car) cdr)))))"
    val NIL = "(λ (on-cons) (λ (on-nil) (on-nil " + VOID + ")))"
    val CAR = "(λ (list) ((list (λ (car) (λ (cdr) car))) " + ERROR + "))"
    val CDR = "(λ (list) ((list (λ (car) (λ (cdr) cdr))) " + ERROR + "))"
    val PAIRhuh = "(λ (list) ((list (λ (_) (λ (_) " + TRUE + "))) (λ (_) " + FALSE + ")))"
    val NULLhuh = "(λ (list) ((list (λ (_) (λ (_) " + FALSE + "))) (λ (_) " + TRUE + ")))"
    
    // Recursion
    val Y = "((λ (y) (λ (F) (F (λ (x) (((y y) F) x))))) (λ (y) (λ (F) (F (λ (x) (((y y) F) x))))))"

    // compilation
    def compile(expIn : Exp): String = expIn match {

      // symbols stay the same
      case RefExp(id) => id;

      // boolean and conditionals
      case BoolExp(bool) => if(bool) {TRUE} else {FALSE};
      case IfExp(cond, ifTrue, ifFalse) => compile(AppExp(cond, List[Exp](LambdaExp(List[String](), ifTrue),LambdaExp(List[String](), ifFalse))))
      case AndExp(cond1, cond2) => compile(IfExp(cond1, cond2, Exp.from(SExp.from("#f"))))
      case OrExp(cond1, cond2) => compile(IfExp(cond1, Exp.from(SExp.from("#t")), cond2))

      // numerals
      case IntExp(value) => churchNumeral(value);
      case ZeroPExp(test) => "(" + ZEROhuh + " " + compile(test) + ")"
      case SubExp(exp1, exp2) => "((" + SUB + " " + compile(exp1) + ") " + compile(exp2) + ")"
      case PlusExp(exp1, exp2) => "((" + SUM + " " + compile(exp1) + ") " + compile(exp2) + ")"
      case TimesExp(exp1, exp2) => "((" + MUL + " " + compile(exp1) + ") " + compile(exp2) + ")"
      case EqExp(exp1, exp2) => compile(AndExp(ZeroPExp(SubExp(exp1, exp2)), ZeroPExp(SubExp(exp2, exp1))))

      // lists
      case NullExp() => NIL
      case ConsExp(car, cdr) => "((" + CONS + " " + compile(car) + ") " + compile(cdr) + ")"
      case CarExp(arg) => "(" + CAR + " " + compile(arg) + ")"
      case CdrExp(arg) => "(" + CDR + " " + compile(arg) + ")"
      case PairPExp(arg) => "(" + PAIRhuh + " " + compile(arg) + ")"
      case NullPExp(arg) => "(" + NULLhuh + " " + compile(arg) + ")"

      // lambdas
      case LambdaExp(params, body) =>
	if (params.size == 0) { "(λ (_) " + compile(body) + ")" }
        else if (params.size == 1) { "(λ (" + params.head + ") " + compile(body) + ")" }
        else { "(λ (" + params.head + ") " + compile(LambdaExp(params.tail, body)) + ")" }

      // binding forms
      case LetExp(vars, exps, body) => compile(AppExp(LambdaExp(vars, body), exps))
      case LetRecExp(fun, lambda, body) => compile(LetExp(List[String](fun),List[Exp](AppExp(Exp.from(SExp.from(Y)), List[Exp](LambdaExp(List[String](fun),lambda)))), body))

      // application
      case AppExp(fun, args) => 
	if (args.size == 0) { compile(AppExp(fun, List[Exp](Exp.from(SExp.from(VOID))))) }
        else if (args.size == 1) { "(" + compile(fun) + " " + compile(args.head) + ")" }
        else { compile(AppExp(AppExp(fun, List[Exp](args.head)), args.tail)) }
        
      case _ => "Unknown expression: " + expIn.toString();
    }
    
    val output = compile(input).toString()
    print(output)
  }
}
