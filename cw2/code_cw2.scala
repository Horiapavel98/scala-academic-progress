  // A simple lexer inspired by work of Sulzmann & Lu
  //==================================================


  import scala.language.implicitConversions    
  import scala.language.reflectiveCalls

  // regular expressions including records
  abstract class Rexp 
  case object ZERO extends Rexp
  case object ONE extends Rexp
  //case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
  case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
  //case class RANGE(chars : Set[Char]) extends Rexp
  case class STAR(r: Rexp) extends Rexp 
  case class PLUS(r: Rexp) extends Rexp
  case class OPTIONAL(r: Rexp) extends Rexp
  case class NTIMES(r: Rexp, n: Int) extends Rexp
  case class RECD(x: String, r: Rexp) extends Rexp
  case class CFUN(f : Char => Boolean) extends Rexp

  /* --- Imported from CW1 ---  */
  def CHAR(c : Char) : Char => Boolean = {
    (ch) => c == ch
  }
  
  /* Changed the definition from CW1 here:
  * 
  * - now the function takes a string as input(for convenience)
  * - in cw1 took List[Char]
  */
  def RANGE(chars : String) : Char => Boolean = {
    (ch) => chars.contains(ch)
  }
  /*------------------------- */

  // values  
  abstract class Val
  case object Empty extends Val
  case class Chr(c: Char) extends Val
  case class Sequ(v1: Val, v2: Val) extends Val
  case class Left(v: Val) extends Val
  case class Right(v: Val) extends Val
  case class Stars(vs: List[Val]) extends Val
  case class Rec(x: String, v: Val) extends Val

  // some convenience for typing in regular expressions
  def charlist2rexp(s : List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CFUN(CHAR(c))
    case c::s => SEQ(CFUN(CHAR(c)), charlist2rexp(s))
  }
  implicit def string2rexp(s : String) : Rexp = 
    charlist2rexp(s.toList)

  implicit def RexpOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~ (s: Rexp) = SEQ(r, s)
  }

  implicit def stringOps(s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def % = STAR(s)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
    def $ (r: Rexp) = RECD(s, r)
  }

  def nullable(r: Rexp) : Boolean = r match {
    case ZERO => false
    case ONE => true
    case CFUN(_) => false
    //case CHAR(_) => false
    //case RANGE(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case PLUS(r1) => nullable(r1)
    case OPTIONAL(_) => true
    case NTIMES(r1, i) => if (i == 0) true else nullable(r1)
    case RECD(_, r1) => nullable(r1)
  }

  def der(c: Char, r: Rexp) : Rexp = r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CFUN(f) => if(f(c)) ONE else ZERO
    //case CHAR(d) => if (c == d) ONE else ZERO
    //case RANGE(cxs) => if (cxs.contains(c)) ONE else ZERO
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) => 
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r) => SEQ(der(c, r), STAR(r))
    case PLUS(r) => SEQ(der(c, r), STAR(r))
    case OPTIONAL(r1) => der(c, r1)
    case NTIMES(r1, i) => 
      if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
    case RECD(_, r1) => der(c, r1)
  }


  // extracts a string from value
  def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v) => flatten(v)
  }


  // extracts an environment from a value;
  // used for tokenise a string
  def env(v: Val) : List[(String, String)] = v match {
    case Empty => Nil
    case Chr(c) => Nil
    case Left(v) => env(v)
    case Right(v) => env(v)
    case Sequ(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v) => (x, flatten(v))::env(v)
  }

  // The Injection Part of the lexer


def mkeps(r: Rexp) : Val = r match {
    case ONE => Empty
    case ALT(r1, r2) => 
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
    case STAR(r) => Stars(Nil)
    case PLUS(r) => Sequ(mkeps(r), mkeps(STAR(r))) // r{+} = r ~ r.%
    case OPTIONAL(r) => mkeps(r) // r + 1 ? Check for case where r is nullable
    case NTIMES(r, n) => Stars(List.tabulate(n)(_ => mkeps(r)))
    case RECD(x, r) => Rec(x, mkeps(r))
}


  def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
    case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CFUN(_), Empty) => Chr(c)
    //case (CHAR(d), Empty) => Chr(c) 
    //case (RANGE(chs), Empty) => Chr(c)
    case (OPTIONAL(r), v) => inj(r, c, v)
    case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (NTIMES(r, n), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
  }

  // some "rectification" functions for simplification
  def F_ID(v: Val): Val = v
  def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
  def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
  def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case Right(v) => Right(f2(v))
    case Left(v) => Left(f1(v))
  }
  def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
  }
  def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
    (v:Val) => Sequ(f1(Empty), f2(v))
  def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
    (v:Val) => Sequ(f1(v), f2(Empty))
  def F_RECD(f: Val => Val) = (v:Val) => v match {
    case Rec(x, v) => Rec(x, f(v))
  }
  def F_ERROR(v: Val): Val = throw new Exception("error")

  def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (r2s, F_RIGHT(f2s))
        case (_, ZERO) => (r1s, F_LEFT(f1s))
        case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                  else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
      }
    }
    case SEQ(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (ZERO, F_ERROR)
        case (_, ZERO) => (ZERO, F_ERROR)
        case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
        case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
        case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
      }
    }
    case r => (r, F_ID)
  }

  // lexing functions including simplification
  def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else 
      { throw new Exception("lexing error") } 
    case c::cs => {
      val (r_simp, f_simp) = simp(der(c, r))
      inj(r, c, f_simp(lex_simp(r_simp, cs)))
    }
  }

  def lexing_simp(r: Rexp, s: String) = 
    env(lex_simp(r, s.toList))

  // The Lexing Rules for the Fun Language
  /*
  def PLUS(r: Rexp) = r ~ r.%

  def Range(s : List[Char]) : Rexp = s match {
    case Nil => ZERO
    case c::Nil => CHAR(c)
    case c::s => ALT(CHAR(c), Range(s))
  }
  def RANGE(s: String) = Range(s.toList)
  */

  // Question 1
  val SYM : Rexp = CFUN(RANGE("ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz_"))
  val DIGIT : Rexp = CFUN(RANGE("0123456789"))
  val DIGITS_NO_ZERO : Rexp = CFUN(RANGE("123456789"))
  val ID = SYM ~ (SYM | DIGIT).% 
  // val NUM = PLUS(DIGIT)
  val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "for" | "to" | "true" | "false" 
  val SEMI: Rexp = ";"
  val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "%" | "/" | "&&" | "||"
  val WHITESPACE = PLUS(" " | "\n" | "\t")
  val RPAREN: Rexp = "{" | "("
  val LPAREN: Rexp = "}" | ")"
  val STRING: Rexp = "\"" ~ (SYM | DIGIT | WHITESPACE).% ~ "\""

  val NUMBER = DIGIT | DIGITS_NO_ZERO ~ PLUS(DIGIT)


  val WHILE_REGS = (("k" $ KEYWORD) | 
                    ("i" $ ID) | 
                    ("o" $ OP) | 
                    ("n" $ NUMBER) | 
                    ("s" $ SEMI) | 
                    ("str" $ STRING) |
                    ("p" $ (LPAREN | RPAREN)) | 
                    ("w" $ WHITESPACE)).%

val numberWithLeadingZeros = "0000213"
val numberWithNoLeadingZeros = "213"

lexing_simp(WHILE_REGS, numberWithLeadingZeros) // List((n,0), (n,0), (n,0), (n,0), (n,213)) | 5 numbers, recognizes 0 as different numbers
lexing_simp(WHILE_REGS, numberWithNoLeadingZeros) // List((n,213)) | 1 number

// Question 2

val REXE : Rexp = NTIMES(ALT(CFUN(CHAR('a')), ONE), 3) // (a+1){3}
lex_simp(REXE, "aa".toList)

val REXE2 : Rexp = NTIMES(CFUN(CHAR('a')) , 3) // a{3}
lex_simp(REXE2, "aaa".toList)

val REXE3 : Rexp = CFUN(RANGE("abc")) 
lex_simp(REXE3, "c".toList)

val REXE4 : Rexp = NTIMES(ALT(ALT(CFUN(CHAR('a')), CFUN(CHAR('b'))), ONE) , 4) // ((a+b) + 1){4}
lex_simp(REXE4, "bba".toList)


// Testing read n;

val prog0 = """read n;"""
println(lexing_simp(WHILE_REGS, prog0))

// Question 3

// escapes strings and prints them out as "", "\n" and so on
def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: List[(String, String)]) =
  tks.map{ case (s1, s2) => (s1, esc(s2))}

def filterWhiteSpaces(ls : List[(String, String)]) : List[(String, String)] = ls.filter(_._1 != "w")

val fibonacci = """
write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
  temp := minus2;
  minus2 := minus1 + minus2;
  minus1 := temp;
  n := n - 1
};
write "Result";
write minus2
"""

println("lexing Fib")
println(escape(lexing_simp(WHILE_REGS, fibonacci)).mkString("\n"))
println(filterWhiteSpaces(escape(lexing_simp(WHILE_REGS, fibonacci))).mkString("\n")) // filtered white spaces


val threeNestedLoops = """
start := 1000;
x := start;
y := start;
z := start;
while 0 < x do {
 while 0 < y do {
  while 0 < z do {
    z := z - 1
  };
  z := start;
  y := y - 1
 };     
 y := start;
 x := x - 1
}
"""

println("lexing Loops")
println(escape(lexing_simp(WHILE_REGS, threeNestedLoops)).mkString("\n"))
println(filterWhiteSpaces(escape(lexing_simp(WHILE_REGS, threeNestedLoops))).mkString("\n")) // filtered white spaces

val factorsForNumbers = """
write "Input n please";
read n;
write "The factors of n are";
f := 2;
while n != 1 do {
 while (n / f) * f == n do {
  write f;
  n := n / f
 };
 f := f + 1
};
"""

println("lexing Factors")
println(escape(lexing_simp(WHILE_REGS, factorsForNumbers)).mkString("\n"))
println(filterWhiteSpaces(escape(lexing_simp(WHILE_REGS, factorsForNumbers))).mkString("\n")) // filtered white spaces

