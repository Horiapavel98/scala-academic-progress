  // A simple lexer inspired by work of Sulzmann & Lu
  //==================================================

  // Code imported from coursework 02

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
  val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "%" | "/" | "&&" | "||" | "<=" | ">="
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

/* Coursework 03 part STARTED */

// Generate tokens for the WHILE language
// and serialise them into a .tks file

import java.io._

abstract class Token extends Serializable 
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token

// transforms pairs into tokens
val token : PartialFunction[(String, String), Token] = {
  case ("s", _) => T_SEMI
  case ("p", "(") => T_LPAREN
  case ("p", ")") => T_RPAREN
  case ("p", "{") => T_LPAREN
  case ("p", "}") => T_RPAREN
  case ("i", s) => T_ID(s)
  case ("o", s) => T_OP(s)
  case ("n", s) => T_NUM(s.toInt)
  case ("k", s) => T_KWD(s)
  case ("str", s) => T_STR(s)
}

// filters out all un-interesting tokens
def tokenise(s: String) : List[Token] = 
  lexing_simp(WHILE_REGS, s).collect(token)


def serialise[T](fname: String, data: T) = {
  import scala.util.Using
  Using(new ObjectOutputStream(new FileOutputStream(fname))) {
    out => out.writeObject(data)
  }
}

def main(args: Array[String]) : Unit = {
  val fname = args(0)
  val tname = fname.stripSuffix(".while") ++ ".tks"
  val file = io.Source.fromFile(fname).mkString
  serialise(tname, tokenise(file))
}

// A parser and interpreter for the While language
// 

import scala.language.implicitConversions
import scala.language.reflectiveCalls

// more convenience for the semantic actions later on
case class ~[+A, +B](_1: A, _2: B)


type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I) : Set[T] =
    for ((head, tail) <- parse(ts); if tail.isEmpty) yield head
}

class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

class AltParser[I : IsSeq, T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I : IsSeq, T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

// Question 1 and 2 CW03

// Parses a token at the head of a list of tokens if it matches t: Token
case class TokenParser(t : Token) extends Parser[List[Token], Token] {
  def parse(tks : List[Token]) = tks match {
    case head::rest => if (head == t) Set((head, rest)) else Set()
    case _ => Set()
  }
}

// val a = T_KWD("write")
// a.parse(tokenise("write n"))

// Parses and extracts the string from the string token at the head of the token list if there is any
case object AnyString extends Parser[List[Token], String] {
  def parse(tks : List[Token]) = tks match {
    case T_STR(str)::rest => Set((str, rest))
    case _ => Set()
  }
}

// Test AnyString parser
// val a = AnyString
// a.parse(List(T_STR("abababab"), T_STR("DADA")))

implicit def token2TokenParser(t : Token) = TokenParser(t)

implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

implicit def TokenOps(s: Token) = new {
  def || (q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](s, q)
  def || (r: Token) = new AltParser[List[Token], Token](s, r)
  def ==>[S] (f: => Token => S) = new FunParser[List[Token], Token, S](s, f)
  def ~[S](q : => Parser[List[Token], S]) = 
    new SeqParser[List[Token], Token, S](s, q)
  def ~(r: Parser[List[Token], Token]) = 
    new SeqParser[List[Token], Token, Token](s, r)
  
}

// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp 

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class WriteId(s: String) extends Stmt
case class WriteStr(s: String) extends Stmt
case class Read(s: String) extends Stmt


case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp

case object IdTokenParser extends Parser[List[Token], String] {
  def parse(tks : List[Token]) = tks match {
    case T_ID(id)::rest => Set((id, rest))
    case _ => Set()
  }
}

case object NumTokenParser extends Parser[List[Token], Int] {
  def parse(tks : List[Token]) = tks match {
    case T_NUM(n)::rest => Set((n, rest))
    case _ => Set()
  }
}


lazy val AExp: Parser[List[Token], AExp] = 
  (Te ~ T_OP("+") ~ AExp) ==> { case x ~ _ ~ z => Aop("+", x, z): AExp } ||
  (Te ~ T_OP("-") ~ AExp) ==> { case x ~ _ ~ z => Aop("-", x, z): AExp } || Te 
lazy val Te: Parser[List[Token], AExp] = 
  (Fa ~ T_OP("*") ~ Te) ==> { case x ~ _ ~ z => Aop("*", x, z): AExp } || 
  (Fa ~ T_OP("/") ~ Te) ==> { case x ~ _ ~ z => Aop("/", x, z): AExp } ||
  (Fa ~ T_OP("%") ~ Te) ==> { case x ~ _ ~ z => Aop("%", x, z): AExp } || Fa  
lazy val Fa: Parser[List[Token], AExp] = 
   (T_LPAREN ~ AExp ~ T_RPAREN) ==> { case _ ~ y ~ _ => y } || 
   IdTokenParser ==> Var || 
   NumTokenParser ==> Num

// Test 17 * (3+3)
AExp.parse_all(List(T_NUM(17), T_OP("*"), T_LPAREN, T_NUM(3), T_OP("+"), T_NUM(3), T_RPAREN))
// Test (121 % 2)
AExp.parse_all(List(T_LPAREN, T_NUM(121), T_OP("%"), T_NUM(2), T_RPAREN))

// boolean expressions with some simple nesting
lazy val BExp: Parser[List[Token], BExp] = 
   (AExp ~ T_OP("==") ~ AExp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
   (AExp ~ T_OP("!=") ~ AExp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
   (AExp ~ T_OP("<") ~ AExp) ==> { case x ~ _ ~ z => Bop("<", x, z): BExp } || 
   (AExp ~ T_OP(">") ~ AExp) ==> { case x ~ _ ~ z => Bop(">", x, z): BExp } ||
   (AExp ~ T_OP("<=") ~ AExp) ==> { case x ~ _ ~ z => Bop("<=", x, z): BExp} ||
   (AExp ~ T_OP(">=") ~ AExp) ==> { case x ~ _ ~ z => Bop(">=", x, z): BExp} ||
   (T_LPAREN ~ BExp ~ T_RPAREN ~ T_OP("&&") ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => And(y, v): BExp } ||
   (T_LPAREN ~ BExp ~ T_RPAREN ~ T_OP("||") ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => Or(y, v): BExp } ||
   (T_KWD("true") ==> (_ => True: BExp )) || 
   (T_KWD("false") ==> (_ => False: BExp )) ||
   (T_LPAREN ~ BExp ~ T_RPAREN) ==> { case _ ~ x ~ _ => x }

// Test (3>3) && (17!=18)
BExp.parse_all(List(T_LPAREN, T_NUM(3), T_OP(">"), T_NUM(3), T_RPAREN, T_OP("&&"), T_LPAREN, T_NUM(17), T_OP("!="), T_NUM(18), T_RPAREN))
// Test (3>3) || (17!=18)
BExp.parse_all(List(T_LPAREN, T_NUM(3), T_OP(">"), T_NUM(3), T_RPAREN, T_OP("||"), T_LPAREN, T_NUM(17), T_OP("!="), T_NUM(18), T_RPAREN))
// Test (3>3) || (17!=18) && (true)
BExp.parse_all(List(T_LPAREN, T_NUM(3), T_OP(">"), T_NUM(3), T_RPAREN, T_OP("||"), T_LPAREN, T_NUM(17), T_OP("!="), T_NUM(18), T_RPAREN, T_OP("&&"), T_LPAREN,  T_KWD("true"), T_RPAREN))
// Test (2<=17) || (200 >= 300)
BExp.parse_all(tokenise("(2<=17) || (200 >= 300)"))

// statement / statements
// Include write for strings as well
lazy val Stmt: Parser[List[Token], Stmt] =
  ((T_KWD("skip") ==> (_ => Skip: Stmt)) ||
   (IdTokenParser ~ T_OP(":=") ~ AExp) ==> { case x ~ _ ~ z => Assign(x, z): Stmt } ||
   (T_KWD("write") ~ IdTokenParser) ==> { case _ ~ y => WriteId(y): Stmt } ||
   (T_KWD("write") ~ AnyString) ==> { case _ ~ x => WriteStr(x): Stmt } ||
   (T_KWD("write") ~ T_LPAREN ~ IdTokenParser ~ T_RPAREN) ==> { case _ ~ _ ~ y ~ _ => WriteId(y): Stmt } ||
   (T_KWD("write") ~ T_LPAREN ~ AnyString ~ T_RPAREN) ==> { case _ ~ _ ~ y ~ _ => WriteStr(y): Stmt } ||
   (T_KWD("read") ~ IdTokenParser) ==> { case _ ~ y => Read(y): Stmt } ||
   (T_KWD("if") ~ BExp ~ T_KWD("then") ~ Block ~ T_KWD("else") ~ Block) ==>
    { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w): Stmt } ||
   (T_KWD("while") ~ BExp ~ T_KWD("do") ~ Block) ==> { case _ ~ y ~ _ ~ w => While(y, w) }) 

// Test skip
Stmt.parse_all(List(T_KWD("skip")))
// Test x := x + 1
Stmt.parse_all(List(T_ID("x"), T_OP(":="), T_ID("x"), T_OP("+"), T_NUM(1)))
// Test write x 
Stmt.parse_all(List(T_KWD("write"), T_ID("x")))
// Test write "muhahaha"
Stmt.parse_all(List(T_KWD("write"), T_STR("muhahaha")))
// Test write(x)
Stmt.parse_all(List(T_KWD("write"), T_LPAREN, T_ID("x"), T_RPAREN))
// Test write("muhahaha")
Stmt.parse_all(List(T_KWD("write"), T_LPAREN, T_STR("muhahaha"), T_RPAREN))
// Test read n
Stmt.parse_all(List(T_KWD("read"), T_ID("n")))
// Test if a == 3 then skip else a := a + 1 
Stmt.parse_all(List(T_KWD("if"), T_ID("a"), T_OP("=="), T_NUM(3), T_KWD("then"), T_KWD("skip"), T_KWD("else"), T_ID("a"), T_OP(":="), T_ID("a"), T_OP("+"), T_NUM(1)))
// Test if (a < b) then skip else a := a * b + 1
Stmt.parse_all(List(T_KWD("if"), T_LPAREN, T_ID("a"), T_OP("<"), T_ID("b"), T_RPAREN, T_KWD("then"), T_KWD("skip"), T_KWD("else"), T_ID("a"), T_OP(":="), T_ID("a"), T_OP("*"), T_ID("b"), T_OP("+"), T_NUM(1)))
// Test while a < 200 do a := a + 2
Stmt.parse_all(List(T_KWD("while"), T_ID("a"), T_OP("<"), T_NUM(200), T_KWD("do"), T_ID("a"), T_OP(":="), T_ID("a"), T_OP("+"), T_NUM(2))) 
// Test while (a < 20) do { x := x * 2; a := a + 1 }
Stmt.parse_all(List(T_KWD("while"), T_LPAREN, T_ID("a"), T_OP("<"), T_NUM(20), T_RPAREN, T_KWD("do"), T_LPAREN, T_ID("x"), T_OP(":="), T_ID("x"), T_OP("*"), T_NUM(2), T_SEMI, T_ID("a"), T_OP(":="), T_ID("a"), T_OP("+"), T_NUM(1), T_RPAREN))
// Test while (a >= 300) do { a := 300 - 16 - 20; b := 4 - b }
Stmt.parse_all(tokenise("while (a >= 300) do { a := 300 - 16 - 20; b := 4 - b }"))

lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ T_SEMI ~ Stmts) ==> { case x ~ _ ~ z => x :: z : Block } ||
  (Stmt ==> ( s => List(s) : Block))

// blocks (enclosed in curly braces)
lazy val Block: Parser[List[Token], Block] =
  ((T_LPAREN ~ Stmts ~ T_RPAREN) ==> { case x ~ y ~ z => y} || 
   (Stmt ==> (s => List(s))))

// Figure 2: CW03

val fib = """write "Fib";
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
             write minus2"""

Stmts.parse_all(tokenise(fib))

// Figure 3: CW03

val threeLoops = """start := 1000;
                    x := start;
                    y := start;
                    z := start;
                    while 0 < x do {
                      while 0 < y do {
                        while 0 < z do { z := z - 1 };
                        z := start;
                        y := y - 1
                      };
                      y := start;
                      x := x - 1
                    }"""

Stmts.parse_all(tokenise(threeLoops))

// Also parse tree for the statement:
// if (a < b) then skip else a := a * b + 1

Stmts.parse_all(tokenise("if (a < b) then skip else a := a * b + 1"))

// Question 3 CW03

type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
  case While(b, bl) => 
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case WriteId(x) => { println(env(x)) ; env }
  case WriteStr(x) => { println(x) ; env }
  case Read(x) => { // Dynamic read from console
    val input = scala.io.StdIn.readInt() 
    env + (x -> input)
    }
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())

def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000.0 + "ms")
    result
}

val fib1 = """n := 10;
             minus1 := 0;
             minus2 := 1;
             temp := 0;
             while (n > 0) do {
                 temp := minus2;
                 minus2 := minus1 + minus2;
                 minus1 := temp;
                 n := n - 1
             };
             result := minus2""".replaceAll("\\s+", "")


// Test fib1
println(eval(Stmts.parse_all(tokenise(fib1)).head)("result"))

// DYNAMIC FIB
// Test fib2 // Made it dynamic to work with read from console
val fib2 = """write "Fib";
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
             write minus2"""

println(eval(Stmts.parse_all(tokenise(fib2)).head))

// Three-nested loops
val threeLoops2 = """start := 400;
                    x := start;
                    y := start;
                    z := start;
                    while 0 < x do {
                      while 0 < y do {
                        while 0 < z do { z := z - 1 };
                        z := start;
                        y := y - 1
                      };
                      y := start;
                      x := x - 1
                    }"""

time(eval(Stmts.parse_all(tokenise(threeLoops2)).head))
println(eval(Stmts.parse_all(tokenise(threeLoops2)).head))

/** Measurements:
  * 100 - 133.0969ms
  * 300 - 3566.0302ms
  * 500 - 12179.1076ms
  * 1000 - 124961.673ms
  * 3000 - >10 mins
  */

// Prime numbers
val primeNumbers = """end := 100;
                      n := 2;
                      while (n < end) do {
                        f := 2;
                        tmp := 0;
                        while ((f < n / 2 + 1) && (tmp == 0)) do {
                          if ((n / f) * f == n) then { tmp := 1 } else { skip };
                          f := f + 1
                        };
                        if (tmp == 0) then { write(n) } else { skip };
                        n := n + 1
                      }"""

time(eval(Stmts.parse_all(tokenise(primeNumbers)).head))
println(eval(Stmts.parse_all(tokenise(primeNumbers)).head))

/* Imported from toks.scala to check them */

// Loops program
//===============

/*
start := 1000; 
x := start;
y := start;
z := start;
while 0 < x do {
 while 0 < y do {
  while 0 < z do { z := z - 1 };
  z := start;
  y := y - 1
 };     
 y := start;
 x := x - 1
}
*/

val loops =
  List(T_ID("start"), T_OP(":="), T_NUM(1000), T_SEMI, T_ID("x"), T_OP(":="), 
       T_ID("start"), T_SEMI, T_ID("y"), T_OP(":="), T_ID("start"), T_SEMI, 
       T_ID("z"), T_OP(":="), T_ID("start"), T_SEMI, T_KWD("while"), T_NUM(0), 
       T_OP("<"), T_ID("x"), T_KWD("do"), T_LPAREN, T_KWD("while"), T_NUM(0), 
       T_OP("<"), T_ID("y"), T_KWD("do"), T_LPAREN, T_KWD("while"), T_NUM(0), 
       T_OP("<"), T_ID("z"), T_KWD("do"), T_LPAREN, T_ID("z"), T_OP(":="), 
       T_ID("z"), T_OP("-"), T_NUM(1), T_RPAREN, T_SEMI, T_ID("z"), T_OP(":="),
       T_ID("start"), T_SEMI, T_ID("y"), T_OP(":="), T_ID("y"), T_OP("-"), 
       T_NUM(1), T_RPAREN, T_SEMI, T_ID("y"), T_OP(":="), T_ID("start"), 
       T_SEMI, T_ID("x"), T_OP(":="), T_ID("x"), T_OP("-"), T_NUM(1), T_RPAREN) 

Stmts.parse_all(loops)

// Fib program
//=============

/*
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
*/

val fib3 =
  List(T_KWD("write"), T_STR("Fib"), T_SEMI, T_KWD("read"), T_ID("n"), 
       T_SEMI, T_ID("minus1"), T_OP(":="), T_NUM(0), T_SEMI, T_ID("minus2"), 
       T_OP(":="), T_NUM(1), T_SEMI, T_KWD("while"), T_ID("n"), T_OP(">"), 
       T_NUM(0), T_KWD("do"), T_LPAREN, T_ID("temp"), T_OP(":="), 
       T_ID("minus2"), T_SEMI, T_ID("minus2"), T_OP(":="), T_ID("minus1"), 
       T_OP("+"), T_ID("minus2"), T_SEMI, T_ID("minus1"), T_OP(":="), 
       T_ID("temp"), T_SEMI, T_ID("n"), T_OP(":="), T_ID("n"), T_OP("-"), 
       T_NUM(1), T_RPAREN, T_SEMI, T_KWD("write"), T_STR("Result"), T_SEMI, 
       T_KWD("write"), T_ID("minus2"))

Stmts.parse_all(fib3)
eval(Stmts.parse_all(fib3).head)
// Factors program
//=================

/*
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
}
*/

val factors = 
  List(T_KWD("write"), T_STR("Input n please"), T_SEMI, T_KWD("read"), 
       T_ID("n"), T_SEMI, T_KWD("write"), T_STR("The factors of n are"), 
       T_SEMI, T_ID("f"), T_OP(":="), T_NUM(2), T_SEMI, T_KWD("while"), 
       T_ID("n"), T_OP("!="), T_NUM(1), T_KWD("do"), T_LPAREN, 
       T_KWD("while"), T_ID("n"), T_OP("/"), T_ID("f"), T_OP("*"), 
       T_ID("f"), T_OP("=="), T_ID("n"), T_KWD("do"), T_LPAREN, 
       T_KWD("write"), T_ID("f"), T_SEMI, T_ID("n"), T_OP(":="), 
       T_ID("n"), T_OP("/"), T_ID("f"), T_RPAREN, T_SEMI, T_ID("f"), 
       T_OP(":="), T_ID("f"), T_OP("+"), T_NUM(1), T_RPAREN)

Stmts.parse_all(factors)
eval(Stmts.parse_all(factors).head) // Division by 0 error: the tokens might be incorrect
// Primes program
//================

/*
end := 100;
n := 2;
while (n < end) do {
  f := 2;
  tmp := 0;
  while ((f < n / 2 + 1) && (tmp == 0)) do {
    if ((n / f) * f == n) then  { tmp := 1 } else { skip };
    f := f + 1
  };
  if (tmp == 0) then { write(n) } else { skip };
  n  := n + 1
}
*/

// This program needs to contain parantheses to boolean expressions.
val primes =
  List(T_ID("end"), T_OP(":="), T_NUM(100), T_SEMI, T_ID("n"), T_OP(":="), 
       T_NUM(2), T_SEMI, T_KWD("while"), T_ID("n"), T_OP("<"), T_ID("end"), 
       T_KWD("do"), T_LPAREN, T_ID("f"), T_OP(":="), T_NUM(2), T_SEMI, 
       T_ID("tmp"), T_OP(":="), T_NUM(0), T_SEMI, T_KWD("while"), T_ID("f"), 
       T_OP("<"), T_ID("n"), T_OP("/"), T_NUM(2), T_OP("+"), T_NUM(1), 
       T_OP("&&"), T_ID("tmp"), T_OP("=="), T_NUM(0), T_KWD("do"), T_LPAREN, 
       T_KWD("if"), T_ID("n"), T_OP("/"), T_ID("f"), T_OP("*"), T_ID("f"), 
       T_OP("=="), T_ID("n"), T_KWD("then"), T_LPAREN, T_ID("tmp"), T_OP(":="),
       T_NUM(1), T_RPAREN, T_KWD("else"), T_LPAREN, T_KWD("skip"), T_RPAREN, 
       T_SEMI, T_ID("f"), T_OP(":="), T_ID("f"), T_OP("+"), T_NUM(1), 
       T_RPAREN, T_SEMI, T_KWD("if"), T_ID("tmp"), T_OP("=="), T_NUM(0), 
       T_KWD("then"), T_LPAREN, T_KWD("write"), T_ID("n"), T_RPAREN, 
       T_KWD("else"), T_LPAREN, T_KWD("skip"), T_RPAREN, T_SEMI, T_ID("n"), 
       T_OP(":="), T_ID("n"), T_OP("+"), T_NUM(1), T_RPAREN)

Stmts.parse_all(primes)
// If you do this instead it will work.
// I also did this example above but I did this here
// to include the programs in toks.scala as well.
eval(Stmts.parse_all(tokenise("""end := 100;
n := 2;
while (n < end) do {
  f := 2;
  tmp := 0;
  while ((f < n / 2 + 1) && (tmp == 0)) do {
    if ((n / f) * f == n) then  { tmp := 1 } else { skip };
    f := f + 1
  };
  if (tmp == 0) then { write(n) } else { skip };
  n  := n + 1
}""")).head)
