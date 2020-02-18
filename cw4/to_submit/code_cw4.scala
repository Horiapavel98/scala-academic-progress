  // A simple lexer inspired by work of Sulzmann & Lu
  //==================================================

  // Code imported from coursework 1, 2 & 3

  /* Wrap contents in an object */

object Compile {

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

  // Question 1
  val SYM : Rexp = CFUN(RANGE("ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz_"))
  val DIGIT : Rexp = CFUN(RANGE("0123456789"))
  val DIGITS_NO_ZERO : Rexp = CFUN(RANGE("123456789"))
  val ID = SYM ~ (SYM | DIGIT).% 
  // val NUM = PLUS(DIGIT)
  val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "for" | "to" | "true" | "false" | "upto"
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

// Parses a token at the head of a list of tokens if it matches t: Token
case class TokenParser(t : Token) extends Parser[List[Token], Token] {
  def parse(tks : List[Token]) = tks match {
    case head::rest => if (head == t) Set((head, rest)) else Set()
    case _ => Set()
  }
}

// Parses and extracts the string from the string token at the head of the token list if there is any
case object AnyString extends Parser[List[Token], String] {
  def parse(tks : List[Token]) = tks match {
    case T_STR(str)::rest => Set((str, rest))
    case _ => Set()
  }
}

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
case class For(s: Stmt, a1: AExp, b: Block) extends Stmt
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
   (T_KWD("while") ~ BExp ~ T_KWD("do") ~ Block) ==> { case _ ~ y ~ _ ~ w => While(y, w) } ||
   (T_KWD("for") ~ Stmt ~ T_KWD("upto") ~ AExp ~ T_KWD("do") ~ Block) ==>
    { case _ ~ s ~ _ ~ a1 ~ _ ~ bl => For(s, a1, bl): Stmt })

lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ T_SEMI ~ Stmts) ==> { case x ~ _ ~ z => x :: z : Block } ||
  (Stmt ==> ( s => List(s) : Block))

// blocks (enclosed in curly braces)
lazy val Block: Parser[List[Token], Block] =
  ((T_LPAREN ~ Stmts ~ T_RPAREN) ==> { case x ~ y ~ z => y} || 
   (Stmt ==> (s => List(s))))


import java.io._
import scala.util._ 

def deserialise[T](fname: String) : Try[T] = {
  import scala.util.Using
  Using(new ObjectInputStream(new FileInputStream(fname))) {
    in => in.readObject.asInstanceOf[T]
  }
}

// Code imported from compile.scala


// compiler headers needed for the JVM
// (contains methods for read and write)
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public <init>()V
    aload_0
    invokespecial java/lang/Object/<init>()V
    return
.end method

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/println(I)V 
    return 
.end method

.method public static writeString(Ljava/lang/String;)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload_0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method 

.method public static read()I
  .limit locals 10
  .limit stack 10

  ldc 0
  istore 1  ; this will hold our final integer
Label1:
  getstatic java/lang/System/in Ljava/io/InputStream;
  invokevirtual java/io/InputStream/read()I
  istore 2
  iload 2
  ldc 10   ; the newline delimiter
  isub
  ifeq Label2
  iload 2
  ldc 32   ; the space delimiter
  isub
  ifeq Label2
  iload 2
  ldc 13   ; the carriage return delimiter
  isub
  ifeq Label2

  iload 2
  ldc 48   ; we have our digit in ASCII, have to subtract it from 48
  isub
  ldc 10
  iload 1
  imul
  iadd
  istore 1
  goto Label1
Label2:
  ;when we come here we have our integer computed in local variable 1
  iload 1
  ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

val ending = """
; COMPILED CODE ENDS
   return

.end method
"""

// Compiler functions


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// convenient string interpolations 
// for instructions and labels
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

// this allows you to write things like
// i"add" and l"Label"


// environments 
type Env = Map[String, Int]


def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
}

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)} \t\t; $s"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

// boolean expression compilation
//  - the jump-label is for where to jump if the condition is not true
def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case True => ""
  case False => i"goto $jmp"
  case Bop("==", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
  case Bop(">", a1, a2) =>
    compile_aexp(a2, env) ++ compile_aexp(a1, env) ++ i"if_icmpge $jmp"
  case Bop("<=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"
  case Bop(">=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
}

// statement compilation
def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
  case Skip => ("", env)
  case Assign(x, a) => {
    val index = env.getOrElse(x, env.keys.size)
    (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
  } 
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    val (instrs1, env1) = compile_block(bl1, env)
    val (instrs2, env2) = compile_block(bl2, env1)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     i"goto $if_end" ++
     l"$if_else" ++
     instrs2 ++
     l"$if_end", env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs1, env1) = compile_block(bl, env)
    (l"$loop_begin" ++
     compile_bexp(b, env, loop_end) ++
     instrs1 ++
     i"goto $loop_begin" ++
     l"$loop_end", env1)
  }
  case For(s, a1, bl) => {
    // Construct labels
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    // We match to find index name
    val index_name = s match {
      case Assign(x, y) => x
      case _ => "i"
    }
    // Construct instructions
    val (c_a1, env1) = compile_stmt(s, env)
    val (instr, env2) = compile_block(bl, env1)
    val (increment, env3) = compile_stmt(Assign(index_name, Aop("+", Var(index_name), Num(1))), env2)
    // Build generated code
    (c_a1 ++
     l"$loop_begin" ++
     compile_bexp(Bop("<=", Var(index_name), a1), env3, loop_end) ++
     instr ++
     increment ++
     i"goto $loop_begin" ++
     l"$loop_end", env3)
  }
  case WriteId(x) => 
    (i"iload ${env(x)} \t\t; $x" ++ 
     i"invokestatic XXX/XXX/write(I)V", env)
  // Add case for writing strings
  case WriteStr(x) =>
    (i"ldc ${x} \t\t; $x" ++
     i"invokestatic XXX/XXX/writeString(Ljava/lang/String;)V", env)
  case Read(x) => {
    val index = env.getOrElse(x, env.keys.size) 
    (i"invokestatic XXX/XXX/read()I" ++ 
     i"istore $index \t\t; $x", env + (x -> index))
  }
}

/*
val a = io.Source.fromFile("test_for.while").mkString.replaceAllLiterally("\r\n", "")
val b = Stmts.parse_all(tokenise(a)).head.head
compile_stmt(b, Map.empty)
*/

// compilation of a block (i.e. list of instructions)
def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}

// main compilation function for blocks
def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map.empty)._1
  (beginning ++ instructions ++ ending).replaceAllLiterally("XXX", class_name)
}


// compiling and running files
//
// JVM files can be assembled with 
//
//    java -jar jvm/jasmin-2.4/jasmin.jar fib.j
//
// and started with
//
//    java fib/fib



import scala.util._
import scala.sys.process._
import scala.io

def compile_tofile(bl: Block, class_name: String) = {
  val output = compile(bl, class_name)
  val fw = new java.io.FileWriter(class_name + ".j") 
  fw.write(output) 
  fw.close()
}

def compile_all(bl: Block, class_name: String) : Unit = {
  compile_tofile(bl, class_name)
  println("compiled ")
  val test = ("java -jar jvm/jasmin-2.4/jasmin.jar " + class_name + ".j").!!
  println("assembled ")
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


def compile_run(bl: Block, class_name: String) : Unit = {
  println("Start compilation")
  compile_all(bl, class_name)
  println("running")
  println("Time: " + time_needed(1, ("java " + class_name + "/" + class_name).!))
}


def main(args: Array[String]) : Unit = {
  val fname = args(0)
  val tname = fname.stripSuffix(".while") ++ ".tks"
  val file = io.Source.fromFile(fname).mkString
  serialise(tname, tokenise(file.replaceAllLiterally("\r\n", "")))
  val tks = deserialise[List[Token]](tname).getOrElse(Nil)
  //println(Stmts.parse_all(tks).head)
  compile_all(Stmts.parse_all(tks).head, args(0).stripSuffix(".while"))
  // println(s"Reading back from ${fname}:\n${tks.mkString("\n")}")  
}

}