// 6CCS3CFL - Strand 1
// Name:          Horia Tudor Pavel Simon
// Student ID:    1731038

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
// case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
// case class RANGE(chars: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, m: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp
case class CFUN(f: Char => Boolean) extends Rexp

def CHAR(c : Char) : Char => Boolean = {
  (ch) => c == ch
}

def RANGE(chars : Set[Char]) : Char => Boolean = {
  (ch) => chars.contains(ch) 
}

def ALL(c : Char) : Boolean = true

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CFUN(_) => false
  // case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  // case RANGE(chars) => false
  case PLUS(r1) => nullable(r1)
  case OPTIONAL(_) => true
  case NTIMES(r1, i) => if (i == 0) true else nullable(r1)
  case UPTO(r1, i) => true
  case FROM(r1, i) => if (i == 0) true else nullable(r1)
  case BETWEEN(r1, i, j) => if (i == 0) true else nullable(r1)
  case NOT(r1) => !nullable(r1)
}

def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CFUN(f) => if (f(c)) ONE else ZERO
  // case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  // case RANGE(chars) => if (chars.contains(c)) ONE else ZERO
  case PLUS(r1) => SEQ(der(c, r1), STAR(r1))
  case OPTIONAL(r1) => der(c, r1)
  case NTIMES(r1, i) => 
    if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
  case UPTO(r1, i) => 
    if (i == 0) ZERO else SEQ(der(c, r1), UPTO(r1, i - 1))
  case FROM(r1, i) =>
    if (i == 0) SEQ(der(c, r1), STAR(r1)) else SEQ(der(c, r1), FROM(r1, i - 1))
  case BETWEEN(r1, i, j) => (r1, i, j) match {
    case (_, 0, _) => if (j == 0) ZERO else SEQ(der(c, r1), BETWEEN(r1, i, j - 1))
    case _ => SEQ(der(c, r1), BETWEEN(r1, i - 1, j - 1)) 
  }
  case NOT(r1) => NOT(der(c,r1))
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO  
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


/* Tests for Question 3 */

val expression = NTIMES(CFUN(CHAR('a')), 3) // a{3}
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

val expression = NTIMES(OPTIONAL(CFUN(CHAR('a'))),3) //  (a?){3} 
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

val expression = UPTO(CFUN(CHAR('a')), 3) //  a{..3} 
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

val expression = UPTO(OPTIONAL(CFUN(CHAR('a'))), 3) //  (a?){..3} 
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

val expression = BETWEEN(CFUN(CHAR('a')), 3, 5) //  a{3..5}
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

val expression = BETWEEN(OPTIONAL(CFUN(CHAR('a'))), 3, 5) //  a?{3..5} 
println("1: " + matcher(expression, ""))
println("2: " + matcher(expression, "a"))
println("3: " + matcher(expression, "aa"))
println("4: " + matcher(expression, "aaa"))
println("5: " + matcher(expression, "aaaaa"))
println("6: " + matcher(expression, "aaaaaa"))

// Testing PLUS
//val a = SEQ(STAR(CHAR('a')), PLUS(CHAR('b')))
//der('a', a)

/* Test for Question 4 */
val a = CFUN(RANGE(Set('a','b','c')))
der('a', a)
der('z', a)

/* Test for Question 5 */
val lcase_letters_and_digits = RANGE(('a' to 'z').toSet ++ ('0' to '9').toSet ++ Set('_', '.', '-'))
val lcase_letters_and_digits_no_underscore = RANGE(('a' to 'z').toSet ++ ('0' to '9').toSet ++ Set('.', '-'))
val lcase_letters_and_dot_only = RANGE(('a' to 'z').toSet ++ Set('.'))
val at = CHAR('@')
val dot = CHAR('.')

val username_at = SEQ(PLUS(CFUN(lcase_letters_and_digits)), CFUN(at))
val domain_dot = SEQ(PLUS(CFUN(lcase_letters_and_digits_no_underscore)), CFUN(dot))
val terminal = BETWEEN(CFUN(lcase_letters_and_dot_only), 2, 6)

val user_domain = SEQ(username_at, domain_dot)
val e_mail = SEQ(user_domain, terminal)

val my_kings_email = "horia.pavel-simon@kcl.ac.uk"

simp(ders(my_kings_email.toList, e_mail))

/* Question 6 */
val begin = SEQ(CFUN(CHAR('/')), CFUN(CHAR('*')))
val middle_left = SEQ(STAR(CFUN(ALL)), CFUN(CHAR('*')))
val middle_right = SEQ(CFUN(CHAR('/')), STAR(CFUN(ALL)))
val middle = SEQ(middle_left, middle_right)
val not_middle = NOT(middle)
val end = SEQ(CFUN(CHAR('*')), CFUN(CHAR('/')))

val expression = SEQ(SEQ(begin, not_middle), end)

val test1 = "/**/"
matcher(expression, test1) // yes

val test2 = "/*foobar*/"
matcher(expression, test2) // yes

val test3 = "/*test*/test*/"
matcher(expression, test3) // no

val test4 = "/*test/*test*/"
matcher(expression, test4) // yes

/* Question 7 */
val r1 = SEQ(SEQ(CFUN(CHAR('a')), CFUN(CHAR('a'))), CFUN(CHAR('a')))
val r2 = SEQ(BETWEEN(CFUN(CHAR('a')), 19, 19), OPTIONAL(CFUN(CHAR('a'))))

val r1_extended = PLUS(PLUS(r1))
val r2_extended = PLUS(PLUS(r2))

val string1 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
matcher(r1_extended, string1) // yes
matcher(r2_extended, string1) // yes

val string2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
matcher(r1_extended, string2) // no
matcher(r2_extended, string2) // no

val string3 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
matcher(r1_extended, string3) // no
matcher(r2_extended, string3) // yes