abstract class Token
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token


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

val fib =
  List(T_KWD("write"), T_STR("Fib"), T_SEMI, T_KWD("read"), T_ID("n"), 
       T_SEMI, T_ID("minus1"), T_OP(":="), T_NUM(0), T_SEMI, T_ID("minus2"), 
       T_OP(":="), T_NUM(1), T_SEMI, T_KWD("while"), T_ID("n"), T_OP(">"), 
       T_NUM(0), T_KWD("do"), T_LPAREN, T_ID("temp"), T_OP(":="), 
       T_ID("minus2"), T_SEMI, T_ID("minus2"), T_OP(":="), T_ID("minus1"), 
       T_OP("+"), T_ID("minus2"), T_SEMI, T_ID("minus1"), T_OP(":="), 
       T_ID("temp"), T_SEMI, T_ID("n"), T_OP(":="), T_ID("n"), T_OP("-"), 
       T_NUM(1), T_RPAREN, T_SEMI, T_KWD("write"), T_STR("Result"), T_SEMI, 
       T_KWD("write"), T_ID("minus2"))



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
