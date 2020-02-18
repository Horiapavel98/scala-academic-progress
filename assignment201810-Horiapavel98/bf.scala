// Part 1 about an Interpreter for the Brainf*** language
//========================================================


// representation of Bf memory 

type Mem = Map[Int, Int]


// (1) Write a function that takes a file name as argument and
// and requests the corresponding file from disk. It Returns the
// content of the file as a String. If the file does not exists,
// the function should Return the empty string.

import io.Source
import scala.util._

def load_bff(name: String) : String = {
      Try(Source.fromFile(name).mkString).getOrElse("")
}



// (2) Complete the functions for safely reading  
// and writing brainf*** memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, provided it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = {
      if(mem.keySet.contains(mp)) mem(mp)
      else 0
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
      mem + (mp -> v)
}



// (3) Implement the two jumping instructions in the 
// brainf*** language. In jumpRight, given a program and 
// a program counter move the program counter to the right 
// until after the *matching* ]-command. Similarly, 
// jumpLeft implements the move to the left to just after
// the *matching* [-command.

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
            if (pc >= prog.length) pc
            else { 
                  prog(pc) match {
                  case ']' => { 
                        if(level == 0) pc + 1 else jumpRight(prog,pc+1,level-1) 
                        }
                  case '[' => jumpRight(prog,pc+1,level+1)
                  case _ => if (pc + 1 == prog.length) pc + 1 else jumpRight(prog,pc+1,level)
            }
      }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = { 
      if (pc < 0) pc
      else {      
            prog(pc) match {
            case '[' => { 
                  if (level == 0) pc + 1 
                  else if (pc == 0) -1 else jumpLeft(prog,pc-1,level-1) 
            }
            case ']' => jumpLeft(prog,pc-1,level+1)
            case _ => { 
                  if (pc == 0) pc 
                  else jumpLeft(prog,pc-1,level) 
                  }
            }
      }
}


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) Complete the compute function that interprets (runs) a brainf***
// program: the arguments are a program (represented as a string), a program 
// counter, a memory counter and a brainf*** memory. It Returns the
// memory at the stage when the execution of the brainf*** program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc, 
// memory pointer and memory need to be updated according to 
// rules of the brainf*** language. Then, recursively, the compute 
// function continues with the command at the new program
// counter. 
//
// Implement the run function that calls compute with the program
// counter and memory counter set to 0.


def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
      //print(mem + " ")
      if (pc < 0 || pc >= prog.length) mem
      else {
            //print(pc + " |")
            prog(pc) match {
                  case '>' => compute(prog,pc+1,mp+1,mem)
                  case '<' => compute(prog,pc+1,mp-1,mem)
                  case '+' => compute(prog,pc+1,mp,write(mem,mp,sread(mem,mp)+1))
                  case '-' => compute(prog,pc+1,mp,write(mem,mp,sread(mem,mp)-1))
                  case '.' => {
                        print(sread(mem,mp).toChar)
                        compute(prog,pc+1,mp,mem)
                  }
                  case ',' => {
                        val input = Console.in.read().toByte
                        compute(prog,pc+1,mp,write(mem,mp,input))
                  }
                  case '[' => {
                        if(sread(mem,mp) == 0) {
                              val newpc = jumpRight(prog,pc+1,0)
                              compute(prog,newpc,mp,mem)
                        }
                        else compute(prog,pc+1,mp,mem)
                  }
                  case ']' => {
                        if (sread(mem,mp) != 0) {
                              val newpc = jumpLeft(prog,pc-1,0) 
                              compute(prog,newpc,mp,mem)
                        }
                        else compute(prog,pc+1,mp,mem)
                  }
                  case _ => compute(prog,pc+1,mp,mem)
            }
      }
}

def run(prog: String, m: Mem = Map()) = {
      compute(prog,0,0,m)
}


/*

// some sample bf-programs collected from the Internet
//=====================================================


// first some contrived (small) programs

// clears the 0-cell
run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// copies content of the 0-cell to 1-cell
run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
run("[>>+>>+<<<<-]", Map(0 -> 42))


// prints out numbers 0 to 9
run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")


// some more "useful" programs

// hello world program 1
run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
      +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")


// draws the Sierpinski triangle
run("""++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[
      ->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<
      ]>.>+[>>]>+]""")

run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
run("""+++++++++++
      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")


//outputs the square numbers up to 10000
run("""++++[>+++++<-]>[<+++++>-]+<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")

//collatz numbers (needs a number to be typed in)
run(""">,[[----------[
      >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
      ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<]>]>>>++>+>>[
      <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
      >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
      >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
      ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,]""")


// infinite collatz (never stops)
run(""">>+>+<[[->>[>>]>>>[>>]+[<<]<<<[<<]>[>[>>]>>+>[>>]<+<[<<]<<<[<
      <]>-]>[>>]>>[<<<<[<<]>+>[>>]>>-]<<<<[<<]+>>]<<[+++++[>+++++++
      +<-]>.<++++++[>--------<-]+<<]>>[>>]+[>>>>[<<+>+>-]<-[>+<-]+<
      [<<->>-[<<+>>[-]]]>>>[<<<+<<+>>>>>-]<<<[>>>+<<<-]<<[[-]>+>>->
      [<+<[<<+>>-]<[>+<-]<[>+<-]>>>>-]<[>+<-]+<[->[>>]<<[->[<+++>-[
      <+++>-[<+++>-[<[-]++>>[-]+>+<<-[<+++>-[<+++>-[<[-]+>>>+<<-[<+
      ++>-[<+++>-]]]]]]]]]<[>+<-]+<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<
      +>-[<+>-[<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-[<+>-]]]]]]]]]]]<[>+<-
      ]+>>]<<[<<]>]<[->>[->+>]<[-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<-
      >>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>
      -[<->>+<-[<+>-[<->>+<-[<+>-]]]]]]]]]]]]]]]]]]]>[<+>-]<+<[<+++
      +++++++>-]<]>>[<+>->>]<<[>+>+<<-]>[<+>-]+>[<->[-]]<[-<<-]<<[<
      <]]++++++[>+++++++<-]>++.------------.[-]>[>>]<<[+++++[>+++++
      +++<-]>.<++++++[>--------<-]+<<]+<]>[<+>-]<]>>>[>>]<<[>[-]<-<
      <]++++++++++.[-]<<<[<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<+>-[<+>-
      [<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-]]]]]]]]]]<[>+<-]+>>]<<[<<]>>]""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)

run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
val b1 = """>++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++
            [>++++++++<-]>.[-]<<>++++++++++[>++++++++++[>++
            ++++++++[>++++++++++[>++++++++++[>++++++++++[>+
            +++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++."""


def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

time_needed(1, run(b1))
*/

