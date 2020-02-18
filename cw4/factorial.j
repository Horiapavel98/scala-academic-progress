
.class public factorial.factorial
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

   ldc "Factorial" 		; "Factorial"
   invokestatic factorial/factorial/writeString(Ljava/lang/String;)V
   invokestatic factorial/factorial/read()I
   istore 0 		; n
   ldc 1
   istore 1 		; x
Loop_begin_0:
   ldc 1
   iload 0 		; n
   if_icmpge Loop_end_1
   iload 1 		; x
   iload 0 		; n
   imul
   istore 1 		; x
   iload 0 		; n
   ldc 1
   isub
   istore 0 		; n
   goto Loop_begin_0
Loop_end_1:
   ldc "Result" 		; "Result"
   invokestatic factorial/factorial/writeString(Ljava/lang/String;)V
   iload 1 		; x
   invokestatic factorial/factorial/write(I)V

; COMPILED CODE ENDS
   return

.end method
