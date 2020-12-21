       IDENTIFICATION DIVISION.
       PROGRAM-ID. Basics.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIn ASSIGN TO FInFileName
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS FStatus.

           SELECT FOut ASSIGN TO FOutFileName
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD FIn.
           01 InLine PIC X(500).
       FD FOut.
           01 OutLine PIC X(500).

       WORKING-STORAGE SECTION.
      *Main loop.
           01 Choice PIC 9 VALUE 0.
           01 Running PIC 9 VALUE 1.
      *FizzBuzz.
           01 FizzNum PIC 999 VALUE 1.
           01 NumDisplay PIC ZZZ9.
      *Fibonacci.
           01 Fib1 PIC 9(4).
           01 Fib2 PIC 9(4).
      *StrReverse./Palindome.
           01 StrInput PIC X(200).
           01 StrOutput PIC X(200).
      *Caesar.
           01 CKey PIC S9(10).
           01 StrLen PIC 999.
           01 i PIC 999.
           01 Case PIC 999.
           01 FInFileName PIC X(80).
           01 FOutFileName PIC X(100).
           01 EOF PIC 9 VALUE 0.
           01 FStatus PIC XX.

       PROCEDURE DIVISION.
           PERFORM UNTIL Running = 0
               DISPLAY " "
               DISPLAY "1: Fizz buzz"
               DISPLAY "2: Fibonacci sequence"
               DISPLAY "3: String reverse"
               DISPLAY "4: Palindrome check"
               DISPLAY "5: Caesar Cipher"
               DISPLAY "0: Exit"
               DISPLAY "Choose: " WITH NO ADVANCING
               ACCEPT Choice
               EVALUATE Choice
                   WHEN 1 PERFORM FizzBuzz
                   WHEN 2 PERFORM Fibonacci
                   WHEN 3 PERFORM StrReverse
                   WHEN 4 PERFORM Palindrome
                   WHEN 5 PERFORM Caesar
                   WHEN 0 MOVE 0 TO Running
               END-EVALUATE
           END-PERFORM.

           STOP RUN.

       FizzBuzz.
           PERFORM VARYING FizzNum FROM 1 BY 1 UNTIL FizzNum = 101
               MOVE FizzNum TO NumDisplay
               DISPLAY NumDisplay WITH NO ADVANCING
               IF FUNCTION MOD(FizzNum, 3) = 0
                   DISPLAY " FIZZ" WITH NO ADVANCING
               END-IF
               IF FUNCTION MOD(FizzNum, 5) = 0
                   DISPLAY " BUZZ" WITH NO ADVANCING
               END-IF
               DISPLAY " "
           END-PERFORM.

       Fibonacci.
           DISPLAY "Enter first number: " WITH NO ADVANCING.
           ACCEPT Fib1.
           DISPLAY "Enter second number: " WITH NO ADVANCING.
           ACCEPT Fib2.

      *To prevent infinite loop
           IF Fib1 EQUALS Fib2 AND Fib1 EQUALS ZERO
               MOVE 1 TO Fib2.

           PERFORM UNTIL Fib1 >= 1000
               MOVE Fib1 TO NumDisplay
               DISPLAY NumDisplay
               COMPUTE Fib2 = Fib2 + Fib1
               COMPUTE Fib1 = Fib2 - Fib1
           END-PERFORM.

       StrReverse.
      *    This can be done with the intrinsic funtion REVERSE(string)
      *    MOVE FUNCTION REVERSE(StrInput) TO StrInput.
       
           DISPLAY "Enter string to reverse: " WITH NO ADVANCING.
           ACCEPT StrInput.

           MOVE LENGTH OF FUNCTION TRIM(StrInput) TO StrLen.
           MOVE SPACES TO StrOutput.

           PERFORM VARYING i FROM 1 BY 1 UNTIL i > StrLen
               MOVE StrInput(i:1) TO StrOutput(StrLen - i + 1:1)
           END-PERFORM.
           DISPLAY "Reversed: " FUNCTION TRIM(StrOutput).

       Palindrome.
           DISPLAY "Enter string: " WITH NO ADVANCING.
           ACCEPT StrInput.

           MOVE FUNCTION REVERSE(FUNCTION TRIM(StrInput)) TO StrOutput.
           DISPLAY "Forward:     " FUNCTION TRIM(StrInput).
           DISPLAY "Reversed:    " FUNCTION TRIM(StrOutput).

           IF FUNCTION TRIM(StrInput) = FUNCTION TRIM(StrOutput)
               DISPLAY FUNCTION TRIM(StrInput) " is a palidrome"
           ELSE
               DISPLAY FUNCTION TRIM(StrInput) " is not a palidrome"
           END-IF.
           
       Caesar.
           DISPLAY "Enter filename: " WITH NO ADVANCING.
           ACCEPT FInFileName.

           OPEN INPUT FIn.
           IF FStatus NOT = "00"
               DISPLAY "Error opening file"
               EXIT PARAGRAPH
           END-IF.

           UNSTRING FInFileName DELIMITED BY '.'
               INTO FOutFileName
           END-UNSTRING.

           STRING 
               FUNCTION TRIM(FOutFileName TRAILING) DELIMITED BY SIZE
               "_encrypted.txt" DELIMITED BY SIZE
               INTO FOutFileName
           END-STRING.

           OPEN OUTPUT FOut.

           DISPLAY "Enter key: " WITH NO ADVANCING.
           ACCEPT CKey.

           MOVE 0 TO EOF
           READ FIn INTO InLine
               AT END MOVE 1 TO EOF
           END-READ
           PERFORM UNTIL EOF = 1

               MOVE LENGTH OF FUNCTION TRIM(InLine TRAILING) TO StrLen
               MOVE SPACES TO OutLine
       
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > StrLen
                   IF InLine(i:1) IS ALPHABETIC AND InLine(i:1) NOT=" "
                       IF InLine(i:1) IS ALPHABETIC-UPPER
                           MOVE FUNCTION ORD("A") TO Case
                       ELSE
                           MOVE FUNCTION ORD("a") TO Case
                       END-IF
       
                       MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(
      -                InLine(i:1)) - Case + CKey, 26) + Case) 
      -                TO OutLine(i:1)
       
                   ELSE
                       MOVE InLine(i:1) TO OutLine(i:1)
                   END-IF
       
               END-PERFORM

               WRITE OutLine
               END-WRITE

               READ FIn INTO InLine
                   AT END MOVE 1 TO EOF
               END-READ
           END-PERFORM.

           DISPLAY "Output written to " FUNCTION TRIM(FOutFileName).
           CLOSE FIn, Fout.
