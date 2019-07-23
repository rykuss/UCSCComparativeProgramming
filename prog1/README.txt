[GITHUB SUBMISSION INFO: asg1-scheme-sbi.pdf is the assignment requirements,
sbi.scm is my solution, testFiles contains a bunch of SBIR files that must be 
ran mostly functionally with Scheme. Program description/analysis listed
below].

Program 1 : Functionality Scheme

Stephen Woodbury
1429496
swoodbur

Description: Build an interpreter that uses scheme to interpret 
SBIR in a mostly functional style

Idea behind Program: 
1)Read our SBIR File 
2)Fill our Label Table
3)Execute Program
  a)Start at line 0, execute program line by line
  b)Determine how long each line is (3, 2, or otherwise)
    -If 3, Must have a Number, Label, and Statement
    -If 2, Determine if second argument in line/list 
     is a label or statement
      +If it's a label, move to otherwise, if it's a statement, 
       proceed to d
    -If otherwise, Move to next line of program
  d)Set line respective to if there were 2 or 3 arguments, 
    call detFunction
  e)Check if there was anything actually passed
    -if not, execute next line of program
    -If so, check to ensure that there is a valid instruction
      +If so, check if it's a goto, if it is, execute program at 
       line number related to label
      +If not, check if it's an if, check if body of statement is true
        ~If so, like goto, jump to line number associated with 
         provided label
        ~If not, execute next line of code
      +If not, call function table to find translated function
       from SBIR to scheme, execute with given arguments,
       then execute next line of program
    -If it isn't a valid instruction, say so, and exit
