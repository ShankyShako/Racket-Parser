# Racket Parser

I had developed a code parser for a small artificial language based on FORTRAN. The objective is to determine if .txt input codes are valid, otherwise an error is thrown along with the line #.

This project consisted of using solely DrRacket, Grammar List, and Racket Command Book. 
## Grammar
1. program -> linelist $$
2. linelist -> line linelist | epsilon
3. line -> label stmt linetail
4. label -> id: | epsilon
5. linetail -> ;stmt+ | epsilon
6. stmt -> id = expr
> if (boolean) stmt
> while (boolean) linelist endwhile
> read id
> write expr
> goto id
> gosub id
> return
> break
> end
7. boolean -> true | false | expr bool-op expr
8. bool-op -> < | > | >= | <= | <> | =
9. expr -> id etail | num etail | (expr)
10. etail -> + expr | - expr | * expr | / expr | epsilon
11. id -> [a-zA-Z][a-z,A-Z,0-9]*
12. num -> numsign digit digit*
12. numsign -> + | - | epsilon
1. digit -> [0-9]

### Sample Code
This would be an example on how to call the command:

Input: (parse "File01.txt")
Expected Output: "accept"

### Sample Files
- File01.txt: Valid, should parse. There's a logic error (infinite loop) but no syntax issue. 
- File02.txt: Undeclared variable, but that's a semantic issue. Should parse OK. 
- File03.txt: Syntax error (mismatched parens) 
- File04.txt: Scan error, illegal character $ 
- File05.txt: Should parse OK. Note that this grammar doesn't specify order of operations, it's just left-to-right. Also note addition of -1, and label that is similar to keyword ('endpoint'). 
