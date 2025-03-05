#lang racket
(require data/either)

(define fileN "File01.txt")
(define isParsed false)


;searches for which line the error is found in and displays either a default error message or a specific error message
(define (FindError errorP errormessage)
  ;starts line counter (since there isnt a 0th line it initializes at 1)
  (define i 1)
  (for ([line (file->lines fileN)])
    (for/list ([phrase (string-split line)])
      (if (string-contains? phrase errorP)
          ;checks for present message and ends program
          (if (string-contains? errormessage "Error")
              (error (string-append errormessage " in line " (~a i)))
              (error (string-append "Syntax Error: " errorP " found in line " (~a i))))
          0)
      );end of phrase loop
    ;increments i by 1 for the counter
    (set! i (+ i 1))
    );end of line loop
  );end of FindError

;Tokenizes each line and appends it onto a list of lines
;also looks for illegal characters
(define (scanner fileName)
  ;checks if file exists
  (if (file-exists? fileName)
      (success "file found")
      (error (string-append "File Error: Filename of " fileName " not found"))
  );end of if
  ;begins token list
  (define tokens '())
  (for ([line (file->lines fileName)])
    ;begins token line
    (define tokenLine '())
    (for/list ([phrase (string-split line)])
      (cond
        ;used to seperate the parenthesis and the phrase connected to it
        [(equal? (string-ref phrase 0) #\()
         (set! tokenLine (append tokenLine '("(")))
         (set! tokenLine (append tokenLine (list (string-trim phrase "("))))]
        ;also used to seperate the parenthesis and the phrase connected to it
        [(equal? (string-ref phrase (- (string-length phrase) 1)) #\))
         (set! tokenLine (append tokenLine (list (string-trim phrase ")"))))
         (set! tokenLine (append tokenLine '(")")))]
        ;check for $$ or for misusage of "$"
        [(string-contains? phrase "$")
         (if (equal? phrase "$$")
             (success (set! tokenLine (append tokenLine (list phrase))))
             (failure (FindError phrase "Scan Error: failed because illegal character $")))]
        [else (set! tokenLine (append tokenLine (list phrase)))]
        );end of conditions
      );end of phrase loop
    ;appens all phrases from line to list
    (set! tokens (append tokens (list tokenLine)))
    );end of line loop
  ;send token value
  (values tokens)
  );end of scanner

;function function, similar to id
(define (function tokenLine tokens)
  ;removes ":" for easier checking
  (let ([id (string-trim (first tokenLine) ":")])
    (for/list ([chr (string->list id)])
      ;checks for valid function entry
      (if (not (or (char-numeric? chr)(char-alphabetic? chr)))
          (failure (FindError id (string-append "Syntax Error: invalid characters for function call " id ":")))
          (success "valid function"))
      )
    ;calls to stmt to read function instructions
    (stmt (rest tokenLine) tokens))
  );end of function

;Checks to see which statement name it would then sends to necassary function, all else fails go to ID
(define (stmt tokenLine tokens)
  (cond
    [(equal? (first tokenLine) "if")
     (iffunc (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "while")
     (while (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "read")
     (ID (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "write")
     (expr (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "goto")
     (ID (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "gosub")
     (ID (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "return")
     (linetail (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "break")
     (linetail (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "end")
     (linetail (rest tokenLine) tokens)]
    [else
     (ID tokenLine tokens)]
    );end of cond
  );end of stmt

;if function similar to while function
(define (iffunc tokenLine tokens)
  (define boolLine '())
  (if
   (equal? (string-ref (first tokenLine) 0) #\()
   (success (set! boolLine (boolean (rest tokenLine))))
   (failure (FindError (first tokenLine)))
   );end of if
  (set! boolLine(remove ")" boolLine))
  ;used to make false line parser to bring it back into this function when finished with boolean
  (boolexp boolLine (list "0" (list "EndSim")))
  (define ifToken (append (second tokens) (list (list "EndSim"))))
  (lineParser ifToken)
  (lineParser (rest (rest tokens)))
  );end of iffunc


;while function
(define (while tokenLine tokens)
  ;starts initial expression statement for boolean of while function
  (define boolLine '())
  (if
   ;checks for if parenthesis is found
   (equal? (string-ref (first tokenLine) 0) #\()
   ;removes "("
   (success (set! boolLine (boolean (rest tokenLine))))
   ;if no "(" then there is a syntax error
   (failure (FindError (first tokenLine) "Syntax Error: no parenthesis found for while boolean"))
   );end of if
  (set! boolLine(remove ")" boolLine))
  ;used to make false line parser to bring it back into this function when finished with boolean
  (boolexp boolLine (list "0" (list "EndSim")))
  ;flag for when "endwhile" is found
  (define foundEnd false)
  (define whileTokens '())
  (define afterTokens '())
  (for/list ([line (rest tokens)])
    (if foundEnd
        (set! afterTokens (append afterTokens (list line)))
        (set! whileTokens (append whileTokens (list line)))
        );end of if
    (for/list ([phrase line])
      (if (string-contains? phrase "endwhile")
          (set! foundEnd true) (success "keep looking for end"))
      );end of phrase loop
    );end of token loop
  ;checks if "endwhile" was found, if not send error
  (if foundEnd
      (success "'endwhile' found")
      (failure (FindError "while" "Syntax Error: endwhile not found"))
      );end of if
  ;removes "endwhile" list to prevent errors, replaced with EndSim instead to show end of simulation
  (set! whileTokens (remove (list "endwhile") whileTokens))
  (set! whileTokens (append whileTokens (list (list "EndSim"))))
  ;simulates all of tokens found in while loop
  (lineParser whileTokens)
  ;throws rest of tokens back into the recursion
  (lineParser afterTokens)
  );end of while

;creates bool list and converts remaining line into that list, removes parenthesis matching the first parenthesis
(define (boolean tokenLine)
  (define boolLine '())
  ;flag for when end parenthesis is found, turns false if found
  (define notEndParen true)
  (for/list ([phrase tokenLine])
    ;IF 1: checks for if end parenthesis was found, if further phrases are afterwards append anyways and check for legality later
    (if (and notEndParen (not (equal? (string-ref phrase (- (string-length phrase) 1)) #\))))
        (set! boolLine (append boolLine (list phrase)))
        ;IF 2: when if 1 bool is false then turns notEndParen false so if 1 it stays false
        (if notEndParen (begin (set! notEndParen false)
                               ;IF 3: splits parenthesis if unable to successfully split from scanner
                               (if (equal? (string-trim phrase ")") "") (set! boolLine (append boolLine (list phrase)))
                                   (set! boolLine (append boolLine (list (string-trim phrase ")"))))
                                   );end of if 3
                               )
            (set! boolLine (append boolLine (list phrase)))
            );end of if 2
        );end of if 1
    )
  ;returns the finished array
  (values boolLine)
  )

;ID function
(define (ID tokenLine tokens)
  (let ([id (first tokenLine)])
    (for/list ([chr (string->list id)])
      ;checks for valid id entry
      (if (not (or (char-numeric? chr) (char-alphabetic? chr)))
          (failure (FindError id (string-append "Syntax Error: invalid characters for id call " id)))
          (success 0)
          );end of if
      );end of character loop
    );end of check for valid entry
  ;checks for extra statements such as "=" to assign, if nothing valid go to linetail function to check for ";"
  (if (empty? (rest tokenLine)) (lineParser (rest tokens))
      (if (equal? (second tokenLine) "=")
          (success (expr (rest (rest tokenLine)) tokens))
          (failure (linetail (rest tokenLine) tokens))
          );end of if 2
      );end of if 1
  );end of ID

;expression function (after bool-operator or for none-bool expressions)
(define (expr tokenLine tokens)
  ;checks for inner expressions within expressions
  (if (equal? (first tokenLine) "(")
      (set! tokenLine (boolean  (rest tokenLine)))
      (success "not parenthesis" ));end of if
  ;checks for negative numbers and turns then positive for parsing(reading the negative and outputing the rest of the digit)
  (if (equal? (string-ref (first tokenLine) 0) #\-)
      (set! tokenLine (append (list (string-trim (first tokenLine) "-")) (rest tokenLine)))
      (success "not negative"));end of if
  (let ([var (first tokenLine)])
    (cond
      ;checks for id, error if not found
      [(for/list ([chr (string->list var)])
         (if (not (or (char-numeric? chr) (char-alphabetic? chr)))
             (failure (FindError var "Syntax Error: invalid characters for an expression"))
             (success "maybe an id or digit")))]
      ;checks for digit
      [(for/list ([chr (string->list var)])
         (if (not (char-numeric? chr))
             (failure (FindError var "Syntax Error: invalid characters for an expression"))
             (success "definitely a digit")))]
      ;anything else will return an error
      [else (FindError var "0")]
      );end of conditions
    );end of variable check
  ;IF 1: checks for any remaining numbers, calls back to recursion if not
  (if (empty? (rest tokenLine))
      (lineParser (rest tokens))
      ;IF 2: checks for operations
      (if (or
           (equal? "+" (second tokenLine))
           (equal? "-" (second tokenLine))
           (equal? "*" (second tokenLine))
           (equal? "/" (second tokenLine)));end of or
          (success (expr (rest (rest tokenLine)) tokens))
          (failure (linetail (rest tokenLine) tokens))
          );end of if 2
      );end of if 1
  );end of expr

;bool expression function (before bool-operator)
(define (boolexp tokenLine tokens)
  (let ([var (first tokenLine)])
    (cond
      ;checks for id, error if not found
      [(for/list ([chr (string->list var)])
         (if (not (or (char-numeric? chr) (char-alphabetic? chr)))
             (failure (FindError var "Syntax Error: invalid characters for an expression"))
             (success "maybe an id or digit")))]
      ;checks for digit
      [(for/list ([chr (string->list var)])
         (if (not (char-numeric? chr))
             (failure (FindError var "Syntax Error: invalid characters for an expression"))
             (success "definitely a digit")))]
      ;anything else will return an error
      [else (FindError var "0")]
      );end of condition
    );end of variable check
  (cond
    ;checks for true or false calls and ends boolean
    [(equal? (first tokenLine) "true") (linetail (rest tokenLine) tokens)]
    [(equal? (first tokenLine) "false") (linetail (rest tokenLine) tokens)]
    [else (if (empty? (rest tokenLine))
              (FindError (first tokenLine) "Syntax Error: bool function incorrectly phrased.")
              ;checks for operations and calls back onto itself,
              ;or checks for bool-operators and finishes call and goes to expr
              (cond
                [(equal? "+" (second tokenLine)) (boolexp (rest (rest tokenLine)) tokens)]
                [(equal? "-" (second tokenLine)) (boolexp (rest (rest tokenLine)) tokens)]
                [(equal? "*" (second tokenLine)) (boolexp (rest (rest tokenLine)) tokens)]
                [(equal? "/" (second tokenLine)) (boolexp (rest (rest tokenLine)) tokens)]
                [(equal? "<" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [(equal? ">" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [(equal? ">=" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [(equal? "<=" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [(equal? "<>" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [(equal? "=" (second tokenLine)) (expr (rest (rest tokenLine)) tokens)]
                [else (linetail (rest tokenLine) tokens)]
                );end of condition 2
              )]
    );end of condition 1
  );end of boolexp

;linetail function, checks for extra statements using the ";" symbol
(define (linetail tokenLine tokens)
  (if (empty? tokenLine) (lineParser (rest tokens))
      (if (equal? (first tokenLine) ";")
          (success (stmt (rest tokenLine) tokens))
          (failure (FindError (first tokenLine) (string-append "Syntax Error: invalid sequence of commands, such as " (first tokenLine))))
          );end of if 2
      );end of if 1
  );end of linetail

;lineParser, recursively called within all other functions to keep the program reading line by line. Stops when $$ is inputed
(define (lineParser tokens)
  (let ([tokenLine (first tokens)])
    ;ensures function does not call an empty list, and if list is empty before a $$ is found to stop the recursion, then sends error
    (if (empty? tokenLine) (FindError "" "Syntax Error: no end phrase $$ found")
        (cond
          ;specifically for while function to simulate its loop
          [(equal? (first tokenLine) "EndSim") (success "back to while")]
          ;function call first to catch the ":" at the end
          [(equal? (string-ref (first tokenLine) (- (string-length (first tokenLine)) 1)) #\:) (function tokenLine tokens)]
          ;as long as it is a character it will go to statement function, it will check for valid and invalid characters at ID function
          [(char-alphabetic? (string-ref (first tokenLine) 0)) (stmt tokenLine tokens)]
          ;calls onto nothing so that stack falls and goes to display "accept"
          [(equal? (first tokenLine) "$$" ) (set! isParsed true)]
          ;if all other lines fail it is not in order with the code language
          [else (FindError (first tokenLine) (string-append "Syntax Error: failed due to invalid token" (first tokenLine)))]
          ));end of if
    );end of linecheck
  );end of lineParser

(define (parse fileName)
  ;sends fileName to callback if errors found
  (set! fileN fileName)
  ;tokenizes file
  (define tokens (scanner fileName))
  ;flag to check if line has ended
  (set! isParsed false)
  ;parses tokens
  (lineParser tokens)

  ;when function finishes, display accept
  (if isParsed
  (display "accept")
  (failure (FindError "" "Syntax Error: no end phrase $$ found")))
  );end of parse
