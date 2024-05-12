# C23 grammar and scanner for Bison

NOTE! THIS IS A WORK-IN-PROGRESS!
No guarantees for correctness or any usefulness. Untested!

After having searched in vain for a Bison grammar for C23, I have made
this attempt at making one myself. I share it mainly for reviewing,
although I believe it is reasonably correct. 

This is based on Thiago Adams https://github.com/thradams/cgrammar
(from which this repository is forked) and the C11 yacc grammar and
lex scanner files provided by Jutta Degener from
https://www.quut.com/c/ANSI-C-grammar-y.html

I have cross-checked the grammar once with the n3220.pdf draft,
but it may still contain errors.

For easier cross-reference, comments are included referring the section
of the standard where a rule is defined.

Rules pertaining to lexical structure (sec. 6.4), preprocessing (sec. 6.10)
and rules from sec. 7.24 and 7.31 (summarized in annex A.5) for various
"char-sequence"s have been commented out with leading "// ". The start and
end of the major parts are marked as:
```
//START_LEXICAL_GRAMMAR
//END_LEXICAL_GRAMMAR
//START_MAIN_GRAMMAR
//END_MAIN_GRAMMAR
//START_ATTRIBUTES_GRAMMAR
//END_ATTRIBUTES_GRAMMAR
//START_PREPROCESSING_GRAMMAR
//END_PREPROCESSING_GRAMMAR
//START_FLOATING_POINT_SEQ_GRAMMAR
//END_FLOATING_POINT_SEQ_GRAMMAR
```

All the kept rules in the MAIN_GRAMMAR have been reformatted and indented
similar (but not identical) to the formatting used in the C11 grammar. This
grep command will extract only the kept rules, and hints at the formatting:

     grep -E '^([A-Za-z_][A-Za-z_]*( *[/].*)?$|        [:|;])' C23.y

(Note that indentation is spaces only, no tabs. For diffing with the C11.y
grammar, if you wish to do so, either use diff -w or replace all indentation
with 8 spaces.)

I have left in some #include lines which refer to code I am working on. This
code is not provided as of yet. Edit to suit your needs. The Bison and flex
preambles are set up (as best as I could) for reentrant parsing and using
parse-param to provide a pointer to a structure for housekeeping and
returning a parse result. Likewise, the C23.l file contains a call to a
function make_cst_leafnode(). The result is kept in the yyextra variable,
so each token will have a reference to any preceding whitespace and
comments. Maybe one day I'll publish the code, but I suggest you just remove
what you can't use, and/or adapt it to your needs.

The lexer has been "casually" and minimally updated for C23, but I have not
implemented the exact lexical rules as given by n3220.pdf yet. Mainly it is
the original C11.l file, with some modifications and a few extra keywords.

Worth of note - and help or advice is most welcome! - is that I have added
an ATTRIBUTE_HACK token. With the grammar rules as provided by the standard,
I get 57 shift/reduce conflicts from Bison. The hack defines a '[' followed
by whitespace and another '[' as the token ATTRIBUTE_HACK, and the '[' '['
in the rule attribute_specifier is changed to this. This seems to help with
the shift/reduce conflicts (down to "just" 37!) and more correct parsing
(although this is still completely untested!) I would appreciate any
assistance in making the grammar correct and unambiguous.


13. May, 2024, Lasse Hillerøe Petersen

(original readme text by Thiago Adams follows:)

# C23 grammar for parser generators

This is a C23 grammar extracted from n3096 C 23 draff 
written in a text format matching almost 1:1 the pdf version.

![](grammar.png)

grammar

Less than 3 chars means token. For instance [ is a token.
opt is reserved.
"literals" are tokens for instance "goto".
identifiers are productions.

```

```
