L0 is natural language software based on Natural Semantic Metalanguage (NSM). https://en.wikipedia.org/wiki/Natural_semantic_metalanguage

At the moment, the code here is for a parsing technique in the Applicative Universal Grammar (AUG) paradigm in linguistics. https://en.wikipedia.org/wiki/Applicative_universal_grammar

The current effort is an incremental rewrite of a revival of the parser described (and coded in Haskell by Paul Hudak and Mark Jones) here: http://web.cecs.pdx.edu/~mpj/pubs/aug.html

The goal is a kind of NSM researchers' workbench -- a desktop app in which reductive paraphrases of natural language elements can be checked for consistency and translatability into other languages that have their NSM primes and syntax specified. This has been done in Prolog -- see https://github.com/Yakushima/NSM-DALIA. Haskell has a logic programming extension, but if it proves too hard to use, the system may be ported to Curry, which combines logic programming and functional programming paradigms in a more natural way. https://en.wikipedia.org/wiki/Curry_(programming_language)

Setup: Try https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html with "L0" for each "my-project".
