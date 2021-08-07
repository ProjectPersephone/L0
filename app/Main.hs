-- was module AUG

module Main where

import Lib

-- import Options.Applicative
-- import Data.Semigroup ((<>))
import Debug.Trace

-------------------------------------------------------------------------------
-- This file contains Haskell source code for the programs described in:
--
--               Using Types to Parse Natural Language
--
--         Mark P. Jones           Paul Hudak and Sebastian Shaumyan
--     University of Nottingham            Yale University
--      Nottingham, England           New Haven, Connecticut, USA
--
-- Proceedings of the Glasgow Workshop on Functional Programming, July 1995.
-- Published by Springer-Verlag in the Workshops in Computer Science series.
-------------------------------------------------------------------------------

----- left over from raw src in paper:
--module AUG where 

import Data.List(transpose)

-- Types and Trees: -----------------------------------------------------------

-- AUG types: term (T), sentence (S), and O (AUG type, AUG type)
--     need to comply with Eq interface
--
data Type = T | S | O Type Type   deriving Eq

-- text output representation
--     (not clear where d operator precedence comes from)
--
instance Show Type where
  showsPrec d T       = showString "T"
  showsPrec d S       = showString "S"
  showsPrec d (O x y) = showString "O" . shows x . shows y

-- tree of Types, each node can be an atomic string (leaf) or
--	a tree node tagged with Before/After constructors.
--	See function "app", below,
--	After and Before at least tell us whether a given
--	a Typed_Tree is the result of a forward or back application.
--	https://wiki.haskell.org/Constructor

type Typed_Tree
      = (Tree,[Type]) 
data Tree
      = Atom String | After Typed_Tree Typed_Tree | Before Typed_Tree Typed_Tree

-- Sentences: -------------------------------------------------------

type Sentence = [Typed_Tree]

-- words():
--  "breaks a [white-space-separated] string up into a list of words"
--
--   https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:words
--   (Will have to do until I crunch down to the phonemic/alphabetic level.)

-- So sentence() makes a list of Typed_Tree nodes from each word in a string.

sentence :: String -> Sentence

sentence = map morpheme_to_Typed_Tree . words
 where morpheme_to_Typed_Tree w = (Atom w, morpheme_types w)


-- Enumerating Types/Trees: ---------------------------------------------------

-- "The function splits is used here to generate all pairs of non-empty lists
-- (ls,rs) such that ls ++ rs = ts." -- zero pronouns or zero anything?

-- The typed trees of a sentence is all typed trees such that
--   when the words are split in two, and the two parts form typed trees,
--   the two typed trees can (syntactically) combine

typed_trees_of       :: Sentence -> [Typed_Tree]

typed_trees_of [one_typed_tree]               -- singleton
    = [one_typed_tree]
typed_trees_of this_sentence
    = [ tt | (before_part,after_part) <- all_splits_of this_sentence,
             before  <- typed_trees_of before_part,
             after   <- typed_trees_of after_part,
             tt <- combination_of before after
      ]

-- all_splits_of - make a list of pairs of all "before" parts with
--    all corresponding "after" parts.
--
--    Uses zip([...],[...]) to pair the befores and afters

all_splits_of       :: [a] -> [([a],[a])]

all_splits_of these_trees
    = zip (all_before_parts_of these_trees) (all_after_parts_of these_trees)

-- all_before_parts_of - make a list of sublists copied from
--     all but the last element in a list of typed trees
--     ranging from singleton of the first to all but last
-- 
--     Uses map (first_elt:) on all befores

all_before_parts_of :: [a] -> [[a]]

all_before_parts_of [one_typed_tree]
   = []    -- nothing after first

all_before_parts_of (one_typed_tree:more_typed_trees)
   = map (one_typed_tree:) ([]:all_before_parts_of more_typed_trees)

-- all_after_parts_of - make a list of sublists copied from
--     all but the first element in a list of typed trees
--     to the end of that list
--     
--     Uses simple (head:tail) list construction

all_after_parts_of  :: [a] -> [[a]]

all_after_parts_of [one_typed_tree]
    = []   -- nothing after last

all_after_parts_of (one_typed_tree:more_typed_trees)
    = more_typed_trees : all_after_parts_of more_typed_trees

-- The combination_of a pair of Typed Trees is
--   the union of applying
--      the "before" Typed Tree to the "after" Typed Tree
--    and
--      the "after" Typed Tree to the "before" Typed Tree;
--
--   The After/Before constructors seem to just tag the
--   results with the direction of application.
--
--   Uses list concatenation (++) for union.

combination_of      :: Typed_Tree -> Typed_Tree -> [Typed_Tree]

combination_of before after
  = app After before after ++ app Before after before

-- app:
--
-- This part of the code
--     (before_or_after (this_tree,[O x y]) (some_other_tree,[x]), [y])
-- says if   this_tree has type       O x y
--      and  some_other_tree has type x,
--      then application of this_tree to some_other_tree has type y.
-- 
-- E.g. if this_tree has type OTS and some_other_tree has type T,
-- the composition this_tree . some_other_tree has type S.
--   Reduction:
--      (Tr1,OTS) app (Tr2,T) -> (Tr3,S)
 
    -- Naively
    --
    -- app before_or_after
    --     (this_tree,this_trees_types)
    --     (other_tree,other_trees_types)
    -- {
    --	    list = []
    --      for all (O type1 type2) in this_trees_types
    --        if type1 is in other_trees_types
    --             list += ( before_or_after
    --                       (this_tree, [O type1 type2])
    --                       (other_tree, [type1])
    --                       [type2]
    --                     )
    -- }
    --------------------------------
    -- ... is O(n x m), n=len(this_trees_types), m=len(other_trees_types)
    -- so, try a recursive procedure:
    --------------------------------
    --
    -- app before_or_after
    --     (_,[])
    --     (_,_)
    -- = []
    --
    -- app before_or_after
    --     (this_tree,[O type1 type2:the_rest])
    --     (other_tree,other_trees_types)
    -- {
    --    L = app before_or_after (this_tree (the_rest))
    --                            (other_tree,other_trees_types)
    --    if type1 is in other_trees_types
    --       return [( before_or_after
    --                       (this_tree, [O type1 type2])
    --                       (other_tree, [type1])
    --                       [type2]
    --               )
    --               : L
    --              ]
    --    else 
    --       return L
    -- }
    --------------------------
    -- Tail-recursive version:
    --------------------------
    -- app before_or_after
    --     (this_tree,[O type1 type2:the_rest])
    --     (other_tree,other_trees_types)
    -- =
    --    if type1 is in other_trees_types
    --       [( before_or_after
    --                 (this_tree, [O type1 type2])
    --                 (other_tree, [type1])
    --                 [type2]
    --        )
    --        : 
    --        app before_or_after (this_tree (the_rest))
    --                            (other_tree,other_trees_types)
    --        ]
    --    else
    --       app before_or_after (this_tree (the_rest))
    --                           (other_tree,other_trees_types)
    -- 
    -- app before_or_after
    --     (this_tree,[T:the_rest])
    --     (other_tree,other_trees_types)
    -- = 
    --       app before_or_after (this_tree (the_rest))
    --                           (other_tree,other_trees_types)
    --
    -- app before_or_after
    --     (this_tree,[])
    --     (other_tree,other_trees_types)
    --     = []
    --
    -------
    -- The above could probably be done more compactly with a case expression
  
app :: 
    (Typed_Tree -> Typed_Tree -> Tree)
    -> Typed_Tree -> Typed_Tree
    -> [Typed_Tree]
  
app before_or_after (this_tree,this_trees_types) (other_tree,other_trees_types)
  = [ ( before_or_after             -- just carry the constructor tag in
           (this_tree,[O type1 type2])
           (other_tree,[type1]),
        [type2]
      )
    | (O type1 type2) <- this_trees_types,
      tx <- other_trees_types,
      type1 == tx
    ]

-- A More Sophisticated Algorithm: -------------------------------------------


-- cache() maps sentences to an upper-left triangular array of
-- sub-sentences and morphemes.
--
-- Think of the triangle as being tilted 45 degrees clockwise.
--
-- Each element of the array is a digest of all the words spanned
-- from the diagonals below the element. Thus, each cache element
-- represents a subcache.
--
-- The array is represented by a list (of "rows") of lists
-- (each in a "column") of lists of Sentences.
--
-- Each Sentence as originally input starts out as a
-- list of Typed_Tree nodes, one for each morpheme. (I suppose
-- that "[Typed_Tree]" could be just "Sentence" but "Sentence"
-- is a misnomer, since you can also have terms.)

-- A triangular array is a list of list of lists of typed trees,
-- so I guess you have to peel away some layers to get to any sentences
-- derived at the upper left corner (or apex if you look at it as a
-- heap structure.)

fast_Typed_Trees       = head . head . cache

-- if a "sentence" [(Typed_Tree,[Type])] is singleton, its cache entry
-- is just itself, located in the pyramid/array.
--
-- if a "sentence" is longer ...
-- first build the cache out for all but the first element
-- then tack it on to something like this:
-- 
--   Try the first element of the "sentence" with everything
--   in the cache built out for all-but-first.
--
--   But what's that "transpose" about? It's array transposition
--   when an array is represented as a list of lists. E.g.,
--
--      Input: transpose [[1,2,3],[4,5,6],[7,8,9]]
--      Output: [[1,4,7],[2,5,8],[3,6,9]]
--
--   This flips the cache built up (so far) around the "triangle tip".
--   In the triangle, this seems to recurse down to the
--   end of a row (side of the triangle) while the "transpose"
--   seems to be about recursion down to the end of the column
--   (other side of the triangle) adding the head of the
--   Typed_Tree list to the cache on the way up.
--
-- With a pair that maps Typed_Tree nodes onto cache entries,
-- produce a row(?) of Typed_Tree.
--
-- "is" is cache entries built from a and the tail of the
-- cache so far.
-- 
-- g pairs off is and ts elements, one-by-one
-- and sees if they combine; if they do, add them to
-- some kind of result set for g. The "reverse is"
-- may reflect how the recursion builds up "is" in
-- the "wrong" order.
--
-- at a leaf: just cache the morpheme there
--
-- at a branch:
--    is = what's built from a branch and the rest of the morphemes
--         from the branch point
--    g  = take what's built there,
--

-- cache sentence "I am ill" =
--   sentence "I am ill" = [(I,[T]),(am,[OOTTOTS]),(ill,[OTT])]
--
--     [(am ill I,[S])]      [(am ill,[OTS])]    [(ill,[OTT])]
--
--     []                    [(am,[OOTTOTS])]
--
--     [(I,[T])]

type Row   = [[Typed_Tree]]
-- type Cache = [[[Typed_Tree]]]
type Cache = [Row]

-- cache: turn a list of Typed_Trees into a cache

cache :: Sentence -> Cache

cache [one_word]
   = [[[one_word]]]

cache (this_typed_tree:the_rest)
   = [build this_typed_tree (transpose rs)] ++ rs
     where rs = cache the_rest

-- The call to build will be wrapped in [...] so it's really
-- returning a Cache result.

build :: Typed_Tree -> Cache -> [[Typed_Tree]]

build typed_tree []
  = [[typed_tree]]

-- guessing that "is" meant something like "intermediates"
build typed_tree (row:rest_of_rows)
  = g (reverse intermediates) row : intermediates
    where intermediates       = build typed_tree rest_of_rows
          g is row = [ r | (i,t) <- zip is row,
                        ti   <- i,
                        tt   <- t,
                        r    <- combination_of ti tt ]

explain :: String -> IO ()

explain = putStrLn . unlines . map draw_Typed_Tree . fast_Typed_Trees . sentence

-- Drawing trees: -------------------------------------------------------------

-- on Japanese Windows, the backslash comes out as a yen symbol.
-- possible workaround: https://hackage.haskell.org/package/unicode-show

draw_Typed_Tree    :: Typed_Tree -> String
draw_Typed_Tree tr  = unlines ((show (tree tr) ++ ":\n") : ptr)
 where (_,_,_,ptr)   = tpic tr
       tpic (tr,ty)  = oneAbove (pic tr) (label (show ty))
       pic (Atom w)  = label w
       pic (After l r) = sideBySide (tpic l) (tpic r)
       pic (Before l r) = sideBySide (tpic r) (tpic l)
 
label a = (1,  l, c, [ " " ++ a])
 where l = 1 + length a
       c = 1 + l`div`2

sideBySide (hl,wl,cl,pl) (hr,wr,cr,pr) = (h+1,w,c,p)
 where h   = hl `max` hr
       w   = wl + wr
       c   = (cl + wl+cr+1) `div` 2
       p   = zipWith (++) (copy (h-hl) (copy wl ' ') ++ pl)
                          (copy (h-hr) (copy wr ' ') ++ pr) ++ [tie]
       tie = copy (cl-1)   ' ' ++ "\\" ++
             copy (c-cl-1) '_' ++ "_" ++ copy (cr+wl-c-1) '_' ++
             "/" ++ copy (wr - cr) ' '

oneAbove (ht,wt,ct,pt) (hb,wb,cb,pb) = (ht+hb, w, c, p)
 where c     = ct `max` cb
       w     = c + ((wt-ct) `max` (wb-cb))
       p     = addMargins (c-ct) ((w+ct)-(wt+c)) pt ++
               addMargins (c-cb) ((w+cb)-(wb+c)) pb

addMargins l r = map (\s -> lm ++ s ++ rm)
 where lm = copy l ' '
       rm = copy r ' '

copy n x = take n (repeat x)

-- A simple lexicon, sufficient for examples in the paper: --------------------

-- Morphemes are represented as Strings for now
--
type Morpheme       = String

-- a morpheme can have more than one Type (each = T, S, or O Type Type)
--
morpheme_types  :: Morpheme -> [Type]

-- the types of a morpheme can be found with dictionary lookup
--
morpheme_types w = find_morpheme w dictionary

-- Dictionaries can be empty or nodes paired with typelists and subdictionaries
--
data Dictionary = Nil | Node Morpheme [Type] Dictionary Dictionary

-- Dictionaries are instances of Show: here's the output algorithm
-- (I guess String gets us Ord, for ordered insertions and Show-ing.)
--
instance Show Dictionary where
   showsPrec d Nil = id
   showsPrec d (Node w ts l r)
     = shows l .
       showString w . showString " :: " . shows ts . showChar '\n' .
       shows r

-- Adding a type/word(=morpheme) pair to a dictionaries yields a new dictionary
-- If the morpheme already has a dictionary entry, add the type-list to
-- its existing type-list; else add to the left/right branch depending on
-- lexicographic ordering for String
--
add_morpheme         :: Type -> Morpheme -> Dictionary -> Dictionary
add_morpheme t w Nil  = Node w [t] Nil Nil
add_morpheme t w (Node v ts l r)
        | w == v = Node v (t:ts) l r
        | w <  v = Node v ts (add_morpheme t w l) r
        | w >  v = Node v ts l (add_morpheme t w r)

-- Return the type-list of a possible morpheme (String, in this implementation)
-- or an empty list if not found. (Rename to typesOfThisMorpheme?)
--
find_morpheme        :: Morpheme -> Dictionary -> [Type]
find_morpheme w Nil   = []
find_morpheme w (Node v ts l r) 
        | w == v = ts
        | w <  v = find_morpheme w l
        | w >  v = find_morpheme w r

-- add_words_like takes a list of morphemes, a Type (S, T, O Type Type), and a dictionary
-- and creates new dictionary with entries for all of the listed morphemes,
-- of that same Type.
--
add_words_like      :: [Morpheme] -> Type -> Dictionary -> Dictionary
add_words_like some_words this_type dictionary =
        foldr (add_morpheme this_type) dictionary some_words

-- add_some takes a list of morpheme-type pairs and makes a dictionary
--
add_some      :: [(Morpheme,Type)] -> Dictionary -> Dictionary
add_some words_paired_with_types dictionary =
        foldr ($) dictionary [
           add_morpheme this_type a_word
              | (a_word,this_type) <- words_paired_with_types
        ]

-- dictionary builds the whole dictionary, attributing types to morphemes
-- as needed.
--
dictionary :: Dictionary 
dictionary  = add_words_like nouns               T
            $ add_words_like intransitive_verbs  (O T S)
            $ add_words_like transitive_verbs    (O T (O T S))
            $ add_words_like adjectives          (O T T)
            $ add_words_like adverbs             (O (O T S) (O T S))
            $ add_some other_morphemes
            $ Nil

-- nouns but with no personal/impersonal distinctions
nouns       = ["hat", "wine", "boy", "girl", "father", "mother",
                "Boston", "I", "friend", "word", "home", "he", "enemy",
                "Moscow", "London", "New_Haven", "film", "John", "dog", "Mary"
              , "they" -- hm, also no plural distinctions
              ]
-- verbs but with no tense distinctions
intransitive_verbs = ["came", "lives", "comes", "saw", "slept", "runs"]
transitive_verbs   = ["knew", "see", "knocked", "thinks", "was", "killed", "loves" ]
-- some iffy adjectives
adjectives   = ["the", -- iffy
                "my", "his", "her", -- iffy for IS(MINE)
                "old", "ill",
                "a", -- also iffy
                "young",
                "exciting", "interesting",
                "this", -- NSM
                "small" -- NSM
                ]

-- ... and adverbs
adverbs      = ["home", "late", "early", "soundly", "quickly"]

-- ... and lots of punting
other_morphemes   = [("that", ost), ("in", otd1), ("tomorrow", oss),
               ("will", d2), ("down", d2), ("was", oap1),
               ("very", oaa), ("today", d1), ("who", otop1t),
               ("from", otd1), ("which", otop1a), ("is", oap1)
             , ("am", oap1)
             , ("are", oap1)
               ]

oss         = O S S                -- e.g., one-word time adjunct ("tomorrow")
ost         = O S T                -- "that", e.g., relative clause
a           = O T T                -- adjective, can combine with other adjectives
p1          = O T S                -- predicate, can apply adjectival phrase?
p2          = O T p1               -- pred w/more terms??? -- two-place???
p3          = O T p2               -- ... yet more terms??? -- three-place???
d1          = O p1 p1              -- ??? "today" (but not "tomorrow"???)
d2          = O p2 p2              -- ??? "will" AND "down"?
otd1        = O T d1               -- "from", "in"
oaa         = O a a                -- "very"
otop1t      = O T (O p1 T)         -- "who"
oap1        = O a p1               -- "was", "is"
otop1a      = O T (O p1 a)         -- "which"

test1       = "the boy came home late"
test2       = "my friend lives in Boston"
test3       = "tomorrow I will see my old friend"
test4       = "he knew that his mother was ill"
test5       = "he knocked down his enemy"
test6       = "the film was very interesting"
test7       = "my old friend who comes from Moscow"
test8       = "my old friend thinks that the film was exciting"
test9       = "the film which he saw today was very interesting"
test0       = "my old friend who comes from Moscow thinks that the film \
              \which he saw today was very interesting"
test_a      = "I was ill"
test_b      = "I I" -- no output
test_c      = "ill was I" -- works, different trees
test_d      = "was ill I" -- works, different trees

-- was in "sentence" section, but probably only to be quotable in the literate
-- programming document that mentions these

myfriend  = "my friend lives in Boston"
oldfriend = "my old friend who comes from Moscow"
-- Changed "long" to "verylong" because of ambiguity with Options.Applicative.long
-- but I think this is leftover test code; the same sentence is test0, below
verylong      = "my old friend who comes from Moscow thinks that\
           \ the film which he saw today was very interesting"

-- Miscellaneous utilities: ---------------------------------------------------

tree          :: Typed_Tree -> Tree 
tree (tr, ty)  = tr

instance Show Tree where
  showsPrec d (Atom s)           = showString s
  showsPrec d (After t (Atom s,_)) = shows (tree t)  .  showChar ' '    .
                                   showString s
  showsPrec d (After t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'
  showsPrec d (Before t (Atom s,_)) = shows (tree t)  .
                                   showChar ' '    .
                                   showString s
  showsPrec d (Before t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'

-------------------------------------------------------------------------------


main :: IO ()
-- main = explain test_a
main = explain test_d

