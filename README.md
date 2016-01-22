This is a small project I'm working on in my spare time in order to learn
functional programming techniques as well as Haskell specific ideas. Why am
I learning Haskell, you ask? Becasue it's cool. Simply put, I think the strict
paradigm enforcement in Haskell makes me think about problems in different
ways that has a positive impact on how I write code in other languages. Before
writing in Haskell I hadn't really considered the benefits of pure functions,
functional operations on collections, etc.


This is chess program in haskell with the following features:

 - Supports two players
 - Enforces rules
    - Pieces can only move to legal spaces
    - When pieces are taken they are accounted for
    - En Passant rules (eventually)
    - Pawn promotion
 - Board is drawn with terminal output
 - Maybe a simple AI to play against you. Sounds like a fun project
