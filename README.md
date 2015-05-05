Monads in OCaml
===============

I've been reading the original monad paper called
[Monads for Functional Programming][MFP] and as an
excercise to understand monads better I decided to
re-implement the example code in OCaml.

Notable deviation from paper is that I'm using the
(nowadays) more conventional `>>=` operator instead
of `⋆` as well as `return` instead of `unit`.

[MFP]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
