Monads in OCaml and F#
======================

I've been reading the original monad paper called
[Monads for Functional Programming][MFP] and as an
excercise to understand monads better I decided to
re-implement the example code in OCaml. As I am also
learning F# I decided to convert it to F# as well

Notable deviation from paper is that I'm using the
(nowadays) more conventional `>>=` operator instead
of `⋆` as well as `return` instead of `unit`
(`return'` in case of F#).

[MFP]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
