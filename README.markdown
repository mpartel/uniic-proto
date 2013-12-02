This is a prototype of a programming language I designed for my master's thesis.

It is a limited imperative language, with variable reassignment, while loops,
that sort of stuff, but the twist is, it gets
[compiled to a purely functional language](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.3282).
Its aim was to study to what extent one could make functional programming
"look imperative", and on the other hand,
which desireable imperative patterns would not be nicely expressible
in a purely functional setting.

Uniqueness typing, inspired by [Clean](http://wiki.clean.cs.ru.nl/),
was chosen for modeling I/O and mutable data structures instead of monads
because it seemed to fit the imperative style better.
It was extended with a simple borrowing system that allows passing
unique values as parameters and still using them afterwards.
