https://projecteuler.net/problem=53

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, <sup>5</sup>C<sub>3</sub> = 10.

In general,

> <sup>n</sup>C<sub>r</sub> = (n!)/(r!(nr)!),

where r &le; n, n! = n&times;(n1)&times;...&times;3&times;2&times;1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: <sup>23</sup>C<sub>10</sub> = 1144066.

How many, not necessarily distinct, values of <sup>n</sup>C<sub>r</sub>, for 1 &le; n &le; 100, are greater than one-million?
