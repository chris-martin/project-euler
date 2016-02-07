Use [Stack](http://docs.haskellstack.org/en/stable/README.html) to build.

Fast tests
-----------

The fast tests are everything in `Euler.Util`, and answer checks for
problems with fast answers.

To run all of the fast tests:

    stack test :Fast

Slow tests
----------

Answer checks for problems with slower answers are separated into
their own test suites to be run individually.

To run a particular slow problem's test:

    stack test :Problem[number]
