Use [Stack](http://docs.haskellstack.org/en/stable/README.html) to build.

To run the doctests:

    stack test euler:doctest
    
To check problems with fast solutions:

    stack test euler:fast

To check a single problem *n*:

    stack test euler:one --test-arguments=n
