-----------
title: Top-level CSSR explainer (via patat)
author: Sam Stites
-----------

Stuff

-----------

# 0.0 Problem: Time series

Consider a time series:

    00011110001100011110000111101101111111111000110001101101100111100111100

How do we recover the underlying state machine? (see EP.dot)

        .<-----. .--- p(0.5)=1 --->.
    p(0.5)=>0   A                  B
        '----->' '<--- 0=P(1.0) ---'

How do we do it causally?

-----------

# 0.1 Introduction: Other variations

-----------

# 0.2 Introduction: CSSR

I need to recap on CSSR, but basically it's the version without recursive data structures.

Problems:
- Handling some edgecases get hacky
- Isabelle and Holmes robot scenario

-----------

# 1.0 ROCS

Three phases
- initializing a parse tree
- growing to a looping tree
- refinement of the looping tree

-----------

# 1.1 ROCS: Initializing a Parse Tree (1/1)

Causal is easy => find conditional probabilities. Take:

    00011110001100011110000111101101111111111000110001101101100111100111100

-----------

# 1.1 ROCS: Initializing a Parse Tree (1/3)

Slide a window of length 4 across it:

    00011110001100011110000111101101111111111000110001101101100111100111100
    0001          '--'            '--'          '--'
     0011          '--'            '--'          '--'
      0111    ...   '--'      ...   '--'    ...   '--'
       1111          '--'            '--'          '--'
        1110          '--'            '--'          '--'

You get the picture. It will return a stream of windows:

    0001,0011,0111,1111,1110,1100,1000,0001,0011,0110,1100...

-----------

# 1.2 ROCS: Initializing a Parse Tree (2/3)

Here are the first few length-four windows (since each element in a time series is called an
"event", we will call each window a "history"):

    History: 0001   History: 0011   History: 0111   History: 1111   History: 1110
    Timesteps:      Timesteps:      Timesteps:      Timesteps:      Timesteps:
      t3 = 1          t3 = 1          t3 = 1          t3 = 1          t3 = 0     <---most dependent
      t2 = 0          t2 = 1          t2 = 1          t2 = 1          t2 = 1
      t1 = 0          t1 = 0          t1 = 1          t1 = 1          t1 = 1
      t0 = 0          t0 = 0          t0 = 0          t0 = 1          t0 = 1     <---least dependent

Notice that t1 is conditioned on t0 being present (ie: 0 must exist in order for 00 to exist).
So we wind up with a heirarchy. This heirarchy can be codified into a tree data structure (called the "parse-tree"):

                {}
            _,-'  '-._
          ,'          '._
        0'               `1
    (counts=1)       (counts=3)
        |                 |
        |                 |
       10                11
    (counts=1)       (counts=3)
        |                 |    '.
        |                 |      '.
       110              011       111
    (counts=1)       (counts=1) (counts=1)
        |                |         |     '.
        |                |         |       '.
       1110             0011      0111      1111
    (counts=1)       (counts=1) (counts=1) (counts=1)

-----------

# 1.3 ROCS: Initializing a Parse Tree (3/3)

Over time, for a time series of 1000 events, this becomes:

                                              {}
                                          _,-'  '-._
                                        ,'          '._
                                       0               `1
                                 (counts=323)        (counts=674)
                                  ,'      '.                |
                                ,'          '.              |
                              00             10             1
                       (counts=159)      (counts=164)      ...
                          ,'  '.              |
                        ,'      '.            |
                    000'          `100       110
                 (counts=75)  (counts=84)(counts=164)
                _.-'  |            |         |    `-.
            _.-'      |            |         |       `-.
       0000'        1000          1100      0110       1110
    (counts=39) (counts=36)    (counts=84)(counts=78)(counts=86)

So if we want something like "what is the probability of 1000, given 000", we can instantly know that it is 36/75, or 48%

