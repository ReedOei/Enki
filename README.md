# Overview

[![Build Status](https://travis-ci.org/ReedOei/Enki.svg?branch=master)](https://travis-ci.org/ReedOei/Enki)

Enki is intended to be a logic language for teaching programming to people with no programming experience.
It is still a work in progress, and is not yet ready to be used for this purpose.

To run an Enki file (see examples below or in the [examples](https://github.com/ReedOei/Enki/tree/master/examples) directory, use the following command:

```bash
enki run FILENAME [OUTPUT FILE PATH]
```

To compile the file:
```bash
enki compile FILENAME <OUTPUT FILE PATH>
```

To see the code output, run:

```bash
enki FILENAME
```

# Installation

You will need [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Then you can run:

```bash
git clone https://github.com/ReedOei/Enki
cd Enki
stack install
echo "export ENKI_PATH=\"$(pwd)/libraries/base\"" >> ~/.bashrc
```

# Examples

Sample code to solve [Problem 1](https://projecteuler.net/problem=1) on [Project Euler](https://projecteuler.net/):

```enki
A divides B if B = A*N.

multiples of A in List is
    when List = H :: T, A divides H then prepend H to multiples of A in T;
    when List = H :: T then multiples of A in T;
    when List = empty then empty.

answer less than N is
    ThreeMult = multiples of 3 in range 1 to N,
    FiveMult = multiples of 5 in range 1 to N,
    FifteenMult = multiples of 15 in range 1 to N,
    sum of ThreeMult + sum of FiveMult - sum of FifteenMult.

display as text answer less than 999.
```

Alternatively, we may use partial application and higher order functions to rewrite `multiples of _ in _` as:

```enki
multiples of A in List is filter List with (A divides _).
```

Sample code to solve [Problem 2](https://projecteuler.net/problem=2) on [Project Euler](https://projecteuler.net/):

```enki
fib nums A B Limit is
    when A > Limit then empty;
    when A <= Limit then prepend A to fib nums B (A + B) Limit.

fib seq up to N is fib nums 0 1 N.

display as text sum of multiples of 2 in fib seq up to 4000000.

```

