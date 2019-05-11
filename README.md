# Overview

[![Build Status](https://travis-ci.org/ReedOei/Enki.svg?branch=master)](https://travis-ci.org/ReedOei/Enki)

Enki is intended to be a logic language for teaching programming to people with no programming experience.
It is still a work in progress, and it not ready to be used for this purpose.

To run an enki file (see examples below or in the [examples](https://github.com/ReedOei/Enki/tree/master/examples) directory, use the following command:

```bash
enki run FILENAME
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

```
A divides B if B = A*N.

multiples of A in List is
    when List = cons H T, A divides H then cons H (multiples of A in T).
    when List = cons H T then multiples of A in T.
    otherwise then empty.

answer less than N is
    ThreeMult = multiples of 3 in range 1 to N,
    FiveMult = multiples of 5 in range 1 to N,
    FifteenMult = multiples of 15 in range 1 to N,
    sum ThreeMult + sum FiveMult - sum FifteenMult.

display as text answer less than 999.
```

