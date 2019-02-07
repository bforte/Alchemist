# Alchemist - a non-deterministic esolang based on chemical reaction networks

## Basics

A program consists of rules

    A1 + ... + An -> B1 + ... Bm

There is a state called *universe* of atoms, if there are enough atoms of
`A1..An` then such a rule is *applicable*. The program will start with some
initial state and iterate indefinitely until no rules are applicable. From all
the applicable rules a random one will be chosen and applied.

There are two special types of atoms which can only appear on the right hand
side of a reaction:

 - `Out_"some string"`: Prints `some string` to *stdout*
 - `In_someAtom`: Reads an integer from *stdin* and adds that many atoms of
   type `someAtom`

Regular atoms consist can only contain characters `[0-9A-Za-z_]`.

## Multiplicity

The atoms can have a number in front of them (eg. `2H + O -> H2O`) which is
short-hand for `H + H + O -> H2O`. This is the reason why atoms cannot start
with a number.

One can also write a `0` in front of an atom which means the rule is only
applicable if there is no such atom at all, this can be used for control-flow.

## Initial Universe

The initial universe is either determined by user inputs, constant inputs or
both:

  - **user inputs** are given via the commandline when invoking the
    interpreter. The arguments are like the left hand-side of a rule (LHS),
    for example `alchemist MyFantasticProgram.crn "101x + 3a"` will use an
    initial universe with 3 `a`-atoms 101 `x`s. They  can also be be separate
    arguments, so `alchemist MyFantasticProgram.crn 101x 3a` will do the same.
  - **constant inputs** are given in the source code directely. These inputs
    are also of the form LHS and are specified after the rules,
    separated with a `!`

Usually these two will be merged, ie. the interpreter adds the numbers of the
inputs of them, you can override the constant inputs completely by using the
`--override` flag.

By default the universe will initially contain the atom `_`.

For more information, see the [*Examples*](https://github.com/bforte/alchemist/tree/master/examples).

## Interpreter

    bmo@Ooo % alchemist -h
    usage: alchemist [-h] [-s SEED] (-e EXPR | FILE) INPUTS
      -e         --expression     evaluate expression
      -o         --override       override constant inputs (merges by default)
      -s SEED    --seed=SEED      set seed
      -d[LEVEL]  --debug[=LEVEL]  set debug level (0 to 2)
      -h         --help           print this help

### Seeding

The `SEED` can be any string to achieve deterministic results, the program will
print the used seed to *stderr*. For example:

    seed: "1957628721 1"

If we now run `alchemist -s "1957628721 1" Program.crn` we will get the same
results as in the previous run.

### Debugging

Debug levels:
  - **0:** quiet; prints only the used seed
  - **1:** helpful; like **0** but also prints the remaining universe at the end
  - **2:** noisy; like **1** but also prints each applied rule
