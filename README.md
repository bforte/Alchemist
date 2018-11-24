# crn - a non-deterministic esolang based on chemical reaction networks

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

For more information, see
[*examples/*](https://github.com/bforte/crn/tree/master/examples).
