# automata
A simple library for defining, simulating and visualizing automata.
## Installing automata
### Compiling from source
The easiest way to build automata from source is to use [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. Change to the automata source directory and issue the following commands:
   ```haskell
   stack setup
   stack install
   ```
## Roadmap
This is the roadmap of automata for the next step.
- [ ] Support DFA, NFA, EpsNFA and RE (In progress)
- [ ] Conversion between the different forms (In progress)
- [ ] Minimization of DFA's
- [ ] Test emptiness, membership and equivalence
- [ ] Algebraic operations on automaton
- [ ] Generate graphviz or tikz code to visualize the automaton