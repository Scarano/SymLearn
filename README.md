# HAL

This is an implementation of a model for program synthesis from input/output examples. It is related to the EC algorithm presented in Dechter et al. (2013; “Boostrap Learning via Modular Concept Discovery”), which learns a prior over programs from a sample of positive instances, but differs in that the distribution over programs is conditioned on features of training examples. This it has in common with Programming-by-Example models such as Menon et al. (2013; “A Machine Learning Framework for Programming by Example”), but it differs from the latter in that the example features are also *learned* programs (not hard-coded).

Subprograms are considered to represent concepts; composing subprograms into a more complex program defines a more abstract concept. This is important to the motivation of the model, so its working title is Higher-order Abstraction Learning, or **HAL**.

This code probably still won’t make much sense until I a) write up a formal description of the model, and b) add more comments to the code. If you’re brave, however, start with [`postsearch.clj`](src/genlearn/postsearch.clj). The function `run-hal` is where the top-level iteration happens, and is designed to look as much like pseudo-code as possible.

So far, my experiments focus on the program domain of integer arithmetic (e.g., sum the primes up to *n*). I take advantage of the fact that large sets of synthetic problems in this domain can be easily created, by generating random type-safe expression trees. Although random programs are boring, the features needed to learn them are interesting. The expression language is a typed [combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic) with the routing combinators introduced by Liang et al. (2010; “Learning Programs: a Hierarchical Bayesian Approach”).

I’m still tuning parameters and tweaking the model....

## Usage

```
lein javac
lein run -m genlearn.postsearch/-main hal.config
```

*(TODO: Explain what the output means.)*

## License

All code copyright © 2016 Samuel Scarano, except for the few small functions in `util.clj` explicitly indicated in comments as having been copied from another source. 

Distributed with NO LICENSE at the moment. You may view and fork my code in accordance with the GitHub terms of service.



