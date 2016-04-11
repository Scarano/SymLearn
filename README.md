# HAL

Implementation of the higher-order abstraction learning (HAL) model.

There is an informal description of the model at [astoundment.com/HAL.html](http://astoundment.com/HAL.html).

This code probably still won't make much sense until I add more comments. If you're brave, however, start with `src/symlearn/postsearch.clj`. The function `run-hal` is where the top-level iteration happens, and is designed to look as much like pseudo-code as possible.

## Usage

```
lein javac
lein run -m genlearn.postsearch/-main hal.config
```

*(TODO: Explain what the output means.)*

## License

All code copyright © 2016 Samuel Scarano, except for the few small sections of code explicitly indicated in comments as having been copied from another source. 

Distributed with NO LICENSE. So far, it's a solo project, and I don't see a need to choose a license yet.

That means that although I'm sharing the code publicly, I'm not signing away any use or distribution rights. You may view and fork my code in accordance with the GitHub terms of service. I also have no problem with you cloning and running my code, that's why it's here. In the unlikely case that you want to do anything else with it, please get in touch with me.



