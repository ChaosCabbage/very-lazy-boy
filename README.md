# A gameboy CPU emulator in Haskell
I'm a complete novice in both emulator writing and Haskell writing. For that reason, I thought this would be a good idea, maybe...

Notice that I say "CPU emulator": I don't imagine this will go as far as graphics. I'll probably get bored.
Hopefully, I'll document everything to my own satisfaction.

I started off by looking at this:
https://github.com/trez/LazyNES

First, I decided that the whole CPU monad thing was very complicated, and tried to do it by simply passing the CPU object everywhere.
As I learn more, I've been realising that this is exactly what the monad abstracts, and I'll probably do the same thing.

**Current status:** I'd say 0.01%
