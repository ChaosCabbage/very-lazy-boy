# A gameboy CPU emulator in Haskell
I'm a complete novice in both emulator writing and Haskell writing. For that reason, I thought this would be a good idea, maybe...

Notice that I say "CPU emulator": I don't imagine this will go as far as graphics. I'll probably get bored.
Hopefully, I'll document everything to my own satisfaction.

I started off by looking at this:
https://github.com/trez/LazyNES

First, I decided that the whole CPU monad thing was very complicated, and tried to do it by simply passing the CPU object everywhere.
As I learn more, I've been realising that this is exactly what the monad abstracts.
I tried `Reader (CPUEnvironment s) (ST s a)` but it seems difficult to make it work. The compiler complains that the two "s" types might be different. Manually writing the monad seems to be the way to go.

I've been trying to avoid "do" notation as much as I can. I'd rather figure out the binding functions. It seems easier to figure out what's really going on that way.

**Current status:** I'd say 0.02% complete.
