# A gameboy CPU emulator in Haskell
I'm a complete novice in both emulator writing and Haskell writing. For that reason, I thought this would be a good idea, maybe...

I'll probably get bored, but hopefully, I'll document everything to my own satisfaction.

Right now the main exe is a debugger: 
* Press return to step one instruction.
* Type MEM, return, then a hex memory address to view the contents of that address.
* Type QUIT to exit.

## Detsils/Blog

I started off by looking at this:
https://github.com/trez/LazyNES

First, I decided that the whole CPU monad thing was very complicated, and tried to do it by simply passing the CPU object everywhere.
As I learn more, I've been realising that this is exactly what the monad abstracts.
I tried `Reader (CPUEnvironment s) (ST s a)` but it seems difficult to make it work. The compiler complains that the two "s" types might be different. I think you would need to write "forall s." everywhere.
Manually writing the monad seems to be the way to go. 

Now I've written this CPU monad, it's pretty cool. It's transformed everything into imperative code.
I need to make sure to avoid spaghetti!
I've been trying to avoid "do" notation as much as I can. I'd rather figure out the binding functions. It seems easier to figure out what's really going on that way.

I tried to make the internal functions match the gameboy assembler, e.g. there is a `ld` function to handle all the different LD instructions and the arguments are functions to get the source and destination. It's not really working. All the different versions take a different number of cycles so I still have to hack those in. 

**Current status:** I'd say 3% complete.
Tetris is stuck in a loop again because I haven't done interrupts.
