# A gameboy emulator in Haskell
I'm a complete novice in both emulator writing and Haskell writing. For that reason, I thought this would be a good idea, maybe...

I'll probably get bored, but hopefully, I'll document everything to my own satisfaction.

Right now the main exe is a debugger: 
* Press return to step one instruction.
* Type MEM, return, then a hex memory address to view the contents of that address.
* Type QUIT to exit.

## Buildy & Runny
Requires [stack](https://docs.haskellstack.org)
```
stack build
stack exec gameboy-debugger-exe
```
Couldn't get much easier than that.

## Details/Blog

I started off by looking at this:
https://github.com/trez/LazyNES

First, I decided that the whole CPU monad thing was very complicated, and tried to do it by simply passing the CPU object everywhere.
As I learn more, I've been realising that this is exactly what the monad abstracts.
I tried `Reader (CPUEnvironment s) (ST s a)` but it seems difficult to make it work. The compiler complains that the two "s" types might be different. I think you would need to write "forall s." everywhere.
Manually writing the monad seems to be the way to go. 

Now I've written this CPU monad, it's pretty cool. It's transformed everything into imperative code.
I need to make sure to avoid spaghetti!

There was a point when I could run a lot of instructions, but the emulator crashed when the game tried to access unusable
memory at 0xFEFF. Clearly something was wrong, but I didn't know what. If I was a good boy, I would be writing unit tests,
but I'm not familiar with any haskell testing frameworks, and anyway, writing a gameboy debugger was much more fun!
It's pretty simple: If you press enter, run the gameboy one step, then exit the ST monad and freeze the gameboy state.
On the next step, the state is unfrozen.

**Current status:** Nearly got my first frame!

- Implemented the framework for most instructions. Haven't actually gone and filled them all in.
- Implemented a mechanism to read and write to IO ports.
- Implemented interrupts.
- Badly need some tests

Current problem: Tetris jumps into the ROM1 bank, which I haven't loaded! It `NOP`s for a while and then crashes.

**Current questions:**
I'm concerned that exiting the ST monad too often will impact performance. Maybe the graphics can go _inside_ the ST monad?
Or maybe I don't have to worry too much. It's only a gameboy, after all.
