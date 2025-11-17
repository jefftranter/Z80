These are some simple examples of C programs that are cross-compiled
with z88dk and will run on a Heathkit H89 running Heathkit's HDOS
operating system.

The HDOS support is a work in progress. It currently supports basic
console i/o, startup, command line arguments, making HDOS system
calls, and termination.

File i/o is in progress.

You can disassemble a binary by using a command like:
  z88dk-dis -x a.map -o 0x2280 a.bin

For more background, see https://github.com/z88dk/z88dk/wiki/Classic--Homebrew
