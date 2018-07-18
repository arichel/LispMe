This is the ANSI C version of LispMe. Just like LispMe, it's
distributed under the GNU General Public License, which means
NO WARRANTY.

It's a subset of LispMe running on the Palm, there's no graphics,
user interface etc., just plain text IO (and file IO, too), so this
is pretty much a bare-bones R4RS Scheme.

Further notes and differences to LispMe on the Palm:

* The files should compile with any decent C compiler on Windows and Linux.
  This has been tested with GNU C (Cygwin).

* The invocation syntax is 
    LispMe <option>* <file>*  
  where <option> can be
    -h or -?   display help
    -a <size>  set atom size
    -m <size>  set heap size
    -r <size>  set FP size
  sizes are integers (bytes), optionally followed by k (kilo) or M (mega)
  <file>s are LispMe source files, which are loaded in the order given

* The file `.lispme.scm' is always loaded as the first file, put your
  standard definitions here.

* Of course you can use the file redirection modifiers of your
  shell:
    <infile >outfile 2>errfile
  All informational output (prompt, statistics, error messages) is
  output on stderr. 

* Commands are entered into the REP loop by typing an exclamation mark
  as the very first char, followed by the command:

  :r       resets LispMe's heap
  :q       quits LispMe
  :m       print memory statistics
  :n       list all defined names
  :p       pop last loaded file
  :l<file> load a file (no blanks before filename!)

* Any other input is interpreted as an expression to be evaluated

* You can interrupt evaluation/printing with Ctrl-C (SIGINT), resuming
  the top-level REP-loop. If everything fails, you can exit LispMe
  with Ctrl-Break (SIGBREAK) or SIGTERM.

* For a test and an example invoke:
    LispMe -m 10m -r 2048 fib.scm hanoi.scm <demo.inp >demo.out 2>nul
  and compare the demo.out with demo.res, both files should be identical.

* Another example using macros and call/cc for backtracking:
    LispMe -m 10m nondet.scm europe.scm 
  and evaluate (color-europe) to obtain a 4-color map of western/middle
  Europe in about 1 second on a 2.6 GHz processor or evaluate
  (length (bag-of (color-europe))) to get 2592 solutions in
  about 24 seconds.

* Executing speed seems to be about 16 million SECD steps per second
  on a 2.6 GHz Intel chip (no swapping). Compare this with 8000-10000
  steps per second on a Palm!

* The opcodes in vm.h are NOT the same as in the PalmOS version since
  many Palm functions are missing and the opcodes should be contiguous.

* BTW, I'm NOT planning to extend LispMe to a general purpose Scheme
  system on desktop (Windows NT, MAC, Linux) platforms, since there is
  already a bunch of very usable Schemes for those systems.

  
