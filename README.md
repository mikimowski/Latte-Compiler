# Latte - Compiler

## Overview

Compiler, written in Haskell, for a Java-like language called Latte. It translates Latte source code into assembly (x86 - 32 bit) in AT&T syntax, links created `*.s` file with runtime library and compiles it to executable file via GCC. Developed as an [assignment](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/) for compilers and programming languages course at the University of Warsaw.

### Project structure
```
src/
    Main.hs                                  - entry point
    frontend/                                - module for static analysis (parsing, typechecking, declarations etc.)
    backend/                                 - module responsible for code translation
    utils/                                   - module containing common helpers
    bnfc/
lib/*                                        - language built-in functions written in C
tests/                                       - subset of public tests 
```

### Latte

[tests/examples](tests/examples) contains examples of valid and invalid Latte programs.


## Miscellaneous notes

A bunch of additional remarks.

### Running
After unpacking, run from current directory (works on `students` host):
```
make                                    -- generates latc_x86
./run_tests.sh latc_x86                 -- runs all the tests. For bad tests error is expected
```

### For loop
Semantically
``` for (type a: exprArr) { ... } ```
is equivalent to
```
int __for_loop_idx = 0;
arrType __for_loop_arr = eval(exprArr);
if __for_loop_arr not NULL {
    while (__for_loop_idx < __for_loop_arr.length) {
        type a = __for_loop_arr[__for_loop_idx];
        ...
    }
}
```
This decision was made because of dependencies of Garbage Collector using Stack Frames and problem of unwinding nested blocks on early returns.

### Garbage collector
Garbage collectors is based on reference counts.
To enable `ref_cnt` objects/arrays/string are stored in memory following this patterns:

- string memory layout: `[dtr, ref_cnt, c0, c1, ...]`
- array's memory layout: `[dtr, ref_cnt, length, a0, a1, ....]`
- object's memory layout: `[dtr, ref_cnt, vTable, fields...]`

To allow 'early return' from the functions with nested blocks I adopted solution based on `stack of blockFrames`:
- `frame` := just list of indices (locations ~ offset from EBP)
- `blockFrames` := stack of frames

Frame on top keeps track of the indices of the variables declared in the current block.
Those indices indicate memory location of the variables. Namely `(EBP + idx)`.

### Stack of block frames
1. Stack holds frames. Each frame holds indices (location with respect to EBP) of variables declared in current block/scope
2. Only early return the whole stack is rewinded. Ref cnt for variables are updated in the reversed ordered, stack becomes empty, and return is called.
3. From code generation perspective (Monad's state) the caller is responsible of memorizing his stack from the moment of function call.

Example of rewinding nested blocks only early return
```
-- rewinds all the blocks for given functions
 {
    int a;
 }
 int x;
 {
    int s;
    {
         int x;
         return;
    }
}
-- will decr ref_cnt for x,s,x', but not for a;
```

### Dead code
Dead code is iteratively removed during the process
1. In the frontend, evaluation of constants is done as well as removal of unreachable blocks - see frontend/ProgramSimplifier.hs
2. In the backend, it's ensured that dead code is removed during iterative postprocessing - see backend/CodeOptimizer.hs

### Some of adopted code optimizations
1. In the backend, iterative removal of trivial operations is done - see backend/CodeOptimizer.hs
2. Because Latte addopted lazy evaluation for logic operations, the same order of computation was adopted for arithmetic operations. Thus, this way of writing code was optimized - see core023.s, core023a.s, and core core023b. Namely, EAX, ECX registers are extensively used there to optimize simple arithmetic operations.

Due to the way the code is generated it allows to remove useless "PUSH POPS" for subsequent binary operations (see core023.s). This way EAX and ECX are first used to extract variables and then they act as accumulators for the results, where variables are read with respect to EDP. Obviously we still write to the ESP, however it's much more efficient than e.g.:
```
PUSH EAX,
read val to ECX
POP EAX
add ECX EAX
PUSH EAX
```
3. `Conds` are generated following the guideline from the lectures. In addition, "trivial jumps" are removed. It's performed during the postprocessing. During code emitting all the jumps are negated, and putted in the proper order, thus later trivial jumps are easy to catch and remove. (See `ERel`)
4. Code is iteratively simplified during postprocessing with removal of trivial fragments
5. Simple functions are inlined e.g. see `core003.s` (most of the code has literally vanished)

### Registers 
- `EBX` is allocated for 'inner for loop counter' in array destructor (of complex types). It's pushed at the beginning and restored at the end.
- `ESI` is allocated for 'self' in the class destructor. It's pushed at the beginning and restored at the end.
- `EAX` and `ECX` are mainly used during the computations, as well as for their optimization.
- For arithmetic on constants and variables `ECX` and `EAX` are allocated to extract their values and perform operations. See `emitLoadSimple` in `Compiler.hs`
- Result of comples expression is evaluated to `EAX`, instead of being pushed into the stack, thus there are no inefficient `PUSH/POP` routines.

## Resources
- https://stackoverflow.com/questions/33556511/how-do-objects-work-in-x86-at-the-assembly-level
- https://stackoverflow.com/questions/14765406/function-prologue-and-epilogue-in-c
- https://stackoverflow.com/questions/9153282/lea-assembly-instruction
- https://stackoverflow.com/questions/1658294/whats-the-purpose-of-the-lea-instruction
- CALL variations https://c9x.me/x86/html/file_module_x86_id_26.html
- Memory addressing http://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
