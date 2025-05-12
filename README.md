# haskellpiler

I've been curious about runtimes for a while now - how the JVM/CLR works, multithreading, event loops, async and coroutines, garbage collection. This project is only to learn about those - I'm not releasing a new language anytime soon (though that would be cool). 

This is my second attempt - in my first attempt I bit off a bit too much and attempted it in C++ with LLVM, though I didn't have much experience with low-level programming. This time I started with Haskell (I love Haskell’s syntax and a functional language is a little nicer for a recursive parsing) compiling into bytecode for a C-based interpreter that manually manages memory. That's all, though - I ain't touching assembly.
