# haskellpiler

I've been curious about compilers for a while now, especially how the runtimes such as the JVM for Java and CLR for C# work under the hood. I've built a small compiler to learn.

This is my second attempt - in my first attempt I bit off a bit too much and attempted it in C++ with LLVM, though I didn't have much experience with low-level programming. This time I started with Haskell (I love Haskell’s syntax and a functional language is a little nicer for a recursive parsing) compiling into bytecode for a C-based interpreter.

Currently I'm at the stage of modifying the produced bytecode such that it resembles Java .class files or .NET .dlls. I'd love to implement multithreading, async, and garbage collection next, just to see how they work.
