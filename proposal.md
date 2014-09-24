# Language Propoal: Sheets
*What's your thread count?*

Benjamin Barg, Gabriel Blanco, Ruchir Khaitan, Amelia Brunner
*****
### Motivation

In recent years, parallelism, or doing multiple things at once, has been a powerful tool to gain performance from hardware. While the concept isn't novel at all (nondeterministic finite automata existed long before digital computers), it is more relevant because of increasing hardware support for various types of concurrent program execution. CPUs are commonly multicore now, and support multithreading, a task-based paradigm for execution parallelism. C and C++ support multithreading with POSIX threads. Unfortunaley, while the pthread library allows for a lot of optimization, it is fairly verbose, and requires the user to manage all potential problems, such as data race conditions that arise from task parallelism. 

**TODO**: (move this topic into the last paragraph that talk about existing language paradigms that we like) Golang provides an elegant solution by using channels, and forcing threads to share data not over shared memory, but instead over two-way pipes. 

GPUs have also gained a lot of processing power, and are intrinsically better than traditional CPUs at performing relatively simple operations across large data sets in parallel, simply by virtue of their hardware design. By focusing on very high throughput, high latency operations, and including hardware support for thousands of threads and efficient thread context switching, GPUs are able to perform massive amounts of computation across independent pieces of data (elements in an array) much faster than CPU, and they scale better than CPUs as data size grows. The GPU multiprocessing paradigm is thus fundamentally different than that of CPUs, and is data-based rather than task based. 
  
Currently, OpenCL is the only available general purpose programming language that can run on both CPUs and GPUs (and FPGAs). It's very powerful, but it exposes a lot of hardware details and options that are confusing for someone unfamiliar with general purpose GPU programming or concurrent programming. Our primary motivation then is for the programmer who wants to take advantage of the performance benefits of multiprocessing, and more specifically the large performance benefits of GPU execution for programs operating on large data sets, but is unfamiliar with hardware specifics and the details of concurrent programming and doesn't need all the optimization features provided by OpenCL.    

### Goals

- make basic parallelism features available to programmers used to procdural languages (C/C++, Java, etc.)
- provide concise, "Go-like" syntax for both task and data parallelism features
- leverage OpenCL to support high performance data parallelism on the GPU
- expose OpenCL's distributed computation model with simplified syntax

### Domain Features

- `pfor`
- `map`, `reduce`
- `spawn`
- `barrier`
- type inference
- `:=` variable declaration and assignment
- file I/O primitives

### Code Sample
Say we have a file (or several files) containing on the order of a billion 32-bit integers, and we want to find their sum...