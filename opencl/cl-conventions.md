# OpenCL Conventions
## Arrays

- OpenCL 1.1 requires that the number of work items divide the array
  size of an incoming kernel.

- Proposition: if you declare an array with a set size, it is on CPU
  stack. If you declare it without a size, its size will be set when
  you use it as the result of a gfunc.

- Proposition: we ALWAYS pass in the size of the intput and output (if
  different) of arrays into the GPU so we can avoid segfaults.
  + We might not always have to do this, but I can see situations
    (such as when a work item encompasses multiple indices and the
    work item size doesn't divide the size of the array you are
    processing)

- Proposition: we MAY need to write some function for map_reduce that
  figures out how to set up NDRange dimensions based on input size.

## Names

- a gfunc with name "gpu_function" will be represented by the kernel
  string "__KERNEL_gpu_function"

## Function calls

- the return type of a gfunc will always be the FIRST argument of the subsequent kernel string

- we apparently do NOT have to make memory buffers for primitive types that we pass by value

- we NEED to make sure that our convention for the naming of gfunc
arguments when they are allocated as cl_mem buffers won't cause conflicts
  + possible strategy: have a group of global variables
    `__clmem_arg_n` that we can reassign each time

## Build

- the names for the platform and device, as required by cl_helper
  files, are available in a header file "platform/aws-g2.2xlarge"

## OpenCL api

- How do we calculate OpenCL `local_work_size` (argument of `clEnqueueNDRangeKernel`)?

## CPU/GPU communication

- Do we have to wait for our call to finish before we read it's
  result? Or can we just do a blocking read?

## Dimensions

- `global_work_size`: This is the number of work items that happen;
which is NOT tied (but is I guess max-bounded) to the size of the
array that comes in. That size doesn't have a name here. REMEMBER THIS.

- `local_work_size`: Number of work-items per work group. Basically,
  number of work_items that see the same local memory.

## Higher-order functions

- vector only did `map`, `reduce`, and `pfor`
- we are kind of doing something different in that we are asking the
programmer to do a SLIGHT bit more (think about memory locality)
- doing the reduce thing wouldn't be terrible I guess; we'd just have
  to write that function in the output kernel file (or in the
  associated kernel string string) and then call it in our reduce function)
- so, generating `reduce` would be like

```
" code code code code " ^ mapped_function ^ "code code code code"
```
- YUP this is EXACTLY how vector does it
- pfors are more complicated because you don't just substitute a
  function in

- SO, I need to think about how to implement reduce; do I???

- let's think about block size:

```
gfunc
float[] compress(float[] wave_samples):
	for i in block.out:
		block.out[i] = wave_samples[id]
```
Basically, this function takes in an array and sets the next `block_size`
objects to the value of the initial one. In order to make things easier for ourselves, `for index in object` gets mapped to:

```
for (int index = 0; index < object.size; index = index + 1):
```

We can invoke that kernel with a variable size.

```c
float[] compressed_wave = compress(samples).[5]
```

This is going to return an array of the same length as samples, but
with every five values set to the value of the first one.

Alright, let's get this into a format that OpenCL is comfortable with.

First of all, there's no synchronization in this really. Honestly, the
BEST way to do this is like the following:

```c
__kernel
void compress(
              __global float *out,
              __global float *wave_samples,
              __global const int wave_samples_length,
              __global const int block_size)
{
    const int id = get_global_id(0);

    if (id < wave_samples_length) {
        out[id] = wave_samples[id - (id % block_size)];
    } else {
        out [id] = INFINITY;
    }
}
```

We'd then invoke this kernel as so:

```c
int *gdims = { ceil(wave_samples.size / compress.block_size) };
int *ldims = NULL;

clEnqueueNDRangeKernel(__sheets_queue,
                       __cl_kernel_compress,
                       1,
                       0,
                       ldims,
                       gdims,
                       NULL,
                       NULL,
                       &__cl_err);
```

But, that doesn't really map the loop in a straightforward way, especially if the loop does special things. Further, the point of blocksize was to allow you to do things where you have a group of items that all depend on each-other (which now seems like a sort of silly construct).

So, what we'll really do is just transform that statement; add a check that we're not writing/accessing out of range; and leave it up to the programmer to supply an operation that isn't meaningless when `block_size` doesn't divide the total work size.

Here's our "dumber" implementation, albeit with size checks. N

```c
__kernel
void compress(
              __global float *out,
              __global float *wave_samples,
              __global const int wave_samples_length,
              __global const int block_size)
{
    const int i = get_global_id(0);

    // -----------------------------------------
    // set these up every time
    // -----------------------------------------
    int _block_start = i * block_size;
    int _block_end = i * block_size + block_size;
    int _i;
    // -----------------------------------------

    // first for ... in ...
    for (_i = _block_start; _i < _block_end; _i++) {
        if (_i < wave_samples_length) {
            out[_i] = wave_samples[_block_start];
        }
    }
}
```

Let's quickly note that `block.id` is the same as `_block_start`; it's just the start
of the region that this work item corresponds to. In fact, thinking of it as having a bounded region is pretty spot on.
