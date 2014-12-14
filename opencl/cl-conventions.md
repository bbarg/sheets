# OpenCL Conventions

## Arrays

- OpenCL 1.1 requires that the number of work items divide the array
  size of an incoming kernel.

## Names

- a gfunc with name "gpu_function" will be represented by the kernel
  string "__KERNEL_gpu_function"

## Function calls

- the return type of a gfunc will always be the FIRST argument of the subsequent kernel string

## Build 

- the names for the platform and device, as required by cl_helper
  files, are available in a header file "platform/aws-g2.2xlarge"