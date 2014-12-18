#Sheets
*What's your thread count?*
---------------------------

Ben Barg - bbb2123
Amelia Brunner - arb2135
Gabriel Blanco - gab2135
Ruchir Khaitan - rk2660

Project Completion Date: December 17th 2014

## Tests

To run our test suite, `cd` to `compiler` and run `make test`.

## Build

To compile a sheets program, follow these instructions:

1. Move your_file.sht to the `build` directory.
1. Run `./weave SHEET=your_file`
1. `your_file.c` and `your_file` (the executable) will now exist in `build`.

`weave` will build the compiler if it is not currently built. You can remove generated files in `build` with `make clean`.

Note that the only currently supported platform is on the our `g2.2xlarge` instance. The `opencl` directory, which contains our support OpenCl helper libraries requires the following files to exist at the following locations:

- `/usr/lib64/libnvidia-opencl.so.1`
- `/opt/opencl-headers-1.1/include/CL/cl.h`
- `/opt/intel-opencl-icd-4.6.0.92/lib64/libOpenCl.so.1`
- `/etc/OpenCL/vendors/nvidia.icd`
- `/etc/OpenCL/vendors/intel64.icd`
