#include <stdio.h>
#include <platform/aws-g2.2xlarge>
#include <CL/cl.h>

#define NKERNELS

const char *__KERNEL_band_restrict =
  "__kernel void band_restrict(\n"
  "const float *out, const float *wav, float thresh_low, float thresh_hi\n"
  ")\n"
  "{\n"
  "const int id = get_global_id(0);\n"
  "float freq = wav[id];\n"
  "if (freq > thresh_hi) {\n"
  "out[id] = 0\n"
  "}\n"
  "else if (freq < thresh_low) {\n"
  "out[id] = 0\n"
  "}\n"
  "else {\n"
  "out[id] = wav[id]"
  "}\n"
  "}\n";

const char *kernel_strings[NKERNELS] = { __KERNEL_band_restrict };
cl_kernel compiled_kernels[NKERNELS];

main (int argv, char **argv)
{
  // create context and command queue
  cl_context ctx;
  cl_command_queue gpu_queue;
  
  create_context_on(SHEETS_PLAT_NAME, SHEETS_DEV_NAME, &ctx, &gpu_queue, 0);

  // compile kernels
  int i;
  for (i = 0; i < NKERNELS; i++) {
    compiled_kernels[i] = kernel_from_string( /* here lies TODO */
  }


}
