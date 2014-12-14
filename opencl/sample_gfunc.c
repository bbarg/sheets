#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "platform/aws-g2.2xlarge"
#include "cl-helper.h"
#include <CL/cl.h>

#define NKERNELS

const char *__KERNEL_band_restrict =
  "__kernel void band_restrict(\n"
  "__global const float *out, __global const float *wav, const float thresh_low, const float thresh_hi\n"
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

////// SAME IN EVERY FILE

const char *kernel_strings[NKERNELS] = { __KERNEL_band_restrict };
const char *kernel_names[NKERNELS]   = { "__KERNEL_band_restrict" };
cl_kernel compiled_kernels[NKERNELS];

////// [END]

main (int argv, char **argv)
{
  ////// SAME IN EVERY FILE

  // create context and command queue
  cl_context       __sheets_context;
  cl_command_queue __sheets_queue;
  int              _i;
  cl_int           __cl_err;
  
  create_context_on(SHEETS_PLAT_NAME,
		    SHEETS_DEV_NAME,
		    &__sheets_context,
		    &__sheets_queue,
		    0);

  // compile kernels
  for (_i = 0; _i < NKERNELS; _i++) {
    compiled_kernels[_i] = kernel_from_string(&__sheets_context,
					      kernel_strings[_i],
					      kernel_names[_i],
					      SHEETS_KERNEL_COMPILE_OPTS);
  }

  ////// [END]

  size_t __SIZE_wav = 10000;

  float *wav[__SIZE_wav];
  char *file_name = "mytune.wav";

  for (_i = 0; _i < 10000; _i++) {
    wav[_i] = random() / RAND_MAX;
  }

  printf("Beginning wav masking from file [%s]\n", file_name);

  /////////////////
  ////// GFUNC CALL
  /////////////////

  /// create variables for function arguments given as literals
  float __band_restrict_ARG2 = 0.1112f;
  float __band_restrict_ARG3 = 0.7888f;

  /// return array (always arg0)
  cl_mem __CLMEM_band_restrict_ARG0 = clCreateBuffer(__sheets_queue, 
						     CL_MEM_WRITE_ONLY, 
						     __SIZE_wav, 
						     NULL, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, #NAME);
					      
  /// input arrays
  cl_mem __CLMEM_band_restrict_ARG1 = clCreateBuffer(__sheets_queue, 
						     CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
						     __SIZE_wav, 
						     wav, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, #NAME);
  
  cl_mem __CLMEM_band_restrict_ARG2 = clCreateBuffer(__sheets_queue, 
						     CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
						     sizeof(__band_restrict_ARG2), 
						     __band_restrict_ARG2, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, #NAME);

  cl_mem __CLMEM_band_restrict_ARG3 = clCreateBuffer(__sheets_queue, 
						     CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
						     sizeof(__band_restrict_ARG3), 
						     __band_restrict_ARG3, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, #NAME);

  /// set up kernel arguments
  SET_4_KERNEL_ARGS(compiled_kernels[0],
		    __CLMEM_band_restrict_ARG0,
		    __CLMEM_band_restrict_ARG1,
		    __CLMEM_band_restrict_ARG2,
		    __CLMEM_band_restrict_ARG3);

  /// enqueue kernel
  cl_event __CLEVENT_band_restrict_CALL;
  CALL_CL_GUARDED(clEnqueueNDRangeKernel,
		  (__sheets_queue,
		   compiled_kernels[0],
		   1,		/* 1 dimension */
		   0,		/* 0 offset */
		   &__SIZE_wav,
		   NULL		/* let OpenCL break things up */
		   0,		/* no events in wait list */
		   NULL,	/* empty wait list */
		   &__CLEVENT_band_restrict)
		  );

  /// allocate space for cpu return array
  float out[__SIZE_wav];
  
  cl_event __CLEVENT_band_restrict_RET;
  CALL_CL_GUARDED(clEnqueueReadBuffer,
		  (__sheets_queue,
		   __CLMEM_band_restrict_ARG0,
		   CL_TRUE,	 /* blocking read */
		   0,		 /* 0 offset */
		   __SIZE_wav, 	 /* read whole buffer */
		   (void *) out, /* host pointer */
		   1,		 /* wait for gfunc to finish */
		   &__CLEVENT_band_restrict_CALL, /* "" */
		   &__CLEVENT_band_restrict_RET)	 
		  );
  
  ////// [END] GFUNC CALL

  printf("Done wav masking from file [%s]\n", file_name);

  ////// Validate call

  for (_i = 0; _i < __SIZE_wav; _i++) {
    assert(out[_i] > __band_restrict_ARG2 &&
	   out[_i] < __band_restrict_ARG3);
  }

  return 0;
}
