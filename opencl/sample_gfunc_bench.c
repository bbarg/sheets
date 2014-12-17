#include <assert.h>
#include <stdio.h>
#include <stdlib.h>		/* malloc */
#include <math.h>		/* random */
#include "aws-g2.2xlarge.h" /* sheets platform defs */
#include "cl-helper.h"		  
#include "timing.h"
#include <CL/cl.h>

#define NKERNELS 1

char *kernel_strings[NKERNELS] = {
  "__kernel void band_restrict(\n"
  "__global float *out, __global const float *wav, const float thresh_low, const float thresh_hi\n"
  ")\n"
  "{\n"
  "const int id = get_global_id(0);\n"
  "float freq = wav[id];\n"
  "if (freq > thresh_hi) {\n"
  "out[id] = 0;\n"
  "}\n"
  "else if (freq < thresh_low) {\n"
  "out[id] = 0;\n"
  "}\n"
  "else {\n"
  "out[id] = wav[id];"
  "}\n"
  "}\n"
};

/////////////////////////
////// SAME IN EVERY FILE
/////////////////////////

char *kernel_names[NKERNELS] = { "band_restrict" };
cl_kernel compiled_kernels[NKERNELS];

////// [END]

int
in_thrsh(float v, float lo, float hi)
{
  return (v > lo && v < hi);
}

int
main (int argv, char **argc)
{
  /////////////////////////
  ////// SAME IN EVERY FILE
  /////////////////////////

  // create context and command queue
  cl_context       __sheets_context;
  cl_command_queue __sheets_queue;
  int              _i;
  cl_int           __cl_err;
  
  create_context_on(SHEETS_PLAT_NAME,
		    SHEETS_DEV_NAME,
		    0,		/* choose the first (only) available device */
		    &__sheets_context,
		    &__sheets_queue,
		    0);

  // compile kernels
  for (_i = 0; _i < NKERNELS; _i++) {
    compiled_kernels[_i] = kernel_from_string(__sheets_context,
					      kernel_strings[_i],
					      kernel_names[_i],
					      SHEETS_KERNEL_COMPILE_OPTS);
  }

  ////// [END]

  size_t __SIZE_wav = atoi(argc[1]);

  float wav[__SIZE_wav];
  const char *file_name = "mytune.wav";
  int in_thrsh_cnt = 0;

  timestamp_type st;
  timestamp_type end;

  get_timestamp(&st);		
  for (_i = 0; _i < __SIZE_wav; _i++) {
    wav[_i] = (float) rand() / RAND_MAX;
    if (in_thrsh(wav[_i], 0.1112, 0.7888))
      in_thrsh_cnt++;
  }
  get_timestamp(&end);

  printf("cpu execution took %f seconds\n", timestamp_diff_in_seconds(st, end));

  get_timestamp(&st);

  /////////////////
  ////// GFUNC CALL
  /////////////////

  /// create variables for function arguments given as literals
  float __PRIM_band_restrict_ARG2 = 0.1112f;
  float __PRIM_band_restrict_ARG3 = 0.7888f;

  /// return array (always arg0)
  cl_mem __CLMEM_band_restrict_ARG0 = clCreateBuffer(__sheets_context, 
						     CL_MEM_WRITE_ONLY, 
						     sizeof(float) * __SIZE_wav, 
						     NULL, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, "clCreateBuffer");
					      
  /// input arrays
  cl_mem __CLMEM_band_restrict_ARG1 = clCreateBuffer(__sheets_context, 
						     CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
						     sizeof(float) * __SIZE_wav, 
						     (void *) wav, 
						     &__cl_err);
  CHECK_CL_ERROR(__cl_err, "clCreateBuffer");

  /// write to device memory
  CALL_CL_GUARDED(clEnqueueWriteBuffer,
		 (__sheets_queue,
		  __CLMEM_band_restrict_ARG1,
		  CL_TRUE,	/* blocking write */
		  0, 		/* no offset */
		  sizeof(float) * __SIZE_wav,
		  wav,
		  0,		/* no wait list */
		  NULL,
		  NULL)
		  );
  
  /// set up kernel arguments
  SET_4_KERNEL_ARGS(compiled_kernels[0],
		    __CLMEM_band_restrict_ARG0,
		    __CLMEM_band_restrict_ARG1,
		    __PRIM_band_restrict_ARG2,
		    __PRIM_band_restrict_ARG3);

  /// enqueue kernel
  cl_event __CLEVENT_band_restrict_CALL;
  CALL_CL_GUARDED(clEnqueueNDRangeKernel,
		  (__sheets_queue,
		   compiled_kernels[0],
		   1,		/* 1 dimension */
		   0,		/* 0 offset */
		   &__SIZE_wav,
		   NULL,	/* let OpenCL break things up */
		   0,		/* no events in wait list */
		   NULL,	/* empty wait list */
		   &__CLEVENT_band_restrict_CALL)
		  );

  /// allocate space for cpu return array
  float out[__SIZE_wav];
  
  CALL_CL_GUARDED(clEnqueueReadBuffer,
		  (__sheets_queue,
		   __CLMEM_band_restrict_ARG0,
		   CL_TRUE,	 /* blocking read */
		   0,		 /* 0 offset */
		   sizeof(float) * __SIZE_wav, 	 /* read whole buffer */
		   (void *) out, /* host pointer */
		   1,		 /* wait for gfunc to finish */
		   &__CLEVENT_band_restrict_CALL, /* "" */
		   NULL)			  /* no need to wait for this call though */
		  );
  
  ////// [END] GFUNC CALL

  get_timestamp(&end);

  printf("gfunc call took %f seconds\n", timestamp_diff_in_seconds(st, end));

  ////// Validate call
  int c = 0;

  for (_i = 0; _i < __SIZE_wav; _i++) {
    if (in_thrsh(out[_i], 0.1112, 0.7888)) {
      c++;
    } else if(out[_i]) {
      exit(1);
    }
  }

  printf("\n");
	 
  assert(in_thrsh_cnt == c);

  //////////////
  ////// CLEANUP
  //////////////

  CALL_CL_GUARDED(clReleaseMemObject, (__CLMEM_band_restrict_ARG0));
  CALL_CL_GUARDED(clReleaseMemObject, (__CLMEM_band_restrict_ARG1));
  for (_i = 0; _i < NKERNELS; _i++) {
    CALL_CL_GUARDED(clReleaseKernel, (compiled_kernels[_i]));
  }
  CALL_CL_GUARDED(clReleaseCommandQueue, (__sheets_queue));
  CALL_CL_GUARDED(clReleaseContext, (__sheets_context));

  return 0;
}
