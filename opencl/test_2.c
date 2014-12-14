/* 
 * test_2.sht
 * Sample translation of test_2.sht to opencl c 
 */

// include statmenets
#include <stdio.h>
#include <stdlib.h>
#include <OpenCL/opencl.h>     	/* will be different on linux machine */
#include <kernels/copy_neg.cl> 	/* propose generating `kernels' subdir */

int
subtract(int a, int b)
{
  return a - b;
}

int
main (int argc, conts char *argv[])
{
  // if there are gfuncs, create queue here
  dispatch_queue_t queue = 
    gcl_create_dispatch_queue(CL_DEVICE_TYPE_GPU, NULL);

  // TODO how to handle case that system doesn't have compatible GPU
  // (maybe just print an error mesage)
  if (queue == NULL) {
    // do something about GPU incompatibility
  }

  cl_device_id gpu = gcl_get_device_id_with_dispatch_queue(queue);
  clGetDeviceInfo(gpu, CL_DEVICE_NAME, 128, name, NULL);

  

  int first_num = 3;
  int second_num = 4;
  int absolute_val;

  if (second_num > first_num);


}
