# Makefile for OpenCL code
#
# Paths are correct for amazon3 aws instance.

OPENCL_INC=/opt/opencl-headers-1.1/include
OPENCL_LIB=/usr/lib64
PLAT_INC=../opencl/platforms/include
CL_HELPER_INC=../opencl/

CC      = gcc
CFLAGS  = -Wall -g -I$(OPENCL_INC) -I$(PLAT_INC) -I$(CL_HELPER_INC) -std=gnu99
LDFLAGS = -L$(OPENCL_LIB) -lOpenCL -lrt -L../opencl

weave : $(SHEET)

$(SHEET) : ../opencl/cl-helper.o

.PHONY : clean
clean : 
	rm -f *.o *.c *.proc.sht