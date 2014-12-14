# Makefile for OpenCL code
#
# Paths are correct for amazon3 aws instance.

OPENCL_INC=/opt/opencl-headers-1.1/include
PLAT_INC=./platforms/include
OPENCL_LIB=/usr/lib64

CC      = gcc
CFLAGS  = -Wall -g -I$(OPENCL_INC) -I$(PLAT_INC) -std=gnu99
LDFLAGS = -L$(OPENCL_LIB) -lOpenCL -lrt

EXE     = sample_gfunc

all : $(EXE)

sample_gfunc : cl-helper.o

.PHONY : clean
clean : 
	rm -f *.o $(EXE)