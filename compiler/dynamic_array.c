#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#define MAX_DIM 3
struct d_array {
	/*N dimensional integer array 0 < N < 4  
         */

	int dim[MAX_DIM + 1];
	int size[MAX_DIM + 1];
	
	int* data;
	int total_size; //num_elements
	int total_capacity //total capacity
	int num_dimensions; //the N in N-dimensional 
	int ref_count; 

};

void init_array(struct d_array* arr, int dim_1, int dim_2, int dim_3) {

	int i, j; 
	arr->num_dimensions = 0;
	arr->total_capacity = 1; 
	memset(arr->dim, 0, MAX_DIM + 1);  
	memset(arr->size, 0, MAX_DIM + 1);
	if(dim_2 == 0 ) {
		arr->num_dimensions = 1;
	}
	else if (dim_3 == 0) {
		arr->num_dimensions = 2;
	}
	else {
		arr->num_dimensions = 3;
	}
	arr->dim[0] = dim_1;
	arr->dim[1] = dim_2;
	arr->dim[2] = dim_3;  
	while(arr->dim[i] > 0) {
		arr->total_capacity *= arr->dim[i++];
	}
	arr->total_size = 0;
	arr->ref_count = -1; //Not implemented right now
	
	arr->data = (int*) calloc (arr->total_capacity, sizeof(int));

}

void insert(struct d_array* arr, int data, int dim_1, int dim_2, int dim_3) {
	int offset = 0; 
	switch (arr->num_dimensions) {
		case 1: 
			offset = dim_1;
			break;
		case 2:
			offset = dim_1* arr->[1] + dim_2; 
			break; 
		case 3:
			offset = ((dim_1 * arr->dim[1] + dim_2) * arr->dim[2] + dim_3); 
			break;

	}

	arr->data[offset] = data; 

};

int get(struct d_array* arr, int dim_1, int dim_2, int dim_3) {
 int offset = 0; 
	switch (arr->num_dimensions) {
		case 1: 
			offset = dim_1;
			break;
		case 2:
			offset = dim_1* arr->[1] + dim_2; 
			break; 
		case 3:
			offset = ((dim_1 * arr->dim[1] + dim_2) * arr->dim[2] + dim_3); 
			break;

	}

	return arr->data[offset]; 
}




