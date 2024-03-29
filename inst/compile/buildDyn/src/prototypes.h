/* Headers of functions used in internal .cpp and .cu code  */

/* * * * * * * * */
/* CPP FUNCTIONS */
/* * * * * * * * */


/* * * * * * * * * */
/* CUDA FUNCTIONS  */
/* * * * * * * * * */


/* Functions in gpu_mem.cu that handle allocating, freeing, and copying memory  */
/* between the host and the device                                              */

/* Function which allocates memory on the device that can be accessed by the GPU  */
extern void* malloc_device(size_t);

/* Function which frees memory on the device that can be accessed by the GPU  */
extern void free_device(void*);

/* Function which copies memory from the host to the device */
extern void memcpy_to_device(void*, void*, size_t);

/* Function which copies memory from the device to the host */
extern void memcpy_to_host(void*, void*, size_t);


/* Functions in kernel.cu that prepare __constant__ memory for execution on GPU */

/* Function which initializes iteration lengths for loops using machine generated code  */
extern void initialize_iter_lens();

/* Function which initializes expression lengths using machine generated code  */
extern void initialize_expr_lens();

/* Function which initializes intermediate evaluation Rvars using machine generated code  */
extern void initialize_int_evals();

/* Function which allocates background memory on GPU  */
extern void allocate_background_mem(int, int);

/* Function which frees the GPU memory allocated for intermediate evaluations */
extern void free_int_evals();

/* Function which frees all memory on the GPU not associated with a global variable */
extern void free_background_mem();

/* Function which stores Rvar structures in __constant__ memory */
extern void store_vars();

/* Function which stores length info for iteration loops in __constant__ memory */
extern void store_iter_lens();

/* Function which stores length info for expressions in __constant__ memory */
extern void store_expr_lens();

/* Function which stores intermediate evaluation Rvars in __constant__ memory */
extern void store_int_evals();

/* Function which calls the machine generated kernel code */
extern void call_device();
