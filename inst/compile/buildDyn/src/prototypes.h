/* Headers of functions used in internal .cpp and .cu code  */

/* * * * * * * * */
/* CPP FUNCTIONS */
/* * * * * * * * */


/* * * * * * * * * */
/* CUDA FUNCTIONS  */
/* * * * * * * * * */

/* Functions in gpu_mem.cu that handle allocating, freeing, and copying memory  */
/* between the host and the device                                              */

extern void* malloc_device(size_t);

extern void free_device(void*);

extern void memcpy_to_device(void*, void*, size_t);

extern void memcpy_to_host(void*, void*, size_t);

/* Functions in kernel.cu that prepare __constant__ memory for execution on GPU */

/* Function which initializes iteration lengths for loops using machine generated code  */
extern void initialize_iter_lens();

/* Function which stores Rvar structures in __constant__ memory */
extern void store_vars();

/* Function which stores length info for iteration loops in __constant__ memory */
extern void store_iter_lens();

/* Function which calls the machine generated kernel code */
extern void call_device();
