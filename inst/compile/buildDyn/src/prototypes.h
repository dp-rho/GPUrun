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

extern void store_vars();
