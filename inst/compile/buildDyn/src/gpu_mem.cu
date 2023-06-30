#include "commands.h"
#include "cuda_headers.h"

/* The array of globally accessible pointers to gpu ready dynamic memory  */
void* gpu_mem[MAX_GPU_POINTERS];


/*
 * Searches for an available pointer that can be dynamically allocated
 * with memory that is avaible the to the GPU.
 */

void* malloc_device(size_t size) {

  /* Find an available pointer or return NULL otherwise      */
  /* Use a static variable to minimize unnecessary searching */
  static int index = 0;
  int i = 0;
  for (; i < MAX_GPU_POINTERS; i++) {
    index = index % MAX_GPU_POINTERS;
    if (!gpu_mem[index++]) break;
  }

  /* Case where there were no available pointers  */
  if (i == MAX_GPU_POINTERS) {
    return NULL;
  }

  /* Allocate the memory using cuda library */
  cudaError_t err = cudaMalloc(&gpu_mem[index], size);
  if (err != cudaSuccess) {
    printf("CUDA error: Failed to allocate device memory (%s)\n",
           cudaGetErrorString(err));
    return NULL;
  }

  return gpu_mem[index];
}


/* 
 * Seaches for the input pointer in the global pointer array
 * and frees the dynamically allocated memory of the pointer
 * before setting it to NULL.
 */

void free_device(void* mem) {

  /* Find the pointer in the global array      */
  for (int i = 0; i < MAX_GPU_POINTERS; i++) {
    if (gpu_mem[i] == mem) {
      cudaFree(gpu_mem[i]);
      gpu_mem[i] = NULL;
      return;
    }
  }
}


/*
 * Wrapper function for Host -> Device memory
 */

void memcpy_to_device(void* dev_mem, void* host_mem, size_t size) {
  cudaError_t err = cudaMemcpy(dev_mem, host_mem, size, cudaMemcpyHostToDevice);
  if (err != cudaSuccess) {
    printf("CUDA error: Failed to copy memory from host to device (%s)\n",
           cudaGetErrorString(err));
  }
}


/* 
 * Wrapper function for Device -> Host memory
 */

void memcpy_to_host(void* host_mem, void* dev_mem, size_t size) {
  cudaError_t err = cudaMemcpy(host_mem, dev_mem, size, cudaMemcpyDeviceToHost);
  if (err != cudaSuccess) {
    printf("CUDA error: Failed to copy memory from device to host (%s)\n",
           cudaGetErrorString(err));
  }

}


