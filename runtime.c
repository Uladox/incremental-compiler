#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include "scheme_utils.h"

extern ptr scheme_entry();

static void print_ptr(ptr x){
    if((x & int_tag_mask) == int_tag){
        printf("%d", ((int)x) >> int_tag_shift);
    } else if(x == bool_f){
        printf("#f");
    } else if(x == bool_t){
        printf("#t");
    } else if (x == nil){
        printf("'()");    
    } else if(GET_TAG(x) == char_tag) {
        printf("%c", ((int)x) >> fx_tag_shift);   
    } else {
        printf("#<unknown 0x%08x>", x);
    }
    printf("\n");
}

char *allocate_protected_space(int size)
{
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
		 PROT_READ | PROT_WRITE,
		 MAP_ANONYMOUS | MAP_PRIVATE,
		 0, 0);
  if(p == MAP_FAILED) {
    printf("Map failed in allocating protected space\n");
    exit (EXIT_FAILURE);
  }
  status = mprotect(p, page, PROT_NONE);
  if(status != 0) {
        printf("Protection failed in allocating protected space\n");
	exit(EXIT_FAILURE);
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0) {
        printf("Protection failed in allocating protected space\n");
	exit(EXIT_FAILURE);
  }
  return p + page;
}

void deallocate_protected_space(char *p, int size)
{
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0) {
        printf("Deallocation failed in deallocating protected space\n");
	exit(EXIT_FAILURE);
  }
}

int main(int argc, char** argv){
  int stack_size = (16 * 4096);
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
