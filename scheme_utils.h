#ifndef SCHEME_COSTANTS
#define SCHEME_COSTANTS

#include <stdlib.h>
#include <stdio.h>

/* define all scheme constants */
#define bool_f 0x2F
#define bool_t 0x6F
/* The position of the
   one bit that makes 
   true != false 
   and the bit itself
*/
#define bool_bit_pos 6
#define bool_bit 0x40
#define nil 0x3F
/* define some shifting constants*/
#define int_tag_shift 2
#define fx_tag_shift 8
#define fx_tag_clean 24
#define GET_TAG(X) ((X << fx_tag_clean) >> fx_tag_clean)
/* define some tags for types */
//ints are special so they hold more
//since only they last two bits are
//used for type
#define int_tag_mask 0x03
#define int_tag 0x00
#define char_tag 0x0F

typedef unsigned int ptr;

ptr scheme_char(char c);

ptr scheme_int(int i);

#endif
