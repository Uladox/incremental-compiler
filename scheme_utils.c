#include <stdio.h>
#include <stdlib.h>

#include "scheme_utils.h"

ptr scheme_char(char c)
{
    return (c<<fx_tag_shift)|char_tag;
}

ptr scheme_int(int i)
{
  return i << int_tag_shift;
}
