#include <lib/runtime.h>
#include "hardware.h"
#include "PufAddress.h"



static void multi(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a*b;
    }
}

static void add(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a+b;
    }
}

static void sub(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a-b;
    }
}

static void division(uint32_t a,uint32_t b)
{
    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a/b;
    }
}

static void modulo(uint32_t a,uint32_t b)
{
    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a%b;
    }
}
