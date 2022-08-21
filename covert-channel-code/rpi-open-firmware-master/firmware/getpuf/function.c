#include <runtime.h>
#include "hardware.h"
#include "PufAddress.h"



void multi(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a*b;
    }
}

void add(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a+b;
    }
}

void sub(uint32_t a, uint32_t b)
{

    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a-b;
    }
}

void division(uint32_t a,uint32_t b)
{
    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a/b;
    }
}

void modulo(uint32_t a,uint32_t b)
{
    for(int i=0; i<1000000; i++)
    {
      uint32_t c=a%b;
    }
}
