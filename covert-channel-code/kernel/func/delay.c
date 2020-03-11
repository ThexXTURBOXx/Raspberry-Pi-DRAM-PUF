#include <stddef.h>
#include <stdint.h>
#include "address.h"

#define ST_CLO *(const volatile unsigned long *)(0x20003004)

static inline void delay_ms(uint32_t t) {
	uint32_t tv = ST_CLO;
	for (;;) {
		/* nop still takes a cycle i think? */
		__asm__ __volatile__ ("nop" :::);
		// if ((ST_CLO - tv) > t)
		if ((ST_CLO - tv) > (t*1000))
			return;
	}
}

static inline void delay_s(uint32_t t)
{
  uint32_t tv = ST_CLO;
  for (;;) {
    /* nop still takes a cycle i think? */
    asm volatile ("nop" :::);
    if ((ST_CLO - tv) > (t*1000000))
      return;
  }
}

static inline void udelay(uint32_t t) {
  uint32_t tv = ST_CLO;
  for (;;) {
    /* nop still takes a cycle i think? */
    __asm__ __volatile__ ("nop" :::);
    // if ((ST_CLO - tv) > t)
    if ((ST_CLO - tv) > t)
      return;
  }
}

static inline void delay(int32_t count)
{
    asm volatile("__delay_%=: subs %[count], %[count], #1; bne __delay_%=\n"
         : "=r"(count): [count]"0"(count) : "cc");
}
