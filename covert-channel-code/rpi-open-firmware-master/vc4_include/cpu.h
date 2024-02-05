#pragma once

#include <hardware.h>

static inline void __attribute__((noreturn)) hang_cpu() {
	/* disable interrupts  */
	__asm__ __volatile__ ("di");

	/* loop */
	for (;;) {
		__asm__ __volatile__ ("nop");
	}
}

/*
by PlutoniumBob@raspi-forum with changes by bzt (https://forums.raspberrypi.com/viewtopic.php?t=300056)
possibly inspired by https://github.com/raspberrypi/linux/blob/223d1247c0b0c0659a65949b6b9c3de53fd14223/drivers/watchdog/bcm2835_wdt.c#L100
*/
static inline void __attribute__((noreturn)) reboot()
{
	uint32_t partition = PM_RSTS;
	partition &= ~0xfffffaaa;
	PM_RSTS = PM_PASSWORD | partition; // boot from partition 0
	PM_WDOG = PM_PASSWORD | 1; // set some timeout
	PM_RSTC = PM_PASSWORD | PM_RSTC_WRCFG_FULL_RESET;
	hang_cpu();
}
