#define HIGH_BITS_SET							  0xc0000000
#define BANK_BITS 								  28:26
#define BANK_SET                                  0x00000000
#define BANK_MSB                                  28
#define BANK_LSB                                  26
#define ROW_BITS 								  25:12
#define ROW_SET                                   0x00000000
#define ROW_MSB                                   25
#define ROW_LSB                                   12
#define COL_BITS 								  11:2
#define COL_SET                                   0x00000000
#define COL_MSB                                   11
#define COL_LSB                                   2
#define HIGH_BITS_ADD							  0x00000000

#define GPIO_BASE 0x20200000 // for raspi2 & 3 0x20200000 for raspi1
#define LED_GPFSEL 4
#define LED_GPFBIT 21
#define LED_GPCLR 11
#define LED_GPIO_BIT 15
#define LED_GPSET 8
 
    // The offsets for reach register.
 
    // Controls actuation of pull up/down to ALL GPIO pins.
#define GPPUD (GPIO_BASE + 0x94)
 
    // Controls actuation of pull up/down for specific GPIO pin.
#define GPPUDCLK0 (GPIO_BASE + 0x98)
 
    // The base address for UART.
#define UART0_BASE 0x20201000 // for raspi2 & 3 0x20201000 for raspi1
 
    // The offsets for reach register for the UART.
#define UART0_DR     (UART0_BASE + 0x00)
#define UART0_RSRECR (UART0_BASE + 0x04)
#define UART0_FR     (UART0_BASE + 0x18)
#define UART0_ILPR   (UART0_BASE + 0x20)
#define UART0_IBRD   (UART0_BASE + 0x24)
#define UART0_FBRD   (UART0_BASE + 0x28)
#define UART0_LCRH   (UART0_BASE + 0x2C)
#define UART0_CR     (UART0_BASE + 0x30)
#define UART0_IFLS   (UART0_BASE + 0x34)
#define UART0_IMSC   (UART0_BASE + 0x38)
#define UART0_RIS    (UART0_BASE + 0x3C)
#define UART0_MIS    (UART0_BASE + 0x40)
#define UART0_ICR    (UART0_BASE + 0x44)
#define UART0_DMACR  (UART0_BASE + 0x48)
#define UART0_ITCR   (UART0_BASE + 0x80)
#define UART0_ITIP   (UART0_BASE + 0x84)
#define UART0_ITOP   (UART0_BASE + 0x88)
#define UART0_TDR    (UART0_BASE + 0x8C)

#define TS_TSENSSTAT 0x00030006

        /* ARM Mailbox */
#define ARM_SBM_OWN0 0x2000B800 /* ARM_BASE + 0x800 */
#define ARM_SBM_OWN1 0x2000B900

#define ARM_0_MAIL1_STA (ARM_SBM_OWN0 + 0xB8) /* Status register */
#define ARM_0_MAIL1_WRT (ARM_SBM_OWN0 + 0xA0) /* Write ARM->VC4 */
#define ARM_0_MAIL0_RD  (ARM_SBM_OWN0 + 0x80)
#define ARM_1_MAIL1_RD  (ARM_SBM_OWN1 + 0xA0)

#define ARM_MS_FULL 0x80000000