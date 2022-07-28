#define HIGH_BITS_SET							  0xc0000000
#define BANK_BITS 								  28:26
#define BANK_SET                                  0x00000000
#define BANK_MSB                                  28
#define BANK_LSB                                  26
#define BANK_RBC_LSB							  15
#define ROW_BITS 								  25:12
#define ROW_SET                                   0x00000000
#define ROW_MSB                                   25
#define ROW_LSB                                   12
#define COL_BITS 								  11:2
#define COL_SET                                   0x00000000
#define COL_MSB                                   11
#define COL_LSB                                   2
#define HIGH_BITS_ADD							  0x00000000

#define GPIO_BASE 0x7e200000 // for raspi2 & 3 0x20200000 for raspi1
#define LED_GPFSEL 4
#define LED_GPFBIT 21
#define LED_GPCLR 11
#define LED_GPIO_BIT 15
#define LED_GPSET 8