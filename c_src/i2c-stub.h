#ifndef I2C_STUB
#define I2c_STUB

#define I2C_SLAVE 0x0 /* dummy I2C address */

#include <stdint.h>

static int16_t dummy_byte_data = 0x50;
static int32_t dummy_word_data = 0x8385;

static inline int16_t i2c_smbus_read_byte_data(int file, const uint8_t reg) {
  return dummy_byte_data;
};

static inline int32_t i2c_smbus_read_word_data(int file, const uint8_t reg) {
  return dummy_word_data;
};

static inline int8_t i2c_smbus_write_byte_data(int file, const uint8_t reg,
					       const uint8_t byte_data) {
  dummy_byte_data = byte_data;
  return 0;
}; 

static inline int8_t i2c_smbus_write_word_data(int file, const uint8_t reg,
					       const uint16_t word_data) {
  dummy_word_data = word_data;
  return 0;
}; 

#endif
