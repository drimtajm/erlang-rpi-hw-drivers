#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/types.h>

#ifdef __ARM_EABI__
// ARM architecture, we're probably on a Raspberry Pi. Include "real" SPI headers
#include <linux/spi/spidev.h>
#else
// Not on ARM, probably compiling on a PC. Include SPI stub headers
#warning "Not on a Raspberry Pi!"
#include "spi-stub.h"
#endif

#include "erl_nif.h"

#ifdef __ARM_EABI__
static const char *device_filename = "/dev/spidev0.0";
#else
static const char *device_filename = "spi-bus-stub";
#endif
static uint8_t mode = 0;
static uint8_t bits = 8;
static uint32_t speed = 500000;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_other;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    atom_ok    = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_other = enif_make_atom(env, "other");
    return 0;
}

static ERL_NIF_TERM errno2atom(ErlNifEnv *env, const int error_code) {
  switch (error_code) {
  case EACCES: return enif_make_atom(env, "eacces");
  case EINVAL: return enif_make_atom(env, "einval");
  case ENOENT: return enif_make_atom(env, "enoent");
  case EBADF:  return enif_make_atom(env, "ebadf");
    // TODO: implement other error codes
  default:     return enif_make_tuple(env, 2, atom_other,
				      enif_make_int(env, error_code));
  }
}

static ERL_NIF_TERM make_error(ErlNifEnv *env, const int error_code) {
  return enif_make_tuple(env, 2, atom_error, errno2atom(env, error_code));
}

static ERL_NIF_TERM open_spi_bus_nif(ErlNifEnv *env, int argc,
				     const ERL_NIF_TERM argv[]) {
  int file, result;
  if ((file = open(device_filename, O_RDWR)) < 0) {
    result = errno;
    return make_error(env, result);
  }
  // The bus is open, now set bus attributes
  // Set mode 0
  if (ioctl(file, SPI_IOC_WR_MODE, &mode) < 0) {
    return make_error(env, errno);
  }
  if (ioctl(file, SPI_IOC_RD_MODE, &mode) < 0) {
    return make_error(env, errno);
  }
  // Set number of bits to 8
  if (ioctl(file, SPI_IOC_WR_BITS_PER_WORD, &bits) < 0) {
    return make_error(env, errno);
  }
  if (ioctl(file, SPI_IOC_RD_BITS_PER_WORD, &bits) < 0) {
    return make_error(env, errno);
  }
  // Set speed
  if (ioctl(file, SPI_IOC_WR_MAX_SPEED_HZ, &speed) < 0) {
    return make_error(env, errno);
  }
  if (ioctl(file, SPI_IOC_RD_MAX_SPEED_HZ, &speed) < 0) {
    return make_error(env, errno);
  }
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, file));
}

static ERL_NIF_TERM transfer_spi_data_nif(ErlNifEnv *env, int argc,
					  const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int byte_count, byte_value;
  ERL_NIF_TERM head, tail, list;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_list_length(env, argv[1], &byte_count)) {
    return enif_make_badarg(env);
  }
  uint8_t tx[byte_count];
  uint8_t rx[byte_count];
  list = argv[1];
  unsigned int i=0;
  while (i < byte_count) {
    if (!enif_get_list_cell(env, list, &head, &tail)) {
      return enif_make_badarg(env);
    }
    if (!enif_get_uint(env, head, &byte_value)) {
      return enif_make_badarg(env);
    }
    tx[i] = (uint8_t)byte_value;
    list = tail;
    ++i;
  }

  struct spi_ioc_transfer tr = {
    .tx_buf = (unsigned long)tx,
    .rx_buf = (unsigned long)rx,
    .len = byte_count,
    .delay_usecs = 0,
    .speed_hz = speed,
    .bits_per_word = bits,
    .cs_change = 1,
  };

  if (!ioctl(file, SPI_IOC_MESSAGE(1), &tr)) {
    return make_error(env, errno);
  }
  ERL_NIF_TERM result[byte_count];
  i = 0;
  while (i < byte_count) {
    result[i] = enif_make_int(env, rx[i]);
    ++i;
  }
  return enif_make_tuple(env, 2, atom_ok,
			 enif_make_list_from_array(env, result, byte_count));
}

static ERL_NIF_TERM close_spi_bus_nif(ErlNifEnv *env, int argc,
				      const ERL_NIF_TERM argv[]) {
  int file, result;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((result = close(file)) < 0) {
    result = errno;
    return enif_make_tuple(env, 2, atom_error, errno2atom(env, result));
  }
  return atom_ok;
}

static ErlNifFunc nif_funcs[] =
    {
      {"open_spi_bus_nif",               0, open_spi_bus_nif},
      {"transfer_spi_data_nif",          2, transfer_spi_data_nif},
      {"close_spi_bus_nif",              1, close_spi_bus_nif}
    };

ERL_NIF_INIT(spi_interface, nif_funcs, load, NULL, NULL, NULL)
