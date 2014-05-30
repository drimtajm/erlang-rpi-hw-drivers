// An Erlang NIF which interfaces the i2c driver exposed by ioctl. 
// Copyright (C) 2013 Angela Johansson
//
// This file is part of free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This i2c interface is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this code.  If not, see <http://www.gnu.org/licenses/>.

#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>

#ifdef __ARM_EABI__
// ARM architecture, we're probably on a Raspberry Pi. Include "real" I2C headers
#include <linux/i2c-dev.h>
#else
// Not on ARM, probably compiling on a PC. Include I2C stub headers
#include "i2c-stub.h"
#endif

#include "erl_nif.h"

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

static ERL_NIF_TERM open_i2c_bus_nif(ErlNifEnv *env, int argc,
				     const ERL_NIF_TERM argv[]) {
  int file, address, result;
  if (!enif_get_int(env, argv[0], &address)) {
    return enif_make_badarg(env);
  }
#ifdef __ARM_EABI__
  // My Raspberry Pi uses I2C bus 0.
  // If necessary, change the line to match the bus number of _your_ system:
  char *filename = "/dev/i2c-0";
#else
  char *filename = "i2c-bus-stub";
#endif
  if ((file = open(filename, O_RDWR)) < 0) {
    result = errno;
    return make_error(env, result);
  }
  // The bus is open, now communicate to the device pointed out by address
  if (ioctl(file, I2C_SLAVE, address) < 0) {
    return make_error(env, errno);
  }
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, file));
}

static unsigned int swap_lower_bytes(const unsigned int val) {
  const unsigned int tmp = val & 0xFFFF;
  return ((tmp << 8) | (tmp >> 8)) & 0xFFFF;
}

static ERL_NIF_TERM read_i2c_raw_byte_nif(ErlNifEnv *env, int argc,
					  const ERL_NIF_TERM argv[]) {
  int file;
  int16_t result;
  uint8_t value;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((result = read(file, &value, 1)) != 1) {
    return make_error(env, errno);
  }
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, value));
}

static ERL_NIF_TERM read_i2c_raw_word_nif(ErlNifEnv *env, int argc,
					  const ERL_NIF_TERM argv[]) {
  int file;
  int32_t result;
  uint16_t value0;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((result = read(file, &value0, 2)) != 2) {
    return make_error(env, errno);
  }
  unsigned int value = swap_lower_bytes(value0);
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, value));
}

static ERL_NIF_TERM read_i2c_raw_signed_word_nif(ErlNifEnv *env, int argc,
						 const ERL_NIF_TERM argv[]) {
  int file;
  int32_t result;
  uint16_t value0;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((result = read(file, &value0, 2)) != 2) {
    return make_error(env, errno);
  }
  int value = (int16_t)swap_lower_bytes(value0);
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, value));
}

static ERL_NIF_TERM read_i2c_smbus_byte_nif(ErlNifEnv *env, int argc,
					    const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int reg;
  int16_t result;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[1], &reg)) || (reg >= 256)) {
    return enif_make_badarg(env);
  }
  if ((result = i2c_smbus_read_byte_data(file, reg)) < 0) {
    return make_error(env, errno);
  }
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, result));
}

static ERL_NIF_TERM read_i2c_smbus_word_nif(ErlNifEnv *env, int argc,
					    const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int reg;
  int32_t result;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[1], &reg)) || (reg >= 256)) {
    return enif_make_badarg(env);
  }
  if ((result = i2c_smbus_read_word_data(file, reg)) < 0) {
    return make_error(env, errno);
  }
  unsigned int value = swap_lower_bytes(result);
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, value));
}

static ERL_NIF_TERM read_i2c_smbus_signed_word_nif(ErlNifEnv *env, int argc,
						   const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int reg;
  int32_t result;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[1], &reg)) || (reg >= 256)) {
    return enif_make_badarg(env);
  }
  if ((result = i2c_smbus_read_word_data(file, reg)) < 0) {
    return make_error(env, errno);
  }
  int value = (int16_t)swap_lower_bytes(result);
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, value));
}

static ERL_NIF_TERM write_i2c_smbus_byte_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int reg, value0;
  int32_t result;
  uint8_t value;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[1], &reg)) || (reg >= 256)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[2], &value0)) || (value0 >= 256)) {
    return enif_make_badarg(env);
  }
  value = value0;
  if ((result = i2c_smbus_write_byte_data(file, reg, value)) < 0) {
    return make_error(env, errno);
  }
  return atom_ok;
}

static ERL_NIF_TERM write_i2c_smbus_word_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int file;
  unsigned int reg, value0;
  int32_t result;
  uint16_t value;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[1], &reg)) || (reg >= 256)) {
    return enif_make_badarg(env);
  }
  if ((!enif_get_uint(env, argv[2], &value0)) || (value0 >= 65536)) {
    return enif_make_badarg(env);
  }
  value = swap_lower_bytes(value0);
  if ((result = i2c_smbus_write_word_data(file, reg, value)) < 0) {
    return make_error(env, errno);
  }
  return atom_ok;
}

static ERL_NIF_TERM close_i2c_bus_nif(ErlNifEnv *env, int argc,
				      const ERL_NIF_TERM argv[]) {
  int file, result;
  if (!enif_get_int(env, argv[0], &file)) {
    return enif_make_badarg(env);
  }
  if ((result = close(file)) < 0) {
    result = errno;
    return enif_make_tuple(env, 2, atom_error, errno2atom(env, result));
    //return enif_make_tuple(env, 2, atom_error, enif_make_int(env, result));  
  }
  return atom_ok;
}

static ErlNifFunc nif_funcs[] =
    {
      {"open_i2c_bus_nif",               1, open_i2c_bus_nif},
      {"read_i2c_raw_byte_nif",          1, read_i2c_raw_byte_nif},
      {"read_i2c_raw_word_nif",          1, read_i2c_raw_word_nif},
      {"read_i2c_raw_signed_word_nif",   1, read_i2c_raw_signed_word_nif},
      {"read_i2c_smbus_byte_nif",        2, read_i2c_smbus_byte_nif},
      {"read_i2c_smbus_word_nif",        2, read_i2c_smbus_word_nif},
      {"read_i2c_smbus_signed_word_nif", 2, read_i2c_smbus_signed_word_nif},
      {"write_i2c_smbus_byte_nif",       3, write_i2c_smbus_byte_nif},
      {"write_i2c_smbus_word_nif",       3, write_i2c_smbus_word_nif},
      {"close_i2c_bus_nif",              1, close_i2c_bus_nif}
    };

ERL_NIF_INIT(i2c_interface, nif_funcs, load, NULL, NULL, NULL)
