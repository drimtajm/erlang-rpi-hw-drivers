{application, 'erlang-rpi-hw-drivers', [
	      {description, "Erlang GPIO drivers for the Raspberry Pi"},
	      {vsn, "1.0"},
	      {modules, [i2c_interface, gpio, spi_interface, led_array_test]},
	      {applications, [kernel, stdlib]}]}.