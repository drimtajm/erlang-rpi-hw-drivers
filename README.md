erlang-rpi-hw-drivers
=====================

Different Erlang drivers for the Raspberry Pi GPIO

Prerequisites
---------------------
* Install packages i2c-tools and libi2c-dev to be able to use the I2C bus functionality
* Make sure I2C is not blacklisted in /etc/modprobe.d/raspi-blacklist.conf

Notes
---------------------
* GPIO pins can only be accesed as root
* I2C bus clock frequency can be changed by manually unloading and reloading the kernel module with extra arguments

TODO
---------------------
* Make the I2C code revision independant.
    Right now, the device filename is hardcoded, although it is different between Raspberry Pi Model A and Model B.
* Add (hardware) SPI functionality
    Most examples for the Raspberry Pi propose "bit banging" the SPI bus through "regular" GPIO pins.
    This is ridiculous since the Raspberry Pi provides hardware support for SPI at 10 MHz and there is a good kernel module for SPI, just like there is for I2C.
* Add UART functionality
