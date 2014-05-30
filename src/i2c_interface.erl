%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc
%%% An Erlang NIF that interfaces the I2C bus on the Raspberry Pi
%%%
%%% This interface uses the I2C ioctl driver which provides access to the
%%% I2C bus with a file descriptor. A precondition to using this software
%%% is a working I2C bus and an installed i2c-tools package.
%%%
%%% i2c_interface is free software:  you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% i2c_interface is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with this software.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% @end
%%% Created : 30 Jul 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(i2c_interface).

-export([open_i2c_bus/1, close_i2c_bus/1]).
-export([read_i2c_raw_byte/1, read_i2c_raw_word/1,
	 read_i2c_raw_signed_word/1, read_i2c_smbus_byte/2,
	 read_i2c_smbus_word/2, read_i2c_smbus_signed_word/2]).
-export([write_i2c_smbus_byte/3, write_i2c_smbus_word/3]).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-ifndef(test).
-on_load(on_load/0).
-else.
-export([on_load/0]).
-endif.

-type file_descriptor()  :: pos_integer().
-type i2c_address()      :: pos_integer().
-type register_address() :: byte().
-type error_code()       :: atom().
-type word()             :: 0..65535.

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    Filename = filename:join(PrivDir, ?MODULE),
    ok = erlang:load_nif(Filename, 0).


-spec(open_i2c_bus(i2c_address()) ->
	     {ok, file_descriptor()} | {error, error_code()}).
%%% @doc This opens the I2C bus, connects to the device at the specified
%%%      address and returns a file handle to the open bus.
open_i2c_bus(I2CAddress) ->
    open_i2c_bus_nif(I2CAddress).

-spec(close_i2c_bus(file_descriptor()) ->
	     ok | {error, error_code()}).
%%% @doc This closes the I2C bus for a given file handle.
close_i2c_bus(FileHandle) ->
    close_i2c_bus_nif(FileHandle).

-spec(read_i2c_raw_byte(file_descriptor()) ->
	     {ok, byte()} | {error, error_code()}).
%%% @doc This reads a raw byte value from the I2C device.
read_i2c_raw_byte(FileHandle) ->
    read_i2c_raw_byte_nif(FileHandle).

-spec(read_i2c_raw_word(file_descriptor()) ->
	     {ok, word()} | {error, error_code()}).
%%% @doc This reads a raw word value from the I2C device.
read_i2c_raw_word(FileHandle) ->
    read_i2c_raw_word_nif(FileHandle).

-spec(read_i2c_raw_signed_word(file_descriptor()) ->
	     {ok, word()} | {error, error_code()}).
%%% @doc This reads a signed word value from the I2C device.
read_i2c_raw_signed_word(FileHandle) ->
    read_i2c_raw_signed_word_nif(FileHandle).

-spec(read_i2c_smbus_byte(file_descriptor(), register_address()) ->
	     {ok, byte()} | {error, error_code()}).
%%% @doc This reads a register value from the I2C device.
read_i2c_smbus_byte(FileHandle, Register) ->
    read_i2c_smbus_byte_nif(FileHandle, Register).

-spec(read_i2c_smbus_word(file_descriptor(), register_address()) ->
	     {ok, word()} | {error, error_code()}).
%%% @doc This reads a register value from the I2C device.
read_i2c_smbus_word(FileHandle, Register) ->
    read_i2c_smbus_word_nif(FileHandle, Register).

-spec(read_i2c_smbus_signed_word(file_descriptor(), register_address()) ->
	     {ok, word()} | {error, error_code()}).
%%% @doc This reads a signed register value from the I2C device.
read_i2c_smbus_signed_word(FileHandle, Register) ->
    read_i2c_smbus_signed_word_nif(FileHandle, Register).

-spec(write_i2c_smbus_byte(file_descriptor(), register_address(),
		     byte()) ->
	     ok | {error, error_code()}).
%%% @doc This writes a byte value to a register on the I2C device.
write_i2c_smbus_byte(FileHandle, Register, Value) ->
    write_i2c_smbus_byte_nif(FileHandle, Register, Value).

-spec(write_i2c_smbus_word(file_descriptor(), register_address(),
		     word()) ->
	     ok | {error, error_code()}).
%%% @doc This writes a word value to a register on the I2C device.
write_i2c_smbus_word(FileHandle, Register, Value) ->
    write_i2c_smbus_word_nif(FileHandle, Register, Value).

%%%%%%%%%%%%%%%
%% Define stubs for NIF functions

open_i2c_bus_nif(_I2CAddress)                            -> ?nif_stub.
read_i2c_raw_byte_nif(_FileHandle)                       -> ?nif_stub.
read_i2c_raw_word_nif(_FileHandle)                       -> ?nif_stub.
read_i2c_raw_signed_word_nif(_FileHandle)                -> ?nif_stub.
read_i2c_smbus_byte_nif(_FileHandle, _Register)          -> ?nif_stub.
read_i2c_smbus_word_nif(_FileHandle, _Register)          -> ?nif_stub.
read_i2c_smbus_signed_word_nif(_FileHandle, _Register)   -> ?nif_stub.
write_i2c_smbus_byte_nif(_FileHandle, _Register, _Value) -> ?nif_stub.
write_i2c_smbus_word_nif(_FileHandle, _Register, _Value) -> ?nif_stub.
close_i2c_bus_nif(_FileHandle)                           -> ?nif_stub.

%%
%%%%%%%%%%%%%%%
