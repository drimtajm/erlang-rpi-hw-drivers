-module(spi_interface).

-export([open_spi_bus/1, close_spi_bus/1]).
-export([transfer_spi_data/2, transfer_spi_data_no_cs/2,
	 chip_enable/1, chip_disable/1]).

-define(nif_stub,
	erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-ifndef(test).
-on_load(on_load/0).
-else.
-export([on_load/0]).
-endif.

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

open_spi_bus(CEPin) ->
    {ok, FileHandle} = open_spi_bus_nif(),
    gpio:open_pin(CEPin),
    gpio:set_output(CEPin),
    gpio:write_pin(CEPin, 1),
    {ok, {FileHandle, CEPin}}.

close_spi_bus({FileHandle, CEPin}) ->
    gpio:close_pin(CEPin),
    close_spi_bus_nif(FileHandle).

transfer_spi_data({FileHandle, CEPin}, Data)
  when is_list(Data) ->
    gpio:write_pin(CEPin, 0),
    {ok, Result} = transfer_spi_data_nif(FileHandle, Data),
    gpio:write_pin(CEPin, 1),
    {ok, Result}.

transfer_spi_data_no_cs({FileHandle, _CEPin}, Data)
  when is_list(Data) ->
    transfer_spi_data_nif(FileHandle, Data).

chip_enable({_FileHandle, CEPin}) ->
    gpio:write_pin(CEPin, 0).

chip_disable({_FileHandle, CEPin}) ->
    gpio:write_pin(CEPin, 1).

%%%%%%%%%%%%%%%
%% Define stubs for NIF functions

open_spi_bus_nif()                    -> ?nif_stub.
transfer_spi_data_nif(_Handle, _Data) -> ?nif_stub.
close_spi_bus_nif(_Handle)            -> ?nif_stub.

%%
%%%%%%%%%%%%%%%
