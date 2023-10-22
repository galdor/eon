-module(eon_string).

-export([binary/1, string/1]).

-spec binary(unicode:chardata()) -> binary().
binary(Data) ->
  case unicode:characters_to_binary(Data) of
    Bin when is_binary(Bin) ->
      Bin;
    {error, _, Rest} ->
      error({invalid_unicode_data, Rest});
    {incomplete, _, Rest} ->
      error({truncated_unicode_data, Rest})
  end.

-spec string(unicode:chardata()) -> string().
string(Data) ->
  case unicode:characters_to_list(Data) of
    String when is_list(String) ->
      String;
    {error, _, Rest} ->
      error({invalid_unicode_data, Rest});
    {incomplete, _, Rest} ->
      error({truncated_unicode_data, Rest})
  end.
