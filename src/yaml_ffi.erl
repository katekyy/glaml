-module(yaml_ffi).

-export([parse_file/1, parse_string/1]).

-include_lib("yamerl/include/yamerl_errors.hrl").

-type yamerl_doc() :: any().

-type doc_error() :: {doc_error, binary(), {integer(), integer()}}.
-type document() :: {document, list(doc_node())}.
-type doc_node() ::
    {doc_node_map, list({doc_node(), doc_node()})}
    | {doc_node_seq, list(doc_node())}
    | {doc_node_str, binary()}
    | {doc_node_int, integer()}
    | {doc_node_float, float()}
    | doc_node_nil.

% public
-spec parse_file(Path :: binary()) -> {ok, document()} | {error, doc_error()}.
parse_file(Path) ->
    hande_yamerl_result(file_detailed(Path)).

% public
-spec parse_string(String :: binary()) -> {ok, document()} | {error, doc_error()}.
parse_string(String) ->
    hande_yamerl_result(string_detailed(String)).

-spec hande_yamerl_result(Res :: {ok, yamerl_doc() | empty_doc} | {error, yamerl_error()}) ->
    {ok, document()} | {error, doc_error()}.
hande_yamerl_result(Res) ->
    case Res of
        {ok, YamerlDoc} ->
            {ok, from_yamerl(YamerlDoc)};
        {error,
            {yamerl_parsing_error, _Type, Message, undefined, undefined, _Name, _Token, _Extra}} ->
            {error, {doc_error, list_to_binary(Message), {0, 0}}};
        {error, {yamerl_parsing_error, _Type, Message, Line, Column, _Name, _Token, _Extra}} ->
            {error, {doc_error, list_to_binary(Message), {Line, Column}}};
        {error, {yamerl_invalid_option, _Type, Message, _Option}} ->
            {error, {doc_error, list_to_binary(Message), {0, 0}}}
    end.

-spec string_detailed(String :: binary()) ->
    {ok, yamerl_doc() | empty_doc} | {error, yamerl_error()}.
string_detailed(String) ->
    try
        case yamerl_constr:string(String, [{detailed_constr, true}]) of
            [YamerlDoc] -> {ok, YamerlDoc};
            [] -> {ok, empty_doc}
        end
    catch
        throw:#yamerl_exception{errors = Errors} ->
            [First | _] = Errors,
            {error, First}
    end.

-spec file_detailed(Path :: binary()) ->
    {ok, yamerl_doc() | empty_doc} | {error, yamerl_error()}.
file_detailed(Path) ->
    try
        case yamerl_constr:file(Path, [{detailed_constr, true}]) of
            [YamerlDoc] -> {ok, YamerlDoc};
            [] -> {ok, empty_doc}
        end
    catch
        throw:#yamerl_exception{errors = Errors} ->
            [First | _] = Errors,
            {error, First}
    end.

-spec from_yamerl(YamerlDoc :: yamerl_doc()) -> document().
from_yamerl(YamerlDoc) ->
    case YamerlDoc of
        {yamerl_doc, RootNode} ->
            {document, from_yamerl_node(RootNode)};
        empty_doc ->
            {document, {doc_node_seq, []}}
    end.

-spec from_yamerl_node(YamerlNode :: any()) -> doc_node().
from_yamerl_node(YamerlNode) ->
    case YamerlNode of
        {yamerl_null, yamerl_node_null, _, _Line} ->
            doc_node_nil;
        {yamerl_str, yamerl_node_str, _, _Line, String} ->
            {doc_node_str, list_to_binary(String)};
        {yamerl_int, yamerl_node_int, _, _Line, Integer} ->
            {doc_node_int, Integer};
        {yamerl_float, yamerl_node_float, _, _Line, Float} ->
            {doc_node_float, Float};
        {yamerl_seq, yamerl_node_seq, _, _Line, Elements, _Count} ->
            {doc_node_seq, from_yamerl_nodes(Elements)};
        {yamerl_map, yamerl_node_map, _, _Line, Elements} ->
            {doc_node_map, from_yamerl_map_elems(Elements)}
    end.

-spec from_yamerl_nodes(YamerlNodes :: list(any())) -> list(doc_node()).
from_yamerl_nodes(YamerlNodes) ->
    case YamerlNodes of
        [] -> [];
        [Elem | Tail] -> [from_yamerl_node(Elem) | from_yamerl_nodes(Tail)]
    end.

-spec from_yamerl_map_elem(YamerlElem :: any()) -> {doc_node(), doc_node()}.
from_yamerl_map_elem(YamerlElem) ->
    case YamerlElem of
        {Key, Value} -> {from_yamerl_node(Key), from_yamerl_node(Value)}
    end.

-spec from_yamerl_map_elems(YamerlMapElems :: list(any())) -> list({doc_node(), doc_node()}).
from_yamerl_map_elems(YamerlMapElems) ->
    case YamerlMapElems of
        [] -> [];
        [Elem | Tail] -> [from_yamerl_map_elem(Elem) | from_yamerl_map_elems(Tail)]
    end.
