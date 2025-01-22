-module(yaml_ffi).

-include_lib("yamerl/include/yamerl_errors.hrl").

-export([parse_file/1, parse_string/1]).

-type document_node() ::
                    node_nil
                    | {node_str, String :: binary()}
                    | {node_bool, boolean()}
                    | {node_int, integer()}
                    | {node_float, float()}
                    | {node_seq, list(document_node())}
                    | {node_map, list({document_node(), document_node()})}.

-type document() :: {document, RootNode :: document_node()}.

% TODO
-type yaml_error() :: any().

% public
-spec parse_file(Path :: iolist() | binary()) -> {ok, list(document())} | {error, yaml_error()}.
parse_file(Path) ->
    try
        Docs = map_yamerl_docs(yamerl_constr:file(Path, [{detailed_constr, true}])),
        {ok, Docs}
    catch
        throw:#yamerl_exception{errors = [First | _]} ->
            {error, map_yamerl_error(First)};
        error:_ ->
            {error, {yaml_error, unexpected_parsing_error}}
    end.

% public
-spec parse_string(String :: iolist() | binary()) -> {ok, list(document())} | {error, yaml_error()}.
parse_string(String) ->
    try
        Docs = map_yamerl_docs(yamerl_constr:string(String, [{detailed_constr, true}])),
        {ok, Docs}
    catch
        throw:#yamerl_exception{errors = [First | _]} ->
            {error, map_yamerl_error(First)};
        error:_ ->
            {error, {yaml_error, unexpected_parsing_error}}
    end.

-spec map_yamerl_error(Error :: #yamerl_parsing_error{} | #yamerl_invalid_option{}) -> yaml_error().
map_yamerl_error(Error) ->
    case Error of
        #yamerl_parsing_error{text = undefined} ->
            {yaml_error, unexpected_parsing_error};

        #yamerl_parsing_error{text = Message, line = undefined, column = undefined} ->
           {yaml_error, unicode:characters_to_binary(Message), {0, 0}};

        #yamerl_parsing_error{text = Message, line = Line, column = Col} ->
            {yaml_error, unicode:characters_to_binary(Message), {Line, Col}};

        #yamerl_invalid_option{text = undefined} ->
            {yaml_error, unexpected_parsing_error};

        #yamerl_invalid_option{text = Message} ->
            {yaml_error, unicode:characters_to_binary(Message), {0, 0}}
    end.

-spec map_yamerl_docs(Documents :: list({yamerl_doc, any()})) -> list(document()).
map_yamerl_docs(Documents) ->
    lists:map(fun map_yamerl_doc/1, Documents).

-spec map_yamerl_doc(Document :: {yamerl_doc, any()}) -> document().
map_yamerl_doc(Document) ->
    {yamerl_doc, RootNode} = Document,
    {document, map_yamerl_node(RootNode)}.

% TODO: Add function spec
map_yamerl_node(Node) ->
    case Node of
        {yamerl_null, _, _Tag, _Loc} ->
            node_nil;

        {yamerl_str, _, _Tag, _Loc, String} ->
            {node_str, unicode:characters_to_binary(String)};

        {yamerl_bool, _, _Tag, _Loc, Bool} when is_boolean(Bool) ->
            {node_bool, Bool};

        {yamerl_int, _, _Tag, _Loc, Int} when is_integer(Int) ->
            {node_int, Int};

        {yamerl_float, _, _Tag, _Loc, Float} when is_float(Float) ->
            {node_float, Float};

        {yamerl_seq, _, _Tag, _Loc, Nodes, _Count} when is_list(Nodes) ->
            {node_seq, lists:map(fun map_yamerl_node/1, Nodes)};

        {yamerl_map, _, _Tag, _Loc, Pairs} when is_list(Pairs) ->
            {node_map, map_yamerl_map(Pairs)}
    end.

% TODO: Add function spec
map_yamerl_map(Pairs) ->
    F = fun({Key, Value}) ->
        {map_yamerl_node(Key), map_yamerl_node(Value)}
    end,
    lists:map(F, Pairs).
