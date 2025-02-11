import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// A YAML document error containing a message — `msg` and it's location — `loc`.
///
pub type YamlError {
  UnexpectedParsingError
  ParsingError(msg: String, loc: YamlErrorLoc)
}

pub type YamlErrorLoc {
  YamlErrorLoc(line: Int, column: Int)
}

/// A YAML document.<br />
/// To get the root `Node` call `document_root` on it, like this:
///
/// ```gleam
/// let document = Document(root: NodeNil)
/// let assert NodeNil = document_root(document)
/// ```
///
pub type Document {
  Document(root: Node)
}

/// A YAML document node.
///
pub type Node {
  NodeNil
  NodeStr(String)
  NodeBool(Bool)
  NodeInt(Int)
  NodeFloat(Float)
  NodeSeq(List(Node))
  NodeMap(List(#(Node, Node)))
}

/// Parse a YAML file located in `path` into a list of YAML documents.
///
@external(erlang, "yaml_ffi", "parse_file")
pub fn parse_file(path: String) -> Result(List(Document), YamlError)

/// Parse a string into a list of YAML documents.
///
@external(erlang, "yaml_ffi", "parse_string")
pub fn parse_string(string: String) -> Result(List(Document), YamlError)

/// Gets the root `Node` of a YAML document.
///
/// ## Examples
///
/// ```gleam
/// let document = Document(root: NodeNil)
/// let assert NodeNil = document_root(document)
/// ```
///
pub fn document_root(document: Document) -> Node {
  document.root
}

/// A document selector that contains a sequence of selections leading to a `Node`.
///
pub type Selector {
  Selector(List(Selection))
}

/// A `Node` selection used by `Selector`.
///
pub type Selection {
  SelectMap(key: Node)
  SelectSeq(index: Int)
}

pub type SelectorError {
  NodeNotFound(at: Int)
  SelectorParseError
}

/// Parses the `selector` and queries the given `node` with it.
///
/// ## Examples
///
/// ```gleam
/// let map = NodeMap([
///   #(NodeStr("list"), NodeMap([
///     #(NodeStr("elements"), NodeSeq([NodeInt(101)]))
///   ])),
///   #(NodeStr("linked"), NodeBool(False)),
/// ])
///
/// let assert Ok(NodeInt(101)) = select_sugar(from: map, selector: "list.elements.#0")
/// ```
///
pub fn select_sugar(
  from node: Node,
  selector selector: String,
) -> Result(Node, SelectorError) {
  use selector <- result.try(parse_selector(selector))
  select(node, selector)
}

/// Queries the given `node` with a `Selector`.
///
/// ## Examples
///
/// ```gleam
/// let map = NodeMap([
///   #(NodeStr("lib name"), NodeStr("glaml")),
///   #(NodeStr("stars"), NodeInt(7)),
/// ])
///
/// let assert Ok(NodeInt(7)) = select(from: map, selector: Selector([SelectMap(NodeStr("stars"))]))
/// ```
///
pub fn select(
  from node: Node,
  selector selector: Selector,
) -> Result(Node, SelectorError) {
  do_select(node, selector, 0)
}

fn do_select(
  node: Node,
  selector: Selector,
  select_idx: Int,
) -> Result(Node, SelectorError) {
  case selector {
    Selector([select, ..selector_tail]) ->
      case select {
        SelectSeq(index) ->
          case node {
            NodeSeq(seq) ->
              case list_at(seq, index) {
                option.Some(node) ->
                  do_select(node, Selector(selector_tail), select_idx + 1)
                option.None -> Error(NodeNotFound(select_idx))
              }
            _ -> Error(NodeNotFound(select_idx))
          }
        SelectMap(key) ->
          case node {
            NodeMap(pairs) ->
              case list.key_find(pairs, key) {
                Ok(node) ->
                  do_select(node, Selector(selector_tail), select_idx + 1)
                Error(_) -> Error(NodeNotFound(select_idx))
              }
            _ -> Error(NodeNotFound(select_idx))
          }
      }
    Selector([]) -> Ok(node)
  }
}

fn list_at(l: List(a), index: Int) -> option.Option(a) {
  case l {
    [head, ..tail] ->
      case index {
        0 -> option.Some(head)
        _ -> list_at(tail, index - 1)
      }
    [] -> option.None
  }
}

/// Parses `selector` as a `Selector` and
///
pub fn parse_selector(selector: String) -> Result(Selector, SelectorError) {
  use selections <- result.try(
    do_parse_selector(string.split(selector, on: "."), []),
  )
  Ok(Selector(list.reverse(selections)))
}

fn do_parse_selector(
  selector_parts: List(String),
  acc: List(Selection),
) -> Result(List(Selection), SelectorError) {
  case selector_parts {
    ["", ..tail] -> do_parse_selector(tail, acc)
    [part, ..tail] ->
      case string.starts_with(part, "#") {
        True ->
          case int.parse(string.drop_start(part, 1)) {
            Ok(index) -> do_parse_selector(tail, [SelectSeq(index), ..acc])
            Error(Nil) -> Error(SelectorParseError)
          }
        False -> do_parse_selector(tail, [SelectMap(NodeStr(part)), ..acc])
      }
    [] -> Ok(acc)
  }
}
