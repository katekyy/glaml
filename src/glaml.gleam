import gleam/int
import gleam/list
import gleam/string

pub type Document {
  Document(root: DocNode)
}

/// A Document Error dispatched by yamerl.
/// It contains a string - `msg` and a tuple of 2 integers - `loc`.
/// 
/// The location is: `#(line, column)`.
///
pub type DocError {
  DocError(msg: String, loc: #(Int, Int))
}

pub type DocNode {
  DocNodeNil
  DocNodeStr(string: String)
  DocNodeInt(int: Int)
  DocNodeSeq(nodes: List(DocNode))
  DocNodeMap(nodes: List(#(DocNode, DocNode)))
}

pub type NodeGetError {
  NodeNotFound(which: String)
  InvalidSugar
}

pub type PathRule {
  Seq(idx: Int)
  Map(key: String)
}

/// Parses the YAML file at `path` and returns either `Ok(Document(...))` or `Error(DocError(...))`.
/// 
@external(erlang, "yaml_ffi", "parse_file")
pub fn parse_file(path: String) -> Result(Document, DocError)

/// Parses the YAML string and returns either `Ok(Document(...))` or `Error(DocError(...))`.
/// 
@external(erlang, "yaml_ffi", "parse_string")
pub fn parse_string(string: String) -> Result(Document, DocError)

/// Returns the root node of the `Document`.
/// 
pub fn doc_node(doc: Document) -> DocNode {
  doc.root
}

/// Does the same thing as `get` but instead of a list of `PathRule`s,
/// you write a `String` that will get parsed into a list of `PathRule`s for you.
/// 
/// ## Syntax
/// 
/// ```gleam
/// "some_key" == [Map("some_key")]
/// "#0" == [Seq(0)]
/// "combination.#0.of.these" == [Map("combination"), Seq(0), Map("of"), Map("these")]
/// 
/// // You can write as many dots as you want - an empty key points at the current node.
/// "...test..#0." == [Map("test"), Seq(0)]
/// ```
/// 
pub fn sugar(
  from node: DocNode,
  to path: String,
) -> Result(DocNode, NodeGetError) {
  case build_sugar(string.split(path, ".")) {
    Ok(path) -> get(node, path)
    Error(_nil) -> Error(InvalidSugar)
  }
}

// test.#0.name = ["test", "#0", "name"] = [Map("test"), Seq(0), Map("name")]
fn build_sugar(sugar: List(String)) -> Result(List(PathRule), Nil) {
  case sugar {
    [] -> Ok([])
    ["", ..tail] -> build_sugar(tail)
    [elem, ..tail] ->
      case string.starts_with(elem, "#") {
        True ->
          case int.parse(string.drop_left(elem, 1)) {
            Ok(idx) ->
              case build_sugar(tail) {
                Ok(tail) -> Ok([Seq(idx), ..tail])
                Error(_nil) -> Error(Nil)
              }
            Error(_nil) -> Error(Nil)
          }
        False ->
          case build_sugar(tail) {
            Ok(tail) -> Ok([Map(elem), ..tail])
            Error(_nil) -> Error(Nil)
          }
      }
  }
}

/// Traverses the `DocNode` and tries to find another `DocNode` by matching the `PathRule`s.
/// 
/// ## Example
/// 
/// ```gleam
/// let assert Ok(doc) = parse_string("
/// employees:
///   - name: Gordon
///     surname: Befrey
///     field: Database Infrastructure
///   - name: Caroline
///     surname: Gaster
///     field: Networking
/// ")
/// let doc = doc_node(doc)
/// 
/// get(doc, [Map("employees"), Seq(0), Map("field")])
/// // -> Ok(DocNodeStr("Database Infrastructure"))
/// 
/// get(doc, [Map("employees"), Seq(1), Map("passwords"), Seq(0)])
/// //                                  ~~~~~~~~~~~~~~~~
/// //                                    | reading backwards
/// // -> Error(NodeNotFound("reverse_idx:1,map:passwords"))
/// ```
/// 
pub fn get(
  from node: DocNode,
  to path: List(PathRule),
) -> Result(DocNode, NodeGetError) {
  case path {
    [rule, ..next_rules] ->
      case rule {
        Seq(idx) ->
          case node {
            DocNodeSeq(_) ->
              case seq_at(node, idx) {
                Ok(node) -> get(node, next_rules)
                Error(_nil) -> Error(NodeNotFound(rule_trace(path)))
              }
            _ -> Error(NodeNotFound(rule_trace(path)))
          }
        Map(key) ->
          case node {
            DocNodeMap(pairs) ->
              case
                list.find(pairs, fn(pair) {
                  case pair.0 {
                    DocNodeStr(check_key) -> check_key == key
                    DocNodeInt(check_key) -> int.to_string(check_key) == key
                    _ -> False
                  }
                })
              {
                Ok(pair) -> get(pair.1, next_rules)
                Error(_nil) -> Error(NodeNotFound(rule_trace(path)))
              }
            _ -> Error(NodeNotFound(rule_trace(path)))
          }
      }
    [] -> Ok(node)
  }
}

fn rule_trace(path: List(PathRule)) -> String {
  "reverse_idx:"
  <> int.to_string(list.length(path) - 1)
  <> ","
  <> case path {
    [Map(key), ..] -> "map:" <> key
    [Seq(idx), ..] -> "seq:" <> int.to_string(idx)
    [] -> ""
  }
}

fn seq_at(node: DocNode, idx: Int) -> Result(DocNode, Nil) {
  case node {
    DocNodeSeq([]) -> Error(Nil)
    DocNodeSeq([elem, ..tail]) ->
      case idx {
        0 -> Ok(elem)
        _ -> seq_at(DocNodeSeq(tail), idx - 1)
      }
    _ -> Error(Nil)
  }
}
