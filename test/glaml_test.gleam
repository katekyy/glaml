import gleeunit
import gleeunit/should

import glaml

pub fn main() {
  gleeunit.main()
}

pub fn parse_string_test() {
  let assert Ok([a, b]) =
    glaml.parse_string("x: 2048\ny: 4096\nz: 1024\n---\nx: 0\ny: 0\nz: 0")

  should.equal(
    a,
    glaml.Document(
      glaml.NodeMap([
        #(glaml.NodeStr("x"), glaml.NodeInt(2048)),
        #(glaml.NodeStr("y"), glaml.NodeInt(4096)),
        #(glaml.NodeStr("z"), glaml.NodeInt(1024)),
      ]),
    ),
  )

  should.equal(
    b,
    glaml.Document(
      glaml.NodeMap([
        #(glaml.NodeStr("x"), glaml.NodeInt(0)),
        #(glaml.NodeStr("y"), glaml.NodeInt(0)),
        #(glaml.NodeStr("z"), glaml.NodeInt(0)),
      ]),
    ),
  )
}

pub fn parse_file_test() {
  let assert Ok(docs) = glaml.parse_file("./test/multi_document.yaml")

  should.equal(docs, [
    glaml.Document(glaml.NodeMap([#(glaml.NodeStr("doc"), glaml.NodeInt(1))])),
    glaml.Document(glaml.NodeMap([#(glaml.NodeStr("doc"), glaml.NodeInt(2))])),
    glaml.Document(glaml.NodeMap([#(glaml.NodeStr("doc"), glaml.NodeInt(3))])),
  ])
}

pub fn selector_test() {
  let assert Ok([doc]) = glaml.parse_file("./test/test.yaml")

  glaml.document_root(doc)
  |> glaml.select(
    glaml.Selector([
      glaml.SelectSeq(0),
      glaml.SelectMap(glaml.NodeStr("item_count")),
    ]),
  )
  |> should.equal(Ok(glaml.NodeInt(7)))
}

pub fn sugar_test() {
  let assert Ok([doc]) = glaml.parse_file("./test/test.yaml")

  glaml.select_sugar(glaml.document_root(doc), "#0.display name")
  |> should.equal(Ok(glaml.NodeStr("snow leopard")))
}

pub fn unicode_test() {
  let assert Ok([doc]) = glaml.parse_file("./test/unicode_test.yaml")

  glaml.select_sugar(glaml.document_root(doc), "records.#0.title")
  |> should.equal(Ok(glaml.NodeStr("健康サポート")))
}

pub fn error_test() {
  let node =
    glaml.NodeSeq([glaml.NodeMap([#(glaml.NodeStr("valid"), glaml.NodeNil)])])

  glaml.select(
    from: node,
    selector: glaml.Selector([
      glaml.SelectSeq(0),
      glaml.SelectMap(glaml.NodeStr("invalid")),
    ]),
  )
  |> should.equal(Error(glaml.NodeNotFound(1)))

  glaml.parse_selector("#invalid index")
  |> should.equal(Error(glaml.SelectorParseError))
}
