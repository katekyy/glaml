import gleeunit
import gleeunit/should

import glaml

pub fn main() {
  gleeunit.main()
}

pub fn parser_test() {
  glaml.parse_file("./test/test.yaml")
  |> should.equal(
    Ok(
      glaml.Document(
        glaml.DocNodeSeq([
          glaml.DocNodeMap([
            #(glaml.DocNodeStr("name"), glaml.DocNodeStr("katekyy")),
            #(glaml.DocNodeStr("x"), glaml.DocNodeInt(7)),
          ]),
        ]),
      ),
    ),
  )

  glaml.parse_string(
    "
id: 7
jobs:
  - image: ubuntu:latest
    username: stay-calm
  ",
  )
  |> should.equal(
    Ok(
      glaml.Document(
        glaml.DocNodeMap([
          #(glaml.DocNodeStr("id"), glaml.DocNodeInt(7)),
          #(
            glaml.DocNodeStr("jobs"),
            glaml.DocNodeSeq([
              glaml.DocNodeMap([
                #(glaml.DocNodeStr("image"), glaml.DocNodeStr("ubuntu:latest")),
                #(glaml.DocNodeStr("username"), glaml.DocNodeStr("stay-calm")),
              ]),
            ]),
          ),
        ]),
      ),
    ),
  )

  glaml.parse_file("./test/test2.yaml")
  |> should.equal(
    Ok(
      glaml.Document(
        glaml.DocNodeSeq([
          glaml.DocNodeMap([
            #(glaml.DocNodeStr("name"), glaml.DocNodeStr("Gleam")),
            #(
              glaml.DocNodeStr("repo-url"),
              glaml.DocNodeStr("https://github.com/gleam-lang/gleam"),
            ),
            #(glaml.DocNodeStr("nil"), glaml.DocNodeNil),
            #(glaml.DocNodeStr("nil2"), glaml.DocNodeNil),
            #(
              glaml.DocNodeStr("items"),
              glaml.DocNodeMap([
                #(glaml.DocNodeStr("milk"), glaml.DocNodeStr("1L")),
                #(glaml.DocNodeStr("apple"), glaml.DocNodeInt(10)),
                #(glaml.DocNodeStr("egg"), glaml.DocNodeStr("dozen")),
              ]),
            ),
          ]),
        ]),
      ),
    ),
  )
}

pub fn get_test() {
  let assert Ok(doc) = glaml.parse_file("./test/test2.yaml")

  let assert Ok(doc_node) =
    glaml.doc_node(doc)
    |> glaml.get([])

  should.equal(doc_node, glaml.doc_node(doc))

  glaml.get(doc_node, [glaml.Seq(0), glaml.Map("items"), glaml.Map("apple")])
  |> should.equal(Ok(glaml.DocNodeInt(10)))

  glaml.get(doc_node, [glaml.Seq(1), glaml.Map("items")])
  |> should.equal(Error(glaml.NodeNotFound("reverse_idx:1,seq:1")))
}

pub fn sugar_test() {
  let assert Ok(doc) = glaml.parse_file("./test/test2.yaml")

  let assert Ok(doc) =
    glaml.doc_node(doc)
    |> glaml.sugar("")

  glaml.sugar(doc, ".#0...repo-url..")
  |> should.equal(Ok(glaml.DocNodeStr("https://github.com/gleam-lang/gleam")))
}
