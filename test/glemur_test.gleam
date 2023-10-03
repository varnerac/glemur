import gleam/option.{None, Some}
import gleeunit
import glemur_config
import glemur.{Element, NoContent, XmlDocument}

pub fn main() {
  gleeunit.main()
}

pub fn parse_ns_empty_element_test() {
  let assert Ok(XmlDocument(
    encoding: None,
    el: Element(
      attrs: [],
      pis: [],
      content: NoContent,
      name: #(Some("https://namespace.com/a/"), "empty_el"),
    ),
    pis: [],
    standalone: None,
    version: None,
  )) =
    glemur_config.config1
    |> glemur.parse_document(
      "<a:empty_el xmlns:a=\"https://namespace.com/a/\"/>",
    )
}
// pub fn parse_nested_ns_test() {
//   let assert Ok(#(
//     _state,
//     Element(
//       name: #(Some("https://ns.com/a/"), "el1"),
//       attrs: [],
//       content: [
//         ElContent(Element(
//           name: #(Some("https://ns.com/"), "el2"),
//           attrs: [
//             #(#(None, "bar"), "baz"),
//             #(#(Some("https://ns.com/a/"), "bing"), "bang"),
//           ],
//           content: [],
//         )),
//       ],
//     ),
//   )) =
//     "<a:el1 xmlns=\"https://ns.com/\" xmlns:a=\"https://ns.com/a/\">
//        <el2 bar=\"baz\" a:bing=\"bang\"/>
//      </a:el1>"
//     |> parse.new_state(glemur_config.config1, _)
//     |> parse.parse_el(None, [])
// }

// pub fn parse_empty_element_test() {
//   let assert Ok(#(
//     _state,
//     Element(name: #(None, "empty_el"), attrs: [], content: []),
//   )) =
//     "<empty_el/>"
//     |> parse.new_state(glemur_config.config1, _)
//     |> parse.parse_el(None, [])
//   let assert Ok(#(
//     _state,
//     Element(name: #(None, "empty_el"), attrs: [], content: []),
//   )) =
//     "<empty_el 
//      />"
//     |> parse.new_state(glemur_config.config1, _)
//     |> parse.parse_el(None, [])

//   let assert Error(InvalidName(_)) =
//     "< empty_el/>"
//     |> parse.new_state(glemur_config.config1, _)
//     |> parse.parse_el(None, [])

//   let assert Ok(#(
//     _,
//     Element(
//       name: #(None, "empty_el"),
//       attrs: [#(#(None, "foo"), "bar"), #(#(None, "baz"), "bam")],
//       content: [],
//     ),
//   )) =
//     "<empty_el foo=\"bar\"  baz='bam'  />"
//     |> parse.new_state(glemur_config.config1, _)
//     |> parse.parse_el(None, [])
// }
