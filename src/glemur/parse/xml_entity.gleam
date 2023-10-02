import gleam/option.{Option}
import glemur/parse/el.{Element}

pub type XmlVersion {
  OneDotZero
}

pub type PI {
  PI(target: String, val: Option(String))
}

pub type XmlEntity {
  XmlDeclEntity(
    version: XmlVersion,
    encoding: Option(String),
    standalone: Option(Bool),
  )
  ElementEntity(Element)
  CommentEntity(String)
  PIEntity(PI)
}
