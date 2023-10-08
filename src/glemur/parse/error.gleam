import glemur/parse/util

pub type ParserError {
  InvalidName(String)
  InvalidCharacter(String)
  InvalidCharacterReference(String)
  UnexpectedCharacter(String)
  UnexpectedEndOfStream(String)
  InvalidComment(String)
  ForbiddenComment(String)
  ForbiddenPi(String)
  ForbiddenDtd(String)
  InvalidAttributeNamespacePrefix(String)
  InvalidElementNamespacePrefix(String)
  InvalidElementPrefix(String)
  InvalidNamespaceDecl(String)
  InvalidNamespaceUri(String)

  DuplicateAttributeNames(String)
}

pub fn fc(bs: BitString) {
  bs
  |> util.to_str
  |> ForbiddenComment
  |> Error
}

pub fn ueos(bs: BitString) {
  bs
  |> util.to_str
  |> UnexpectedEndOfStream
  |> Error
}

pub fn uc(bs: BitString) {
  bs
  |> util.to_str
  |> UnexpectedCharacter
  |> Error
}

pub fn invalid_char(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidCharacter
  |> Error
}

pub fn invalid_attr_ns_prefix(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidAttributeNamespacePrefix
  |> Error
}

pub fn invalid_el_ns_prefix(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidElementNamespacePrefix
  |> Error
}

pub fn duplicate_attr_names(bs: BitString) {
  bs
  |> util.to_str
  |> DuplicateAttributeNames
  |> Error
}

pub fn invalid_character_reference(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidCharacterReference
  |> Error
}

pub fn invalid_ns_decl(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidNamespaceDecl
  |> Error
}

pub fn invalid_ns_uri(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidNamespaceUri
  |> Error
}

pub fn invalid_comment(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidComment
  |> Error
}

pub fn invalid_el_prefix(bs: BitString) {
  bs
  |> util.to_str
  |> InvalidElementPrefix
  |> Error
}

pub fn forbidden_dtd(bs: BitString) {
  bs
  |> util.to_str
  |> ForbiddenDtd
  |> Error
}
