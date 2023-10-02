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
  |> util.unsafe_to_string
  |> ForbiddenComment
  |> Error
}

pub fn ueos(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> UnexpectedEndOfStream
  |> Error
}

pub fn uc(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> UnexpectedCharacter
  |> Error
}

pub fn invalid_char(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidCharacter
  |> Error
}

pub fn invalid_attr_ns_prefix(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidAttributeNamespacePrefix
  |> Error
}

pub fn invalid_el_ns_prefix(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidElementNamespacePrefix
  |> Error
}

pub fn duplicate_attr_names(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> DuplicateAttributeNames
  |> Error
}

pub fn invalid_character_reference(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidCharacterReference
  |> Error
}

pub fn invalid_ns_decl(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidNamespaceDecl
  |> Error
}

pub fn invalid_ns_uri(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidNamespaceUri
  |> Error
}

pub fn invalid_comment(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidComment
  |> Error
}

pub fn invalid_el_prefix(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> InvalidElementPrefix
  |> Error
}

pub fn forbidden_dtd(bs: BitString) {
  bs
  |> util.unsafe_to_string
  |> ForbiddenDtd
  |> Error
}
