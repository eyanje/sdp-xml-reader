use bluer::Uuid;
use bluer::UuidExt;
use num_traits::Num;
use sdp_xml::Tag;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use xml::attribute::OwnedAttribute;
use xml::EventReader;
use xml::name::OwnedName;
use xml::reader::XmlEvent;

#[derive(Clone, Debug)]
pub enum Error {
    XmlReaderError(xml::reader::Error),
    EmptyDocument,
    EmptyAttribute,
    UnrecognizedTag(OwnedName),
    UnexpectedEndTag(OwnedName),
    MismatchedEndTag {
        actual: OwnedName,
        expected: OwnedName,
    },
    UnclosedTag(OwnedName),
    MissingValue {
        name: OwnedName,
    },
    MissingId {
        name: OwnedName,
    },
    MalformedBoolean {
        name: OwnedName,
        value: String,
    },
    MalformedHex {
        name: OwnedName,
        value: String,
    },
    ParseIntError {
        name: OwnedName,
        value: String,
        size: usize,
        error: ParseIntError,
    },
    UuidError {
        name: OwnedName,
        value: String,
        error: uuid::Error,
    },
    NotAParent(OwnedName),
    ExtraChildren(OwnedName),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::XmlReaderError(inner) =>
                write!(f, "xml reader error: {}", inner),
            Self::EmptyDocument =>
                write!(f, "empty document"),
            Self::EmptyAttribute =>
                write!(f, "expected attribute child"),
            Self::UnrecognizedTag(name) =>
                write!(f, "unrecognized tag {}", name),
            Self::UnexpectedEndTag(name) =>
                write!(f, "unexpected end tag {}", name),
            Self::MismatchedEndTag { actual, expected } =>
                write!(f, "received end tag for {}, expected {}", actual, expected),
            Self::UnclosedTag(name) => 
                write!(f, "tag {} unclosed", name),
            Self::MissingValue { name } =>
                write!(f, "missing value on {} tag", name),
            Self::MissingId { name } =>
                write!(f, "missing value on {} tag", name),
            Self::MalformedBoolean { name, value } =>
                write!(f, "found malformed boolean {} on tag {}", value, name),
            Self::MalformedHex { name, value } =>
                write!(f, "found malformed hex {} on tag {}", value, name),
            Self::ParseIntError { name, value, size, error } =>
                write!(f, "error parsing int {} of size {} on tag {}: {}",
                        value, size, name, error),
            Self::UuidError { name, value, error } =>
                write!(f, "invalid uuid {} on tag {}: {}", value, name, error),
            Self::NotAParent(name) =>
                write!(f, "tag {} should not have children", name),
            Self::ExtraChildren(name) =>
                write!(f, "tag {} should have one child", name),
        }
    }
}
impl std::error::Error for Error {}

fn find_value<'a>(name: &OwnedName, attributes: &'a Vec<OwnedAttribute>) -> Result<&'a str> {
    let value_str_opt = attributes.iter()
        .find(|a| a.name == OwnedName::local("value"))
        .map(|a| &a.value);
    match value_str_opt {
        Some(v) => Ok(v),
        None => return Err(Error::MissingValue { name: name.clone() }),
    }
}

fn find_id<'a>(name: &OwnedName, attributes: &'a Vec<OwnedAttribute>) -> Result<&'a str> {
    let value_str_opt = attributes.iter()
        .find(|a| a.name == OwnedName::local("id"))
        .map(|a| &a.value);
    match value_str_opt {
        Some(v) => Ok(v),
        None => Err(Error::MissingId { name: name.clone() }),
    }
}


/// Enum type for every SDP tag that exists
/// Allows childless attributes.
#[derive(Clone)]
enum PartialTag {
    Root(Option<Tag>),
    Nil,
    Boolean(bool),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    Uuid(Uuid),
    Text(String),
    RawText(Vec<u8>),
    Url(String),
    Sequence(Vec<Tag>),
    Attribute(u16, Option<Tag>),
    Record(Vec<Tag>),
}

impl PartialTag {
    fn name(&self) -> OwnedName {
        match self {
            Self::Root(..) => OwnedName::local(""),
            Self::Nil => OwnedName::local("nil"),
            Self::Boolean(..) => OwnedName::local("boolean"),
            Self::UInt8(..) => OwnedName::local("uint8"),
            Self::UInt16(..) => OwnedName::local("uint16"),
            Self::UInt32(..) => OwnedName::local("uint32"),
            Self::UInt64(..) => OwnedName::local("uint64"),
            Self::UInt128(..) => OwnedName::local("uint128"),
            Self::Int8(..) => OwnedName::local("int8"),
            Self::Int16(..) => OwnedName::local("int16"),
            Self::Int32(..) => OwnedName::local("int32"),
            Self::Int64(..) => OwnedName::local("int64"),
            Self::Int128(..) => OwnedName::local("int128"),
            Self::Uuid(..) => OwnedName::local("uuid"),
            Self::Text(..) => OwnedName::local("text"),
            Self::RawText(..) => OwnedName::local("text"),
            Self::Url(..) => OwnedName::local("url"),
            Self::Sequence(..) => OwnedName::local("sequence"),
            Self::Attribute(..) => OwnedName::local("attribute"),
            Self::Record(..) => OwnedName::local("record"),
        }
    }

    fn add_child(&mut self, child: Tag) -> Result<()> {
        match self {
            Self::Root(root_child) => {
                if root_child.is_none() {
                    root_child.replace(child);
                } else {
                    return Err(Error::ExtraChildren(self.name()));
                }
            },
            Self::Sequence(children) => {
                children.push(child);
            },
            Self::Attribute(_, attribute_child) => {
                if attribute_child.is_none() {
                    attribute_child.replace(child);
                } else {
                    return Err(Error::ExtraChildren(self.name()));
                }
            },
            Self::Record(children) => {
                children.push(child);
            },
            _ => {
                return Err(Error::NotAParent(self.name()));
            },
        }
        Ok(())
    }
}

impl TryFrom<PartialTag> for Tag {
    type Error = Error;
    fn try_from(partial_tag: PartialTag) -> Result<Self> {
        let tag = match partial_tag {
            PartialTag::Root(child_opt) => {
                if let Some(child) = child_opt {
                    child
                } else {
                    return Err(Error::EmptyDocument);
                }
            },
            PartialTag::Nil => Tag::Nil(),
            PartialTag::Boolean(value) => Tag::Boolean(value),
            PartialTag::UInt8(value) => Tag::UInt8(value),
            PartialTag::UInt16(value) => Tag::UInt16(value),
            PartialTag::UInt32(value) => Tag::UInt32(value),
            PartialTag::UInt64(value) => Tag::UInt64(value),
            PartialTag::UInt128(value) => Tag::UInt128(value),
            PartialTag::Int8(value) => Tag::Int8(value),
            PartialTag::Int16(value) => Tag::Int16(value),
            PartialTag::Int32(value) => Tag::Int32(value),
            PartialTag::Int64(value) => Tag::Int64(value),
            PartialTag::Int128(value) => Tag::Int128(value),
            PartialTag::Uuid(value) => Tag::Uuid(value),
            PartialTag::Text(value) => Tag::Text(value),
            PartialTag::RawText(value) => Tag::RawText(value),
            PartialTag::Url(value) => Tag::Url(value),
            PartialTag::Sequence(children) => Tag::Sequence(children),
            PartialTag::Attribute(id, child_opt) => {
                let child = match child_opt {
                    Some(child) => child,
                    None => {
                        return Err(Error::EmptyAttribute);
                    }
                };
                Tag::Attribute(id, Box::new(child))
            },
            PartialTag::Record(children) => Tag::Record(children),
        };
        Ok(tag)
    }
}

type Result<T> = std::result::Result<T, Error>;

fn parse_boolean(name: &OwnedName, value_str: &str) -> Result<bool> {
    match value_str {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(Error::MalformedBoolean {
            name: name.clone(),
            value: value_str.to_string(),
        }),
    }
}

fn parse_plain_hex<T: Num<FromStrRadixErr = ParseIntError>>(
    name: &OwnedName, value_str: &str,
) -> Result<T> {
    T::from_str_radix(&value_str, 16)
        .map_err(|e| Error::ParseIntError {
            name: name.clone(),
            value: value_str.to_string(),
            size: size_of::<T>(),
            error: e,
        })
}

fn parse_hex<T: Num<FromStrRadixErr = ParseIntError>>(
    name: &OwnedName, value_str: &str
) -> Result<T> {
    if value_str.len() < 2 || &value_str[..2] != "0x" {
        return Err(Error::MalformedHex{
            name: name.clone(),
            value: value_str.to_string(),
        });
    }
    parse_plain_hex(name, &value_str[2..])
}

fn parse_int<T: Num<FromStrRadixErr = ParseIntError>>(
    name: &OwnedName, value_str: &str
) -> Result<T> {
    T::from_str_radix(&value_str, 10)
        .map_err(|e| Error::ParseIntError {
            name: name.clone(),
            value: value_str.to_string(),
            size: size_of::<T>(),
            error: e,
        })
}

fn parse_uuid(name: &OwnedName, value_str: &str) -> Result<Uuid> {
    if value_str.len() >= 2 && &value_str[..2] == "0x" {
        let uuid_u32 = parse_hex(name, value_str)?;
        Ok(Uuid::from_u32(uuid_u32))
    } else {
        Uuid::try_parse(value_str)
            .map_err(|e| Error::UuidError {
                name: name.clone(),
                value: value_str.to_string(),
                error: e,
            })
    }
}

fn hex_to_bytes(name: &OwnedName, value_str: &str) -> Result<Vec<u8>> {
    // Require a complete byte sequence.
    if value_str.len() % 2 != 0 {
        return Err(Error::MalformedHex {
            name: name.clone(),
            value: value_str.to_string(),
        });
    }
    let value_bytes = Vec::from(value_str);
    let value_utf8_res = value_bytes.chunks_exact(2)
       .map(|v| std::str::from_utf8(v))
       .collect();
    let value_utf8: Vec<&str> = match value_utf8_res {
        Ok(v) => v,
        Err(_) => {
            return Err(Error::MalformedHex {
                name: name.clone(),
                value: value_str.to_string(),
            });
        },
    };
    let text_bytes_res = value_utf8
        .into_iter()
        .map(|v| u8::from_str_radix(v, 16))
        .collect();
    let text_bytes = match text_bytes_res {
        Ok(v) => v,
        Err(_) => {
            return Err(Error::MalformedHex {
                name: name.clone(),
                value: value_str.to_string(),
            });
        },
    };
    Ok(text_bytes)
}

pub fn parse_sdp_xml(xml: &[u8]) -> Result<Tag> {
    let parser = EventReader::new(xml);
    let mut stack = vec![PartialTag::Root(None)];
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                match name.local_name.as_ref() {
                    "nil" => {
                        stack.push(PartialTag::Nil);
                    },
                    "boolean" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_boolean(&name, &value_str)?;
                        stack.push(PartialTag::Boolean(value));
                    },
                    "uint8" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_hex(&name, &value_str)?;
                        stack.push(PartialTag::UInt8(value));
                    },
                    "uint16" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_hex(&name, &value_str)?;
                        stack.push(PartialTag::UInt16(value));
                    },
                    "uint32" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_hex(&name, &value_str)?;
                        stack.push(PartialTag::UInt32(value));
                    },
                    "uint64" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_hex(&name, &value_str)?;
                        stack.push(PartialTag::UInt64(value));
                    },
                    "uint128" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_plain_hex(&name, &value_str)?;
                        stack.push(PartialTag::UInt128(value));
                    },
                    "int8" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_int(&name, &value_str)?;
                        stack.push(PartialTag::Int8(value));
                    },
                    "int16" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_int(&name, &value_str)?;
                        stack.push(PartialTag::Int16(value));
                    },
                    "int32" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_int(&name, &value_str)?;
                        stack.push(PartialTag::Int32(value));
                    },
                    "int64" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_int(&name, &value_str)?;
                        stack.push(PartialTag::Int64(value));
                    },
                    "int128" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_plain_hex(&name, &value_str)?;
                        stack.push(PartialTag::Int128(value));
                    },
                    "uuid" => {
                        let value_str = find_value(&name, &attributes)?;
                        let value = parse_uuid(&name, &value_str)?;
                        stack.push(PartialTag::Uuid(value));
                    },
                    "text" => {
                        let value_str = find_value(&name, &attributes)?;
                        let encoding_opt = attributes.iter()
                            .find(|a| a.name == OwnedName::local("encoding"))
                            .map(|a| a.value.as_ref());
                        if encoding_opt == Some("hex") {
                            let value = hex_to_bytes(&name, &value_str)?;
                            stack.push(PartialTag::RawText(value));
                        } else {
                            stack.push(PartialTag::Text(value_str.to_string()));
                        }
                    },
                    "url" => {
                        let value_str = find_value(&name, &attributes)?;
                        stack.push(PartialTag::Url(value_str.to_string()));
                    }
                    "sequence" => {
                        stack.push(PartialTag::Sequence(Vec::new()));
                    }
                    "attribute" => {
                        let id_str = find_id(&name, &attributes)?;
                        let id = parse_hex(&name, &id_str)?;
                        stack.push(PartialTag::Attribute(id, None));
                    },
                    "record" => {
                        stack.push(PartialTag::Record(Vec::new()));
                    },
                    _ => {
                        return Err(Error::UnrecognizedTag(name));
                    }
                }
            }
            Ok(XmlEvent::EndElement { name }) => {
                let top = match stack.pop() {
                    Some(tag) => tag,
                    None => {
                        return Err(Error::UnexpectedEndTag(name));
                    },
                };
                let parent = match stack.last_mut() {
                    Some(tag) => tag,
                    None => {
                        return Err(Error::UnexpectedEndTag(name));
                    },
                };
                if top.name() != name {
                    return Err(Error::MismatchedEndTag {
                        actual: name,
                        expected: top.name(),
                    });
                }
                let complete_top = Tag::try_from(top)?;
                parent.add_child(complete_top)?;
            }
            Err(e) => {
                return Err(Error::XmlReaderError(e));
            }
            // Ignore other tags
            _ => (),
        }
    }
    if stack.len() > 1 {
        return Err(Error::UnclosedTag(stack.last().unwrap().name()));
    }
    Tag::try_from(stack.remove(0))
}



