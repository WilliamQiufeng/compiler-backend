use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::combinator::{map, value};
use nom::multi::separated_list0;
use nom::sequence::{delimited, separated_pair};
use nom::{
    branch::alt,
    character::complete::{char, one_of},
    combinator::{map_res, recognize},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    IResult, Parser,
};

use crate::util::{FromInner, Ref};

use super::ops::DataType;
use super::{IntValue, Operation, Scope, Space, SpaceRef, Value, VoidValue, WeakSpaceRef};

type ScopeContextRef = Ref<ScopeContext>;

struct ScopeContext {
    pub scope: Scope,
    pub spaces: HashMap<String, SpaceRef>,
    global_scope: WeakSpaceRef,
}
struct FunctionContext {
    pub scope_ctx: ScopeContextRef,
    pub function_name: String,
}
struct Program {}

fn hexadecimal_value(input: &str) -> IResult<&str, i64> {
    map_res(
        preceded(
            alt((tag("h"), tag("H"))),
            recognize(many1(terminated(
                one_of("0123456789abcdefABCDEF"),
                many0(char('_')),
            ))),
        ),
        |out: &str| i64::from_str_radix(&str::replace(out, "_", ""), 16),
    )
    .parse(input)
}
fn decimal_value(input: &str) -> IResult<&str, i64> {
    map_res(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        |out: &str| str::replace(out, "_", "").parse::<i64>(),
    )
    .parse(input)
}
fn binary_value(input: &str) -> IResult<&str, i64> {
    map_res(
        preceded(
            alt((tag("b"), tag("B"))),
            recognize(many1(terminated(one_of("01"), many0(char('_'))))),
        ),
        |out: &str| i64::from_str_radix(&str::replace(out, "_", ""), 2),
    )
    .parse(input)
}
fn bool_value(input: &str) -> IResult<&str, bool> {
    alt((value(true, tag("true")), value(false, tag("false"))))(input)
}
fn void_value(input: &str) -> IResult<&str, VoidValue> {
    value(VoidValue, tag("void"))(input)
}
fn int_const(input: &str) -> IResult<&str, IntValue> {
    map(
        alt((hexadecimal_value, decimal_value, binary_value)),
        |out| IntValue {
            value: out,
            storage_type: super::StorageType::Const,
        },
    )(input)
}
fn space_id(input: &str) -> IResult<&str, &str> {
    recognize(preceded(alt((char('@'), char('%'))), alpha1))(input)
}
fn context_space_id<'a>(
    scope_ctx: ScopeContextRef,
) -> impl Fn(&str) -> IResult<&str, SpaceRef> + 'a {
    move |input: &str| {
        map(
            recognize(preceded(alt((char('@'), char('%'))), alpha1)),
            |name: &str| {
                scope_ctx
                    .borrow_mut()
                    .spaces
                    .entry(name.to_string())
                    .or_insert_with(|| {
                        SpaceRef::from_inner(Space::Normal(
                            name.to_string(),
                            None,
                            scope_ctx.borrow().scope.clone(),
                        ))
                    })
                    .clone()
            },
        )(input)
    }
}
fn block_id(input: &str) -> IResult<&str, &str> {
    recognize(preceded(char('#'), alpha1))(input)
}

pub(super) fn data_type(input: &str) -> IResult<&str, DataType> {
    alt((
        value(DataType::I64, tag("i64")),
        value(DataType::F64, tag("f64")),
        value(DataType::Bool, tag("bool")),
        value(DataType::Void, tag("void")),
        map(
            delimited(
                char('['),
                separated_pair(data_type, char(','), int_const),
                char(']'),
            ),
            |(dt, len)| DataType::Array(Box::new(dt), len.value as u32),
        ),
        map(
            delimited(char('{'), separated_list0(char(','), data_type), char('}')),
            DataType::Struct,
        ),
    ))(input)
}
pub(super) fn instruction(input: &str) -> IResult<&str, Operation> {
    todo!()
}
