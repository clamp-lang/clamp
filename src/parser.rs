use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0, multispace1},
    combinator::{cut, map},
    error::VerboseError,
    multi::many0,
    number::complete::float,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

#[derive(PartialEq, Debug)]
pub enum LitKind {
    Bool(bool),
    Integer(i32),
    Float(f32),
    Str(String),
}

#[derive(PartialEq, Debug)]
pub enum TypeKind {
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    String,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(LitKind),
    Type(TypeKind),
    Symbol(String),
    List(Vec<Expr>),
    Fn(Vec<Expr>, Vec<Expr>),
    Struct(Vec<Expr>),
    Def(Box<Expr>, Box<Expr>),
    TypeDef(Box<Expr>, Box<Expr>),
    TypeApplication(Box<Expr>, Box<Expr>),
}

fn parse_number<'a>(i: &'a str) -> IResult<&'a str, LitKind, VerboseError<&'a str>> {
    map(float, |num| {
        if num.fract() == 0.0 {
            LitKind::Integer(num as i32)
        } else {
            LitKind::Float(num)
        }
    })(i)
}

fn parse_string<'a>(i: &'a str) -> IResult<&'a str, LitKind, VerboseError<&'a str>> {
    map(
        delimited(tag("\""), take_until("\""), tag("\"")),
        |string: &'a str| LitKind::Str(string.to_string()),
    )(i)
}

fn parse_bool<'a>(i: &'a str) -> IResult<&'a str, LitKind, VerboseError<&'a str>> {
    alt((
        map(tag("true"), |_| LitKind::Bool(true)),
        map(tag("false"), |_| LitKind::Bool(false)),
    ))(i)
}

fn parse_literal<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(alt((parse_number, parse_string, parse_bool)), Expr::Literal)(i)
}

fn parse_type<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((
        map(tag("i32"), |_| Expr::Type(TypeKind::I32)),
        map(tag("i64"), |_| Expr::Type(TypeKind::I64)),
        map(tag("u32"), |_| Expr::Type(TypeKind::U32)),
        map(tag("u64"), |_| Expr::Type(TypeKind::U64)),
        map(tag("f32"), |_| Expr::Type(TypeKind::F32)),
        map(tag("f64"), |_| Expr::Type(TypeKind::F64)),
        map(tag("char"), |_| Expr::Type(TypeKind::Char)),
        map(tag("string"), |_| Expr::Type(TypeKind::String)),
        map(tag("bool"), |_| Expr::Type(TypeKind::Bool)),
    ))(i)
}

fn parse_symbol<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(terminated(alpha1, multispace0), |s: &'a str| {
        Expr::Symbol(s.to_string())
    })(i)
}

fn parse_list<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            preceded(multispace0, many0(parse_expr)),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::List(exprs),
    )(i)
}

fn parse_args<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>, VerboseError<&'a str>> {
    delimited(
        char('('),
        preceded(multispace0, many0(parse_symbol)),
        cut(preceded(multispace0, char(')'))),
    )(i)
}

fn parse_fn<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            tuple((
                tag("fn"),
                multispace1,
                parse_args,
                multispace0,
                many0(parse_expr),
            )),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::Fn(exprs.2, exprs.4),
    )(i)
}

fn parse_struct<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            tuple((tag("struct"), many0(parse_expr))),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::Struct(exprs.1),
    )(i)
}

fn parse_def<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            tuple((
                tag("def"),
                multispace1,
                parse_symbol,
                multispace0,
                parse_expr,
            )),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::Def(Box::new(exprs.2), Box::new(exprs.4)),
    )(i)
}

fn parse_type_def<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            tuple((
                tag("type"),
                multispace0,
                parse_symbol,
                multispace0,
                parse_expr,
            )),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::TypeDef(Box::new(exprs.2), Box::new(exprs.4)),
    )(i)
}

fn parse_type_application<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    map(
        delimited(
            char('('),
            tuple((
                char(':'),
                multispace0,
                parse_symbol,
                multispace0,
                parse_expr,
            )),
            cut(preceded(multispace0, char(')'))),
        ),
        |exprs| Expr::TypeApplication(Box::new(exprs.2), Box::new(exprs.4)),
    )(i)
}

fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    preceded(
        multispace0,
        alt((
            parse_literal,
            parse_type,
            parse_fn,
            parse_struct,
            parse_type_def,
            parse_def,
            parse_type_application,
            parse_list,
            parse_symbol,
        )),
    )(i)
}

pub fn parse<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>, VerboseError<&'a str>> {
    many0(parse_expr)(i)
}

#[cfg(test)]
mod tests {
    use super::{
        parse_bool, parse_def, parse_expr, parse_fn, parse_list, parse_literal, parse_number,
        parse_string, parse_struct, parse_symbol, parse_type, parse_type_application,
        parse_type_def, Expr, LitKind, TypeKind,
    };

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_number("123"), Ok(("", LitKind::Integer(123))));
        assert_eq!(parse_number("-123"), Ok(("", LitKind::Integer(-123))));
        assert_eq!(parse_number("123ab"), Ok(("ab", LitKind::Integer(123))));
        assert_eq!(parse_number("1.23"), Ok(("", LitKind::Float(1.23))));
        assert_eq!(parse_number("-1.23"), Ok(("", LitKind::Float(-1.23))));
        assert_eq!(parse_number("1.23abc"), Ok(("abc", LitKind::Float(1.23))));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse_string("\"abc\""),
            Ok(("", LitKind::Str("abc".to_string())))
        );
        assert_eq!(
            parse_string("\"abc\" \"def\""),
            Ok((" \"def\"", LitKind::Str("abc".to_string())))
        );
        assert_eq!(
            parse_string("\"abc\" 123"),
            Ok((" 123", LitKind::Str("abc".to_string())))
        );
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_bool("true"), Ok(("", LitKind::Bool(true))));
        assert_eq!(parse_bool("false"), Ok(("", LitKind::Bool(false))));
        assert_eq!(
            parse_bool("false true"),
            Ok((" true", LitKind::Bool(false)))
        );
    }

    #[test]
    fn test_parse_literal() {
        assert_eq!(
            parse_literal("true"),
            Ok(("", Expr::Literal(LitKind::Bool(true))))
        );
        assert_eq!(
            parse_literal("123"),
            Ok(("", Expr::Literal(LitKind::Integer(123))))
        );
        assert_eq!(
            parse_literal("12.3"),
            Ok(("", Expr::Literal(LitKind::Float(12.3))))
        );
        assert_eq!(
            parse_literal("\"abc\""),
            Ok(("", Expr::Literal(LitKind::Str("abc".to_string()))))
        );
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(parse_type("i32"), Ok(("", Expr::Type(TypeKind::I32))));
        assert_eq!(parse_type("i64"), Ok(("", Expr::Type(TypeKind::I64))));
        assert_eq!(parse_type("u32"), Ok(("", Expr::Type(TypeKind::U32))));
        assert_eq!(parse_type("u64"), Ok(("", Expr::Type(TypeKind::U64))));
        assert_eq!(parse_type("f32"), Ok(("", Expr::Type(TypeKind::F32))));
        assert_eq!(parse_type("f64"), Ok(("", Expr::Type(TypeKind::F64))));
        assert_eq!(parse_type("char"), Ok(("", Expr::Type(TypeKind::Char))));
        assert_eq!(parse_type("string"), Ok(("", Expr::Type(TypeKind::String))));
        assert_eq!(parse_type("bool"), Ok(("", Expr::Type(TypeKind::Bool))));
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(
            parse_list("(\"hello\" \"world\")"),
            Ok((
                "",
                Expr::List(vec![
                    Expr::Literal(LitKind::Str("hello".to_string())),
                    Expr::Literal(LitKind::Str("world".to_string())),
                ]),
            ))
        );
        assert_eq!(
            parse_list("(\"hello\" \"world\" 100)"),
            Ok((
                "",
                Expr::List(vec![
                    Expr::Literal(LitKind::Str("hello".to_string())),
                    Expr::Literal(LitKind::Str("world".to_string())),
                    Expr::Literal(LitKind::Integer(100)),
                ]),
            ))
        );
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(
            parse_symbol("fn sayHello"),
            Ok(("sayHello", Expr::Symbol("fn".to_string())))
        );
        assert_eq!(
            parse_symbol("println \"Hello World\""),
            Ok(("\"Hello World\"", Expr::Symbol("println".to_string())))
        );
    }

    #[test]
    fn test_parse_fn() {
        assert_eq!(
            parse_fn("(fn (name) (print \"Hello\" name))"),
            Ok((
                "",
                Expr::Fn(
                    vec![Expr::Symbol("name".to_string())],
                    vec![Expr::List(vec![
                        Expr::Symbol("print".to_string()),
                        Expr::Literal(LitKind::Str("Hello".to_string())),
                        Expr::Symbol("name".to_string()),
                    ])]
                )
            ))
        );
    }

    #[test]
    fn test_parse_struct() {
        assert_eq!(
            parse_struct("(struct width i32 height i32)"),
            Ok((
                "",
                Expr::Struct(vec![
                    Expr::Symbol("width".to_string()),
                    Expr::Type(TypeKind::I32),
                    Expr::Symbol("height".to_string()),
                    Expr::Type(TypeKind::I32),
                ])
            ))
        );
    }

    #[test]
    fn test_parse_def() {
        assert_eq!(
            parse_def("(def point 1)"),
            Ok((
                "",
                Expr::Def(
                    Box::new(Expr::Symbol("point".to_string())),
                    Box::new(Expr::Literal(LitKind::Integer(1)))
                )
            ))
        );
        assert_eq!(
            parse_def("(def sayHello (fn (name) (print \"Hello\" name)))"),
            Ok((
                "",
                Expr::Def(
                    Box::new(Expr::Symbol("sayHello".to_string())),
                    Box::new(Expr::Fn(
                        vec![Expr::Symbol("name".to_string())],
                        vec![Expr::List(vec![
                            Expr::Symbol("print".to_string()),
                            Expr::Literal(LitKind::Str("Hello".to_string())),
                            Expr::Symbol("name".to_string()),
                        ])]
                    ))
                )
            ))
        );
    }

    #[test]
    fn test_parse_type_def() {
        assert_eq!(
            parse_type_def("(type Name string)"),
            Ok((
                "",
                Expr::TypeDef(
                    Box::new(Expr::Symbol("Name".to_string())),
                    Box::new(Expr::Type(TypeKind::String))
                )
            ))
        );
    }

    #[test]
    fn test_parse_type_application() {
        assert_eq!(
            parse_type_application("(: point Point)"),
            Ok((
                "",
                Expr::TypeApplication(
                    Box::new(Expr::Symbol("point".to_string())),
                    Box::new(Expr::Symbol("Point".to_string()))
                )
            ))
        );
    }

    #[test]
    fn test_parse_expr() {
        assert_eq!(
            parse_expr("(if (equal 3 (add 1 2)) \"OK\" \"NG\")"),
            Ok((
                "",
                Expr::List(vec![
                    Expr::Symbol("if".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("equal".to_string()),
                        Expr::Literal(LitKind::Integer(3)),
                        Expr::List(vec![
                            Expr::Symbol("add".to_string()),
                            Expr::Literal(LitKind::Integer(1)),
                            Expr::Literal(LitKind::Integer(2)),
                        ],),
                    ],),
                    Expr::Literal(LitKind::Str("OK".to_string())),
                    Expr::Literal(LitKind::Str("NG".to_string())),
                ])
            ))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse_expr("(sayHello)"),
            Ok(("", Expr::List(vec![Expr::Symbol("sayHello".to_string())])))
        );
        assert_eq!(
            parse_expr("(sayHello (quote (\"clamp\")))"),
            Ok((
                "",
                Expr::List(vec![
                    Expr::Symbol("sayHello".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("quote".to_string()),
                        Expr::List(vec![Expr::Literal(LitKind::Str("clamp".to_string())),]),
                    ])
                ])
            ))
        );
    }
}
