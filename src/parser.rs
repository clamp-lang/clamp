use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0},
    combinator::{cut, map},
    error::VerboseError,
    multi::many0,
    number::complete::float,
    sequence::{delimited, preceded, terminated},
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
pub enum Expr {
    Literal(LitKind),
    Symbol(String),
    List(Vec<Expr>),
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

fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    preceded(multispace0, alt((parse_literal, parse_list, parse_symbol)))(i)
}

pub fn parse<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>, VerboseError<&'a str>> {
    many0(parse_expr)(i)
}

#[cfg(test)]
mod tests {
    use super::{
        parse_bool, parse_expr, parse_list, parse_literal, parse_number, parse_string,
        parse_symbol, Expr, LitKind,
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
