use core::cmp::min_by_key;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, i64, multispace0, multispace1, satisfy},
    combinator::{all_consuming, map, recognize, verify},
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    StringLiteral(&'a str),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Identifier(&'a str),
    FunctionCall(&'a str, Vec<Expression<'a>>),
    Variable(&'a str),

    Equal(Box<Expression<'a>>, Box<Expression<'a>>),
    NotEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThan(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThanOrEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterThan(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterThanOrEqual(Box<Expression<'a>>, Box<Expression<'a>>),

    Plus(Box<Expression<'a>>, Box<Expression<'a>>),
    Minus(Box<Expression<'a>>, Box<Expression<'a>>),
    Times(Box<Expression<'a>>, Box<Expression<'a>>),
    Divide(Box<Expression<'a>>, Box<Expression<'a>>),
    Modulus(Box<Expression<'a>>, Box<Expression<'a>>),
    Quotient(Box<Expression<'a>>, Box<Expression<'a>>),
    Power(Box<Expression<'a>>, Box<Expression<'a>>),

    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    Not(Box<Expression<'a>>),
}

fn open_bracket(input: &str) -> IResult<&str, char> {
    char('(')(input)
}

fn close_bracket(input: &str) -> IResult<&str, char> {
    char(')')(input)
}

fn comma(input: &str) -> IResult<&str, char> {
    char(',')(input)
}

fn function_call(input: &str) -> IResult<&str, Expression> {
    let (input, (function_name, arguments)) = pair(
        identifier,
        delimited(
            preceded(multispace0, open_bracket),
            separated_list0(preceded(multispace0, comma), expression),
            preceded(multispace0, close_bracket),
        ),
    )(input)?;
    Ok((input, Expression::FunctionCall(function_name, arguments)))
}

fn number_literal(input: &str) -> IResult<&str, Expression> {
    let int = map(i64, |num_const| Expression::IntegerLiteral(num_const))(input);
    let float = map(double, |float_const| Expression::FloatLiteral(float_const))(input);
    if int.is_err() {
        float
    } else if float.is_err() {
        int
    } else {
        // unwrap because due to previous 2 selections, neither int nor float can be Err(_)
        min_by_key(int, float, |x| x.as_ref().unwrap().0.len())
    }
}

fn terminal(input: &str) -> IResult<&str, Expression> {
    preceded(
        multispace0,
        alt((
            map(
                delimited(quote, take_till(is_quote), quote),
                |string_const| Expression::StringLiteral(string_const),
            ),
            map(tag("true"), |_| Expression::BoolLiteral(true)),
            map(tag("false"), |_| Expression::BoolLiteral(false)),
            function_call,
            map(identifier, |identifier| Expression::Variable(identifier)),
            number_literal,
        )),
    )(input)
}

fn depth6(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = terminal(input)?;
    let (input, rest) = many0(preceded(preceded(multispace0, tag("^")), terminal))(input)?;
    Ok((
        input,
        rest.into_iter().fold(initial, |acc, current| {
            Expression::Power(Box::new(acc), Box::new(current))
        }),
    ))
}

fn depth5(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = depth6(input)?;
    let (input, rest) = many0(pair(
        preceded(
            multispace0,
            alt((tag("*"), tag("/"), tag("MOD"), tag("DIV"))),
        ),
        preceded(multispace0, depth6),
    ))(input)?;
    Ok((input, rest.into_iter().fold(initial, |acc, current| match current.0 {
                "*" => Expression::Times,
                "/" => Expression::Divide,
                "MOD" => Expression::Modulus,
                "DIV" => Expression::Quotient,
                _ => unreachable!(),
            }
            (Box::new(acc), Box::new(current.1)))))
}

fn depth4(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = depth5(input)?;
    let (input, rest) = many0(pair(
        preceded(multispace0, alt((tag("+"), tag("-")))),
        preceded(multispace0, depth5),
    ))(input)?;
    Ok((input, rest.into_iter().fold(initial, |prev, current| match current.0 {
                "+" => Expression::Plus,
                "-" => Expression::Minus,
                _ => unreachable!(),
            }
            (Box::new(prev), Box::new(current.1)))))
}

fn depth3(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = depth4(input)?;
    let (input, rest) = many0(pair(
        preceded(
            multispace0,
            alt((
                tag("=="),
                tag("!="),
                tag("<="),
                tag(">="),
                tag("<"),
                tag(">"),
            )),
        ),
        preceded(multispace0, depth4),
    ))(input)?;
    Ok((input, rest.into_iter().fold(initial, |acc, current| match current.0 {
                "==" => Expression::Equal,
                "!=" => Expression::NotEqual,
                "<=" => Expression::LessThanOrEqual,
                ">=" => Expression::GreaterThanOrEqual,
                "<" => Expression::LessThan,
                ">" => Expression::GreaterThan,
                _ => unreachable!(),
            }
            (Box::new(acc), Box::new(current.1))
    )))
}

fn not_depth(input: &str) -> IResult<&str, Expression> {
    alt((
        map(preceded(preceded(multispace0, tag("NOT")), depth3), |exp| {
            Expression::Not(Box::new(exp))
        }),
        depth3,
    ))(input)
}

fn depth2(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = not_depth(input)?;
    let (input, rest) = many0(preceded(
        preceded(multispace0, tag("AND")),
        preceded(multispace0, not_depth),
    ))(input)?;
    Ok((
        input,
        rest.into_iter().fold(initial, |acc, current| {
            Expression::And(Box::new(acc), Box::new(current))
        }),
    ))
}

fn depth1(input: &str) -> IResult<&str, Expression> {
    let (input, initial) = depth2(input)?;
    let (input, rest) = many0(preceded(
        preceded(multispace0, tag("OR")),
        preceded(multispace0, depth2),
    ))(input)?;
    Ok((
        input,
        rest.into_iter().fold(initial, |acc, current| {
            Expression::Or(Box::new(acc), Box::new(current))
        }),
    ))
}

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Assignment(&'a str, Expression<'a>),
    GlobalAssignment(&'a str, Expression<'a>),
    ArrayDefinition(&'a str, Vec<usize>),
    Expression(Expression<'a>),
    For(
        &'a str,
        Expression<'a>,
        Expression<'a>,
        ListOfStatements<'a>,
    ),
    While(Expression<'a>, ListOfStatements<'a>),
    If(
        Vec<(Expression<'a>, Statement<'a>)>,
        Option<ListOfStatements<'a>>,
    ),
    Switch(
        Expression<'a>,
        Vec<(Expression<'a>, Statement<'a>)>,
        Option<ListOfStatements<'a>>,
    ),
    Function(&'a str, Vec<(&'a str, bool)>, ListOfStatements<'a>),
    Return(Expression<'a>),
}

type ListOfStatements<'a> = Vec<Statement<'a>>;

const QUOTES: [char; 3] = ['"', '“', '”'];

fn is_quote(c: char) -> bool {
    QUOTES.contains(&c)
}

fn quote(input: &str) -> IResult<&str, char> {
    // TODO: quotes are hardcoded
    satisfy(is_quote)(input)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    depth1(input)
}

fn is_identifer_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

// TODO: this only contains the endings, the beginnings are not necessary for parsing
const KEYWORDS: [&str; 2] = ["next", "endwhile"];

fn identifier(input: &str) -> IResult<&str, &str> {
    verify(
        preceded(
            multispace0,
            recognize(pair(
                satisfy(|c| is_identifer_char(c) && !c.is_ascii_digit()),
                take_while(is_identifer_char),
            )),
        ),
        |ident: &str| !KEYWORDS.contains(&ident),
    )(input)
}

fn assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (identifier, expression)) = tuple((
        identifier,
        preceded(preceded(multispace0, char('=')), expression),
    ))(input)?;
    Ok((input, Statement::Assignment(identifier, expression)))
}

fn global_assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, assignment) =
        preceded(tag("global"), preceded(multispace1, assignment_statement))(input)?;
    Ok((
        input,
        match assignment {
            Statement::Assignment(identifier, expression) => {
                Statement::GlobalAssignment(identifier, expression)
            }
            _ => unreachable!("assignment_statement can only return Statement::Assignment"),
        },
    ))
}

fn for_loop(input: &str) -> IResult<&str, Statement> {
    let (input, (assignment, end, body)) = delimited(
        tag("for"),
        tuple((
            preceded(multispace1, assignment_statement),
            preceded(preceded(multispace1, tag("to")), expression),
            list_of_statements,
        )),
        preceded(multispace0, pair(tag("next"), identifier)),
    )(input)?;
    Ok((
        input,
        match assignment {
            Statement::Assignment(identifier, expression) => {
                Statement::For(identifier, expression, end, body)
            }
            _ => unreachable!("assignment_statement can only return Statement::Assignment"),
        },
    ))
}

fn while_loop(input: &str) -> IResult<&str, Statement> {
    let (input, (exp, body)) = delimited(
        tag("while"),
        pair(expression, list_of_statements),
        preceded(multispace0, tag("endwhile")),
    )(input)?;
    Ok((input, Statement::While(exp, body)))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    preceded(
        multispace0,
        alt((
            assignment_statement,
            global_assignment_statement,
            for_loop,
            while_loop,
            map(expression, |expression| Statement::Expression(expression)),
        )),
    )(input)
}

#[derive(Debug, PartialEq)]
struct Program<'a>(ListOfStatements<'a>);

fn list_of_statements(input: &str) -> IResult<&str, ListOfStatements> {
    many0(statement)(input)
}

fn program(input: &str) -> IResult<&str, Program> {
    let (input, statements) = all_consuming(terminated(list_of_statements, multispace0))(input)?;
    // TODO check input is empty
    Ok((input, Program(statements)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! ast_test {
        ( $function_name:ident ) => {
            #[test]
            fn $function_name() {
                assert_eq!(
                    program(include_str!(concat!(
                        "../tests/",
                        stringify!($function_name),
                        ".input"
                    )))
                    .unwrap()
                    .1,
                    include!(concat!("../tests/", stringify!($function_name), ".ast"))
                );
            }
        };
    }

    ast_test!(variable);
    ast_test!(variable_quotes);
    ast_test!(variable_whitespace);
    ast_test!(function_call);
    ast_test!(complex_function_call);
    ast_test!(print);
    ast_test!(input);
    ast_test!(for_loop1);
    ast_test!(comparison1);
    ast_test!(empty);
    ast_test!(maths1);
    ast_test!(logical_operators1);
    ast_test!(not1);
    ast_test!(while1);
}
