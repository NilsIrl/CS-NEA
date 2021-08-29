use core::cmp::min_by_key;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, i64, multispace0, multispace1, satisfy},
    combinator::{all_consuming, map, opt, recognize, verify},
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
}

macro_rules! ops {
    ( $( $op:literal => $variant:path ),* $(,)? ) => {
        fn op(input: &str) -> IResult<&str, &str> {
            alt((
                    $(
                        tag($op),
                    )*
            ))(input)
        }

        impl<'a> Expression<'a> {
            fn from_op(op: &str, lhs: Expression<'a>, rhs: Expression<'a>) -> Self {
                let lhs = Box::new(lhs);
                let rhs = Box::new(rhs);
                match op {
                    $(
                    $op => $variant(lhs, rhs),
                    )*
                    _ => unreachable!("Not an operator"),
                }
            }
        }

    }
}

ops! {
    "==" => Expression::Equal,
    "!=" => Expression::NotEqual,
    "<=" => Expression::LessThanOrEqual,
    ">=" => Expression::GreaterThanOrEqual,
    "<" => Expression::LessThan,
    ">" => Expression::GreaterThan,
    "+" => Expression::Plus,
    "-" => Expression::Minus,
    "*" => Expression::Times,
    "/" => Expression::Divide,
    "MOD" => Expression::Modulus,
    "DIV" => Expression::Quotient,
    "^" => Expression::Power,
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

fn non_recursive_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            delimited(quote, take_till(is_quote), quote),
            |string_const| Expression::StringLiteral(string_const),
        ),
        function_call,
        map(identifier, |identifier| Expression::Variable(identifier)),
        number_literal,
    ))(input)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    let (input, (lhs, rhs)) = pair(
        preceded(multispace0, non_recursive_expression),
        opt(pair(preceded(multispace0, op), expression)),
    )(input)?;
    match rhs {
        Some((op, rhs)) => Ok((input, Expression::from_op(op, lhs, rhs))),
        None => Ok((input, lhs)),
    }
}

fn is_identifer_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

const KEYWORDS: [&str; 1] = ["next"];

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

fn statement(input: &str) -> IResult<&str, Statement> {
    preceded(
        multispace0,
        alt((
            assignment_statement,
            global_assignment_statement,
            for_loop,
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
}
