use core::cmp::min_by_key;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{self, char, i64, not_line_ending, satisfy},
    combinator::{all_consuming, map, opt, recognize, value, verify},
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(tag("//"), not_line_ending))(input)
}

fn space0(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        complete::multispace0,
        opt(comment),
        complete::multispace0,
    )))(input)
}

fn space1(input: &str) -> IResult<&str, &str> {
    alt((complete::multispace1, space0))(input)
}

#[derive(Debug, PartialEq, Clone)]
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
        Expression<'a>,
        ListOfStatements<'a>,
    ),
    While(Expression<'a>, ListOfStatements<'a>),
    If(Expression<'a>, ListOfStatements<'a>, ListOfStatements<'a>),
    // asRef: bool
    Function(&'a str, Vec<(&'a str, bool)>, ListOfStatements<'a>),
    Return(Expression<'a>),
}

type ListOfStatements<'a> = Vec<Statement<'a>>;

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
            preceded(space0, open_bracket),
            separated_list0(token(comma), expression),
            preceded(space0, close_bracket),
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
        space0,
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
    let (input, rest) = many0(preceded(preceded(space0, tag("^")), terminal))(input)?;
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
        preceded(space0, alt((tag("*"), tag("/"), tag("MOD"), tag("DIV")))),
        preceded(space0, depth6),
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
        preceded(space0, alt((tag("+"), tag("-")))),
        preceded(space0, depth5),
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
            space0,
            alt((
                tag("=="),
                tag("!="),
                tag("<="),
                tag(">="),
                tag("<"),
                tag(">"),
            )),
        ),
        preceded(space0, depth4),
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
        map(preceded(preceded(space0, tag("NOT")), depth3), |exp| {
            Expression::Not(Box::new(exp))
        }),
        depth3,
    ))(input)
}

fn depth2(input: &str) -> IResult<&str, Expression> {
    map(
        pair(
            not_depth,
            opt(preceded(
                preceded(space0, tag("AND")),
                preceded(space0, depth2),
            )),
        ),
        |(lhs, rhs)| match rhs {
            Some(rhs) => Expression::And(Box::new(lhs), Box::new(rhs)),
            None => lhs,
        },
    )(input)
}

fn depth1(input: &str) -> IResult<&str, Expression> {
    map(
        pair(
            depth2,
            opt(preceded(
                preceded(space0, tag("OR")),
                preceded(space0, depth1),
            )),
        ),
        |(lhs, rhs)| match rhs {
            Some(rhs) => Expression::Or(Box::new(lhs), Box::new(rhs)),
            None => lhs,
        },
    )(input)
}

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
const KEYWORDS: [&str; 10] = [
    "next",
    "endwhile",
    "elseif",
    "else",
    "endif",
    "endswitch",
    "case",
    "default",
    "endfunction",
    "endprocedure",
];

fn identifier(input: &str) -> IResult<&str, &str> {
    verify(
        token(recognize(pair(
            satisfy(|c| is_identifer_char(c) && !c.is_ascii_digit()),
            take_while(is_identifer_char),
        ))),
        |ident: &str| !KEYWORDS.contains(&ident),
    )(input)
}

fn assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (identifier, expression)) = tuple((
        identifier,
        preceded(preceded(space0, char('=')), expression),
    ))(input)?;
    Ok((input, Statement::Assignment(identifier, expression)))
}

fn global_assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, assignment) =
        preceded(tag("global"), preceded(space1, assignment_statement))(input)?;
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
    let (input, (assignment, end, step, body)) = delimited(
        tag("for"),
        tuple((
            preceded(space1, assignment_statement),
            preceded(pair(space1, tag("to")), expression),
            // Maybe depending on the preceding expression, space1 could be
            // replaced by space0
            opt(preceded(pair(space1, tag("step")), expression))
                .map(|step_expression| step_expression.unwrap_or(Expression::IntegerLiteral(1))),
            list_of_statements,
        )),
        preceded(space0, pair(tag("next"), identifier)),
    )(input)?;
    Ok((
        input,
        match assignment {
            Statement::Assignment(identifier, expression) => {
                Statement::For(identifier, expression, end, step, body)
            }
            _ => unreachable!("assignment_statement can only return Statement::Assignment"),
        },
    ))
}

fn while_loop(input: &str) -> IResult<&str, Statement> {
    let (input, (exp, body)) = delimited(
        tag("while"),
        pair(expression, list_of_statements),
        preceded(space0, tag("endwhile")),
    )(input)?;
    Ok((input, Statement::While(exp, body)))
}

fn if_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (exp, body, elseifs, else_body)) = delimited(
        tag("if"),
        tuple((
            expression,
            preceded(pair(space0, tag("then")), list_of_statements),
            many0(preceded(
                pair(space0, tag("elseif")),
                pair(
                    expression,
                    preceded(pair(space0, tag("then")), list_of_statements),
                ),
            )),
            opt(preceded(pair(space0, tag("else")), list_of_statements)),
        )),
        pair(space0, tag("endif")),
    )(input)?;

    Ok((
        input,
        Statement::If(
            exp,
            body,
            elseifs
                .into_iter()
                .rev()
                .fold(else_body.unwrap_or_default(), |acc, (exp, body)| {
                    vec![Statement::If(exp, body, acc)]
                }),
        ),
    ))
}

fn switch_statement(input: &str) -> IResult<&str, Statement> {
    let (input, exp) = delimited(tag("switch"), expression, pair(space0, char(':')))(input)?;
    let (input, cases) = terminated(
        many0(preceded(
            space0,
            pair(
                alt((
                    preceded(
                        tag("case"),
                        map(expression, |rhs| {
                            Expression::Equal(Box::new(exp.clone()), Box::new(rhs))
                        }),
                    ),
                    map(tag("default"), |_| Expression::BoolLiteral(true)),
                )),
                preceded(pair(space0, char(':')), list_of_statements),
            ),
        )),
        pair(space0, tag("endswitch")),
    )(input)?;

    Ok((
        input,
        cases
            .into_iter()
            .rev()
            .fold(Vec::new(), |acc, (exp, body)| {
                vec![Statement::If(exp, body, acc)]
            })
            .remove(0),
    ))
}

fn token<'a, O>(
    f: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(space0, f)
}

/// Parse a function or procedure
fn function(input: &str) -> IResult<&str, Statement> {
    alt((
        delimited(
            tag("function"),
            function_arguments_and_body,
            token(tag("endfunction")),
        ),
        delimited(
            tag("procedure"),
            function_arguments_and_body,
            token(tag("endprocedure")),
        ),
    ))(input)
}

fn function_arguments_and_body(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            preceded(space1, identifier),
            delimited(
                token(open_bracket),
                separated_list0(
                    token(comma),
                    pair(
                        token(identifier),
                        map(
                            opt(preceded(
                                token(char(':')),
                                token(alt((value(false, tag("byVal")), value(true, tag("byRef"))))),
                            )),
                            |opt| opt.unwrap_or_default(),
                        ),
                    ),
                ),
                token(close_bracket),
            ),
            list_of_statements,
        )),
        |(ident, arguments, body)| Statement::Function(ident, arguments, body),
    )(input)
}

fn return_statement(input: &str) -> IResult<&str, Statement> {
    map(preceded(tag("return"), expression), Statement::Return)(input)
}

fn statement(input: &str) -> IResult<&str, Statement> {
    preceded(
        space0,
        alt((
            global_assignment_statement,
            for_loop,
            while_loop,
            if_statement,
            switch_statement,
            function,
            return_statement,
            assignment_statement,
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
    let (input, statements) = all_consuming(terminated(list_of_statements, space0))(input)?;
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
    ast_test!(if1);
    ast_test!(switch);
    ast_test!(comment1);
    ast_test!(function1);
    ast_test!(procedure1);
}
