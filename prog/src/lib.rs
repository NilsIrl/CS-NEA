use core::cmp::min_by_key;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_while},
    character::complete::{self, char, i64, not_line_ending, satisfy},
    combinator::{all_consuming, map, opt, recognize, value, verify},
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, Parser,
};

/// Holds settings for parsing, by default all fields are set to false
#[derive(Default)]
pub struct ParseSettings {
    /// Accept keywords in a different case than they are originally defined
    ///
    /// Supports: mod
    pub case_sensitive: bool,
    /// The variable used in next must be the same as the one declared in the for loop
    pub for_next_not_enforced: bool,
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(tag("//"), not_line_ending))(input)
}

fn space0(input: &str) -> IResult<&str, &str> {
    recognize(many0(verify(
        recognize(tuple((
            complete::multispace0,
            opt(comment),
            complete::multispace0,
        ))),
        |s: &str| !s.is_empty(),
    )))(input)
}

fn space1(input: &str) -> IResult<&str, &str> {
    // FIXME space0 allows no whietspace to be classified as space1
    // The comment should not be optional
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
struct Assignment<'a>(&'a str, Expression<'a>);

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Assignment(Assignment<'a>),
    GlobalAssignment(Assignment<'a>),
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

fn function_call(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        let (input, (function_name, arguments)) = pair(
            identifier,
            delimited(
                preceded(space0, open_bracket),
                separated_list0(token(comma), expression(parse_settings)),
                preceded(space0, close_bracket),
            ),
        )(input)?;
        Ok((input, Expression::FunctionCall(function_name, arguments)))
    }
}

fn number_literal(input: &str) -> IResult<&str, Expression> {
    let int = map(i64, Expression::IntegerLiteral)(input);
    let float = map(double, Expression::FloatLiteral)(input);
    if int.is_err() {
        float
    } else if float.is_err() {
        int
    } else {
        // unwrap because due to previous 2 selections, neither int nor float can be Err(_)
        min_by_key(int, float, |x| x.as_ref().unwrap().0.len())
    }
}

fn terminal(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        preceded(
            space0,
            alt((
                map(
                    delimited(quote, take_till(is_quote), quote),
                    |string_const| Expression::StringLiteral(string_const),
                ),
                map(tag("true"), |_| Expression::BoolLiteral(true)),
                map(tag("false"), |_| Expression::BoolLiteral(false)),
                function_call(parse_settings),
                map(identifier, |identifier| Expression::Variable(identifier)),
                number_literal,
            )),
        )(input)
    }
}

fn depth6(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        let (input, initial) = terminal(parse_settings)(input)?;
        let (input, rest) = many0(preceded(
            preceded(space0, tag("^")),
            terminal(parse_settings),
        ))(input)?;
        Ok((
            input,
            rest.into_iter().fold(initial, |acc, current| {
                Expression::Power(Box::new(acc), Box::new(current))
            }),
        ))
    }
}

fn tag_with_settings<'a>(
    tag_str: &'a str,
    parse_settings: &'a ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, &str> + 'a {
    move |input: &str| {
        if parse_settings.case_sensitive {
            tag(tag_str)(input)
        } else {
            tag_no_case(tag_str)(input)
        }
    }
}

// This may look like terrible design and it probably is, however it has a few advantages
// There is no variable that the operation is assigned to which means there is also no need to
// check the value of the variable at a later date (something which would add an O(1) or O(n)
// operation
//
// The other advantage is that the initial expression is parsed only once instead of multiple times
// And no vector is allocated (this would happen with the many0 + fold method that was used before)
fn depth5(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        let (input, initial) = depth6(parse_settings)(input)?;
        match preceded(preceded(space0, tag("*")), depth5(parse_settings))(input) {
            Ok((input, rhs)) => Ok((input, Expression::Times(Box::new(initial), Box::new(rhs)))),
            Err(_) => match preceded(preceded(space0, tag("/")), depth5(parse_settings))(input) {
                Ok((input, rhs)) => {
                    Ok((input, Expression::Divide(Box::new(initial), Box::new(rhs))))
                }
                Err(_) => match preceded(
                    preceded(space0, tag_with_settings("MOD", parse_settings)),
                    depth5(parse_settings),
                )(input)
                {
                    Ok((input, rhs)) => {
                        Ok((input, Expression::Modulus(Box::new(initial), Box::new(rhs))))
                    }
                    Err(_) => match preceded(
                        preceded(space0, tag_with_settings("DIV", parse_settings)),
                        depth5(parse_settings),
                    )(input)
                    {
                        Ok((input, rhs)) => Ok((
                            input,
                            Expression::Quotient(Box::new(initial), Box::new(rhs)),
                        )),
                        Err(_) => Ok((input, initial)),
                    },
                },
            },
        }
    }
}

fn depth4(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        let (input, initial) = depth5(parse_settings)(input)?;
        let (input, rest) = many0(pair(
            preceded(space0, alt((tag("+"), tag("-")))),
            preceded(space0, depth5(parse_settings)),
        ))(input)?;
        Ok((input, rest.into_iter().fold(initial, |prev, current| match current.0 {
                "+" => Expression::Plus,
                "-" => Expression::Minus,
                _ => unreachable!(),
            }
            (Box::new(prev), Box::new(current.1)))))
    }
}

fn depth3(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        let (input, initial) = depth4(parse_settings)(input)?;
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
            preceded(space0, depth4(parse_settings)),
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
}

fn not_depth(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        alt((
            map(
                preceded(preceded(space0, tag("NOT")), depth3(parse_settings)),
                |exp| Expression::Not(Box::new(exp)),
            ),
            depth3(parse_settings),
        ))(input)
    }
}

fn depth2(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        map(
            pair(
                not_depth(parse_settings),
                opt(preceded(
                    preceded(space0, tag("AND")),
                    preceded(space0, depth2(parse_settings)),
                )),
            ),
            |(lhs, rhs)| match rhs {
                Some(rhs) => Expression::And(Box::new(lhs), Box::new(rhs)),
                None => lhs,
            },
        )(input)
    }
}

fn depth1(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    move |input: &str| {
        map(
            pair(
                depth2(parse_settings),
                opt(preceded(
                    preceded(space0, tag("OR")),
                    preceded(space0, depth1(parse_settings)),
                )),
            ),
            |(lhs, rhs)| match rhs {
                Some(rhs) => Expression::Or(Box::new(lhs), Box::new(rhs)),
                None => lhs,
            },
        )(input)
    }
}

const QUOTES: [char; 3] = ['"', '“', '”'];

fn is_quote(c: char) -> bool {
    QUOTES.contains(&c)
}

fn quote(input: &str) -> IResult<&str, char> {
    // TODO: quotes are hardcoded
    satisfy(is_quote)(input)
}

fn expression(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Expression> + '_ {
    depth1(parse_settings)
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

fn assignment(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Assignment> + '_ {
    move |input: &str| {
        let (input, (identifier, expression)) = tuple((
            identifier,
            preceded(preceded(space0, char('=')), expression(parse_settings)),
        ))(input)?;
        Ok((input, Assignment(identifier, expression)))
    }
}

fn global_assignment_statement(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        map(
            preceded(tag("global"), preceded(space1, assignment(parse_settings))),
            Statement::GlobalAssignment,
        )(input)
    }
}

fn for_loop(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        let (input, Assignment(variable_name, assigned_value)) =
            preceded(pair(tag("for"), space1), assignment(parse_settings))(input)?;
        let (input, (end_expression, step_expression, body)) = terminated(
            tuple((
                preceded(pair(space1, tag("to")), expression(parse_settings)),
                // Maybe depending on the preceding expression, space1 could be
                // replaced by space0
                opt(preceded(
                    pair(space1, tag("step")),
                    expression(parse_settings),
                ))
                .map(|step_expression| step_expression.unwrap_or(Expression::IntegerLiteral(1))),
                list_of_statements(parse_settings),
            )),
            tuple((space0, tag("next"), |input| {
                if parse_settings.for_next_not_enforced {
                    identifier(input)
                } else {
                    token(tag(variable_name))(input)
                }
            })),
        )(input)?;
        Ok((
            input,
            Statement::For(
                variable_name,
                assigned_value,
                end_expression,
                step_expression,
                body,
            ),
        ))
    }
}

fn while_loop(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        let (input, (exp, body)) = delimited(
            tag("while"),
            pair(
                expression(parse_settings),
                list_of_statements(parse_settings),
            ),
            preceded(space0, tag("endwhile")),
        )(input)?;
        Ok((input, Statement::While(exp, body)))
    }
}

fn if_statement(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        let (input, (exp, body, elseifs, else_body)) = delimited(
            tag("if"),
            tuple((
                expression(parse_settings),
                preceded(
                    pair(space0, tag("then")),
                    list_of_statements(parse_settings),
                ),
                many0(preceded(
                    pair(space0, tag("elseif")),
                    pair(
                        expression(parse_settings),
                        preceded(
                            pair(space0, tag("then")),
                            list_of_statements(parse_settings),
                        ),
                    ),
                )),
                opt(preceded(
                    pair(space0, tag("else")),
                    list_of_statements(parse_settings),
                )),
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
}

fn switch_statement(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        let (input, exp) = delimited(
            tag("switch"),
            expression(parse_settings),
            pair(space0, char(':')),
        )(input)?;
        let (input, cases) = terminated(
            many0(preceded(
                space0,
                pair(
                    alt((
                        preceded(
                            tag("case"),
                            map(expression(parse_settings), |rhs| {
                                Expression::Equal(Box::new(exp.clone()), Box::new(rhs))
                            }),
                        ),
                        map(tag("default"), |_| Expression::BoolLiteral(true)),
                    )),
                    preceded(pair(space0, char(':')), list_of_statements(parse_settings)),
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
}

fn token<'a, O>(
    f: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(space0, f)
}

/// Parse a function or procedure
fn function(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        alt((
            delimited(
                tag("function"),
                function_arguments_and_body(parse_settings),
                token(tag("endfunction")),
            ),
            delimited(
                tag("procedure"),
                function_arguments_and_body(parse_settings),
                token(tag("endprocedure")),
            ),
        ))(input)
    }
}

fn function_arguments_and_body(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        map(
            tuple((
                preceded(space1, identifier),
                delimited(
                    token(open_bracket),
                    separated_list0(
                        token(comma),
                        pair(
                            identifier,
                            map(
                                opt(preceded(
                                    token(char(':')),
                                    token(alt((
                                        value(false, tag("byVal")),
                                        value(true, tag("byRef")),
                                    ))),
                                )),
                                |opt| opt.unwrap_or_default(),
                            ),
                        ),
                    ),
                    token(close_bracket),
                ),
                list_of_statements(parse_settings),
            )),
            |(ident, arguments, body)| Statement::Function(ident, arguments, body),
        )(input)
    }
}

fn return_statement(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        map(
            preceded(tag("return"), expression(parse_settings)),
            Statement::Return,
        )(input)
    }
}

fn statement(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        preceded(
            space0,
            alt((
                global_assignment_statement(parse_settings),
                for_loop(parse_settings),
                while_loop(parse_settings),
                if_statement(parse_settings),
                switch_statement(parse_settings),
                function(parse_settings),
                return_statement(parse_settings),
                map(assignment(parse_settings), Statement::Assignment),
                map(expression(parse_settings), |expression| {
                    Statement::Expression(expression)
                }),
            )),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
pub struct Program<'a>(ListOfStatements<'a>);

fn list_of_statements(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, ListOfStatements> + '_ {
    move |input: &str| many0(statement(parse_settings))(input)
}

fn program(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Program> + '_ {
    move |input: &str| {
        let (input, statements) =
            all_consuming(terminated(list_of_statements(parse_settings), space0))(input)?;
        // TODO check input is empty
        Ok((input, Program(statements)))
    }
}

pub fn parse_program<'a>(
    input: &'a str,
    parse_settings: &ParseSettings,
) -> Result<Program<'a>, nom::error::Error<&'a str>> {
    program(parse_settings)(input)
        .finish()
        .map(|(_, program)| program)
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
                    parse_program(
                        include_str!(concat!("../tests/", stringify!($function_name), ".input")),
                        &ParseSettings::default()
                    )
                    .unwrap(),
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
    ast_test!(thinking_logically);
}
