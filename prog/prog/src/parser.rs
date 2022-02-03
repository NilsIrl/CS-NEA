use core::cmp::min_by_key;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_while},
    character::complete::{self, char, i64, not_line_ending, satisfy},
    combinator::{all_consuming, map, opt, recognize, value, verify},
    multi::{many0, separated_list0, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};

use super::interpreter::Program;

pub struct ParseSettings {
    /// Accept keywords in a different case than they are originally defined
    ///
    /// Supports: mod
    pub case_sensitive: bool,
    /// The variable used in next must be the same as the one declared in the for loop
    pub for_next_not_enforced: bool,
}

impl Default for ParseSettings {
    fn default() -> Self {
        Self {
            case_sensitive: false,
            for_next_not_enforced: false,
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a>(pub String, pub Vec<Expression<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    StringLiteral(&'a str),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    FunctionCall(Call<'a>),
    Variable(String),

    MethodCall(Box<Expression<'a>>, Call<'a>),
    ObjectAttribute(Box<Expression<'a>>, String),
    New(Call<'a>),

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
pub struct Assignment<'a>(pub String, pub Expression<'a>);

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration<'a> {
    pub name: String,
    // asRef: bool
    pub args: Vec<(String, bool)>,
    pub body: ListOfStatements<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Assignment(Assignment<'a>),
    GlobalAssignment(Assignment<'a>),
    ArrayDeclaration(String, Vec<Expression<'a>>),
    ClassDeclaration {
        name: String,
        superclass: Option<String>,
        // isPrivate: bool
        fields: Vec<(bool, String)>,
        // isPrivate: bool
        methods: Vec<(bool, FunctionDeclaration<'a>)>,
    },
    Expression(Expression<'a>),
    For(
        String,
        Expression<'a>,
        Expression<'a>,
        Expression<'a>,
        ListOfStatements<'a>,
    ),
    While(Expression<'a>, ListOfStatements<'a>),
    DoUntil(ListOfStatements<'a>, Expression<'a>),
    If(Expression<'a>, ListOfStatements<'a>, ListOfStatements<'a>),
    // asRef: bool
    Function(FunctionDeclaration<'a>),
    Return(Expression<'a>),
}

pub type ListOfStatements<'a> = Vec<Statement<'a>>;

fn open_bracket(input: &str) -> IResult<&str, char> {
    char('(')(input)
}

fn close_bracket(input: &str) -> IResult<&str, char> {
    char(')')(input)
}

fn comma(input: &str) -> IResult<&str, char> {
    char(',')(input)
}

fn call(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Call> + '_ {
    move |input: &str| {
        let (input, (function_name, arguments)) = pair(
            identifier(parse_settings),
            delimited(
                preceded(space0, open_bracket),
                separated_list0(token(comma), expression(parse_settings)),
                preceded(space0, close_bracket),
            ),
        )(input)?;
        Ok((input, Call(function_name, arguments)))
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
                map(tag_with_settings("true", parse_settings), |_| {
                    Expression::BoolLiteral(true)
                }),
                map(tag_with_settings("false", parse_settings), |_| {
                    Expression::BoolLiteral(false)
                }),
                number_literal,
                map(
                    preceded(
                        pair(tag_with_settings("new", parse_settings), space1),
                        call(parse_settings),
                    ),
                    Expression::New,
                ),
                map(call(parse_settings), Expression::FunctionCall),
                map(identifier(parse_settings), Expression::Variable),
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
    move |input: &str| {
        let (input, expression) = depth1(parse_settings)(input)?;
        Ok(
            match preceded(
                char('.'),
                alt((
                    map(call(parse_settings), Expression::FunctionCall),
                    map(identifier(parse_settings), Expression::Variable),
                )),
            )(input)
            {
                Ok((input, Expression::FunctionCall(call))) => {
                    (input, Expression::MethodCall(Box::new(expression), call))
                }
                Ok((input, Expression::Variable(identifier))) => (
                    input,
                    Expression::ObjectAttribute(Box::new(expression), identifier),
                ),
                Ok((..)) => unreachable!("Can only return FunctionCall or Variable"),
                Err(_) => (input, expression),
            },
        )
    }
}

fn is_identifer_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

// TODO: this only contains the endings, the beginnings are not necessary for parsing
/// Sorted list of keywords that cannot be used as identifiers.
/// This list must be kept sorted otherwise binary search will fail.
const KEYWORDS: [&str; 14] = [
    "case",
    "default",
    "else",
    "elseif",
    "endclass",
    "endfunction",
    "endif",
    "endprocedure",
    "endswitch",
    "endwhile",
    "function",
    "next",
    "procedure",
    "until",
];

fn identifier(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, String> + '_ {
    move |input: &str| {
        map(
            verify(
                token(recognize(pair(
                    satisfy(|c| is_identifer_char(c) && !c.is_ascii_digit()),
                    take_while(is_identifer_char),
                ))),
                |ident: &str| {
                    !if parse_settings.case_sensitive {
                        KEYWORDS.binary_search(&ident).is_ok()
                    } else {
                        KEYWORDS
                            .binary_search(&ident.to_lowercase().as_str())
                            .is_ok()
                    }
                },
            ),
            |ident| {
                if parse_settings.case_sensitive {
                    ident.to_string()
                } else {
                    ident.to_lowercase()
                }
            },
        )(input)
    }
}

fn assignment(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Assignment> + '_ {
    move |input: &str| {
        let (input, (identifier, expression)) = tuple((
            identifier(parse_settings),
            preceded(preceded(space0, char('=')), expression(parse_settings)),
        ))(input)?;
        Ok((input, Assignment(identifier, expression)))
    }
}

fn array_declaration(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        map(
            pair(
                preceded(
                    pair(tag_with_settings("array", parse_settings), space1),
                    identifier(parse_settings),
                ),
                delimited(
                    token(char('[')),
                    separated_list1(token(comma), expression(parse_settings)),
                    token(char(']')),
                ),
            ),
            |(name, dimension)| Statement::ArrayDeclaration(name, dimension),
        )(input)
    }
}

fn class_declaration(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        map(
            delimited(
                tag_with_settings("class", parse_settings),
                tuple((
                    identifier(parse_settings),
                    opt(preceded(
                        pair(space1, tag("inherits")),
                        identifier(parse_settings),
                    )),
                    many0(pair(
                        token(alt((
                            value(false, tag_with_settings("public", parse_settings)),
                            value(true, tag_with_settings("private", parse_settings)),
                        ))),
                        preceded(space1, identifier(parse_settings)),
                    )),
                    many0(pair(
                        token(alt((
                            value(false, tag_with_settings("public", parse_settings)),
                            value(true, tag_with_settings("private", parse_settings)),
                        ))),
                        preceded(space1, function(parse_settings)),
                    )),
                )),
                token(tag_with_settings("endclass", parse_settings)),
            ),
            |(name, superclass, fields, methods)| Statement::ClassDeclaration {
                name,
                superclass,
                fields,
                methods,
            },
        )(input)
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
                    // recognize used to bypass differing types between branches
                    recognize(identifier(parse_settings))(input)
                } else {
                    token(tag(variable_name.as_str()))(input)
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

fn do_until_loop(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, Statement> + '_ {
    move |input: &str| {
        let (input, (exp, body)) = pair(
            preceded(tag("do"), list_of_statements(parse_settings)),
            preceded(pair(space0, tag("until")), expression(parse_settings)),
        )(input)?;
        Ok((input, Statement::DoUntil(exp, body)))
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
fn function(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, FunctionDeclaration> + '_ {
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
) -> impl FnMut(&str) -> IResult<&str, FunctionDeclaration> + '_ {
    move |input: &str| {
        map(
            tuple((
                preceded(space1, identifier(parse_settings)),
                delimited(
                    token(open_bracket),
                    separated_list0(
                        token(comma),
                        pair(
                            identifier(parse_settings),
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
            |(ident, arguments, body)| FunctionDeclaration {
                name: ident,
                args: arguments,
                body,
            },
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
                array_declaration(parse_settings),
                class_declaration(parse_settings),
                global_assignment_statement(parse_settings),
                for_loop(parse_settings),
                while_loop(parse_settings),
                do_until_loop(parse_settings),
                if_statement(parse_settings),
                switch_statement(parse_settings),
                map(function(parse_settings), Statement::Function),
                return_statement(parse_settings),
                map(assignment(parse_settings), Statement::Assignment),
                map(expression(parse_settings), |expression| {
                    Statement::Expression(expression)
                }),
            )),
        )(input)
    }
}

pub fn program(parse_settings: &ParseSettings) -> impl FnMut(&str) -> IResult<&str, Program> + '_ {
    move |input: &str| {
        let (input, statements) =
            all_consuming(terminated(list_of_statements(parse_settings), space0))(input)?;
        Ok((input, Program(statements)))
    }
}

fn list_of_statements(
    parse_settings: &ParseSettings,
) -> impl FnMut(&str) -> IResult<&str, ListOfStatements> + '_ {
    move |input: &str| many0(statement(parse_settings))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn keywords_sorted() {
        assert!(KEYWORDS.is_sorted());
    }

    const CASE_SENSITIVE: ParseSettings = ParseSettings {
        case_sensitive: true,
        for_next_not_enforced: false,
    };

    const CASE_INSENSITIVE: ParseSettings = ParseSettings {
        case_sensitive: false,
        for_next_not_enforced: false,
    };

    macro_rules! ast_test {
        ( $function_name:ident, $parse_settings:expr ) => {
            #[test]
            fn $function_name() {
                assert_eq!(
                    Program::from_str(
                        include_str!(concat!(
                            "../test_data/",
                            stringify!($function_name),
                            ".input"
                        )),
                        $parse_settings
                    )
                    .unwrap(),
                    include!(concat!("../test_data/", stringify!($function_name), ".ast"))
                );
            }
        };
    }

    ast_test!(variable, &CASE_SENSITIVE);
    ast_test!(variable_quotes, &CASE_SENSITIVE);
    ast_test!(variable_whitespace, &CASE_SENSITIVE);
    ast_test!(function_call, &CASE_SENSITIVE);
    ast_test!(complex_function_call, &CASE_SENSITIVE);
    ast_test!(print, &CASE_SENSITIVE);
    ast_test!(input, &CASE_SENSITIVE);
    ast_test!(for_loop1, &CASE_SENSITIVE);
    ast_test!(for_loop2, &CASE_SENSITIVE);
    ast_test!(comparison1, &CASE_SENSITIVE);
    ast_test!(empty, &CASE_SENSITIVE);
    ast_test!(maths1, &CASE_SENSITIVE);
    ast_test!(logical_operators1, &CASE_SENSITIVE);
    ast_test!(not1, &CASE_SENSITIVE);
    ast_test!(while1, &CASE_SENSITIVE);
    ast_test!(if1, &CASE_SENSITIVE);
    ast_test!(switch, &CASE_SENSITIVE);
    ast_test!(comment1, &CASE_SENSITIVE);
    ast_test!(function1, &CASE_SENSITIVE);
    ast_test!(procedure1, &CASE_SENSITIVE);
    ast_test!(thinking_logically, &CASE_INSENSITIVE);
    ast_test!(do_until1, &CASE_SENSITIVE);
    ast_test!(attribute1, &CASE_SENSITIVE);
    ast_test!(method_call1, &CASE_SENSITIVE);
    ast_test!(array_declaration1, &CASE_SENSITIVE);
    ast_test!(class1, &CASE_SENSITIVE);
    ast_test!(new_object, &CASE_SENSITIVE);
    ast_test!(minus_one_literal, &CASE_SENSITIVE);
}
