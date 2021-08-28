use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{alpha1, alphanumeric0, char, i64, multispace0, multispace1},
    combinator::{all_consuming, map, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    StringLiteral(&'a str),
    NumberLiteral(i64),
    Identifier(&'a str),
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
        Box<ListOfStatements<'a>>,
    ),
    While(Expression<'a>, Box<ListOfStatements<'a>>),
    If(
        Vec<(Expression<'a>, Statement<'a>)>,
        Option<Box<Statement<'a>>>,
    ),
    Switch(
        Expression<'a>,
        Vec<(Expression<'a>, Statement<'a>)>,
        Option<Box<Statement<'a>>>,
    ),
    Function(&'a str, Vec<(&'a str, bool)>, Box<ListOfStatements<'a>>),
    Return(Expression<'a>),
}

type ListOfStatements<'a> = Vec<Statement<'a>>;

fn quote(input: &str) -> IResult<&str, char> {
    alt((char('"'), char('“'), char('”')))(input)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    preceded(
        multispace0,
        alt((
            map(
                delimited(
                    quote,
                    take_till(|c| c == '"' || c == '“' || c == '”'),
                    quote,
                ),
                |string_const| Expression::StringLiteral(string_const),
            ),
            map(i64, |num_const| Expression::NumberLiteral(num_const)),
        )),
    )(input)
}

fn identifer(input: &str) -> IResult<&str, &str> {
    preceded(multispace0, recognize(pair(alpha1, alphanumeric0)))(input)
}

fn assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (identifier, expression)) = tuple((
        identifer,
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

fn statement(input: &str) -> IResult<&str, Statement> {
    preceded(
        multispace0,
        alt((assignment_statement, global_assignment_statement)),
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

    #[test]
    fn variable_test() {
        assert_eq!(
            program(include_str!("../tests/variable.input")).unwrap().1,
            include!("../tests/variable.ast")
        );
    }
}
