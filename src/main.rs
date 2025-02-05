mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(
        r#"
        type Person ::
            name : String
            age : Integer

        class Equal Person a ::
            (==) :: a, a => Bool
            sameAge :: a, a => Bool
            sameName :: a, a => Bool

        instance Equal Person {name, age}, {name2, age2} ::
            (==) => name == name2 && age == age2
            sameAge => age == age2
            sameName => name == name2

        let
            john := {name = "john", age = 21}
            harry := {name = "harry", age = 21}
        in sameAge (a,b => a+b) (harry)
        "#
    ).unwrap();
    //let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let (_, result) = parser::parse(&toks).unwrap();
    println!("{:#?}", result);
}
