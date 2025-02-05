
//
//pub fn parse_module<'a>() -> impl Parser<'a, ast::Module> {
//    pair(
//        zero_or_more(parse_definition()),
//        parse_expression())
//    .map(|(defs, expr)| ast::Module(defs, expr))
//}
//
//pub fn parse_fn_def_header<'a>() -> impl Parser<'a, ast::Identifier> {
//    left(
//        match_identifier,
//        match_token(Token::ColonColon))
//}
//
//pub fn parse_fn_def<'a>() -> impl Parser<'a, ast::Def> {
//    pair(
//        pair(
//            parse_fn_def_header(),
//            parse_fn_def_types()),
//        right(
//            parse_fn_def_header(),
//            parse_fn_fnexpr()))
//    .map(|((i, (tp, rt)), exp)| ast::Def::FnDef(i, tp, rt, exp))
//}
//
//pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<ast::TypeName>, ast::TypeName)> {
//    pair(
//        parse_fn_type_params(),
//        right(
//            match_token(Token::FatArrow),
//            match_identifier))
//    .map(|(type_params, return_type)| (type_params, ast::TypeName(return_type)))
//}
//
//pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<ast::TypeName>> {
//    pair(
//        match_identifier,
//        zero_or_more(
//            right(
//                match_token(Token::Comma),
//                match_identifier)))
//    .map(|(first_type, types)| {
//        let mut final_types = vec![ast::TypeName(first_type)];
//        final_types.extend(
//            types.iter()
//                .map(|i| ast::TypeName(i.clone())));
//        final_types
//    })
//}
//
//pub fn parse_type_def<'a>() -> impl Parser<'a, ast::Def> {
//    pair(
//        right(
//            match_token(Token::TypeKeyword),
//            left(
//                match_identifier,
//                match_token(Token::ColonColon))),
//        one_or_more(
//            pair(
//                match_identifier,
//                right(
//                    match_token(Token::Colon),
//                    match_identifier))))
//    .map(|(i, v)| ast::Def::TypeDef(
//            ast::TypeName(i), 
//            v.into_iter().map(|(i, t)| (i, ast::TypeName(t))).collect()))
//}
//
//pub fn parse_class_def<'a>() -> impl Parser<'a, ast::Def> {
//    pair(
//        right(
//            match_token(Token::ClassKeyword),
//            pair(
//                match_identifier,
//                parse_class_generic_types())),
//        parse_class_method_defs())
//    .map(|((class_name, params), methods)|
//        ast::Def::ClassDef(ast::ClassName(class_name), params, methods))
//}
//
//pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<ast::GenericTypeParam>> {
//    one_or_more(match_identifier)
//    .map(|v|
//        v.iter().map(|i|
//            ast::GenericTypeParam(i.clone())).collect())
//}
//
//pub fn parse_class_method_defs<'a>() -> impl Parser<'a, Vec<ast::MethodDef>> {
//    one_or_more(parse_class_method_def())
//}
//
//pub fn parse_class_method_def<'a>() -> impl Parser<'a, ast::MethodDef> {
//    or(
//        parse_class_method_def_operator(),
//        parse_class_method_def_named())
//}
//
//pub fn parse_class_method_def_operator<'a>() -> impl Parser<'a, ast::MethodDef> {
//    pair(
//        left(
//            parse_method_operator,
//            match_token(Token::ColonColon)),
//        pair(
//            parse_class_method_type_params(),
//            right(
//                    match_token(Token::FatArrow),
//                    match_identifier)))
//    .map(|(op, (params, ret))|
//        ast::MethodDef::Operator(op, params, ast::TypeName(ret)))
//}
//
//pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<GenericTypeParam>> {
//    pair(
//        match_identifier,
//        zero_or_more(
//            right(
//                match_token(Token::Comma),
//                match_identifier)))
//    .map(|(first_type, types)| {
//        let mut final_type_params = vec![ast::GenericTypeParam(first_type)];
//
//        final_type_params.extend(
//            types.into_iter()
//                    .map(|t| ast::GenericTypeParam(t)));
//
//        final_type_params
//    })
//}
//
//pub fn parse_method_operator<'a>(input: &'a [Token]) -> ParseResult<'a, ast::MethodOperator> {
//    match input.get(0) {
//        Some(t) => match t {
//            Token::WrappedEqualsEquals => Ok((&input[1..], ast::MethodOperator::EqualsEquals)),
//            Token::WrappedNotEquals => Ok((&input[1..], ast::MethodOperator::NotEquals)),
//            Token::WrappedLessEquals => Ok((&input[1..], ast::MethodOperator::LessEquals)),
//            Token::WrappedGreaterEquals => Ok((&input[1..], ast::MethodOperator::GreaterEquals)),
//            Token::WrappedGreater => Ok((&input[1..], ast::MethodOperator::Greater)),
//            Token::WrappedLess => Ok((&input[1..], ast::MethodOperator::Less)),
//            Token::WrappedPlus => Ok((&input[1..], ast::MethodOperator::Plus)),
//            Token::WrappedMinus => Ok((&input[1..], ast::MethodOperator::Minus)),
//            Token::WrappedDivide => Ok((&input[1..], ast::MethodOperator::Divide)),
//            Token::WrappedMultiply => Ok((&input[1..], ast::MethodOperator::Multiply)),
//            _ => Err(ParserError::CouldNotMatchToken),
//        },
//        None => Err(ParserError::CouldNotMatchToken),
//    }
//}
//
//pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, ast::MethodDef> { 
//    pair(
//        left(
//            match_identifier,
//            match_token(Token::ColonColon)),
//        pair(
//            parse_class_method_type_params(),
//            right(
//                    match_token(Token::FatArrow),
//                    match_identifier)))
//    .map(|(ident, (params, ret))|
//        ast::MethodDef::Named(ident, params, ast::TypeName(ret)))
//}
//
//
//
//pub fn parse_fn_param<'a>() -> impl Parser<'a, ast::FnParam> {
//    or_n(vec![
//        match_identifier.map(|i| ast::FnParam::Identifier(i)),
//        BoxedParser::new(parse_fn_param_set_deconstruct()),
//        BoxedParser::new(parse_fn_param_set_selector()),
//    ])
//}
//
//pub fn parse_fn_param_set_deconstruct<'a>() -> impl Parser<'a, ast::FnParam> {
//    right(
//        match_token(Token::LeftBrace),
//        left(
//                zero_or_one(
//                    pair(
//                        match_identifier,
//                        zero_or_more(
//                            right(
//                                match_token(Token::Comma),
//                                match_identifier)))),
//                match_token(Token::RightBrace)))
//    .map(|v| {
//        let mut final_set_params : Vec<ast::Identifier> = vec![];
//        match v {
//            Some((first_param, params)) => {
//                final_set_params.push(first_param);
//                final_set_params.extend(params);
//            },
//            None => {}
//        };
//        ast::FnParam::SetDeconstruct(final_set_params)
//    })
//}
//
//pub fn parse_fn_param_set_selector<'a>() -> impl Parser<'a, ast::FnParam> {
//    pair(
//        right(
//            match_token(Token::LeftBrace),
//            match_identifier),
//        right(
//            match_token(Token::Colon),
//                left(
//                    match_identifier,
//                    match_token(Token::RightParen))))
//    .map(|(first, rest)| ast::FnParam::SetSelector(first, rest))                
//}
//
//pub fn parse_fn_case_param<'a>() -> impl Parser<'a, ast::FnParam> {
//    or_n(vec![
//        match_identifier.map(|i| ast::FnParam::Identifier(i)),
//        match_literal.map(|l| ast::FnParam::Literal(l)),
//        BoxedParser::new(parse_fn_param_set_deconstruct()),
//    ])
//}
//
//pub fn parse_fn_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
//    pair(
//        parse_fn_param(),
//        zero_or_more(
//            right(
//            match_token(Token::Comma),
//            parse_fn_param())))
//    .map(|(first_param, params)| {
//        let mut final_params = vec![first_param];
//        final_params.extend(params);
//        final_params
//    })
//}
//
//pub fn parse_fn_case_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
//    pair(
//        parse_fn_case_param(),
//        zero_or_more(
//            right(
//            match_token(Token::Comma),
//            parse_fn_case_param())))
//    .map(|(first_param, params)| {
//        let mut final_params = vec![first_param];
//        final_params.extend(params);
//        final_params
//    })
//}
//
//pub fn parse_fn_expression_single<'a>() -> impl Parser<'a, ast::FnExpr> {
//    pair(
//        left(
//            parse_fn_params(),
//            match_token(Token::FatArrow)),
//        parse_expression())
//    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
//}
//
//pub fn parse_fn_fnexpr<'a>() -> impl Parser<'a, ast::FnExpr> {
//    or(
//        parse_fn_expression_single(),
//        parse_fn_expression_case())
//}
//
//pub fn parse_fn_expression<'a>() -> impl Parser<'a, ast::Expr> {
//    parse_fn_fnexpr().map(|f| ast::Expr::FnExpr(P(f)))
//}
//
//pub fn parse_fn_expression_case_single<'a>() -> impl Parser<'a, ast::FnExpr> {
//    pair(
//        left(
//            parse_fn_case_params(),
//            match_token(Token::FatArrow)),
//        parse_expression())
//    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
//}
//
//pub fn parse_fn_expression_case<'a>() -> impl Parser<'a, ast::FnExpr> {
//    right(
//        match_token(Token::CaseKeyword),
//        one_or_more(parse_fn_expression_case_single()))
//    .map(|fn_exprs| ast::FnExpr::Case(fn_exprs))
//}
//
//pub fn parse_fn_call<'a>() -> impl Parser<'a, ast::Expr> {
//    pair(
//        match_identifier,
//        one_or_more(parse_expression()))
//    .map(|(func_name, params)| {
//        ast::Expr::FnCall(func_name, params)
//    })
//}
//
//
//
//
//pub fn parse_equality_expr<'a>() -> impl Parser<'a, ast::Expr> {
//    pair(
//        parse_additive_expr(),
//        zero_or_more(
//            pair(
//                or_n(vec![
//                    match_token_fetch(Token::Greater),
//                    match_token_fetch(Token::GreaterEquals),
//                    match_token_fetch(Token::Less),
//                    match_token_fetch(Token::LessEquals),
//                    match_token_fetch(Token::EqualsEquals),
//                    match_token_fetch(Token::NotEquals),
//                ]),
//                parse_additive_expr(),
//            ),
//        ),
//    )
//    .map(|(initial, rest)| {
//        rest.into_iter().fold(initial, |acc, (op, next_expr)| {
//            let operator = match op {
//                Token::Greater => ast::BinOperator::Greater,
//                Token::GreaterEquals => ast::BinOperator::GreaterEquals,
//                Token::Less => ast::BinOperator::Less,
//                Token::LessEquals => ast::BinOperator::LessEquals,
//                Token::EqualsEquals => ast::BinOperator::EqualsEquals,
//                Token::NotEquals => ast::BinOperator::NotEquals,
//                _ => unreachable!(),
//            };
//            ast::Expr::BinOpExpr(operator, P(acc), P(next_expr))
//        })
//    })
//}
//
//pub fn parse_additive_expr<'a>() -> impl Parser<'a, ast::Expr> {
//    pair(
//        parse_multiplicative_expr(),
//        zero_or_more(
//            pair(
//                or(match_token_fetch(Token::Plus), match_token_fetch(Token::Minus)),
//                parse_multiplicative_expr(),
//            ),
//        ),
//    )
//    .map(|(initial, rest)| {
//        rest.into_iter().fold(initial, |acc, (op, next_expr)| {
//            let operator = match op {
//                Token::Plus => ast::BinOperator::Plus,
//                Token::Minus => ast::BinOperator::Minus,
//                _ => unreachable!(),
//            };
//            ast::Expr::BinOpExpr(operator, P(acc), P(next_expr))
//        })
//    })
//}
//
//pub fn parse_multiplicative_expr<'a>() -> impl Parser<'a, ast::Expr> {
//    pair(
//        parse_unary_minus_or_primary(),
//        zero_or_more(
//            pair(
//                or(match_token_fetch(Token::Multiply), match_token_fetch(Token::Divide)),
//                parse_unary_minus_or_primary(),
//            ),
//        ),
//    )
//    .map(|(initial, rest)| {
//        rest.into_iter().fold(initial, |acc, (op, next_expr)| {
//            let operator = match op {
//                Token::Multiply => ast::BinOperator::Multiply,
//                Token::Divide => ast::BinOperator::Divide,
//                _ => unreachable!(),
//            };
//            ast::Expr::BinOpExpr(operator, P(acc), P(next_expr))
//        })
//    })
//}
//
//pub fn parse_unary_minus_or_primary<'a>() -> BoxedParser<'a, ast::Expr> {
//    or(
//    parse_unary_minus().pred(|expr| !matches!(expr, ast::Expr::LitExpr(_))),
//        parse_primary())
//}
//
//pub fn parse_unary_minus<'a>() -> impl Parser<'a, ast::Expr> {
//    right(match_token(Token::Minus), parse_primary())
//        .map(|expr| ast::Expr::BinOpExpr(
//            ast::BinOperator::Minus,
//            P(ast::Expr::LitExpr(ast::Literal::Integer(0))),
//            P(expr)))
//}
//
//pub fn parse_parenthesized_expression<'a>() -> BoxedParser<'a, ast::Expr> {
//    right(
//        match_token(Token::LeftParen),
//        left(
//            parse_expression(),
//            match_token(Token::RightParen)))
//}
//
//pub fn parse_primary<'a>() -> BoxedParser<'a, ast::Expr> {
//    or_n(vec![
//        parse_parenthesized_expression(),
//        parse_literal().map(|n| ast::Expr::LitExpr(n)),
//        parse_identifier().map(|i| ast::Expr::IdentExpr(i))
//    ])
//}
