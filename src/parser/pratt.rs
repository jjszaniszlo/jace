use std::{borrow::{Borrow, BorrowMut}, cell::RefCell, collections::HashMap, rc::Rc};

use super::{tokenstream::TokenStream, Output, Parser};

pub trait OpBindingPower {
    fn prefix_bp(&self, op: String) -> Option<usize>;
    fn infix_bp(&self, op: String) -> Option<(usize, usize)>;
    fn postfix_bp(&self, op: String) -> Option<usize>;
}

#[derive(Debug)]
pub struct PrattBindingPowers {
    prefix_bp: HashMap<String, usize>,
    infix_bp: HashMap<String, (usize, usize)>,
    postfix_bp: HashMap<String, usize>,
}

impl PrattBindingPowers {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            prefix_bp: HashMap::new(),
            infix_bp: HashMap::new(),
            postfix_bp: HashMap::new(),
        }
    }
}

impl OpBindingPower for PrattBindingPowers {
    #[inline(always)]
    fn prefix_bp(&self, op: String) -> Option<usize> {
        self.prefix_bp.get(&op).copied()
    }

    #[inline(always)]
    fn infix_bp(&self, op: String) -> Option<(usize, usize)> {
        self.infix_bp.get(&op).copied()
    }

    #[inline(always)]
    fn postfix_bp(&self, op: String) -> Option<usize> {
        self.postfix_bp.get(&op).copied()
    }
}

//pub trait BinaryDefFn<'a, O, Op>: Fn(&mut TokenStream<'a>, Op, O, O) -> Output<'a, O> {} 
//impl<'a, T, O, Op> BinaryDefFn<'a, O, Op> for T where
//    T: Fn(&mut TokenStream<'a>, Op, O, O) -> Output<'a, O> {}
//
//pub struct BinaryDef<'a, O, Op>(
//    Op,
//    usize,
//    usize,
//    Rc<RefCell<dyn BinaryDefFn<'a, O, Op>>>);   
//
//pub trait UnaryDefFn<'a, O, Op>: Fn(&mut TokenStream<'a>, Op, O) -> Output<'a, O> {} 
//impl<'a, T, O, Op> UnaryDefFn<'a, O, Op> for T where
//    T: Fn(&mut TokenStream<'a>, Op, O) -> Output<'a, O> {}
//
//pub struct UnaryDef<'a, O, Op>(Op, usize, Rc<RefCell<dyn UnaryDefFn<'a, O, Op>>>);
//
//pub fn pratt<'a, O, Op, P, P1, P2, P3>(
//    atom: impl Borrow<P>,
//    prefix: impl Borrow<P1>,
//    infix: impl Borrow<P2>,
//    postfix: impl Borrow<P3>,
//    min_bp: usize,
//) -> impl Parser<'a, O>
//where
//    P: Parser<'a, O>,
//    P1: Parser<'a, UnaryDef<'a, O, Op>>,
//    P2: Parser<'a, BinaryDef<'a, O, Op>>,
//    P3: Parser<'a, UnaryDef<'a, O, Op>>,
//{
//    move |input: &mut TokenStream<'a>| {
//        let (mut lhs, span) = if let Ok(r) = atom.borrow().parse_next(input) {
//            r
//        } else {
//            let (UnaryDef(op, r_bp, fold), span) = prefix.borrow().parse_next(input)?;
//            let (rhs, span) = pratt::<'a, O, Op, P, P1, P2, P3>(
//                atom.borrow(),
//                prefix.borrow(),
//                infix.borrow(),
//                postfix.borrow(),
//                r_bp,
//            )
//            .parse_next(input)?;
//            let mut fold = fold.as_ref().borrow();
//            fold(input, op, rhs)?
//        };
//
//        loop {
//            if let Ok((UnaryDef(op, l_bp, fold), span)) = postfix.borrow().parse_next(input) {
//                if l_bp < min_bp {
//                    break;
//                }
//
//                (lhs, span) = fold.as_ref().borrow()(input, op, lhs)?;
//            } else if let Ok((BinaryDef(op, l_bp, r_bp, fold), span)) =
//                infix.borrow().parse_next(input)
//            {
//                if l_bp < min_bp {
//                    break;
//                }
//
//                let rhs;
//                (rhs, span) = pratt::<'a, O, Op, P, P1, P2, P3>(
//                    atom.borrow(),
//                    prefix.borrow(),
//                    infix.borrow(),
//                    postfix.borrow(),
//                    r_bp,
//                )
//                .parse_next(input)?;
//
//                (lhs, span) = fold.as_ref().borrow()(input, op, lhs, rhs)?;
//            } else {
//                break;
//            }
//        }
//
//        Ok((lhs, span))
//    }
//}

//    }
//}
//
//pub fn binary_op<'a, I: Clone, O, E, Op, P, F>(
//    op: P,
//    lbp: usize,
//    rbp: usize,
//    fold: F,
//) -> impl Parser<I, BinaryDef<'a, I, O, E, Op>, E>
//where
//    P: Parser<I, Op, E>,
//    E: FromExternalError<I, E>,
//    F: BinaryDefFn<'a, I, O, E, Op>,
//{
//    let fold = Rc::new(RefCell::new(fold));
//    map_res(op, move |o| Ok::<_, E>(BinaryDef(o, lbp, rbp, fold.clone())))
//}
//
//pub fn unary_op<'a, I: Clone, O, E, Op, P, F>(
//    op: P,
//    rbp: usize,
//    fold: F,
//) -> impl Parser<I, UnaryDef<'a, I, O, E, Op>, E>
//where
//    P: Parser<I, Op, E>,
//    E: FromExternalError<I, E>,
//    F: UnaryDefFn<'a, I, O, E, Op>,
//{
//    let fold = Rc::new(RefCell::new(fold));
//    map_res(op, move |o| Ok::<_, E>(UnaryDef(o, rbp, fold.clone())))
//}
//
//
