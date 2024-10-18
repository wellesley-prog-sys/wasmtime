
use easy_smt::*;

fn declare(smt: &mut Context, name: String, val: SExpr) -> SExpr {
    smt.declare_const(name.clone(), val).unwrap();
    smt.atom(name)
}

pub fn popcnt(smt: &mut Context, ty: usize, x: SExpr, id: u32) -> SExpr {
    let mut bits: Vec<_> = (0..ty)
        .map(|i| smt.zero_extend(7, smt.extract(i as i32, i as i32, x)))
        .collect();
    let initial = bits.pop().unwrap();
    let r = bits.iter().fold(initial, |a, b| smt.bvadd(a, *b));

    let id = format!("{ty}_{id}");
    let result = declare(smt, 
        format!("popcnt_{id}"),
        smt.list(vec![
            smt.atoms().und,
            smt.atom("BitVec"),
            smt.numeral(8),
        ]),
    );
    smt.assert(smt.eq(result, r));
    result
}

