use std::collections::HashMap;

use super::transpiler::show_preprocessor;
use super::typechecker::*;

use glsl::parser::Parse as _;
use glsl::syntax::*;

#[derive(Debug, Clone)]
pub struct ShaderProp {
    pub name: String,
    pub prop_type: String,
    pub val_type: String,
    pub val: String,
    pub toggle: bool,
}

impl ShaderProp {
    fn new(name: String, prop_type: String, val_type: String, val: String, toggle: bool) -> Self {
        Self {
            name,
            prop_type,
            val_type,
            val,
            toggle,
        }
    }
}

fn eval_vector_vals(fid: &Identifier, exps: &Vec<Expr>) -> Option<Vec<f32>> {
    let vals = exps
        .iter()
        .flat_map(eval_shallow_const_expr)
        .flat_map(|x| match x {
            Expr::FloatConst(v) => Some(v),
            Expr::DoubleConst(v) => Some(v as f32),
            Expr::IntConst(v) => Some(v as f32),
            Expr::UIntConst(v) => Some(v as f32),
            Expr::BoolConst(v) => Some(if v { 1.0 } else { 0.0 }),
            _ => None,
        })
        .collect::<Vec<f32>>();

    match get_function_ret_type(fid.0.as_str(), exps.iter().map(get_expr_type).collect()) {
        Some(TypeKind::Vector(n)) if n == vals.len() => Some(vals),
        _ => None,
    }
}

fn eval_shallow_const_expr(e: &Expr) -> Option<Expr> {
    match e {
        Expr::Unary(op, r) => match (op, *r.clone()) {
            (UnaryOp::Minus, Expr::FloatConst(v)) => Some(Expr::FloatConst(-v)),
            (UnaryOp::Minus, Expr::IntConst(v)) => Some(Expr::IntConst(-v)),
            (UnaryOp::Minus, Expr::DoubleConst(v)) => Some(Expr::DoubleConst(-v)),
            (UnaryOp::Not, Expr::BoolConst(v)) => Some(Expr::BoolConst(!v)),
            (UnaryOp::Minus, Expr::FunCall(FunIdentifier::Identifier(fid), exps)) => {
                match eval_vector_vals(&fid, &exps) {
                    Some(vals) => Some(Expr::FunCall(
                        FunIdentifier::Identifier(fid),
                        vals.iter().map(|x| Expr::FloatConst(-*x as f32)).collect(),
                    )),
                    _ => Some(e.clone()),
                }
            }
            _ => Some(e.clone()),
        },
        Expr::Binary(op, l, r) => match (op, *l.clone(), *r.clone()) {
            (BinaryOp::Add, Expr::FloatConst(l), Expr::FloatConst(r)) => Some(Expr::FloatConst(l + r)),
            (BinaryOp::Add, Expr::DoubleConst(l), Expr::DoubleConst(r)) => Some(Expr::DoubleConst(l + r)),
            (BinaryOp::Add, Expr::IntConst(l), Expr::IntConst(r)) => Some(Expr::IntConst(l + r)),
            (BinaryOp::Add, Expr::UIntConst(l), Expr::UIntConst(r)) => Some(Expr::UIntConst(l + r)),

            (BinaryOp::Sub, Expr::FloatConst(l), Expr::FloatConst(r)) => Some(Expr::FloatConst(l - r)),
            (BinaryOp::Sub, Expr::DoubleConst(l), Expr::DoubleConst(r)) => Some(Expr::DoubleConst(l - r)),
            (BinaryOp::Sub, Expr::IntConst(l), Expr::IntConst(r)) => Some(Expr::IntConst(l - r)),
            (BinaryOp::Sub, Expr::UIntConst(l), Expr::UIntConst(r)) => Some(Expr::UIntConst(l - r)),

            (BinaryOp::Mult, Expr::FloatConst(l), Expr::FloatConst(r)) => Some(Expr::FloatConst(l * r)),
            (BinaryOp::Mult, Expr::DoubleConst(l), Expr::DoubleConst(r)) => Some(Expr::DoubleConst(l * r)),
            (BinaryOp::Mult, Expr::IntConst(l), Expr::IntConst(r)) => Some(Expr::IntConst(l * r)),
            (BinaryOp::Mult, Expr::UIntConst(l), Expr::UIntConst(r)) => Some(Expr::UIntConst(l * r)),

            (BinaryOp::Div, Expr::FloatConst(l), Expr::FloatConst(r)) => Some(Expr::FloatConst(l / r)),
            (BinaryOp::Div, Expr::DoubleConst(l), Expr::DoubleConst(r)) => Some(Expr::DoubleConst(l / r)),
            (BinaryOp::Div, Expr::IntConst(l), Expr::IntConst(r)) => Some(Expr::IntConst(l / r)),
            (BinaryOp::Div, Expr::UIntConst(l), Expr::UIntConst(r)) => Some(Expr::UIntConst(l / r)),

            (
                bop,
                Expr::FunCall(FunIdentifier::Identifier(fidl), expsl),
                Expr::FunCall(FunIdentifier::Identifier(fidr), expsr),
            ) => match (eval_vector_vals(&fidl, &expsl), eval_vector_vals(&fidr, &expsr)) {
                (Some(valsl), Some(valsr)) => {
                    let vals: Option<Vec<f32>> = match bop {
                        BinaryOp::Add => Some(valsl.iter().zip(valsr.iter()).map(|(l, r)| l + r).collect()),
                        BinaryOp::Sub => Some(valsl.iter().zip(valsr.iter()).map(|(l, r)| l - r).collect()),
                        BinaryOp::Mult => Some(valsl.iter().zip(valsr.iter()).map(|(l, r)| l * r).collect()),
                        BinaryOp::Div => Some(valsl.iter().zip(valsr.iter()).map(|(l, r)| l / r).collect()),
                        _ => None,
                    };
                    if let Some(vals) = vals {
                        Some(Expr::FunCall(
                            FunIdentifier::Identifier(fidl),
                            vals.iter().map(|x| Expr::FloatConst(*x)).collect(),
                        ))
                    } else {
                        None
                    }
                }
                _ => Some(e.clone()),
            },

            _ => Some(e.clone()),
        },
        _ => Some(e.clone()),
    }
}

fn extract_prop(id: &str, e: &Expr) -> Option<ShaderProp> {
    match eval_shallow_const_expr(e) {
        Some(Expr::FloatConst(v)) => Some(ShaderProp::new(
            id.to_owned(),
            "Float".into(),
            "float".into(),
            v.to_string(),
            false,
        )),
        Some(Expr::DoubleConst(v)) => Some(ShaderProp::new(
            id.to_owned(),
            "Float".into(),
            "float".into(),
            v.to_string(),
            false,
        )),
        Some(Expr::IntConst(v)) => Some(ShaderProp::new(
            id.to_owned(),
            "Float".into(),
            "float".into(),
            v.to_string(),
            false,
        )),
        Some(Expr::UIntConst(v)) => Some(ShaderProp::new(
            id.to_owned(),
            "Float".into(),
            "float".into(),
            v.to_string(),
            false,
        )),
        Some(Expr::BoolConst(v)) => Some(ShaderProp::new(
            id.to_owned(),
            "Float".into(),
            "float".into(),
            if v { "1".into() } else { "0".into() },
            false,
        )),
        Some(Expr::FunCall(FunIdentifier::Identifier(ref fid), ref exps)) => match eval_vector_vals(fid, exps) {
            Some(vals) => {
                let ctor = if vals.len() < 3 {
                    vals.iter()
                        .chain(std::iter::repeat(&0.0))
                        .map(|x| x.to_string())
                        .take(3)
                        .collect::<Vec<_>>()
                        .join(",")
                } else {
                    vals.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
                };
                Some(ShaderProp::new(
                    id.to_owned(),
                    "Vector".into(),
                    format!("float{}", vals.len()),
                    format!("({})", ctor),
                    false,
                ))
            }
            _ => None,
        },
        _ => None,
    }
}

pub fn process_globals(tu: &mut TranslationUnit, extract_props: bool) -> Vec<ShaderProp> {
    let mut res = Vec::new();

    fn is_const(ty: &TypeQualifierSpec) -> bool {
        match ty {
            TypeQualifierSpec::Storage(StorageQualifier::Const) => true,
            _ => false,
        }
    }

    if extract_props {
        for decl in tu.0 .0.iter_mut() {
            match decl {
                ExternalDeclaration::Declaration(Declaration::InitDeclaratorList(ref mut idl)) => {
                    match (&idl.head.name, &idl.head.initializer, &mut idl.head.ty.qualifier) {
                        (Some(name), Some(Initializer::Simple(init)), Some(qual)) => {
                            if qual.qualifiers.0.iter().any(is_const) {
                                if let Some(prop) = extract_prop(name.as_str(), init.as_ref()) {
                                    res.push(prop);
                                    idl.head.initializer = None;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }

    res
}

// Need this hack in an attempt to support the preprocessor since GLSL
// crate doesn't really support it.
pub fn process_macros(s: String, extract_props: bool) -> (String, HashMap<usize, String>, Vec<ShaderProp>) {
    let mut buff = String::new();
    let mut defs: HashMap<usize, String> = HashMap::new();
    let mut props: Vec<ShaderProp> = Vec::new(); //name, type, value, toggle

    push_sym();
    for (i, line) in s.lines().enumerate() {
        if line.trim_start().starts_with("#") {
            // Marker declaration
            buff.push_str(format!("float __LINE{}__;\n", i).as_str());

            // Parse def, extract prop
            let def = Preprocessor::parse(line.trim_start()).unwrap();
            let prop = match def {
                Preprocessor::Define(PreprocessorDefine::ObjectLike { ref ident, ref value }) if extract_props => {
                    Statement::parse(&value)
                        .ok()
                        .and_then(|x| if let Statement::Simple(s) = x { Some(s) } else { None })
                        .and_then(|x| {
                            if let SimpleStatement::Expression(Some(expr)) = *x {
                                Some(expr)
                            } else {
                                None
                            }
                        })
                        .or(Expr::parse(&value).ok())
                        .and_then(|x| extract_prop(ident.as_str(), &x))
                }
                _ => None,
            };

            // Handle prop if found, otherwise handle define normally
            if let Some(prop) = prop {
                defs.insert(i, format!("{} {};\n", prop.val_type, prop.name));
                props.push(prop);
            } else {
                let mut rep = String::new();
                show_preprocessor(&mut rep, &def);
                defs.insert(i, rep);
            }
        } else {
            buff.push_str(line);
            buff.push_str("\n");
        }
    }

    (buff, defs, props)
}

// Put back macros
pub fn replace_macros(s: String, defs: HashMap<usize, String>) -> String {
    let mut buff = String::new();

    for line in s.lines() {
        let trimmed = line.trim_start();

        if trimmed.starts_with("float __LINE") || trimmed.starts_with("static float __LINE") {
            let skip = if trimmed.starts_with("static") { 19 } else { 12 };
            let num: usize = trimmed
                .chars()
                .skip(skip)
                .take_while(|a| a.is_numeric())
                .collect::<String>()
                .parse()
                .unwrap();

            // TODO: Keep preceeding whitespace
            if let Some(rep) = defs.get(&num) {
                buff.push_str(rep.as_str().trim_start());
            }
        } else {
            buff.push_str(line);
            buff.push_str("\n");
        }
    }

    buff
}

// Raymarching handling stuff
enum PropDeclaration<'a> {
    Single(&'a mut SingleDeclaration),
    SingleNoType(&'a mut SingleDeclarationNoType),
}

fn find_param<'a>(fdef: &'a mut FunctionDefinition, lut: Vec<&str>) -> Option<PropDeclaration<'a>> {
    fn get_statement_decls<'a>(stmt: &'a mut Statement, lut: &Vec<&str>) -> Option<PropDeclaration<'a>> {
        match stmt {
            Statement::Simple(sstmt) => match **sstmt {
                SimpleStatement::Declaration(Declaration::InitDeclaratorList(ref mut decl)) => {
                    let rest = decl
                        .tail
                        .iter_mut()
                        .find(|x| lut.contains(&x.ident.ident.0.to_lowercase().as_str()))
                        .map(|x| PropDeclaration::SingleNoType(x));
                    if let Some(ref name) = decl.head.name {
                        if lut.contains(&name.0.to_lowercase().as_str()) {
                            Some(PropDeclaration::Single(&mut decl.head))
                        } else {
                            rest
                        }
                    } else {
                        rest
                    }
                }
                SimpleStatement::Selection(ref mut sel) => match sel.rest {
                    SelectionRestStatement::Statement(ref mut stmt) => get_statement_decls(stmt, lut),
                    SelectionRestStatement::Else(ref mut t, ref mut f) => {
                        get_statement_decls(t, lut).or(get_statement_decls(f, lut))
                    }
                },
                SimpleStatement::Switch(ref mut sw) => {
                    sw.body.iter_mut().find_map(|stmt| get_statement_decls(stmt, lut))
                }
                SimpleStatement::Iteration(ref mut it) => match it {
                    IterationStatement::While(_, stmt)
                    | IterationStatement::DoWhile(stmt, _)
                    | IterationStatement::For(_, _, stmt) => get_statement_decls(stmt, lut),
                },
                _ => None,
            },
            Statement::Compound(cstmt) => cstmt
                .statement_list
                .iter_mut()
                .find_map(|stmt| get_statement_decls(stmt, lut)),
        }
    };

    fdef.statement
        .statement_list
        .iter_mut()
        .find_map(|stmt| get_statement_decls(stmt, &lut))
}

fn remove_param(fdef: &mut FunctionDefinition, name: &str) {
    fn remove_param_stmt(stmt: &mut Statement, name: &str) -> bool {
        match stmt {
            Statement::Simple(decl) => match **decl {
                // Remove
                SimpleStatement::Expression(Some(Expr::Assignment(ref l, ref op, ref _r))) => {
                    if *op == AssignmentOp::Equal {
                        match **l {
                            Expr::Variable(ref id) => id.0.as_str() == name,
                            Expr::Dot(ref e, _) | Expr::Bracket(ref e, _) => match **e {
                                Expr::Variable(ref id) => id.0.as_str() == name,
                                _ => false,
                            },
                            _ => false,
                        }
                    } else {
                        false
                    }
                }
                SimpleStatement::Switch(ref mut sw) => {
                    remove_param_stmt_vec(&mut sw.body, name);
                    false
                }
                SimpleStatement::Selection(ref mut sel) => match sel.rest {
                    SelectionRestStatement::Statement(ref mut stmt) => {
                        if let Statement::Compound(ref mut cstmt) = **stmt {
                            remove_param_stmt_vec(&mut cstmt.statement_list, name);
                        }
                        remove_param_stmt(stmt, name)
                    }
                    SelectionRestStatement::Else(ref mut t, ref mut f) => {
                        if let Statement::Compound(ref mut cstmt) = **t {
                            remove_param_stmt_vec(&mut cstmt.statement_list, name);
                        }
                        if let Statement::Compound(ref mut cstmt) = **f {
                            remove_param_stmt_vec(&mut cstmt.statement_list, name);
                        }
                        if remove_param_stmt(t, name) {
                            *t = Box::new(Statement::parse("0xDEADBEEF;").unwrap())
                        } else if remove_param_stmt(f, name) {
                            *f = Box::new(Statement::parse("0xDEADBEEF;").unwrap())
                        }
                        false
                    }
                },
                SimpleStatement::Iteration(ref mut it) => match it {
                    IterationStatement::While(_, stmt)
                    | IterationStatement::DoWhile(stmt, _)
                    | IterationStatement::For(_, _, stmt) => {
                        if let Statement::Compound(ref mut cstmt) = **stmt {
                            remove_param_stmt_vec(&mut cstmt.statement_list, name);
                        }
                        remove_param_stmt(stmt, name)
                    }
                },
                _ => false,
            },
            Statement::Compound(cstmt) => {
                remove_param_stmt_vec(&mut cstmt.statement_list, name);
                false
            }
        }
    }

    fn remove_param_stmt_vec(stmts: &mut Vec<Statement>, name: &str) {
        stmts.drain_filter(|x| remove_param_stmt(x, name)).for_each(|_x| ());
    }

    remove_param_stmt_vec(&mut fdef.statement.statement_list, name);
}

pub fn handle_raymarch_param(fdef: &mut FunctionDefinition, lut: Vec<&str>, rep: &str) {
    let name = {
        let param = find_param(fdef, lut);

        if let Some(PropDeclaration::Single(p)) = param {
            let was_initalized = p.initializer.is_some();
            p.initializer = Some(Initializer::parse(rep).unwrap());
            p.name.clone().map(|x| x.0.clone()).map(|x| (x, was_initalized))
        } else if let Some(PropDeclaration::SingleNoType(p)) = param {
            let was_initalized = p.initializer.is_some();
            p.initializer = Some(Initializer::parse(rep).unwrap());
            Some((p.ident.ident.0.clone(), was_initalized))
        } else {
            None
        }
    };

    if let Some((name, was_initialized)) = name {
        if was_initialized {
            remove_param(fdef, name.as_str())
        }
    }
}
