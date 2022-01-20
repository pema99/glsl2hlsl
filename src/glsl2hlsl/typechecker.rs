use glsl::syntax::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum TypeKind {
    Matrix(usize, usize),
    Vector(usize),
    Scalar,
    Struct(String),
}

// Symbol table
static mut SYM_TABLE: Vec<HashMap<String, TypeKind>> = Vec::new();
pub fn add_sym(name: String, ty: TypeKind) {
    unsafe {
        SYM_TABLE.last_mut().unwrap().insert(name, ty);
    }
}
pub fn push_sym() {
    unsafe {
        SYM_TABLE.push(HashMap::new());
    }
}
pub fn pop_sym() {
    unsafe {
        SYM_TABLE.pop();
    }
}
pub fn clear_sym() {
    unsafe {
        SYM_TABLE.clear();
    }
}
pub fn lookup_sym(name: &str) -> Option<TypeKind> {
    unsafe {
        SYM_TABLE
            .last()
            .and_then(|x| x.get(name))
            .or(SYM_TABLE.first().and_then(|x| x.get(name)))
            .map(|x| x.clone())
    }
}

// Expression typing
pub fn escape_invalid_glsl_id(s: &str) -> &str {
    match s {
        "line" => "line_",
        "lineadj" => "lineadj_",
        "linear" => "linear_",
        "pass" => "pass_",
        "Buffer" => "Buffer_",
        "ByteAddressBuffer" => "ByteAddressBuffer_",
        "BlendState" => "BlendState_",
        "AppendStructuredBuffer" => "AppendStructuredBuffer_",
        "asm" => "asm_",
        "asm_fragment" => "asm_fragment_",
        "cbuffer" => "cbuffer_",
        "centroid" => "centroid_",
        "class" => "class_",
        "column_major" => "column_major_",
        "compile" => "compile_",
        "compile_fragment" => "compile_fragment_",
        "CompileShader" => "CompileShader_",
        "ComputeShader" => "ComputeShader_",
        "ConsumeStructuredBuffer" => "ConsumeStructuredBuffer_",
        "DepthStencilState" => "DepthStencilState_",
        "DepthStencilView" => "DepthStencilView_",
        "DomainShader" => "DomainShader_",
        "dword" => "dword_",
        "export" => "export_",
        "extern" => "extern_",
        "fxgroup" => "fxgroup_",
        "GeometryShader" => "GeometryShader_",
        "groupshared" => "groupshared_",
        "HullShader" => "HullShader_",
        "inline" => "inline_",
        "InputPatch" => "InputPatch_",
        "interface" => "interface_",
        "LineStream" => "LineStream_",
        "matrix" => "matrix_",
        "min16float" => "min16float_",
        "min10float" => "min10float_",
        "min16int" => "min16int_",
        "min12int" => "min12int_",
        "min16uint" => "min16uint_",
        "namespace" => "namespace_",
        "nointerpolation" => "nointerpolation_",
        "noperspective" => "noperspective_",
        "NULL" => "NULL_",
        "OutputPatch" => "OutputPatch_",
        "packoffset" => "packoffset_",
        "pixelfragment" => "pixelfragment_",
        "PixelShader" => "PixelShader_",
        "point" => "point_",
        "PointStream" => "PointStream_",
        "precise" => "precise_",
        "RasterizerState" => "RasterizerState_",
        "RenderTargetView" => "RenderTargetView_",
        "register" => "register_",
        "row_major" => "row_major_",
        "RWBuffer" => "RWBuffer_",
        "RWByteAddressBuffer" => "RWByteAddressBuffer_",
        "RWStructuredBuffer" => "RWStructuredBuffer_",
        "RWTexture1D" => "RWTexture1D_",
        "RWTexture1DArray" => "RWTexture1DArray_",
        "RWTexture2D" => "RWTexture2D_",
        "RWTexture2DArray" => "RWTexture2DArray_",
        "RWTexture3D" => "RWTexture3D_",
        "sample" => "sample_",
        "sampler" => "sampler_",
        "SamplerState" => "SamplerState_",
        "SamplerComparisonState" => "SamplerComparisonState_",
        "shared" => "shared_",
        "snorm" => "snorm_",
        "stateblock" => "stateblock_",
        "stateblock_state" => "stateblock_state_",
        "string" => "string_",
        "StructuredBuffer" => "StructuredBuffer_",
        "tbuffer" => "tbuffer_",
        "technique" => "technique_",
        "technique10" => "technique10_",
        "technique11" => "technique11_",
        "Texture1D" => "Texture1D_",
        "Texture1DArray" => "Texture1DArray_",
        "Texture2D" => "Texture2D_",
        "Texture2DArray" => "Texture2DArray_",
        "Texture2DMS" => "Texture2DMS_",
        "Texture2DMSArray" => "Texture2DMSArray_",
        "Texture3D" => "Texture3D_",
        "TextureCube" => "TextureCube_",
        "TextureCubeArray" => "TextureCubeArray_",
        "typedef" => "typedef_",
        "triangle" => "triangle_",
        "triangleadj" => "triangleadj_",
        "TriangleStream" => "TriangleStream_",
        "unorm" => "unorm_",
        "unsigned" => "unsigned_",
        "vector" => "vector_",
        "vertexfragment" => "vertexfragment_",
        "VertexShader" => "VertexShader_",
        "volatile" => "volatile_",

        a => a,
    }
}

pub fn translate_glsl_id(s: &str) -> &str {
    match s {
        // Vector types
        "bvec2" => "bool2",
        "bvec3" => "bool3",
        "bvec4" => "bool4",
        "ivec2" => "int2",
        "ivec3" => "int3",
        "ivec4" => "int4",
        "uvec2" => "uint2",
        "uvec3" => "uint3",
        "uvec4" => "uint4",
        "dvec2" => "double2",
        "dvec3" => "double3",
        "dvec4" => "double4",
        "vec2" => "float2",
        "vec3" => "float3",
        "vec4" => "float4",

        //Matrix types
        "mat2" => "float2x2",
        "mat3" => "float3x3",
        "mat4" => "float4x4",
        "mat2x2" => "float2x2",
        "mat2x3" => "float2x3",
        "mat2x4" => "float2x4",
        "mat3x2" => "float3x2",
        "mat3x3" => "float3x3",
        "mat3x4" => "float3x4",
        "mat4x2" => "float4x2",
        "mat4x3" => "float4x3",
        "mat4x4" => "float4x4",

        // Builtins
        "mix" => "lerp",
        "fract" => "frac",
        "texture" => "tex2D",
        "tex2DLod" => "tex2Dlod",
        "textureGrad" => "tex2Dgrad",
        "refrac" => "refract",
        "mod" => "glsl_mod",
        "atan" => "atan2",
        "floatBitsToInt" => "asint",
        "intBitsToFloat" | "uintBitsToFloat" => "asfloat",
        "dFdx" | "dFdxCoarse" => "ddx",
        "dFdy" | "dFdyCoarse" => "ddy",
        "dFdxFine" => "ddx_fine",
        "dFdyFine" => "ddy_fine",
        "inversesqrt" => "rsqrt",

        a => escape_invalid_glsl_id(a),
    }
}

pub fn get_function_ret_type<'a>(s: &str, args: Vec<Option<TypeKind>>) -> Option<TypeKind> {
    match translate_glsl_id(s) {
        // Vector types
        "bool2" => Some(TypeKind::Vector(2)),
        "bool3" => Some(TypeKind::Vector(3)),
        "bool4" => Some(TypeKind::Vector(4)),
        "int2" => Some(TypeKind::Vector(2)),
        "int3" => Some(TypeKind::Vector(3)),
        "int4" => Some(TypeKind::Vector(4)),
        "uint2" => Some(TypeKind::Vector(2)),
        "uint3" => Some(TypeKind::Vector(3)),
        "uint4" => Some(TypeKind::Vector(4)),
        "double2" => Some(TypeKind::Vector(2)),
        "double3" => Some(TypeKind::Vector(3)),
        "double4" => Some(TypeKind::Vector(4)),
        "float2" => Some(TypeKind::Vector(2)),
        "float3" => Some(TypeKind::Vector(3)),
        "float4" => Some(TypeKind::Vector(4)),

        //Matrix types
        "float2x2" => Some(TypeKind::Matrix(2, 2)),
        "float3x3" => Some(TypeKind::Matrix(3, 3)),
        "float4x4" => Some(TypeKind::Matrix(4, 4)),
        "float2x3" => Some(TypeKind::Matrix(2, 3)),
        "float2x4" => Some(TypeKind::Matrix(2, 4)),
        "float3x2" => Some(TypeKind::Matrix(3, 2)),
        "float3x4" => Some(TypeKind::Matrix(3, 4)),
        "float4x2" => Some(TypeKind::Matrix(4, 2)),
        "float4x3" => Some(TypeKind::Matrix(4, 3)),

        // Builtins
        "lerp" => args[0].clone(),
        "frac" => args[0].clone(),
        "tex2D" => Some(TypeKind::Vector(4)),
        "tex2Dlod" => Some(TypeKind::Vector(4)),
        "refract" => args[0].clone(),
        "glsl_mod" => args[0].clone(),
        "atan2" => args[0].clone(),
        "asint" => args[0].clone(),
        "asfloat" => Some(TypeKind::Scalar),
        "ddx" => args[0].clone(),
        "ddy" => args[0].clone(),
        "ddx_fine" => args[0].clone(),
        "ddy_fine" => args[0].clone(),
        "rsqrt" => args[0].clone(),
        "transpose" => match args[0] {
            Some(TypeKind::Matrix(m, n)) => Some(TypeKind::Matrix(n, m)),
            _ => lookup_sym(s)
        }

        _ => lookup_sym(s),
    }
}

pub fn get_expr_type(e: &Expr) -> Option<TypeKind> {
    match e {
        Expr::Variable(ref i) => lookup_sym(i.as_str()),
        Expr::IntConst(ref _x) => Some(TypeKind::Scalar),
        Expr::UIntConst(ref _x) => Some(TypeKind::Scalar),
        Expr::BoolConst(ref _x) => Some(TypeKind::Scalar),
        Expr::FloatConst(ref _x) => Some(TypeKind::Scalar),
        Expr::DoubleConst(ref _x) => Some(TypeKind::Scalar),
        Expr::Unary(ref _op, ref e) => get_expr_type(e),
        Expr::Binary(ref op, ref l, ref r) => {
            let (l, r) = (get_expr_type(l), get_expr_type(r));
            match (l.clone(), op, r.clone()) {
                (Some(_), _, Some(TypeKind::Scalar)) => l, // anything op scalar = scalar
                (Some(TypeKind::Scalar), _, Some(_)) => r, // scalar op anything = scalar
                (Some(TypeKind::Vector(_)), _, Some(TypeKind::Vector(_))) => l, // componentwise vector
                (Some(TypeKind::Matrix(_, _)), BinaryOp::Mult, Some(TypeKind::Matrix(_, _))) => Some(TypeKind::Scalar), // matrix multiplication
                (Some(TypeKind::Matrix(_, _)), _, Some(TypeKind::Matrix(_, _))) => l, // componentwise matrix
                (Some(TypeKind::Vector(_)), BinaryOp::Mult, Some(TypeKind::Matrix(_, _))) => l, // vector matrix mul
                (Some(TypeKind::Matrix(_, _)), BinaryOp::Mult, Some(TypeKind::Vector(_))) => r, // matrix vector mul
                _ => None,
            }
        }
        Expr::Ternary(ref _c, ref s, ref e) => {
            let (l, r) = (get_expr_type(s), get_expr_type(e));
            match (l.clone(), r.clone()) {
                (_, Some(TypeKind::Scalar)) => l,
                (Some(TypeKind::Scalar), _) => r,
                _ => l,
            }
        }
        Expr::Assignment(ref _v, ref _op, ref e) => get_expr_type(e),
        Expr::Bracket(ref _e, ref _a) => None, // TODO: array ignored for now
        Expr::FunCall(FunIdentifier::Identifier(ref id), ref args) => {
            get_function_ret_type(id.0.as_str(), args.iter().map(get_expr_type).collect())
        } // TODO: this can't handle overloads
        Expr::Dot(ref e, ref i) => {
            match get_expr_type(e) {
                Some(TypeKind::Scalar) | Some(TypeKind::Vector(_)) => {
                    // swizzling
                    if i.0.len() == 1 {
                        return Some(TypeKind::Scalar);
                    } else {
                        return Some(TypeKind::Vector(i.0.len()));
                    }
                }
                Some(TypeKind::Matrix(_, _)) => Some(TypeKind::Scalar), // matrix access
                Some(TypeKind::Struct(name)) => lookup_sym(format!("{}.{}", name, i.0).as_str()),
                a => a,
            }
        }
        Expr::PostInc(ref e) => get_expr_type(e),
        Expr::PostDec(ref e) => get_expr_type(e),
        Expr::Comma(ref _a, ref b) => get_expr_type(b),
        _ => None,
    }
}

// Utility
pub fn is_matrix(e: &Expr) -> bool {
    match get_expr_type(e) {
        Some(TypeKind::Matrix(_, _)) => true,
        _ => false,
    }
}

pub fn is_scalar(e: &Expr) -> bool {
    match get_expr_type(e) {
        Some(TypeKind::Scalar) => true,
        _ => false,
    }
}

fn is_struct(e: &Expr) -> bool {
    match get_expr_type(e) {
        Some(TypeKind::Struct(_)) => true,
        _ => false,
    }
}

pub fn is_vector(e: &Expr) -> bool {
    match get_expr_type(e) {
        Some(TypeKind::Vector(_)) => true,
        _ => false,
    }
}

pub fn is_constructor<'a>(s: &str) -> bool {
    match translate_glsl_id(s) {
        // Vector types
        "bool2" => true,
        "bool3" => true,
        "bool4" => true,
        "int2" => true,
        "int3" => true,
        "int4" => true,
        "uint2" => true,
        "uint3" => true,
        "uint4" => true,
        "double2" => true,
        "double3" => true,
        "double4" => true,
        "float2" => true,
        "float3" => true,
        "float4" => true,

        //Matrix types
        "float2x2" => true,
        "float3x3" => true,
        "float4x4" => true,
        "float2x3" => true,
        "float2x4" => true,
        "float3x2" => true,
        "float3x4" => true,
        "float4x2" => true,
        "float4x3" => true,

        _ => false
    }
}

pub fn typespec_to_typekind(ty: &TypeSpecifierNonArray) -> Option<TypeKind> {
    match ty {
        TypeSpecifierNonArray::Bool
        | TypeSpecifierNonArray::Int
        | TypeSpecifierNonArray::UInt
        | TypeSpecifierNonArray::Float
        | TypeSpecifierNonArray::Double => Some(TypeKind::Scalar),
        TypeSpecifierNonArray::Vec2
        | TypeSpecifierNonArray::DVec2
        | TypeSpecifierNonArray::BVec2
        | TypeSpecifierNonArray::IVec2
        | TypeSpecifierNonArray::UVec2 => Some(TypeKind::Vector(2)),
        TypeSpecifierNonArray::Vec3
        | TypeSpecifierNonArray::DVec3
        | TypeSpecifierNonArray::BVec3
        | TypeSpecifierNonArray::IVec3
        | TypeSpecifierNonArray::UVec3 => Some(TypeKind::Vector(3)),
        TypeSpecifierNonArray::Vec4
        | TypeSpecifierNonArray::DVec4
        | TypeSpecifierNonArray::BVec4
        | TypeSpecifierNonArray::IVec4
        | TypeSpecifierNonArray::UVec4 => Some(TypeKind::Vector(4)),
        TypeSpecifierNonArray::Mat2 | TypeSpecifierNonArray::DMat2 => Some(TypeKind::Matrix(2, 2)),
        TypeSpecifierNonArray::Mat3 | TypeSpecifierNonArray::DMat3 => Some(TypeKind::Matrix(3, 3)),
        TypeSpecifierNonArray::Mat4 | TypeSpecifierNonArray::DMat4 => Some(TypeKind::Matrix(4, 4)),
        TypeSpecifierNonArray::Mat23 | TypeSpecifierNonArray::DMat23 => Some(TypeKind::Matrix(2, 3)),
        TypeSpecifierNonArray::Mat24 | TypeSpecifierNonArray::DMat24 => Some(TypeKind::Matrix(2, 4)),
        TypeSpecifierNonArray::Mat32 | TypeSpecifierNonArray::DMat32 => Some(TypeKind::Matrix(3, 2)),
        TypeSpecifierNonArray::Mat34 | TypeSpecifierNonArray::DMat34 => Some(TypeKind::Matrix(3, 4)),
        TypeSpecifierNonArray::Mat42 | TypeSpecifierNonArray::DMat42 => Some(TypeKind::Matrix(4, 2)),
        TypeSpecifierNonArray::Mat43 | TypeSpecifierNonArray::DMat43 => Some(TypeKind::Matrix(4, 3)),
        TypeSpecifierNonArray::Struct(ref s) => {
            if let Some(id) = &s.name {
                Some(TypeKind::Struct(id.0.clone()))
            } else {
                None
            }
        }
        TypeSpecifierNonArray::TypeName(ref tn) => Some(TypeKind::Struct(tn.0.clone())),
        _ => None,
    }
}
