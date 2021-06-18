use serde::Deserialize;

use crate::transpile;

#[derive(Deserialize, Debug)]
#[allow(non_snake_case)]
struct ShaderContainer {
    Shader: Shader,
}

#[derive(Deserialize, Debug)]
pub struct Shader {
    info: ShaderInfo,
    renderpass: Vec<ShaderRenderPass>,
}

#[derive(Deserialize, Debug)]
struct ShaderInfo {
    name: String,
}

#[derive(Deserialize, Debug)]
struct ShaderRenderPass {
    inputs: Vec<ShaderInput>,
    code: String,
}

#[derive(Deserialize, Debug)]
struct ShaderInput {
    src: String,
    ctype: String,
    channel: usize,
    sampler: ShaderSampler,
}

#[derive(Deserialize, Debug)]
struct ShaderSampler {
    filter: String,
    wrap: String,
    vflip: String,
    srgb: String,
}

pub struct ShaderFile {
    pub name: String,
    pub contents: String,
}

pub fn download_shader(id: &str) -> Result<Shader, ureq::Error> {
    let url = format!("https://www.shadertoy.com/api/v1/shaders/{}?key=NtHtMm", id);
    let shader = ureq::get(&url).call()?.into_json::<ShaderContainer>()?.Shader;
    Ok(shader)
}

pub fn make_shader(json: &str) -> Result<Shader, serde_json::Error> {
    Ok(serde_json::from_str::<ShaderContainer>(json)?.Shader)
}

fn generate_guid(shader: &Shader) -> String {
    let hash = md5::compute(&shader.info.name);
    let mut res = String::with_capacity(32);
    for byte in &hash.0 {
        res.push_str(format!("{:02X}", byte).as_str());
    }
    res
}

fn get_shader_file(shader: &Shader, extract_props: bool, raymarch: bool) -> ShaderFile {
    ShaderFile {
        name: format!("{}.shader", shader.info.name.clone()),
        contents: transpile(shader.renderpass[0].code.clone(), extract_props, raymarch),
    }
}

pub fn get_shader_meta_file(shader: &Shader, guid: &String) -> ShaderFile {
    let content = format!(
        "fileFormatVersion: 2
guid: {}
ShaderImporter:
    externalObjects: {{}}
    defaultTextures: []
    nonModifiableTextures: []
    userData: 
    assetBundleName: 
    assetBundleVariant: 
    ",
        guid
    );

    ShaderFile {
        name: format!("{}.shader.meta", shader.info.name.clone()),
        contents: content,
    }
}

fn get_image_files(shader: &Shader) -> Vec<ShaderFile> {
    shader.renderpass[0]
        .inputs
        .iter()
        .filter(|inp| inp.ctype == "texture")
        .map(|inp| ShaderFile {
            name: format!("iChannel{}.png", inp.channel),
            contents: format!("shadertoy.com{}", inp.src),
        })
        .collect()
}

fn get_material_file(shader: &Shader, shader_guid: &String) -> ShaderFile {
    let content = format!(
        "%YAML 1.1
%TAG !u! tag:unity3d.com,2011:
--- !u!21 &2100000
Material:
  serializedVersion: 6
  m_ObjectHideFlags: 0
  m_CorrespondingSourceObject: {{fileID: 0}}
  m_PrefabInstance: {{fileID: 0}}
  m_PrefabAsset: {{fileID: 0}}
  m_Name: TestMaterial
  m_Shader: {{fileID: 4800000, guid: {}, type: 3}}
  m_ShaderKeywords: 
  m_LightmapFlags: 4
  m_EnableInstancingVariants: 0
  m_DoubleSidedGI: 0
  m_CustomRenderQueue: -1
  stringTagMap: {{}}
  disabledShaderPasses: []
  m_SavedProperties:
    serializedVersion: 0
    m_TexEnvs:
    - _MainTex:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _SecondTex:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _ThirdTex:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _FourthTex:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    m_Floats:
    - _DstBlend: 0
    - _Mode: 0
    - _SrcBlend: 1
    - _ZWrite: 1
    - _GammaCorrect: 1
    m_Colors:
    - _Mouse: {{r: 0.5, g: 0.5, b: 0.5, a: 0.5}}
",
        shader_guid
    );

    ShaderFile {
        name: format!("{}.mat", shader.info.name.clone()),
        contents: content,
    }
}

pub fn get_files(shader: &Shader, extract_props: bool, raymarch: bool) -> Vec<ShaderFile> {
    let shader_guid = generate_guid(shader);
    let shader_file = get_shader_file(shader, extract_props, raymarch);
    let shader_meta_file = get_shader_meta_file(shader, &shader_guid);

    let mat_file = get_material_file(shader, &shader_guid);

    vec![shader_file, shader_meta_file, mat_file]
}
