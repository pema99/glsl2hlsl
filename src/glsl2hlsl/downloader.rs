use serde::Deserialize;

use crate::transpile;
use crate::add_buffer_num;
use crate::reset_buffer_num;
use crate::BUFFER_NUM;

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
    name: String,
    r#type: String,
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

#[derive(Clone)]
pub enum ShaderType {
    MainImage(String, Option<String>, Vec<(usize, String)>),
    Buffer(usize, String),
    Common(String)
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

// fn get_shader_file(shader: &Shader, extract_props: bool, raymarch: bool) -> ShaderFile {
//     ShaderFile {
//         name: format!("{}.shader", shader.info.name.clone()),
//         contents: transpile(shader.renderpass[0].code.clone(), extract_props, raymarch),
//     }
// }


pub fn get_common(shader: &Shader) -> Option<String> {
    shader.renderpass.iter()
    .find(|ri| ri.r#type == "common")
    .map(|rp| transpile(rp.code.clone(), false, false, ShaderType::Common(format!("{}{}",shader.info.name.clone(), rp.name.clone()))))
}

pub fn get_buffers(shader: &Shader) -> Vec<(usize, String)> {
    shader.renderpass.iter()
    .filter(|ri| ri.r#type == "buffer")
    .map(|rp| {
            let mm = (unsafe{BUFFER_NUM},transpile(rp.code.clone(), false, false, ShaderType::Buffer(unsafe {BUFFER_NUM}, rp.name.clone())));
            add_buffer_num();
            mm
        }
    )
    .collect()
}

pub fn get_shader_file(shader: &Shader, extract_props: bool, raymarch: bool, common: Option<String>, buffers: Vec<(usize, String)>) -> Vec<ShaderFile> {
    shader.renderpass.iter()
    .filter(|ri| ri.r#type == "image")
    .map(|rp| ShaderFile {
        name: format!("{}.shader", shader.info.name.clone()),
        contents: transpile(rp.code.clone(), extract_props, raymarch, ShaderType::MainImage(format!("{}{}",shader.info.name.clone(), rp.name.clone()),common.clone(), buffers.clone()))
    })
    .collect()
}

//Need to verify that this works first.
pub fn get_image_files(shader: &Shader) -> Vec<ShaderFile> {
    //TODO: Set up cors proxy for images.
    shader.renderpass.iter().flat_map(|rp| {
        rp.inputs.iter()
        .filter(|inp| inp.ctype == "texture")
        .map(|inp| ShaderFile {
            name: format!("iChannel{} {}.{}",  rp.name, inp.channel, inp.src.chars().skip_while(|&c| c != '.').collect::<String>()),
            contents: format!("https://www.shadertoy.com{}", inp.src),
        })
        .collect::<Vec<_>>()
    })
    .collect()     
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

pub fn get_mat_meta_file(name: &String, guid: &String) -> ShaderFile {
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
        name: format!("{}.mat.meta", name.clone()),
        contents: content,
    }
}



fn get_material_file(name: &String, shader_guid: &String, buffers: Vec<(usize, String)> ) -> ShaderFile {
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
    - _MainTex_0:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _SecondTex_0:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _ThirdTex_0:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _FourthTex_0:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    {}
    m_Floats:
    - _DstBlend: 0
    - _Mode: 0
    - _SrcBlend: 1
    - _ZWrite: 1
    - _GammaCorrect: 1
    m_Colors:
    - _Mouse: {{r: 0.5, g: 0.5, b: 0.5, a: 0.5}}
",
        shader_guid,
        buffers.iter().map(
            |(i,_)| format!("- _MainTex_{}:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _SecondTex_{}:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _ThirdTex_{}:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}
    - _FourthTex_{}:
        m_Texture: {{fileID: 0}}
        m_Scale: {{x: 1, y: 1}}
        m_Offset: {{x: 0, y: 0}}", i, i, i, i
            )
        )
        .collect::<Vec<_>>()
        .concat()
    );

    ShaderFile {
        name: format!("{}.mat", name),
        contents: content,
    }
}


fn get_crt_file(shader_name: &String, shader_guid: &String, id: &usize) -> ShaderFile {
    let content = format!(
"%YAML 1.1  
%TAG !u! tag:unity3d.com,2011:
--- !u!86 &8600000
CustomRenderTexture:
  m_ObjectHideFlags: 0
  m_CorrespondingSourceObject: {{fileID: 0}}
  m_PrefabInstance: {{fileID: 0}}
  m_PrefabAsset: {{fileID: 0}}
  m_Name: Buffer {} CRT
  m_ImageContentsHash:
    serializedVersion: 2
    Hash: 00000000000000000000000000000000
  m_ForcedFallbackFormat: 4
  m_DownscaleFallback: 0
  serializedVersion: 3
  m_Width: 800
  m_Height: 450
  m_AntiAliasing: 1
  m_MipCount: -1
  m_DepthFormat: 0
  m_ColorFormat: 52
  m_MipMap: 0
  m_GenerateMips: 1
  m_SRGB: 0
  m_UseDynamicScale: 0
  m_BindMS: 0
  m_EnableCompatibleFormat: 1
  m_TextureSettings:
    serializedVersion: 2
    m_FilterMode: 0
    m_Aniso: 1
    m_MipBias: 0
    m_WrapU: 1
    m_WrapV: 1
    m_WrapW: 1
  m_Dimension: 2
  m_VolumeDepth: 1
  m_Material: {{fileID: 2100000, guid: {}, type: 2}}
  m_InitSource: 0
  m_InitMaterial: {{fileID: 0}}
  m_InitColor: {{r: 1, g: 1, b: 1, a: 1}}
  m_InitTexture: {{fileID: 0}}
  m_UpdateMode: 1
  m_InitializationMode: 2
  m_UpdateZoneSpace: 0
  m_CurrentUpdateZoneSpace: 0
  m_UpdateZones:
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  - updateZoneCenter: {{x: 0.5, y: 0.5, z: 0.5}}
    updateZoneSize: {{x: 1, y: 1, z: 1}}
    rotation: 0
    passIndex: -1
    needSwap: 1
  m_UpdatePeriod: 0
  m_ShaderPass: {}
  m_CubemapFaceMask: 4294967295
  m_DoubleBuffered: 1
  m_WrapUpdateZones: 0
",      id,  
        shader_guid,
        id
    );

    ShaderFile {
        name: format!("buffer{}_{}.asset", shader_name, id),
        contents: content,
    }
}

pub fn test_file(shader: &Shader, extract_props: bool, raymarch: bool) -> Vec<(usize, String)> {
    get_buffers(shader)

}


pub fn get_files(shader: &Shader, extract_props: bool, raymarch: bool) -> Vec<ShaderFile> {
    let shader_guid = generate_guid(shader);
    reset_buffer_num();
    add_buffer_num();
    let common = get_common(shader);
    let buffers = get_buffers(shader);
    reset_buffer_num();
    let mut shader_files = get_shader_file(shader, extract_props, raymarch, common, buffers.clone());
    let shader_meta_file = get_shader_meta_file(shader, &shader_guid.clone());
    let mat_file = get_material_file(&shader.info.name.clone(), &shader_guid.clone(), buffers.clone());
    let mut other_mat_files : Vec<ShaderFile> = buffers.iter()
                            .map(|(n, _)| get_material_file(&format!("{} Buffer{}", shader.info.name.clone(),n),&shader_guid.clone(), buffers.clone()))
                            .collect();
    let mut crt_files : Vec<ShaderFile> = buffers.iter()
                            .map(|(n, _)| get_crt_file(&shader.info.name.clone(),&shader_guid.clone(), n))
                            .collect();
    let mut other_files = vec![shader_meta_file, mat_file];
    shader_files.append(&mut other_files);
    shader_files.append(&mut crt_files);
    shader_files.append(&mut other_mat_files);
    shader_files
}
