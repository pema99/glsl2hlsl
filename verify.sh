#!/usr/bin/env bash
# Usage: ./verify.sh <input.glsl>
# Runs glsl2hlsl on the GLSL, prepends the debugger's cbuffer preamble,
# then compiles via slangc to validate.
set -e
GLSL="$1"
if [ -z "$GLSL" ]; then echo "Usage: $0 <input.glsl>"; exit 2; fi

ROOT="$(cd "$(dirname "$0")" && pwd)"
TRANSPILER="$ROOT/target/release/main.exe"
SLANGC="C:/Users/Pema Malling/Downloads/slang-2026.8-windows-x86_64/bin/slangc.exe"

OUT_HLSL="${GLSL%.glsl}.transpiled.hlsl"
OUT_FULL="${GLSL%.glsl}.full.hlsl"

"$TRANSPILER" "$GLSL"
mv "${GLSL}.shader" "$OUT_HLSL"

cat > "$OUT_FULL" <<'EOF'
cbuffer DebuggerGlobals : register(b0) {
    float2 _WarpSize;
    float2 _Resolution;
    float _Time;
    float4x4 _View;
    float4x4 _Projection;
    float4 _Mouse;
};

EOF
cat "$OUT_HLSL" >> "$OUT_FULL"

echo "=== compiling $OUT_FULL ==="
"$SLANGC" "$OUT_FULL" -entry frag -stage fragment -target hlsl -profile sm_6_0 -o /dev/null
echo "OK"
