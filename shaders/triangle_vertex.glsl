#version 330 core

layout(location = 0) in vec3 xyz;

void main() {
    gl_Position.xyz = xyz;
    gl_Position.w = 1.0;
}