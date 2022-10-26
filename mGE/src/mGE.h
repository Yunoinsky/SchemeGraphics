#ifndef _MGE_H_
#define _MGE_H_
#define PATH(FILE) "../../../../src/"##FILE
#include <GLFW/glfw3.h>
#include <glad/glad.h>
#include <stdio.h>
#include <stdlib.h>
#include <stb_image.h>

#ifdef WIN32
#define EXPORT extern __declspec (dllexport)
#else
#define EXPORT extern
#endif

GLFWwindow * window;
unsigned int VAO, TEX, SDPRG;

EXPORT GLFWwindow * engineInit(int w, int h, const char* title);
EXPORT unsigned int engineProcess();
EXPORT void engineTerm();

EXPORT unsigned int processInput();

void framebuffer_size_callback(GLFWwindow* window, int width, int height);

EXPORT unsigned int backgroundInit(int w, int h, const char* vertfn, const char* fragfn, unsigned int use_filename);
EXPORT void backgroundDraw(unsigned char* data, int width, int heigth);

EXPORT unsigned int loadShader(const char* filename, enum shaderType);
EXPORT unsigned int linkShaders(unsigned int vert, unsigned int frag);
EXPORT unsigned int loadShaderProgram(const char *vertfn, const char *fragfn, unsigned int use_filename);
EXPORT unsigned int test(unsigned int input);


#endif //_MGE_H_
