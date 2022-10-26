#include "mGE.h"

GLFWwindow * engineInit(int w, int h, const char* title) {
  glfwInit();
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  window = glfwCreateWindow(w, h, title, NULL, NULL);
  
  if (window == NULL) {
    printf("Failed to create GLFW window");
    glfwTerminate();
    exit(-1);
  }
  glfwMakeContextCurrent(window);
  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
    printf("Failed to initialize GLAD");
    exit(-1);
  }
  glViewport(0, 0, w, h);
  glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
  // stbi_set_flip_vertically_on_load(1);
  // glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
  // glfwSetCursorPosCallback(window, mouse_callback);
  return window;
}

unsigned int engineProcess(){
  return glfwWindowShouldClose(window);
}

void engineTerm() {
  glfwTerminate();  
}

unsigned int processInput() {
  if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) {
    glfwSetWindowShouldClose(window, 1);
  }
  return 0;
}

void framebuffer_size_callback(GLFWwindow* window, int width, int height) {
  glViewport(0, 0, width,height);
}
unsigned int backgroundInit(int w, int h, const char* vertfn, const char* fragfn, unsigned int use_filename) {
  float vertices[] = {
    0.9f, 0.9f, 1.0f, 1.0f,
    0.9f, -0.9f, 1.0f, 0.0f,
    -0.9f, -0.9f, 0.0f, 0.0f,
    -0.9f, 0.9f,  0.0f, 1.0f,
  };
  unsigned int elements[] = {
    0, 1, 2,
    0, 2, 3
  };
  SDPRG = loadShaderProgram(vertfn, fragfn, use_filename);
  unsigned int VBO, EBO;
  glGenVertexArrays(1, &VAO);
  glGenBuffers(1, &VBO);
  glGenBuffers(1, &EBO);
  glBindVertexArray(VAO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(elements), elements, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), 0);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
  glEnableVertexAttribArray(1);
  
  glGenTextures(1, &TEX);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindVertexArray(0);
  return VAO;
}

void backgroundDraw(unsigned char* data, int width, int height) {
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
  glBindTexture(GL_TEXTURE_2D, TEX);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
  glGenerateMipmap(GL_TEXTURE_2D);
  
  glUseProgram(SDPRG);
  glBindVertexArray(VAO);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
  glBindVertexArray(0);
  glfwSwapBuffers(window);
  glfwPollEvents();
}

unsigned int loadShader(const char* filename, unsigned int shaderType, unsigned int use_filename) {
  unsigned int shader = glCreateShader(shaderType);
  char* string;
  if (use_filename != 0) {
    FILE *f = fopen(filename, "rb");
    if (f == NULL) {
      printf("ERROR::SHADER::INVALID FILE::");
      printf(filename);
      exit(-1);
    }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    string = malloc(fsize+1);
    fread(string, fsize, 1, f);
    fclose(f);
    string[fsize]=0;
  } else {
    string = filename;
  }
  glShaderSource(shader, 1, &string, NULL);
  glCompileShader(shader);
  if (use_filename != 0) free(string);
  int success;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
  if (!success) {
    char infoLog[512];
    glGetShaderInfoLog(shader, 512, NULL, infoLog);
    printf("ERROR::SHADER::COMPILATION_FAILED::");
    printf(filename);
    printf("\n");
    printf(infoLog);
    exit(-1);
  }
  return shader;
}
unsigned int linkShaders(unsigned int vert, unsigned int frag) {
  unsigned int shaderProgram = glCreateProgram();
  glAttachShader(shaderProgram, vert);
  glAttachShader(shaderProgram, frag);
  glLinkProgram(shaderProgram);
  int success;
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
  if (!success) {
    char infoLog[512];
    glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
    printf("ERROR::SHADER::LINK_FAILED");
    exit(-1);
  }
  return shaderProgram;
}
unsigned int loadShaderProgram(const char *vertfn, const char *fragfn, unsigned int use_filename) {
  unsigned int vertShader = loadShader(vertfn, GL_VERTEX_SHADER, use_filename);
  unsigned int fragShader = loadShader(fragfn, GL_FRAGMENT_SHADER, use_filename);
  unsigned int shaderProgram = linkShaders(vertShader, fragShader);
  glDeleteShader(vertShader);
  glDeleteShader(fragShader);
  return shaderProgram;
}

unsigned int test(unsigned int input) {
  printf("Input is: %u\n", input);
  printf("VAO is: %u\n", VAO);
  return VAO;
}
