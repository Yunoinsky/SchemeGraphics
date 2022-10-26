#include "mGE.h"
#include <string.h>

int main(int argc, char** argv) {
  engineInit(800, 600, "Hello!");
  backgroundInit(800, 600, PATH("Vert.glsl"), PATH("Frag.glsl"), 1);
  unsigned char * data;
  data = (unsigned char*)calloc(800*600*3,sizeof(unsigned char));
  while(engineProcess()) {
    processInput();
    backgroundDraw(data, 100, 100);
    unsigned char a = ((int)((sin(glfwGetTime())+1)*127) % 255);
    for (int i =0; i<800*600*3; i++) {
      data[i]= a;
    }
  }
  engineTerm();
  free(data);
  return 0;
}
