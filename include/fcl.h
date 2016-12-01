#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

/* Buffer size in number of bytes */
#define INIT_BUFFER_SIZE (4*1024)

/* Copy values to a new buffer, double the size, free the old buffer. */
int double_buffersize(int* bufferSize, void** buffer_ptr) {
  int newBufferSize = 2*(*bufferSize);
  //printf("Doubling the buffer, new buffer size: %d bytes\n", newBufferSize);
  int* buffer = *buffer_ptr;
  int* newBuffer = malloc (newBufferSize);
  if (newBuffer != NULL) {
    memcpy(newBuffer, buffer, *bufferSize);
    free(buffer);
    *buffer_ptr = newBuffer;
    *bufferSize = newBufferSize;
    return 1;
  } else {
    return 0;
  }
}

int* read_csv_ints(FILE* handle, int* valuesRead) {
  int bufferSize = INIT_BUFFER_SIZE;
  //printf("Alloc initial buffer, size: %d bytes\n", bufferSize);
  int* buffer_ptr = (int*) malloc (bufferSize);
  if (buffer_ptr == NULL) {
    return 0;
  }

  int i = 0;
  size_t bufsize = 1024;
  char* line = (char*)malloc(bufsize * sizeof(char));  
  //printf("Reading first line\n");
  while (getline(&line, &bufsize, handle) > 0) {
    const char* tok;
    tok = strtok(line, " ,\n");
    while (tok != NULL) {
      buffer_ptr[i] = atoi(tok);
      i++;
      if (sizeof(int)*i >= bufferSize) {
        //printf("Read %d integers, resize buffer\n", i);        
        if(!double_buffersize(&bufferSize, (void**)&buffer_ptr)) {
          return NULL;
        }
      }
      tok = strtok (NULL, " ,\n");
    }
  }
  *valuesRead = i;
  return buffer_ptr;
}

/* Arg, complete COPY-PASTE of above, just changed to read doubles */
double* read_csv_doubles(FILE* handle, int* valuesRead) {
  int bufferSize = INIT_BUFFER_SIZE;
  // printf("Alloc initial buffer, size: %d bytes\n", bufferSize);
  double* buffer_ptr = (double*) malloc (bufferSize);
  if (buffer_ptr == NULL) {
    return NULL;
  }

  int i = 0;
	size_t bufsize = 1024;
  char* line = (char*)malloc(bufsize * sizeof(char));
  //printf("Reading first line\n");
  while (getline(&line, &bufsize, handle) > 0) {
    const char* tok;
    tok = strtok(line, " ,\n");
    while (tok != NULL) {
      buffer_ptr[i] = atof(tok);
      i++;
      if (sizeof(double)*i >= bufferSize) {
        //printf("Read %d integers, resize buffer\n", i);        
        if(!double_buffersize(&bufferSize, (void**)&buffer_ptr)) {
          return NULL;
        }
      }
      tok = strtok (NULL, " ,\n");
    }
  }
  *valuesRead = i;
  return buffer_ptr;
}

int* readIntVecFile(int* valuesRead, char* filename) {
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error reading %s: %d (%s)\n", filename, errno, strerror(errno));
    exit(0);
  }

  int* res = read_csv_ints(file, valuesRead);
  if (res == NULL) {
    printf("readIntVecFile: Error reading file '%s'\n", filename);
    fclose(file);
    exit(0);
  }
  fclose(file);
  return res;
}

double* readDoubleVecFile(int* valuesRead, char* filename) {
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error reading %s: %d (%s)\n", filename, errno, strerror(errno));
    exit(0);
  }

  double* res = read_csv_doubles(file, valuesRead);
  if (res == NULL) {
    printf("readDoubleVecFile: Error reading file '%s'\n", filename);
    fclose(file);
    exit(0);
  }
  fclose(file);
  return res;
}
