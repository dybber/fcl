//////////////////////////////////////
// reduce
//////////////////////////////////////

input = ...

int groups = length(input) / 256;
output = allocate(int, groups);

int blockSize = 128;

distribute<grid>(i = 0 : groups) {
    int len = blockSize;
    tmp = allocate(int, len);
    parfor<block>(j = 0 : len) {
        tmp[j] = input[j] + input[j + len];
    }
    synchronize;

    while(len > 1) {
       len2 = len/2;
       parfor<block>(j = 0 : len) {
           tmp[j] = tmp[j] + tmp[j + len2];
       }
       synchronize; // barrier
       len = len2;
    }
    parfor<block>(j = 0 : len) {
        output[j] = tmp[j];
    }
}
synchronize; // clFinish()

///////////////////////////////////////

int x = c;
allocate x, int, n;

distribute<grid> (i = 0 : n) {
  parfor<block>(j = 0 : m) {
    if(i < c) {
        f(i);
    }
  }
  synchronize;
}
