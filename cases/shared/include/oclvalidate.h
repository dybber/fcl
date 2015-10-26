#ifdef _DEBUG

#ifndef __CLU_H
#define __CLU_H


#include <stdio.h>
void OCL_VALIDATE(cl_int in_status)
{
    if (in_status != CL_SUCCESS)
    {
      printf("%s(%d) OpenCL returned: %d (%s)\n", 
             __FILE__, __LINE__, in_status, cluPrintError(in_status));
    }
}
#else
#define OCL_VALIDATE(in_status) ((void) in_status)

#endif
#endif
