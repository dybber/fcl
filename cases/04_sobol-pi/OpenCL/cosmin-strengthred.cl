#pragma OPENCL EXTENSION cl_khr_fp64: enable

#include <stdint.h>
#include "sobol.h"
 
#define DUMMY 1
 
/*********************************************/
/********** SOBOL NUMBER GENERATOR ***********/
/*********************************************/

__kernel void mlfi_genmatrix_uniform2 ( 
        __constant LoopROScalars*   ro_scal,
        __constant uint8_t*         sobol_fix_ind,
        __global   int*             sobol_v_dir,
        __global   double*          md_zd
) {
    uint32_t i, j, k;    
    uint32_t seq_count  = ( get_global_id (0) << ro_scal->log_chunk );
    uint32_t sobol_dim  = ro_scal->sobol_dim;

#if(DUMMY==0)
    uint8_t  rmb_size = 0;
    uint8_t  rmb[32];
#endif
    
    md_zd = md_zd +  
                    ( ( (get_global_id (0) >> lgWARP) << (ro_scal->log_chunk+lgWARP) )*sobol_dim + 
                    (get_global_id(0) & (WARP-1)) );
    __global double* md_zd_tmp = md_zd;
 
    uint32_t UB = (seq_count + (1 << ro_scal->log_chunk) < ro_scal->num_gpuits) ?
               seq_count + (1 << ro_scal->log_chunk) : ro_scal->num_gpuits  ;
    UB = (UB > seq_count) ? UB - seq_count : 0;
 
    if( UB > 0 ) {
        uint32_t gs;
#if(DUMMY==0)  
        // Compute gs == the Gray code rep of seq_count
        gs = seq_count+1 + ro_scal->sobol_count_ini; 
        gs = gs ^ (gs>>1);
  
        // Compute the position of the 1 bits
        for( k = 0; k < ro_scal->sobol_bits; ++k ) { 
            if(gs & 1) {
                rmb[rmb_size] = (uint8_t)k;
                rmb_size ++;
            }
            gs = gs >> 1;
        }
#endif
        // Compute the random number under the INDEPENDENT formulas
        for( i = 0, j=0; j < sobol_dim; j++, i += WARP ) {
            uint32_t accum = 0;
            md_zd_tmp = md_zd;
 
            // FIRST ITER COMPUTED INDEPENDENTLY!
#if(DUMMY==0)
            for(k=0; k<rmb_size; k++) {
                accum ^= sobol_v_dir[ j*ro_scal->sobol_bits + rmb[k] ];
            }
#else  
            gs = seq_count+1 + ro_scal->sobol_count_ini; 
            gs = gs ^ (gs>>1);
            for(k=0; k<ro_scal->sobol_bits; k++) {
                if(gs & 1) {
                    accum ^= sobol_v_dir[ j*ro_scal->sobol_bits + k ];
                }
                gs = gs >> 1;
            }            
#endif
            md_zd_tmp[ i ] = ro_scal->sob_norm_fact*accum;
#if 1
            // THE REST OF CHUNK-1 ITERATIONS COMPUTED UNDER RECURRENT FORMULA
            for( k = 1; k < UB-1; k++ ) {
                accum = accum ^ sobol_v_dir[ j*ro_scal->sobol_bits + sobol_fix_ind[k] ];
                
                md_zd_tmp += (sobol_dim << lgWARP);
                md_zd_tmp[ i ] = ro_scal->sob_norm_fact*accum;
            }
            
            gs = (seq_count + UB - 1 + ro_scal->sobol_count_ini) >> ro_scal->log_chunk;
            uint32_t ell = ro_scal->log_chunk;
            while(gs & 1) {
                ell++;
                gs >>= 1;
            }
            accum = accum ^ sobol_v_dir[j*ro_scal->sobol_bits + ell];
            md_zd_tmp += (sobol_dim << lgWARP);
            md_zd_tmp[ i ] = ro_scal->sob_norm_fact*accum;
#else
            for( k = seq_count+1 + ro_scal->sobol_count_ini; k < UB + ro_scal->sobol_count_ini; k++ ) {
                gs = k;
                uint32_t ell = 0;
                while(gs & 1) {
                    ell++;
                    gs >>= 1;
                }
                       
                accum = accum ^ sobol_v_dir[j*ro_scal->sobol_bits + ell];
                
                md_zd_tmp += (sobol_dim << lgWARP);
                md_zd_tmp[ i ] = ro_scal->sob_norm_fact*accum;
            }
#endif
        }
    }   // end IF (seq_count < UB)

}
