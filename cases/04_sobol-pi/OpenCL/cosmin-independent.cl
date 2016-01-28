#include "sobol.h"

/*********************************/
/*** Independent formula Only ****/
/*********************************/
__kernel void mlfi_genmatrix_uniform2 ( 
        __constant LoopROScalars*   ro_scal,
        __constant UINT8*         sobol_fix_ind,
        __global   INT32*         sobol_v_dir,
        __global   double*          md_zd
) {
    UINT32 i, j, k, m;    
    UINT32 seq_count  = ( get_global_id (0) << ro_scal->log_chunk );
    UINT32 sobol_dim  = ro_scal->sobol_dim;

    UINT8  rmb_size = (UINT8)0;
    UINT8  rmb[32];
    
    md_zd = md_zd +  
                    ( ( (get_global_id (0) >> lgWARP) << (ro_scal->log_chunk+lgWARP) )*sobol_dim + 
                    (get_global_id(0) & (WARP-1)) );
    __global double* md_zd_tmp = md_zd;
 
    UINT32 UB = (seq_count + (1 << ro_scal->log_chunk) < ro_scal->num_mcits) ?
               seq_count + (1 << ro_scal->log_chunk) : ro_scal->num_mcits  ;
    UB = (UB > seq_count) ? UB - seq_count : 0;
 
    //if( UB > 0 ) 
    for(m=0; m<UB; m++) {
        UINT32 gs;
        rmb_size = 0;

        // Compute gs == the Gray code rep of seq_count
        gs = seq_count+m+1 + ro_scal->sobol_count_ini; 
        gs = gs ^ (gs>>1);
  
        // Compute the position of the 1 bits
        for( k = 0; k < ro_scal->sobol_bits; ++k ) { 
            if(gs & 1) {
                rmb[rmb_size] = (UINT8)k;
                rmb_size ++;
            }
            gs = gs >> 1;
        }

        // Compute the random number under the INDEPENDENT formulas
        for( i = 0, j=0; j < sobol_dim; j++, i += WARP ) {
            UINT32 accum = 0;
 
            for(k=0; k<rmb_size; k++) {
                accum ^= sobol_v_dir[ j*ro_scal->sobol_bits + rmb[k] ];
            }
            md_zd_tmp[ i ] = ro_scal->sob_norm_fact*accum;
        }

        md_zd_tmp += (sobol_dim << lgWARP);
    }   // end IF (seq_count < UB)
}
