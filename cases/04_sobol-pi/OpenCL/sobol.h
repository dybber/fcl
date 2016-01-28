#define lgWARP          5
#define WARP            (1<<lgWARP)
#define logMAX_CHUNK    8

typedef unsigned int UINT8;
typedef unsigned int UINT32; 
typedef unsigned int INT32; 

typedef struct {
    /*** Contract Characteristics ***/
    UINT32   num_mcits;       // # of Monte-Carlo iterations
    UINT32   sobol_bits;      // # of bits in Sobol-number rep
    UINT32   sobol_dim;       // # of dimensions of Sobol sequence

    UINT32   sobol_count_ini; // initial number from which the Sobol sequence starts
    double     sob_norm_fact;   // maximal sobol int 1/(1<<sobol_bits)

    UINT32   chunk;           // loop strip mining factor
    UINT32   log_chunk;       // log_2 of strip mining factor

    UINT32   logBLOCK;
    UINT32   BLOCK;

//    UINT32   sobol_dim;       // == md_dim * md_nb_path_dates
} LoopROScalars __attribute__ ((aligned (32)));
