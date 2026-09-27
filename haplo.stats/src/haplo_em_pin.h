/* $Author: schaid $ */
/* $Date: 2007/02/27 20:18:01 $ */

typedef struct HAP_T {
  int id;
  int code;
  int pair_id;
  int keep;
  int *loci;
  double post, wt;
} HAP;


typedef struct HAPUNIQUE_T {
  int code;
  int keep;
  int *loci;
  double prior;
} HAPUNIQUE;

static int iminarg1, iminarg2;

# define imin(a,b) (iminarg1=(a), iminarg2=(b), \
		    (iminarg1) < (iminarg2) ? (iminarg1) : (iminarg2) )

/* Windows compatibility */

#ifdef _WINDOWS
#define CDECL __cdecl
#else
#define CDECL
#endif


/************************** Function prototypes ***********************************/

static HAP* new_hap(int id, int pair_id, double wt, double prior, double post);

static void write_hap_list(HAP** so, int n_hap);


static int CDECL cmp_hap(const void *to_one, const void *to_two);

static int CDECL cmp_subId_hapPairId(const void *to_one, const void *to_two);

static int CDECL cmp_hap_code(const void *to_one, const void *to_two);

static int code_haps(int n_hap, HAP **hap_list);

static int hap_enum(HAP ***hap_list_ptr, double **prior_ptr, int *max_haps, int *n_alleles, int insert_loc, 
		     int n_hap, int *pair_id);

static HAP* copy_hap(HAP *old);

static int num_het(HAP* h1,HAP* h2);

static void hap_prior(int n_hap, HAP** hap_list, double *prior, int n_u_hap,
                      double min_prior);

static int hap_posterior(int n_hap, HAP **hap_list, double *prior, 
			  int n_u_hap, double min_posterior, double *lnlike);

static int **int_vec_to_mat(int *Yvec, int nrow, int ncol);

static int **int_matrix(int nrow, int ncol);

static void set_posterior(int n_hap, HAP **hap_list, int *random_start);

static int ranAS183_seed(int iseed1, int iseed2, int iseed3);

static double ranAS183(void);

static void errmsg(char *string);

static HAPUNIQUE* copy_hap_unique(HAP *old, double *prior);

static void unique_haps(int n_hap, HAP **hap_list, HAPUNIQUE **u_hap_list, double *prior);

static int count_unique_haps(int n_hap, HAP **hap_list);

static void write_prior(int n, double *prior);

static void write_unique_hap_list(HAPUNIQUE** so, int n_hap);

static void divideKeep(HAP **hap_list, int n, int *nReturn);

static void add_more_memory(HAP ***hap_list, double **prior,int *max_haps);

static void insert_new_hap_pair(HAP ***hap_list_ptr, double **prior_ptr, 
                                int *max_haps, int insert_loc,
                                HAP *h1_old, HAP *h2_old, 
                                int a1_new, int a2_new,
                                int *pair_id, int *j);

static void overwrite_hap(HAP *new, HAP *old);

void checkIntMax(int *intMax);
