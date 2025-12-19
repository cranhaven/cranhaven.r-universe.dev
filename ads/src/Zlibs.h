double un_point(double,double,double,double,double,double,double,double,double);
double deux_point(double,double,double,double,double,double,double,double,double);
double ununun_point(double,double,double,double,double,double,double,double,double);
double trois_point(double,double,double,double,double,double,double,double,double);
double deuxun_point(double,double,double,double,double,double,double,double,double);
double deuxbord_point(double,double,double,double,double,double,double,double,double);

int in_droite(double,double,double,double,double,double,double,double,int);
int in_triangle(double,double,double,double,double,double,double,double,int);

void ic(int,int,double **,double **,double *,double *,int);

double perim_in_rect(double, double, double, double, double, double, double);
double perim_in_disq(double,double,double,double,double,double);
double perim_triangle(double,double,double,int,double *,double *,double *,double *,double *,double *);

extern int ripley_rect(int*,double*,double*,double*,double*,double*,double*,int*,double*,double*,double*);
extern int ripley_disq(int *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int ripley_tr_rect(int *,double *,double *,double *,double *,double *,double *,int *,double *,double *,
                   double *,double *,double *,double *,int *,double *,double *,double *);
extern int ripley_tr_disq(int *,double *,double *,double *,double *,double *,int *,double *,double *,double *,double *,
                   double *,double *,int *,double *,double *,double *);

extern int ripley_rect_ic(int *,double *,double *,double *,double *,double *,double *,double *,int *,double *,int *,
                   double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *);
extern int ripley_disq_ic(int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,double *,
                   double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *);
extern int ripley_tr_rect_ic(int *,double *,double *,double *,double *,double *,double *,double *,int *, double *,
                      double *,double *,double *,double *,double *,int *,double *,int *,double *,double *,double *,double *,
                      double *,double *,double *,double *,double *,double *,double *,double *);
extern int ripley_tr_disq_ic(int *,double *,double *,double *,double *,double *,double *,int *, double *, double *,
                      double *,double *,double *,double *,int *,double *,int *,double *,double *,double *,double *,double *,
                      double *,double *,double *,double *,double *,double *,double *);

extern int ripleylocal_rect(int*,double *,double *,double*,double*,double*,double*,int*,double*,double *,double *);
extern int ripleylocal_disq(int *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int ripleylocal_tr_rect(int *,double *,double *,double *,double *,double *,double *,int *,
                        double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int ripleylocal_tr_disq(int *,double *,double *,double *,double *,double *,int *,
                        double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);

extern int density_rect(int*,double*,double*,double*,double*,double*,double*,int*,double*,double*,double*,int*,double*);
extern int density_disq(int*,double *,double *,double*,double*,double*,int*,double*,double *,double *,int*,double *);
extern int density_tr_rect(int*,double *,double *,double*,double*,double*,double*,int*,double *,double *,double *,
                    double *,double *,double *,int*,double*,double *,double *,int*,double *);
extern int density_tr_disq(int*,double *,double *,double*,double*,double*,int*,double *,double *,double *,double *,
                    double *,double *,int*,double*,double *,double *,int*,double *);

extern int intertype_rect(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                   int *,double *,double *,double *);
extern int intertype_disq(int *,double *,double *,int *,double *,double *,double *,double *,double *,int *,
                   double *,double *,double *);
extern int intertype_tr_rect(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,int *,
                      double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int intertype_tr_disq(int *,double *,double *,int *,double *,double *,double *,double *,double *,int *,
                      double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);

extern int intertype_rect_ic(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                      double *,int *,double *,int *,int *,double *,int *, int *, int *, double *,double *,double *,double *,double *,double *,double *,
                      double *,double *,double *,double *);
extern int intertype_disq_ic(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,int *,
                      double *,int *,int *,double *,int *,int *,int *,double *,double *,double *,double *,double *,double *,double *,double *,
                      double *,double *,double *);
extern int intertype_tr_rect_ic(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                         double *,int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,int *,int *,int *,double *,
                         double *,double *,double *,double *,double *,double *,double *,double *,double *,double *);
extern int intertype_tr_disq_ic(int *,double *,double *,int *, double *, double *,double *,double *,double *,double *,
                         int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,int *,int *,int*,double *,double *,
                         double *,double *,double *,double *,double *,double *,double *,double *,double *);

extern int intertypelocal_rect(int*,double *,double *,int*,double *,double *,double*,double*,double*,double*,
                        int*,double*,double *,double *);
extern int intertypelocal_disq(int *,double *,double *,int *,double *,double *,double *,double *,	double *,int *,
                        double *,double *,double *);
extern int intertypelocal_tr_rect(int *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                           int *,double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int intertypelocal_tr_disq(int *,double *,double *,int *,double *,double *,double *,double *,double *,int *,
                           double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);

void s_alea_rect(int,double[],double[],double,double,double,double,double);
void s_alea_disq(int,double *,double *,double,double,double,double);
void s_alea_tr_rect(int,double *,double *,double,double,double,double,int,double *,double *,double *,double *,
                    double *,double *,double);
void s_alea_tr_disq(int ,double *,double *,double,double,double,int,double *,double *,double *,double *,
                    double *,double *,double);

int randlabelling(double *, double *, int, double *, double *,int, double *, double *,int *);
int randshifting_rect(int *,double *,double *,int,double *,double *,double,double,double,double,double);
int randshifting_disq(int *,double *,double *,int,double *,double *,double,double,double,double);
int randshifting_tr_rect(int *,double *,double *,int,double *,double *,double,double,double,double,int,
                         double *,double *,double *,double *,double *,double *,double);
int randshifting_tr_disq(int *,double *,double *,int,double *,double *,double,double,double,int,
                         double *,double *,double *,double *,double *,double *,double);
int randomlab(double *,double *,int,int *,int,double **,int *,double **);
void randmark(int ,double *,double *);
int randomdist(int *,int,double *,double *);
extern int corr_rect(int *,double *,double *,double *, double *,double *,double *,double *,int *,double *,double *,double *);
extern int corr_disq(int *,double *,double *,double *, double *,double *,double *,int *,double *,double *,double *);
extern int corr_tr_rect(int *,double *,double *,double *, double *,double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,int *,double *,double *,double *);
extern int corr_tr_disq(int *,double *,double *,double *, double *,double *,double *,int *,double *,double *,double *,double *,double *,double *,int *,double *,double *,double *);
extern int corr_rect_ic(int *,double *,double *,double *, double *,double *,double *,double *,int *,double *,int *, double *,double *,double *,double *,double *, double *,double *, double *, double *);
extern int corr_disq_ic(int *,double *,double *,double *, double *,double *,double *,int *,double *,int *, double *,double *,double *,double *,double *, double *,double *, double *, double *);
extern int corr_tr_rect_ic(int *,double *,double *,double *, double *,double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,int *,double *,int *, double *,double *,double *,double *,double *, double *,double *, double *, double *);
extern int corr_tr_disq_ic(int *,double *x,double *,double *, double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,int *,double *,int *, double *,double *,double *,double *,double *, double *,double *, double *, double *);
extern int shimatani_rect(int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,double *, double *,int *);
extern int shimatani_disq(int *,double *,double *, double *,double *,double *,int *,double *,int *,int *,double *,double *, double *,int *);
extern int shimatani_tr_rect(int *,double *,double *, double *,double *,double *,double *,int *, double *, double *, double *, double *, double *, double *, int *,double *,int *,int *,double *,double *, double *,int *);
extern int shimatani_tr_disq(int *,double *,double *, double *,double *,double *,int *, double *, double *, double *, double *, double *, double *, int *,double *,int *,int *,double *,double *, double *,int *);
extern int shimatani_rect_ic(int *,double *,double *, double *,double *,double *,double *,int *,double *,int *,double *,int *,int *,double *,double *,
                      double *, double *,double *,double *,double *,double *,double *,double *,int *);
extern int shimatani_disq_ic(int *,double *,double *, double *,double *,double *,int *,double *,int *,double *,int *,int *,double *,double *,
                      double *, double *,double *,double *,double *,double *,double *,double *,int *);
extern int shimatani_tr_rect_ic(int *,double *,double *, double *,double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,
                         int *,double *,int *,double *,int *,int *,double *,double *,double *, double *,double *,double *,double *,double *,double *,double *,int *);
extern int shimatani_tr_disq_ic(int *,double *,double *, double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,
                         int *,double *,int *,double *,int *,int *,double *,double *,double *, double *,double *,double *,double *,double *,double *,double *,int *);

extern int rao_rect(int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,int *,double *,double *,double *,double *,
             double *,double *,double *,int *);
extern int rao_rect_ic(int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,int *,int *,double *,double *,
                double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,int *);
extern int rao_disq(int *,double *,double *,double *,double *,double *,int *,double *,int *,int *,int *,double *,double *,double *,double *,
             double *,double *,double *,int *);
extern int rao_disq_ic(int *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,int *,int *,double *,double *,
                double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,int *);
extern int rao_tr_rect(int *,double *,double *,double *,double *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                int *,double *,int *,int *,int *,double *,double *,double *,double *,double *,double *,double *,int *);

extern int rao_tr_rect_ic(int *,double *,double *,double *,double *,double *,double *,int *,double *,double *,double *,double *,double *,double *,
                   int *,double *,int *,int *,double *,int *,int *,double *,double *,double *,double *,double *,double *,double *,double *,
                   double *,double *,double *,double *,double *,int *);
extern int rao_tr_disq(int *,double *,double *,double *,double *,double *,int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,int *,
                double *,double *,double *,double *,double *,double *,double *,int *);
extern int rao_tr_disq_ic(int *,double *,double *,double *,double *,double *,int *,double *,double *,double *,double *,double *,double *,int *,double *,int *,int *,double *,
                   int *,int *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,double *,int *);

extern int mimetic_rect(int *,double *,double *, double *,double *,double *,double *,double *,double *, int *,double *,double *,int *,int *,double *,double *,double *,double *,double *,int *);
extern int mimetic_disq(int *,double *,double *,double *,double *,double *,double *,double *, int *, double *, double *, int *, int *, double *,double *,double *,double *,double *,int *);
extern int mimetic_tr_rect(int *,double *,double *, double *,double *,double *,double *,double *,int *, double *, double *, double *, double *, double *, double *,
                    double *, int *, double *, double *, int *, int *, double *,double *, double *,double *,double *,int *);
extern int mimetic_tr_disq(int *,double *,double *, double *,double *,double *,double *,int *, double *,double *,double *,double *,double *,double *cy,
                    double *, int *, double *, double *, int *, int *, double *,double *,double *,double *,double *,int *);
double echange_point_rect(int,double *,double *,double,double,double,double,double,double,double,double *,int *,double *,double *,double *);
double echange_point_disq(int,double *,double *,double,double,double,double,double,double,double *,int *,double *,double *,double *);
double echange_point_tr_rect(int,double *,double *,double,double,double,double,int *,double *,double *,double *,double *,double *,double *,
                             double,double,double,double *,int *,double *,double *,double *);
double echange_point_tr_disq(int,double *,double *,double,double,double,int *,double *,double *,double *,double *,double *,double *,
                             double,double,double,double *,int *,double *,double *,double *);
extern int shen(int *,double *,double *,int *,double *,int *,int *,double *,double *,double *,double *, double *, int *);
extern int shen_ic(int *,double *,double *,int *,double *,int *,double *,int *,int *,double *,double *,double *,
            double *, double *, double *,double *,double *,double *,double *,double *,int *);
int intertype(int *,double *,double *,int *, double *, double *,int *,double *,double *,double *);
