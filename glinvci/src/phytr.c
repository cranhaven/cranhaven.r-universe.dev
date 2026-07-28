/* -*- tab-width: 8; indent-tabs-mode: t; -*- */

/* CRAN checks will complain with unused variables. They exist on purpose and
   this attribute will mute the compiler warning. */
#if defined(__GNUC__) || defined(__clang__)
#define __UNUSED __attribute__((unused))
#else
#define __UNUSED
#endif

/* Microsoft had only added support to the C99 _Pragma() in 2020 summer. For older
   MS compilers we need __pragma(). */
#ifdef _OPENMP
#ifdef _MSC_VER
#define __PRAGMA__ __pragma
#else
#define __PRAGMA__ _Pragma
#endif
#include"my_pragma.h"
#else
#define __PRAGMA__(x)
#endif

#define __TESTED__ 1
#include"my_confinfo.h"

/* If including omp.h, include it AFTER Rinternals.h, because
   Rinternals.h defines a macro called match to Rf_match and
   Intel 2021 wants to expand this macro into #pragma omp begin declare variant,
   which is written in Intel's version of omp.h. */
/* #include<omp.h> */

#include<R.h>
#include<R_ext/Print.h>
#include<stdlib.h>
#include<stdio.h>
#include<strings.h>
#include<Rinternals.h>
#include<Rmath.h>



/* Old versions of OpenMP (for example in Open64 compilers) doesn't support the atomic
   read/write with pointer assignments. In that case we resort to locking the entire
   piece of code. */
#ifdef _OPENMP
#if _OPENMP > 201400
#define __ATOMIC_READ__  __PRAGMA__("omp atomic read")
#define __ATOMIC_WRITE__ __PRAGMA__("omp atomic write")
#else
#define __ATOMIC_READ__  __PRAGMA__("omp critical")
#define __ATOMIC_WRITE__ __PRAGMA__("omp critical")
#endif
#else
#define __ATOMIC_READ__
#define __ATOMIC_WRITE__
#endif

/* Per thread stack's length for walking the tree, allocated on the heap.
   If integers are 4 bytes then expect the program to malloc about
      (n_thread+1) * PTSTACKLEN * 20 bytes.
 */
#define PTSTACKLEN (50*1024*4)

/* Struct that represents the data carried by a node. Pointers will be to the heap.
   Invariant:  1. ndat for the global root exists, fields are allocated, but has no meaning.
               2. x is non-null if and only if the node associated with it is a tip
*/
struct ndat {
	/* These are allocated and used at all circumstances. */
	int ku;
	int ndesc;		/* Number of descendants */
	double *x;
	
	/* These are allocated and used only when the first-order derivatives are needed. */
	double *a, *HPhi, *Lamb,
		*dodv, *dodphi, *dgamdv, *dgamdw, *dgamdphi, *dcdw, *dcdv, *dddv,
		*dlikdv, *dlikdw, *dlikdphi;

	/* These are allocated and used only when the Hessian is needed  */
	double *H, *b;		         /* Isn't allocated for root but allocated for all others */
	double *invV, *so, *sgam, *sc;   /* Allocated for all */
};


/* Contains both intermediate results of Hessian computation as well as
   indicies of corresponding parameters in the packed Hessian matrix. The
   root doesn't have this structure attached to it, but instead, the
   corresponding memory of the root contains a rootbk structure which
   has different sort of information, including meta information about
   the entire tree.
*/
struct hselfbktip { double *invVPhi, *invVxw;                                };
struct hselfbkgen { double *invVLsO, *invVLsOPhi, *VmVLV, *invVLb, *Hto; };
struct nonrootbk {
	long Phi, w, V;	/* Indices of the parameters in the gradient vector. Filled in Rnewnode() */
	union {
		struct hselfbktip hsbktip;
		struct hselfbkgen hsbkgen;
	} u;
};

/* Bookkeeping variables at the root node. */
struct rootbk {
	long nparam;
	int xavail;
	double *hessflat;
	int hessflat_needs_free;
};

/* Book keeping variables, used only in gradwk() and grad() as tmp vars. */
struct diffbk {  double *F, *z, *K; };

/* Represents a node in a general tree.
   
    Invariant: 1.   0 <= id of tips <= number of tips - 1.
               2.   node.id + 1 == ape's node id for all nodes.
               3.   id of all the children are unique and never repeat.
               4.   content in ndat is has meaning only if the node is not a global root.
*/
struct node {
	int id;
	struct ndat ndat;
	struct node *chd, *nxtsb;   /* First most child and next sibling resp. */
	union {
		struct nonrootbk hnbk;
		struct rootbk rbk;
	} u;
};

struct llst {			/* Linked list containing matrices/vectors */
	struct llst *nxt;
	int siz;                /* Not the length of dat[1]. Depends on the context. */
	double dat[1];		/* Allocate bigger memory to store this */
};
struct llstptr { 		/* Linked list containing simple pointers */
	struct llstptr *nxt;
	int siz;
	void *dat;
};

enum hessparcomb { IVV, IVPHI, IVW, IPHIPHI, IPHIW, IWW };

/* Book keeping for the global-level tree walk in the Hessian computation */
struct hessglbbk {
	struct llst	*fm;	struct llst	*fmlfm;
	struct llst	*qm;	struct llstptr	*a;
	int mdim;
};


/* Represents missing-ness. Stores all informations about NA/NaN etc.
   The core V-w-Phi space computation will NOT see any NA/NaN. Note that
   tip traits are "sanitised" before even enter into `struct node`. This
   structure is necessary for even constructing the arguments for
   Rnewnodes.  */
struct miss { int *M; };
enum missingness { LOST, USUAL, MISS };

/* Function for accessing V-w-Phi from both the list format or a vector format */
typedef size_t (*fn_getvwphi)(SEXP, struct node *, int, double **, double **, double **, void *, size_t);

struct dfdqdk {   /* Passed into Fortran */
	double *R;	int kr;
	int knu;	int knv;
	int kmu;	int kmv;
};
#define DFQKSIZ sizeof(struct dfdqdk)

/*
  We store the Hessian as a uplo='L' format. It's simply a storage scheme and
  users won't see this at all.
*/
extern void hessdiag2ltri_(double *Hnew, int *nnew, double *Hold, int *nold, int *m, int *k, int *istart);
extern void diag2ltri_(double *d, int *k, double *out);
extern void hchnlndiag_(double *Hnew, int *nnew, double *Hold, int *nold, double *par,
			double *djacthis, int *ildjac, int *joffset, int *m, int *istart, int *k);
extern void hesschopnondiag_(double *Hnew, int *nnew, double *Hold, int *nold, int *m, int *istart, int *k);
extern void hesscpyskip_(double *Hnew, int *nnew, double *Hold, int *nold, int *m, int *istart, int *ihowmuch);
extern void bilinupdt_(double *d, double *hessflat, long *npar, long *idx1, long *idx2, double *dir, int *ndir);
extern void houspdh_(double *Horig, double *par, double *djac, int *ldjac, int *joffset, int *m,
		     int *k, int *npar_orig, int *npar_new, int *ithis, double *out);
extern void houlnspdh_(double *Horig, double *par, double *djac, int *ldjac, int *joffset, int *m,
		       int *k, int *npar_orig, int *npar_new, int *ithis, double *out);
extern void houchnsymh_(double *Horig, int *m, int *k, int *nparorig, int *ithis, double *out);
extern void dchnunchol_(double *DFDH, double *L, int *m, int *k, double *DFDL);
extern void dlnchnunchol_(double *DFDH, double *L, int *m, int *k, double *DFDL);
extern void lnunchol_(double *sig_x, int *k, double *wsp, int *lwsp, double *out, int *info);
extern void unchol_(double *sig_x, int *k, double *wsp, int *lwsp, double *out, int *info);
extern void diagone_(double *A, int *siz);
extern void diagoneclr_(double *A, int *siz);
extern void ndmerg_(double *V, double *w, double *Phi, int *kv, int *ku,
		    double *c, double *gam, double *o, double *d,
		    double *cout, double *gamout, double *oout, double *dout, int *info);
extern void dmerg_(double *V, double *w, double *Phi, int *kv, int *ku,
		   double *c, double *gam, double *o, double *d,
		   double *cout, double *gamout, double *oout, double *dout,
		   double *a, double *HPhi, double *Lamb,
		   double *dodv, double *dodphi,
		   double *dgamdv, double *dgamdw, double *dgamdphi,
		   double *dcdw, double *dcdv, double *dddv, int *info);
extern void hmerg_(double *V, double *w, double *Phi, int *kv, int *ku,
		   double *c, double *gam, double *o, double *d,
		   double *cout, double *gamout, double *oout, double *dout,
		   double *a, double *b, double *invV, double *H, double *HPhi, double *Lamb,
		   double *dodv, double *dodphi, double *dgamdv, double *dgamdw,
		   double *dgamdphi, double *dcdw, double *dcdv, double *dddv, int *info);
extern void ndtcgod_(double *V, double *w, double *Phi, double *x, int *kv, int *ku, double *c,
		     double *gam, double *o, double *d, int *info);
extern void dtcgod_(double *V, double *w, double *Phi, double *x, int *kv, int *ku, double *c,
		    double *gam, double *o, double *d,
		    double *dodv, double *dodphi,
		    double *dgamdv, double *dgamdw, double *dgamdphi,
		    double *dcdw, double *dcdv, double *dddv, int *info);
extern void htcgod_(double *V, double *w, double *Phi, double *x, int *kv, int *ku, double *c,
		    double *gam, double *o, double *d, double *solV, double *b,
		    double *dodv, double *dodphi,
		    double *dgamdv, double *dgamdw, double *dgamdphi,
		    double *dcdw, double *dcdv, double *dddv, int *info);
extern void phygausslik_(double *c, double *gam, double *o, double *d, double *x0, int *siz0, int *k, double *lik);
extern void ddcr_(int *kr, int *ku, double *x0, double *dodv, double *dodphi,
		  double *dgamdv, double *dgamdw, double *dgamdphi,
		  double *dcdw, double *dcdv, double *dddv,
		  double *dlikdv, double *dlikdw, double *dlikdphi);
extern void fzkdown_(double *Fb, double *zb, double *Kb, double *HPhib, double *ab, double *Lambb, double *x0,
		     int *siz_c, int *siz_b, int *siz_a, int *siz_r,
		     double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev,
		     double *dgamdphiev, double *dcdwev, double *dcdvev, double *dddvev,
		     double *dlikdv, double *dlikdw, double *dlikdphi, double *Fa, double *za, double *Ka);
extern void gesylcpy_(double *dst, double *src, int *k);
extern void sylgecpy_(double *dst, double *src, int *k);
extern void lsylgecpy_(double *dst, double *src, long *k);
extern void hselfbkgen_(double *solV, double *Lamb, double *sO, double *Phi, double *b, double *H,
			int *kv, int *ku,
			double *solVLsO, double *solVLsOPhi, double *VmVLV, double *solVLb, double *Hto);
extern void hselfbktip_(double *solV, double *x, double *w, double *Phi, int *kv, int *ku,
			double *solVPhi, double *solVxw);
/* extern void printmat_(double *A, int *krow, int* kcol); */
extern void dbledifftopgen_(int *ictx, int *i, int *j, int *m, int *n, int *kr, int *kv, int *ku,
			double *solVLsO, double *solVLsOPhi, double *VmVLV, double *solVLB,
			double *Hto, double *x0, double *d2L);
extern void dbledifftoptip_(int *ictx, int *i, int *j, int *m, int *n, int *kr, int *kv, int *ku,
			double *solV, double *solVPhi, double *solVxw, double *x0, double *d2L);
extern long ijtouplolidx_(long *siz, long *i, long *j);
extern int iijtouplolidx_(int *siz, int *i, int *j);
extern void symhessvv_(int *i, int *j, int *m, int *n,
		       double *dlijmn, double *dljimn, double *dljinm, double *dlijnm, double *dl);
extern void symhessvany_(int *i, int *j, double *dlijany, double *dljiany, double *dl);
extern void tmtmdir_(int *kr, int *kv, int *ku, struct llst **fmlfm_c, struct llst **qm_c,
		     struct llst **fm_c, struct llstptr **a_c,
		     double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev,
		     struct dfdqdk *dfqk1_ch, double *K, double *x0, int *istip, double *solVLsO, double *solVLsOPhi, double *VmVLV,
		     double *solVLB, double *Hto, double *bilinmat, double *dir, int *ndir, long *nparam, long *adphi, long *adV, long *adw);
extern void tmtm2_(int *kr, int *kv, int *ku, struct llst **fmlfm_c, struct llst **qm_c,
		   struct llst **fm_c, struct llstptr **a_c,
		   double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev,
		   struct dfdqdk *dfqk1_ch, double *K, double *x0, int *istip, double *solVLsO, double *solVLsOPhi, double *VmVLV,
		   double *solVLB, double *Hto, double *hessflat, long *nparam, long *adphi, long *adV, long *adw);
extern void updategbk_(int *kv, int *ku, struct llst **fmlfm_c, struct llst **fmlfm_new, struct llst **fm_c,
		       struct llst **fm_new, struct llst **qm_c, struct llst **qm_new, double *Lamb, double *HPhi, double *a, int *mdim);
extern void tndown1st_(struct dfdqdk *dfqk1_ch, double *K, double *H, double *HPhi,double *w, double *a, double *f1m,
		       double *q1m, double *Lamb, double *solV, double *solVLsOPhi, int *kr, int *kv, int *ku,
		       struct dfdqdk *dfqk1new_ch);
extern void tndown_(struct dfdqdk *dfqk1_ch, double *HPhi, double *a, int *knv, int *knu, int *kmv, int *kmu, struct dfdqdk *dfqk1new_ch);
extern void tntm_(int *kr, int *kmv, int *kmu, int *knv, int *knu, struct dfdqdk *dfqk1_ch,
		  double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev, double *x0,
		  double *hessflat, long *nparam, long *admphi, long *admV, long *admw, long *adnphi, long *adnV, long *adnw);
extern void tntmdir_(int *kr, int *kmv, int *kmu, int *knv, int *knu, struct dfdqdk *dfqk1_ch,
		     double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev, double *x0,
		     double *bilinmat, double *dir, int *ndir, long *nparam, long *admphi, long *admV, long *admw, long *adnphi, long *adnV, long *adnw);
extern void tntmthrfast_(int *kr, int *knv, int *knu, int *kmv, int *kmu, struct dfdqdk *dfqk1_ch,
			 double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev, double *x0,
			 void *wsp, size_t *lwsp);
extern void tntmcpy_(int *kmv, int *kmu, int *knv, int *knu, void *wsp, size_t *lwsp,
		     double *hessflat, long *nparam, long *admphi, long *admV, long *admw, long *adnphi, long *adnV, long *adnw);
extern void tntmcpydir_(int *kmv, int *kmu, int *knv, int *knu, void *wsp, size_t *lwsp,
			double *bilinmat, double *dir, int *ndir, long *nparam, long *admphi, long *admV, long *admw, long *adnphi, long *adnV, long *adnw);
extern void betadown_(struct llst **falfm_c, struct llst **falfm_new, struct llst **fmg_c,
		      double *f1a, double *q1a, double *HPhi, double *a, int *nk, int *mdim, int *kr, int *kbu, int *kbv, int *kmv);
extern void initfqk4b_(struct dfdqdk *dfqk1_ch, struct llst **fmg_c, struct llst **qmg_c, struct llst **falfm_c, double *f1a, double *q1a,
		       struct llstptr **a_c,  int *nk, int *kr, int *kav, int *kmv, int *kmu,
		       double *dodvev, double *dodphiev, double *dgamdvev, double *dgamdwev, double *dgamdphiev);
extern void initfalfm_beta_(struct llst **falfm_c, struct llst **fmg_c, int *kbu, int *kmv);
extern void dfqk_mmp1_(struct dfdqdk *dfqk1_ch, double *H, double *HPhi, double *w, double *a, double *Lamb, double *solV, double
		       *solVLsOPhi, int *ku, int *kr);
extern void curvifyupdate_(double *H, double *HV, double *Hw, double *HPhi, int *npar, int *ku, int *kv, double *dlikdv, double *dlikdw, double *dlikdphi, double *wsp);

#define DEF_TCGOD(NAME) void NAME (struct node *t, int kv, double *v, double *w, double *phi, double *c, double *gam, double *o, double *d, int *info)
typedef DEF_TCGOD((*fn_tcgod));
#define DEF_MERG(NAME) void NAME (struct node *t, int kv, \
				  double *v, double *w, double *phi, \
				  double *c1, double *gam1, double *o1, double *d1, \
				  double *c, double *gam, double *o, double *d, int *info)
typedef DEF_MERG((*fn_merg));

void dndgcgod (struct node *t, SEXP VwPhi_L, int kv, double *c, double *gam, double *o, double *d,
	       fn_getvwphi get_VwPhi, fn_tcgod tcgod, fn_merg merg, void *wsp, size_t swsp, size_t lwsp, int *info);
void hgcgod     (struct node *t, SEXP VwPhi_L, int kv, double *c, double *gam, double *o, double *d,
		 fn_getvwphi get_VwPhi, void *wsp, size_t swsp, size_t lwsp, int *info);
int hess        (struct node *t, SEXP VwPhi_L, double *x0, fn_getvwphi get_VwPhi, void *wsp, size_t swsp, size_t lwsp, double *extrmem, double *dir, int ndir, int nthd);
void grad       (struct node *t, double *x0, int nthd);
void gradwk     (struct node *a, struct node *b, struct node *c, double *x0, struct diffbk bk, int kr);
int hessglobwk(struct node *m, struct node *parent, struct hessglbbk *gbk, double *x0, struct node *rt, SEXP VwPhi_L,
	       fn_getvwphi get_VwPhi, void *wsp, size_t swsp, size_t lwsp, int *interrupted, double *extrmem, double *dir, int ndir);
void hessselftop(struct node *m, int kv, int ictx, int i, int j, int p, int q, double *x0, struct node *rt, double *hessflat, double *dir, int ndir);
void initgbk(struct hessglbbk *gbk, struct node *rt, struct node *p, int maxdim);
void delgbk(struct hessglbbk gbk);
SEXP Rlistelem(SEXP Rlist, const char *key);
int Rsetlistelem(SEXP Rlist, const char *key, SEXP Robj);


#define dcalloc(LHS, N, ONFAIL) { void* T; if( !(T = malloc((N)*sizeof(double)))) goto ONFAIL; dzero(T,N); LHS=T; }
#define icalloc(LHS, N, ONFAIL) { void* T; if( !(T = malloc((N)*sizeof(int))))    goto ONFAIL; izero(T,N); LHS=T; }
void dzero (double *X, size_t n)           { while(n) X[--n] = 0.0; }
void izero (int *X, size_t n)              { while(n) X[--n] = 0; }
void dset  (double *X, double y, size_t n) { while(n) X[--n] = y; }
void iset  (int *X, int y, size_t n)       { while(n) X[--n] = y; }

void free_tree(struct node *t);
void R_free_tree(SEXP p) {
	void *hessflat;
	struct node *t;
	if ((t = R_ExternalPtrAddr(p))) {
		hessflat = t->u.rbk.hessflat;
		if (t->u.rbk.hessflat_needs_free)
			free(hessflat);
		free_tree(t);
		R_ClearExternalPtr(p);
	}
}
void free_tree(struct node *t) {
	if (t) {
		free_tree(t->nxtsb);	free_tree(t->chd);
		free(t->ndat.x);	free(t->ndat.dlikdv);
		free(t);
	}
}

struct node *newnode(int id, int ku) {
	/* kv should be zero for the global root. If so, no VwPhi mem. is alloc'd. */
	struct node *n;
	
	if (! (n = calloc(1, sizeof(struct node))) ) goto MEMFAIL;
	n->id	= id;
	n->ndat.ku	= ku;
	n->ndat.ndesc	= 0;
	n->ndat.x	= NULL;
	n->ndat.dlikdv	= NULL;
	return n;
MEMFAIL:
	error("newnode(): Failure allocating memory (newnode())");
	return NULL;
}

SEXP Rwrapnode(void *n) {
	SEXP eptr;
	eptr = PROTECT(R_MakeExternalPtr(n, install("phytr_node"), R_NilValue));
	R_RegisterCFinalizerEx(eptr, R_free_tree, TRUE);
	UNPROTECT(1);
	return eptr;
}

void fillhidx(struct node **nra, int *edges, int n, int rootid) {
	int i = 0, lim = rootid, j;
	/* Precondition: root->u.rbk.nparam = 0 */
DOAGAIN:
	for (j=1; edges[j] != i+1; j+=2) ;;          /* Won't OOB unless isolated nodes exist */
	nra[i]->u.hnbk.Phi = nra[rootid]->u.rbk.nparam;
	nra[i]->u.hnbk.w = nra[i]->u.hnbk.Phi + (long)((nra[i]->ndat.ku) * (nra[edges[j-1]-1]->ndat.ku));
	nra[i]->u.hnbk.V = nra[i]->u.hnbk.w + (long)(nra[i]->ndat.ku);
	nra[rootid]->u.rbk.nparam = nra[i]->u.hnbk.V + ((nra[i]->ndat.ku) * (nra[i]->ndat.ku + 1)) / 2;
TESTAGAIN:
	if (++i < lim)  {        goto DOAGAIN;   }
	if (lim != n)   { lim=n; goto TESTAGAIN; }   /* Skip rootid, jump back into the loop. */
}


void settip(struct node *t, SEXP Rxtab) {
	void *tmp;
	struct node *p;
	if (t->id < length(Rxtab)) { 
		if (!(tmp = realloc(t->ndat.x, t->ndat.ku * sizeof(double)))) goto MEMFAIL;
		memcpy((t->ndat.x=tmp), REAL(VECTOR_ELT(Rxtab, t->id)), t->ndat.ku * sizeof(double));
	} else for (p = t->chd; p; p = p->nxtsb)
		settip(p, Rxtab);
	return;
MEMFAIL:
	error("settip(): Failed to allocate memory");
}
SEXP Rsettip(SEXP Rtr, SEXP Rxtab, SEXP Rrawmodobj) {
	struct node *t, *p;
	t = (struct node *)R_ExternalPtrAddr(Rtr);
	t->u.rbk.xavail = 1;
	for (p = t->chd; p; p=p->nxtsb)
		settip(p, Rxtab);
	return Rtr;
}
int fillndesc(struct node *t) {
	struct node *p;
	if (!(t->chd)) return 1;
	for (p=t->chd; p; p=p->nxtsb)
		t->ndat.ndesc += fillndesc(p);
	return 1 + t->ndat.ndesc;
}

void vwphi_paradr2(struct node *t, int *adtab, int ldt) {
	struct node *p;
	adtab[t->id] = t->u.hnbk.Phi+1;
	adtab[t->id+ldt] = (int)(t->u.hnbk.V+((t->ndat.ku)*(t->ndat.ku+1))/2);
	for (p=t->chd; p; p=p->nxtsb)
		vwphi_paradr2(p, adtab, ldt);
}
void vwphi_paradr(struct node *t, int *adtab, int ldt) {
	struct node *p;
	adtab[t->id] = NA_INTEGER; adtab[t->id+ldt] = NA_INTEGER;
	for (p=t->chd; p; p=p->nxtsb) vwphi_paradr2(p, adtab, ldt);
}
SEXP Rvwphi_paradr(SEXP Rt) {
	struct node *t = R_ExternalPtrAddr(Rt);
	int L = t->ndat.ndesc+1;
	SEXP Radtab = PROTECT(allocMatrix(INTSXP, L, 2));
	vwphi_paradr(t, INTEGER(Radtab), L);
	UNPROTECT(1);
	return Radtab;
}

void my_rchkusr(void *dummy) { (void)dummy; R_CheckUserInterrupt(); }
int my_rchk(void) { return !(R_ToplevelExec(my_rchkusr, NULL)); }

/* Get the root ape ID of a tree, that is, the first node that is found to not having a parent. */
SEXP Rgetroot(SEXP edges) {
	int *e, n, i,j,tmp;
	SEXP res;
	res = PROTECT(allocVector(INTSXP, 1));
	e = INTEGER(edges);
	n = length(edges);
	/* A node does not have a parent if and only if it doesn't appear in the
           second column of the edge table. So we find the smallest positive integer
           that is missing in the second column. */
	for (i=1; i<n; i+=2)              if ((tmp=abs(e[i]))<=n) e[tmp-1]=abs(e[tmp-1])*-1;
	for (j=0; j<=n/2 && e[j]<=0; ++j) e[j]*=-1;
	INTEGER(res)[0] = j<=n/2 ? j+1 : -1;
	for (++j; j<n; ++j)               e[j] = abs(e[j]);
	UNPROTECT(1);
	return(res);
}

SEXP Rnewnode(SEXP edges, SEXP xtab, SEXP dimtab) {
	int *e, *d=0;	           /* C table for edges, dimtab. */
	int n;			   /* Number of nodes including root */
	int m;			   /* Number of leaves including root */
	struct node **nra;	   /* Node pointers with random access.
				      Invariant: nra[j] points to node with our ID j */
	int j;
	struct node *p, *t;
	
	e = INTEGER(edges);
	n = length(edges) / 2 + 1;
	d = INTEGER(dimtab);
	if (! (nra = calloc(n, sizeof(void*)))) goto NRAFAIL;
	if (! (p = newnode(e[0]-1, d[e[0]-1]))) goto MEMFAIL; /* Allocate the root */
	p->u.rbk.nparam = 0;
	p->u.rbk.xavail = 0;
	p->u.rbk.hessflat = NULL;
	nra[e[0]-1] = p;
	j=0; do {    /*  Invariant: nra[e[j+1]-1] == NULL.  (child is newly encountered)  */
		if (!nra[e[j]-1]) {
			if (! (p = newnode(e[j]-1, d[e[j]-1]))) goto MEMFAIL;
			nra[e[j]-1] = p;
		}
		if (! (p = newnode(e[j+1]-1, d[e[j+1]-1]))) goto MEMFAIL;
		nra[e[j+1]-1] = p;
		if (! nra[e[j]-1]->chd) nra[e[j]-1]->chd = nra[e[j+1]-1];
		else {
			for (t = nra[e[j]-1]->chd; t->nxtsb; t = t->nxtsb);
			t->nxtsb = nra[e[j+1]-1];
		}
	} while ((j+=2)<=2*n-3); /* Invariant: j+2 points to next row if exists. Of course, j <= (2n-2)-1 = 2n-3 if so. */
	if (!isNull(xtab)) {     /* Fill in the X's when available. */
		nra[e[0]-1]->u.rbk.xavail = 1;
		m = length(xtab);
		j=0; do if (e[j+1] <= m) {   /* Fill in the trait (x) if it were a tip  */
				if (! (nra[e[j+1]-1]->ndat.x = malloc(sizeof(double)*nra[e[j+1]-1]->ndat.ku)))
					goto MEMFAIL;
				memcpy(nra[e[j+1]-1]->ndat.x, REAL(VECTOR_ELT(xtab, e[j+1]-1)),
				       nra[e[j+1]-1]->ndat.ku * sizeof(double));
			} while ((j+=2)<=2*n-3);
	}
	fillhidx(nra, e, n, e[0]-1);
	t = nra[e[0]-1];
	free(nra);
	fillndesc(t);
	return Rwrapnode(t);
MEMFAIL:
	error("Rnewnode(): Failed to allocate memory");
NRAFAIL:  
	error("Rnewnode(): Failed to allocate memory");
	return R_NilValue;
}

SEXP Risnullptr(SEXP thing) {
	if (NULL == R_ExternalPtrAddr(thing))
		return ScalarLogical(1);
	else    return ScalarLogical(0);
}

/* Clone a tree skeleton from an existing tree. The new tree contains nothing
   but a basic topology, ID, ndat.ku and rbk, hnbk; similar to Rnewnode.
   Tip values of the resulting tree are NULL. */
struct node *sktrcpywk(struct node *t);
struct node *sktrcpy(struct node *t) {
	struct node *tnew;
	if (!(tnew=newnode(t->id, t->ndat.ku))) error("sktrcpywk(): Failure allocating memory");
	tnew->u.rbk.nparam =t->u.rbk.nparam;	tnew->u.rbk.xavail =0;
	tnew->ndat.ndesc   =t->ndat.ndesc;
	tnew->chd          =sktrcpywk(t->chd);	tnew->nxtsb =sktrcpywk(t->nxtsb);
	return tnew;
}
struct node *sktrcpywk(struct node *t) {
	struct node *tnew;
	if (!t) return NULL;
	if (!(tnew=newnode(t->id, t->ndat.ku))) error("sktrcpywk(): Failure allocating memory");
	tnew->u.hnbk.Phi=t->u.hnbk.Phi;	tnew->u.hnbk.w   =t->u.hnbk.w;
	tnew->u.hnbk.V  =t->u.hnbk.V;	tnew->ndat.ndesc =t->ndat.ndesc;
	tnew->chd = sktrcpywk(t->chd);	tnew->nxtsb = sktrcpywk(t->nxtsb);
	return tnew;
}
/*SEXP R_clone_tree(SEXP Rctree) {
	return Rwrapnode(sktrcpy((struct node *)R_ExternalPtrAddr(Rctree)));
}
*/

size_t difftmp(struct node *t, void *wsp, int kv) {
	struct node *p;
	size_t nbytes=0;
	if (kv != 0) {
#define KU (t->ndat.ku)
#define MAKETEMP(Y,SIZ)     t->ndat.Y=(void*)((char*)wsp+nbytes); nbytes+=(SIZ)*sizeof(double);
		MAKETEMP(a,      KU);          MAKETEMP(HPhi,    KU*kv);     MAKETEMP(Lamb,   KU*KU);  MAKETEMP(dodv,    kv*kv*KU*KU);
		MAKETEMP(dodphi, kv*kv*KU*kv); MAKETEMP(dgamdv,  kv*KU*KU);  MAKETEMP(dgamdw, kv*KU);  MAKETEMP(dgamdphi,kv*KU*kv);
		MAKETEMP(dcdw,   KU);          MAKETEMP(dcdv,    KU*KU);     MAKETEMP(dddv,   KU*KU);
		dcalloc(t->ndat.dlikdv, KU*(KU+1+kv), MEMFAIL);
		t->ndat.dlikdw   = t->ndat.dlikdv + KU*KU;
		t->ndat.dlikdphi = t->ndat.dlikdw + KU;
#undef MAKETEMP
		dzero(wsp, 2*(KU) + 3*(KU)*(KU) + 2*(KU)*kv + (KU)*(KU)*kv + (KU)*kv*kv + (KU)*(KU)*kv*kv + (KU)*kv*kv*kv);
	}
#undef KU
	for (p = t->chd; p; p = p->nxtsb)
		nbytes += difftmp(p, (char*)wsp+nbytes, t->ndat.ku);
	return nbytes;
MEMFAIL:
	error("difftmp: failed to allocate memory");
}

size_t hesstmp(struct node *t, void *wsp, int kv) {
	struct node *p;
	size_t nbytes=0;
#define KU (t->ndat.ku)
#define MAKE(Y,SIZ)  t->ndat.Y=(void*)((char*)wsp+nbytes); nbytes+=(SIZ)*sizeof(double);
	if (kv != 0) {
		MAKE(H, KU*KU);
		MAKE(b, KU);
	}
	MAKE(so,   KU*KU); MAKE(sgam,KU);
	MAKE(sc,   1);     MAKE(invV, KU*KU);
	dzero(wsp, 3*KU*KU+ 2*KU*KU + 1);
#undef MAKE
#undef KU
	for (p = t->chd; p; p = p->nxtsb) nbytes += hesstmp(p, (void*)((char*)wsp+nbytes), t->ndat.ku);
	return nbytes;
}

int allocdfqk(int kr, int knu, int knv, int kmu, int kmv, struct dfdqdk* D) {
	D->R = malloc((knu*kr*kmu*kmu + knu*kr*kmu*kmv + knu*kmu*kmu + knu*kmu*kmv
		       + knu*kmu + knu*knu*kmu*kmu + knu*knu*kmu*kmv + knu*kr + knu) * sizeof(double));
	D->kr  = kr;
	D->knu = knu;
	D->knv = knv;
	D->kmu = kmu;
	D->kmv = kmv;
	if (!(D->R)) return 0;
	return 1;
}
void deldfqk(struct dfdqdk* D) { free(D->R); }

/* This works but rchk doesn't like this.
SEXP Rlistelem(SEXP Rlist, const char *key) {
    SEXP names;
    int len, i;
    i = 0;
    names  = PROTECT(getAttrib(Rlist, R_NamesSymbol));
    len    = length(names);
AGAIN:
    if (i >= len)                                          goto NOTFOUND;
    if (!strcmp(CHAR(PROTECT(STRING_ELT(names, i))), key)) goto FOUND;
    ++i;
    goto AGAIN;
NOTFOUND:
    UNPROTECT(1+i);
    return R_NilValue;
FOUND:
    UNPROTECT(2+i);
    return VECTOR_ELT(Rlist, i);
}
*/

SEXP Rlistelem(SEXP Rlist, const char *key) {
	SEXP names;
	int len, i;
	names  = PROTECT(getAttrib(Rlist, R_NamesSymbol));
	len    = length(names);
	for (i=0;;++i) {
		if (i >= len)                                          goto NOTFOUND;
		if (!strcmp(CHAR(PROTECT(STRING_ELT(names, i))), key)) goto FOUND;
		UNPROTECT(1);
	}
NOTFOUND:
	UNPROTECT(1);
	return R_NilValue;
FOUND:
	UNPROTECT(2);
	return VECTOR_ELT(Rlist, i);
}
int Rsetlistelem(SEXP Rlist, const char *key, SEXP Robj) {
	SEXP names;
	int len, i;
	names  = PROTECT(getAttrib(Rlist, R_NamesSymbol));
	len    = length(names);
	for (i=0;;++i) {
		if (i >= len)                                          goto NOTFOUND;
		if (!strcmp(CHAR(PROTECT(STRING_ELT(names, i))), key)) goto FOUND;
		UNPROTECT(1);
	}
NOTFOUND:
	UNPROTECT(1);
	return 0;
FOUND:
	SET_VECTOR_ELT(Rlist, i, Robj);
	UNPROTECT(2);
	return i;
}
size_t getvwphi_listnum(SEXP Rlist, struct node *t, int kv, double **V, double **w, double **Phi, void *wsp, size_t lwsp) {
	SEXP VwPhi;
	(void)wsp; (void)lwsp;
	int nset=1;
	VwPhi = PROTECT(VECTOR_ELT(Rlist, t->id));
	if (V)	 { nset++;   *V =REAL(PROTECT(VECTOR_ELT(VwPhi, 0))); }
	if (w)	 { nset++;   *w =REAL(PROTECT(VECTOR_ELT(VwPhi, 1))); }
        if (Phi) { nset++; *Phi =REAL(PROTECT(VECTOR_ELT(VwPhi, 2))); }
	UNPROTECT(nset);
	return 0;
}

/* get_VwPhi */
size_t getvwphi_liststr(SEXP Rlist, struct node *t, int kv, double **V, double **w, double **Phi, void *wsp, size_t lwsp) {
	SEXP VwPhi;
	(void)wsp; (void)lwsp;
	int nset=1;
	VwPhi = PROTECT(VECTOR_ELT(Rlist, t->id));
	if (V)	 { nset++;   *V =REAL(PROTECT(Rlistelem(VwPhi,"V")));  }
	if (w)	 { nset++;   *w =REAL(PROTECT(Rlistelem(VwPhi,"w")));  }
        if (Phi) { nset++; *Phi =REAL(PROTECT(Rlistelem(VwPhi,"Phi")));}
	UNPROTECT(nset);
	return 0;
}

/* get_VwPhi */
size_t getvwphi_vec(SEXP Rvec, struct node *t, int kv, double **V, double **w, double **Phi, void *wsp, size_t lwsp) {
	double *par;
	(void) lwsp;
	size_t nbytes=0;
	par = REAL(Rvec);
	if (V) {
		sylgecpy_(wsp, par + t->u.hnbk.V, &(t->ndat.ku));
		nbytes = (t->ndat.ku)*(t->ndat.ku)*sizeof(double);
		*V = wsp;
	}
	if (w)	    *w = par + t->u.hnbk.w;
        if (Phi)  *Phi = par + t->u.hnbk.Phi;
	return nbytes;
}

typedef size_t (*fn_node2siz)(struct node *t, int);
size_t nd_node2siz (struct node *t, int kv) {
	return (2*(t->ndat.ku)*(t->ndat.ku)+2*(t->ndat.ku)+(t->ndat.ku)*kv+2 )*sizeof(double);
	/* return (t->ndat.x ? (t->ndat.ku)*(t->ndat.ku) : 2+(t->ndat.ku)*(1+2*t->ndat.ku))*sizeof(double); */
}
size_t h_node2siz (struct node *t, int kv) {
	size_t nd;
	nd = nd_node2siz(t, kv);
	return (nd<DFQKSIZ*2)?DFQKSIZ*2:nd;
}

size_t difftmp_node2siz (struct node *t, int kv) {
	return (2*(t->ndat.ku) + 3*(t->ndat.ku)*(t->ndat.ku) +
		 2*(t->ndat.ku)*kv + (t->ndat.ku)*(t->ndat.ku)*kv + (t->ndat.ku)*kv*kv +
		 (t->ndat.ku)*(t->ndat.ku)*kv*kv + (t->ndat.ku)*kv*kv*kv)
		* sizeof(double);
}
size_t hessdifftmp_node2siz (struct node *t, int kv) {
	return difftmp_node2siz(t, kv) + (
		3*(t->ndat.ku)*(t->ndat.ku)+2*(t->ndat.ku)+1 +  /* In hesstmp() */
		(t->ndat.x ? (t->ndat.ku)*kv+(t->ndat.ku) : (t->ndat.ku)*(4*(t->ndat.ku)+(1+kv)))    /* In fillhnbk() */
		)*sizeof(double);
}

void stack_siz (struct node *t, int kv, size_t swsp, size_t *lwsp, fn_node2siz nbytes) {
	struct node *p;
	size_t N;
	if(*lwsp < (swsp += (N = nbytes(t, kv)))) *lwsp+=N;
	for (p = t->chd; p; p=p->nxtsb) stack_siz(p, t->ndat.ku, swsp, lwsp, nbytes);
}
void stack_siz_fixed (struct node *t, size_t swsp, size_t *lwsp, size_t nbytes) {
	struct node *p;
	size_t N;
	if(*lwsp < (swsp += (N = nbytes))) *lwsp+=N;
	for (p = t->chd; p; p=p->nxtsb) stack_siz_fixed(p, swsp, lwsp, nbytes);
}
void sumnode_siz (struct node *t, int kv, size_t *lwsp, fn_node2siz nbytes) {
	struct node *p;
	*lwsp += nbytes(t, kv);
	for (p = t->chd; p; p=p->nxtsb) sumnode_siz(p, t->ndat.ku, lwsp, nbytes);
}
void sumnode_siz_fixed (struct node *t, int kv, size_t *lwsp, size_t nbytes) {
	*lwsp += (1+t->ndat.ndesc) * nbytes;
}


#define CGODBYTES(KU) ((KU)*(KU+1)+2)*sizeof(double)
#define ZEROCGOD(WSP, PTR_C, PTR_GAM, PTR_O, PTR_D, KU) {	  \
		PTR_C= (double*)(WSP);                    PTR_D= (double*)PTR_C+1; \
		PTR_GAM= (double*)PTR_D+1;     PTR_O= (double*)PTR_GAM+KU; \
		*((double*)PTR_C)=0;           *((double*)PTR_D)=0;	\
		dzero(PTR_GAM, (KU));          dzero(PTR_O, (KU)*(KU));	\
	}

DEF_TCGOD(c_dtcgod) {
	dtcgod_(v, w, phi, t->ndat.x, &kv, &(t->ndat.ku), c, gam, o, d, t->ndat.dodv, t->ndat.dodphi, t->ndat.dgamdv,
		t->ndat.dgamdw, t->ndat.dgamdphi, t->ndat.dcdw, t->ndat.dcdv, t->ndat.dddv, info);
}
DEF_TCGOD(c_htcgod) {
	htcgod_(v, w, phi, t->ndat.x, &kv, &(t->ndat.ku), c, gam, o, d, t->ndat.invV, t->ndat.b, t->ndat.dodv, t->ndat.dodphi,
		t->ndat.dgamdv, t->ndat.dgamdw, t->ndat.dgamdphi, t->ndat.dcdw, t->ndat.dcdv, t->ndat.dddv, info);
}
DEF_TCGOD(c_ndtcgod) {
	ndtcgod_(v, w, phi, t->ndat.x, &kv, &(t->ndat.ku), c, gam, o, d, info);
}

DEF_MERG(c_ndmerg) { ndmerg_(v, w, phi, &(kv), &(t->ndat.ku), c1, gam1, o1, d1, c, gam, o, d, info); }
DEF_MERG(c_dmerg) { dmerg_(v, w, phi, &(kv), &(t->ndat.ku), c1, gam1, o1, d1, c, gam, o, d,      \
			   t->ndat.a, t->ndat.HPhi, t->ndat.Lamb, t->ndat.dodv, t->ndat.dodphi,  \
			   t->ndat.dgamdv, t->ndat.dgamdw, t->ndat.dgamdphi,                     \
			   t->ndat.dcdw, t->ndat.dcdv, t->ndat.dddv, info); }
DEF_MERG(c_hmerg) { hmerg_(v, w, phi, &(kv), &(t->ndat.ku), c1, gam1, o1, d1, c, gam, o, d,               \
			   t->ndat.a, t->ndat.b, t->ndat.invV, t->ndat.H, t->ndat.HPhi, t->ndat.Lamb,     \
			   t->ndat.dodv, t->ndat.dodphi, t->ndat.dgamdv, t->ndat.dgamdw, t->ndat.dgamdphi,\
			   t->ndat.dcdw, t->ndat.dcdv, t->ndat.dddv, info); }

void ndphylik(struct node *t, SEXP VwPhi_L, double *x0, int k, double *lik, fn_getvwphi get_VwPhi) {
	struct node *p;
	double *c, *gam, *o, *d;
	int info;
	void *wsp;
	size_t lwsp=0;
	for (p=t->chd; p; p=p->nxtsb) stack_siz(p, t->ndat.ku, 0, &lwsp, &nd_node2siz);
	lwsp += CGODBYTES(t->ndat.ku);
	if (! (wsp = malloc(lwsp)))      goto MEMFAIL;
	/* if (! (wsp = malloc(80*1024*1024*8)))      goto MEMFAIL; */
	ZEROCGOD(wsp, c, gam, o, d, t->ndat.ku);
	for (p = t->chd; p; p = p->nxtsb) {
		dndgcgod(p, VwPhi_L, t->ndat.ku, c, gam, o, d, get_VwPhi, &c_ndtcgod, &c_ndmerg, wsp, CGODBYTES(t->ndat.ku), lwsp, &info);
		switch (info) {
		case -1:
			free(wsp);
			error("*tcgod(): V is numerically non-positive-definite!");
		case -2:
			free(wsp);
			error("mergintern_(): Numerically non-positive-definiteness in the Woodbury formula!");
		case 0:
			break;
		default:
			free(wsp);
			error("Unknown error from dndgcgod: a bug in the C code?");
		};
	}
	phygausslik_(c, gam, o, d, x0, &(t->ndat.ku), &k, lik);
	free(wsp);

	return;
MEMFAIL:
	error("phylik(): Error allocating memory for c, gamma, Omega, Delta.");
}


void dphylik(struct node *t, SEXP VwPhi_L, double *x0, int k, double *lik, fn_getvwphi get_VwPhi, int nthd) {
	struct node *p;
	int info;
	double *c, *gam, *o, *d;
	void *wsp;
	size_t swsp=0, lwsp=0;
	for (p=t->chd; p; p=p->nxtsb) stack_siz(p, t->ndat.ku, 0, &lwsp, &nd_node2siz);
	sumnode_siz(t, t->ndat.ku, &lwsp, &difftmp_node2siz);
	lwsp += CGODBYTES(t->ndat.ku);
	if (! (wsp = malloc(lwsp)))      goto MEMFAIL;
	/* if (! (wsp = malloc(80*1024*1024*8)))      goto MEMFAIL; */
	swsp += difftmp(t, wsp, 0);
	ZEROCGOD((char*)wsp+swsp, c, gam, o, d, t->ndat.ku);
	swsp += CGODBYTES(t->ndat.ku);
	for (p = t->chd; p; p = p->nxtsb) {
		dndgcgod(p, VwPhi_L, t->ndat.ku, c, gam, o, d, get_VwPhi, &c_dtcgod, &c_dmerg, wsp, swsp, lwsp, &info);
		switch (info) {
		case -1:
			free(wsp);
			error("*tcgod(): V is numerically non-positive-definite!");
		case -2:
			free(wsp);
			error("mergintern_(): Numerically non-positive-definiteness in the Woodbury formula!");
		case 0:
			break;
		default:
			free(wsp);
			error("Unknown error from dndgcgod: a bug in the C code?");
		};
	}
	phygausslik_(c, gam, o, d, x0, &(t->ndat.ku), &k, lik);
	grad(t, x0, nthd);
	free(wsp);
	return;
MEMFAIL:
	error("dphylik(): Error allocating memory. ");
}

void hphylik(struct node *t, SEXP VwPhi_L, double *x0, int k, double *lik, fn_getvwphi get_VwPhi, double *hessmem, double *dir, int ndir, int nthd) {
	struct node *p;
	double *d;
	void *wsp;
	size_t lwsp=0, swsp=0;
	int ret, info;
	for (p=t->chd; p; p=p->nxtsb) stack_siz(p, t->ndat.ku, 0, &lwsp, &h_node2siz);
	sumnode_siz(t, t->ndat.ku, &lwsp, &hessdifftmp_node2siz);
	lwsp += sizeof(double);
	if (! (wsp = malloc(lwsp)))      goto MEMFAIL;
	swsp += difftmp(t, wsp, 0);
	swsp += hesstmp(t, (char*)wsp+swsp, 0);
	d=(void*)((char*)wsp+swsp);
	*d=0;
	swsp += sizeof(double);
	for (p = t->chd; p; p = p->nxtsb) {
		hgcgod(p, VwPhi_L, t->ndat.ku, t->ndat.sc, t->ndat.sgam, t->ndat.so, d,
		       get_VwPhi, wsp, swsp, lwsp, &info);
		switch (info) {
		case -1:
			free(wsp);
			error("*tcgod(): V is numerically non-positive-definite!");
		case -2:
			free(wsp);
			error("mergintern_(): Numerically non-positive-definiteness in the Woodbury formula!");
		case 0:
			break;
		default:
			free(wsp);
			error("Unknown error from hgcgod: a bug in the C code?");
		};
	}
	phygausslik_(t->ndat.sc, t->ndat.sgam, t->ndat.so, d, x0, &(t->ndat.ku), &k, lik);
	grad(t, x0, nthd);
	swsp -= sizeof(double);
	ret = hess(t, VwPhi_L, x0, get_VwPhi, wsp, swsp, lwsp, hessmem, dir, ndir, nthd);
	free(wsp);
	switch (ret) {
	case 3:
		goto MEMFAIL;
	case 2:
		error("hphylik(): The C stack is not large enough for your problem size.");
	case 1:
		error("hphylik(): Computation interrupted by user.");
	case 0:
		;		/* Good. */
	};
	return;
MEMFAIL:
	error("hphylik(): Error allocating memory in hphylik()");
}


/* DOUBLE CHECK THIS! */
void mkdiffbk(struct diffbk *p, int kr, int kb) { /* kr is dim of root and kb the parent. */
	double *tmp;
	if(! (tmp = malloc((kr*kb + kb*kb + kb)*sizeof(double))))  goto MEMFAIL;
	dzero(tmp, kr*kb + kb*kb + kb);
	p->z = tmp;
	p->K = tmp + kb;
	p->F = p->K + kb * kb;
	return;
MEMFAIL:
	error("mkdiffbk(): Error allocating memory");
}
void freediffbk(struct diffbk *p) { free(p->z); }

void grad(struct node *t, double *x0, int nthd) {
	/* Pre-condition: t is the global root. */
	struct diffbk bk = {0};
	mkdiffbk(&bk, t->ndat.ku, t->ndat.ku);
	diagone_(bk.F, &(t->ndat.ku));
	__PRAGMA__("omp parallel num_threads(nthd)")
	{
		__PRAGMA__("omp master")
		{
			struct node *p, *q;
			for (p = t->chd; p; p = p->nxtsb) {
				/* The direct children of the root requires different treatment. */
				ddcr_(&(t->ndat.ku), &(p->ndat.ku), x0, p->ndat.dodv, p->ndat.dodphi,
				      p->ndat.dgamdv, p->ndat.dgamdw, p->ndat.dgamdphi, p->ndat.dcdw, p->ndat.dcdv,
				      p->ndat.dddv, p->ndat.dlikdv, p->ndat.dlikdw, p->ndat.dlikdphi);
				for (q = p->chd; q; q = q->nxtsb)
					gradwk(q, p, t, x0, bk, t->ndat.ku);
			}
			__PRAGMA__("omp taskwait")
		}
		__PRAGMA__("omp barrier")
	}
	freediffbk(&bk);
}
void gradwk(struct node *a, struct node *b, struct node *c,
	    double *x0, struct diffbk bk, int kr) {
	/* Pre-condition: 1. kr that of the global root.
	                  2. a is neither the global root nor its direct children
			  3. c → b → a
			  4. bk contains F,k, and z of parent */
	struct node *p;
	struct diffbk newbk;
	mkdiffbk(&newbk, kr, b->ndat.ku);
	fzkdown_(bk.F, bk.z, bk.K, b->ndat.HPhi, b->ndat.a, b->ndat.Lamb, x0,
		 &(c->ndat.ku), &(b->ndat.ku), &(a->ndat.ku), &kr,
		 a->ndat.dodv, a->ndat.dodphi, a->ndat.dgamdv, a->ndat.dgamdw, a->ndat.dgamdphi,
		 a->ndat.dcdw, a->ndat.dcdv, a->ndat.dddv, a->ndat.dlikdv, a->ndat.dlikdw,
		 a->ndat.dlikdphi, newbk.F, newbk.z, newbk.K);
	for (p = a->chd; p; p = p->nxtsb){
		__PRAGMA__("omp task firstprivate(p,a,b,x0,newbk,kr) if (p->ndat.ndesc >=40)")
		gradwk(p, a, b, x0, newbk, kr);
	}
	__PRAGMA__("omp taskwait")
	freediffbk(&newbk);
}

int maxdim(struct node *t) {    /* Return maximum no. of dimen. of traits in the tree */
	struct node *p;
	int tmp, curmax = t->ndat.ku;
	for (p=t->chd; p; p=p->nxtsb)  curmax = (curmax<(tmp=maxdim(p))) ? tmp : curmax;
	return curmax;
}
void freellst(struct llst *l) {
	struct llst *tmp;
FREEL:
	if (!l) return;
	tmp = l->nxt;
	free(l);
	l = tmp;
	goto FREEL;
}
void freellstptr(struct llstptr *l) {
	struct llstptr *tmp;
FREEL:
	if (!l) return;
	tmp = l->nxt;
	free(l);
	l = tmp;
	goto FREEL;
}


void fillhnbk_wk (struct node *t, SEXP VwPhi_L, int kv, fn_getvwphi get_VwPhi, void *wsp, size_t *swsp, size_t lwsp);
size_t fillhnbk (struct node *t, SEXP VwPhi_L, fn_getvwphi get_VwPhi, void *wsp, size_t swsp, size_t lwsp) {
	size_t orig_swsp = swsp;
	struct node *p;
	for (p = t->chd; p; p = p->nxtsb)
		fillhnbk_wk(p, VwPhi_L, t->ndat.ku, get_VwPhi, wsp, &swsp, lwsp);
	return swsp - orig_swsp;
}
void fillhnbk_wk (struct node *t, SEXP VwPhi_L, int kv, fn_getvwphi get_VwPhi, void *wsp, size_t *swsp, size_t lwsp) {
	double *w, *Phi, *tmp;
	struct node *p;
	get_VwPhi(VwPhi_L, t, kv, NULL, &w, &Phi, (char*)wsp+*swsp, lwsp-*swsp);
#define KU (t->ndat.ku)
	if (t->ndat.x) {
		t->u.hnbk.u.hsbktip.invVPhi = (void*)((char*)wsp+*swsp);   *swsp+=(KU*kv+KU)*sizeof(double);
		t->u.hnbk.u.hsbktip.invVxw  = t->u.hnbk.u.hsbktip.invVPhi + KU*kv;
		dzero(t->u.hnbk.u.hsbktip.invVPhi, KU*kv+KU);
		hselfbktip_(t->ndat.invV, t->ndat.x, w, Phi, &kv, &KU,
			    t->u.hnbk.u.hsbktip.invVPhi, t->u.hnbk.u.hsbktip.invVxw);
	} else {
		/* Maybe we can save some memory here? */
		tmp = (void*)((char*)wsp+*swsp);    *swsp+=(KU*(3*KU+(1+kv)))*sizeof(double);
		t->u.hnbk.u.hsbkgen.invVLsO    = tmp;    tmp += KU*KU;
		t->u.hnbk.u.hsbkgen.invVLsOPhi = tmp;    tmp += KU*kv;
		t->u.hnbk.u.hsbkgen.VmVLV      = tmp;    tmp += KU*KU;
		t->u.hnbk.u.hsbkgen.invVLb     = tmp;    tmp += KU;
		t->u.hnbk.u.hsbkgen.Hto        = tmp;
		hselfbkgen_(t->ndat.invV, t->ndat.Lamb, t->ndat.so, Phi, t->ndat.b, t->ndat.H,
			    &kv, &KU, t->u.hnbk.u.hsbkgen.invVLsO, t->u.hnbk.u.hsbkgen.invVLsOPhi,
			    t->u.hnbk.u.hsbkgen.VmVLV, t->u.hnbk.u.hsbkgen.invVLb, t->u.hnbk.u.hsbkgen.Hto);
	}
	for (p = t->chd; p; p = p->nxtsb)
		fillhnbk_wk(p, VwPhi_L, KU, get_VwPhi, (char*)wsp, swsp, lwsp);
#undef KU
}

static double *thrpv_bilinmat;	/* Thread-private. Only used in online bilinear form updates (that is, when dir is non-NULL). */
__PRAGMA__("omp threadprivate(thrpv_bilinmat)")

int hess(struct node *t, SEXP VwPhi_L, double *x0, fn_getvwphi get_VwPhi, void *wsp, size_t swsp,
	 size_t lwsp, double *extrmem, double *dir, int ndir, int nthd) {
	/* Pre-condition: t is the global root. */
	struct hessglbbk gbk;	/* Notice this is on the stack. */
	int ictx,i,j,m,n, wkret;
	struct node *p;

#define NHESS (((t->u.rbk.nparam) * ((t->u.rbk.nparam)+1))/2)
	if (!dir) {		/* If dir is non-null then extrmem holds a ndir*ndir matrix, already allocated in R-managed memory */
		if (extrmem) {
			if ((t->u.rbk.hessflat_needs_free) && (t->u.rbk.hessflat))
				free(t->u.rbk.hessflat);
			t->u.rbk.hessflat = extrmem;
			t->u.rbk.hessflat_needs_free = 0;
		} else {
			if (!(t->u.rbk.hessflat)) {
				if (!(t->u.rbk.hessflat= malloc(NHESS*sizeof(double))))
					goto MEMFAIL;
				t->u.rbk.hessflat_needs_free = 1;
				dzero(t->u.rbk.hessflat, NHESS);
			}
		}
		dzero(t->u.rbk.hessflat, (size_t)(NHESS));
	} else {
		if ((t->u.rbk.hessflat_needs_free) && (t->u.rbk.hessflat))
			free(t->u.rbk.hessflat);
		t->u.rbk.hessflat = extrmem;
		t->u.rbk.hessflat_needs_free = 0;
	}
	
#undef NPARAM
	swsp += fillhnbk(t, VwPhi_L, get_VwPhi, wsp, swsp, lwsp);
	for (p = t->chd; p; p = p->nxtsb) { /* The direct children of the root needs special treatment */
		double *w;
		get_VwPhi(VwPhi_L, p, t->ndat.ku, NULL, &w, NULL, (char*)wsp+swsp, lwsp-swsp);
		ictx = IVV;
		/* printf("NODE_ID: %d-%d  (A)\n", p->id+1, p->id+1); */
		for (n=1; n <= p->ndat.ku; ++n)
			for (m=n; m <= p->ndat.ku; ++m) {
				i=m; j=n;
			DOCOL:
				while (i <= p->ndat.ku)  hessselftop(p, t->ndat.ku, ictx,i++,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);
				if ((++j) <= p->ndat.ku) {
					i = j;
					goto DOCOL;
				}
			}
		ictx = IVPHI;
		for (n=1; n <= t->ndat.ku; ++n) for (m=1; m <= p->ndat.ku; ++m)
			for (j=1; j <= p->ndat.ku; ++j)	for (i=j; i <= p->ndat.ku; ++i)
				hessselftop(p, t->ndat.ku, ictx, i,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);
		ictx = IVW;
		for (m=1; m <= p->ndat.ku; ++m)   for (j=1; j <= p->ndat.ku; ++j)   for (i=j; i <= p->ndat.ku; ++i)
			hessselftop(p, t->ndat.ku, ictx, i,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);
		ictx = IPHIPHI;
		for (n=1; n <= t->ndat.ku; ++n)
			for (m=1; m <= p->ndat.ku; ++m) {
				i=m; j=n;
			DOCOL2:
				while (i <= p->ndat.ku)  hessselftop(p, t->ndat.ku, ictx,i++,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);
				if ((++j) <= t->ndat.ku) {
					i=1;
					goto DOCOL2;
				}
			}
		ictx = IPHIW;
		for (m=1; m <= p->ndat.ku; ++m)   for (j=1; j <= t->ndat.ku; ++j)   for (i=1; i <= p->ndat.ku; ++i)
			hessselftop(p, t->ndat.ku, ictx,i,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);
		ictx = IWW;
		for (m=1; m <= p->ndat.ku; ++m)   for (i=m; i <= p->ndat.ku; ++i)
			hessselftop(p, t->ndat.ku, ictx,i,j,m,n,x0,t, t->u.rbk.hessflat,dir,ndir);

		if (!(p->ndat.x)) { /* Compute cross second derivatives of the node itself and all of its descendants */
			struct dfdqdk *dfqk1new_ch;
			struct {
				int kv;
				struct node *n, *p;
				struct dfdqdk *dfqk1_ch;
				struct dfdqdk *dfqk1new_ch;
			} *stdesc, curdesc;
			int j;
			struct node *n;
			if (! (dfqk1new_ch = calloc(DFQKSIZ,1)))                                                  goto MEMFAIL;
			if ( !allocdfqk(t->ndat.ku, p->ndat.ku, p->ndat.ku, p->ndat.ku, t->ndat.ku, dfqk1new_ch)) goto MEMFAIL;
			/* If it's a tip then invVLsOPhi isn't defined. */
			dfqk_mmp1_(dfqk1new_ch, p->ndat.H, p->ndat.HPhi, w, p->ndat.a, p->ndat.Lamb,
				   p->ndat.invV, p->u.hnbk.u.hsbkgen.invVLsOPhi, &(p->ndat.ku), &(t->ndat.ku));
			if (!(stdesc = malloc(sizeof(*stdesc) * PTSTACKLEN))) {
				deldfqk(dfqk1new_ch);
				free(dfqk1new_ch);
				goto MEMFAIL;
			}
			for (n = p->chd; n; n = n->nxtsb) {
				curdesc.kv=p->ndat.ku;  curdesc.n=n;  curdesc.dfqk1_ch = dfqk1new_ch;
				j = 0;
DOWNDESC:
				/* HESS_WRITE */
				if (dir)
					tntmdir_(&(t->ndat.ku), &(curdesc.kv), &(curdesc.n->ndat.ku), &(t->ndat.ku), &(p->ndat.ku), curdesc.dfqk1_ch,
						 curdesc.n->ndat.dodv, curdesc.n->ndat.dodphi, curdesc.n->ndat.dgamdv, curdesc.n->ndat.dgamdw,
						 curdesc.n->ndat.dgamdphi, x0, extrmem, dir, &ndir, &(t->u.rbk.nparam), &(p->u.hnbk.Phi), &(p->u.hnbk.V),
						 &(p->u.hnbk.w), &(curdesc.n->u.hnbk.Phi), &(curdesc.n->u.hnbk.V), &(curdesc.n->u.hnbk.w));
				else
					tntm_(&(t->ndat.ku), &(curdesc.kv), &(curdesc.n->ndat.ku), &(t->ndat.ku), &(p->ndat.ku), curdesc.dfqk1_ch,
					      curdesc.n->ndat.dodv, curdesc.n->ndat.dodphi, curdesc.n->ndat.dgamdv, curdesc.n->ndat.dgamdw,
					      curdesc.n->ndat.dgamdphi, x0, t->u.rbk.hessflat, &(t->u.rbk.nparam), &(p->u.hnbk.Phi), &(p->u.hnbk.V),
					      &(p->u.hnbk.w), &(curdesc.n->u.hnbk.Phi), &(curdesc.n->u.hnbk.V), &(curdesc.n->u.hnbk.w));
				if (curdesc.n->chd) {
					if (! (curdesc.dfqk1new_ch = calloc(DFQKSIZ,1))) goto MEMFAIL;
					if (! allocdfqk(t->ndat.ku, curdesc.n->ndat.ku, curdesc.kv,
							p->ndat.ku, t->ndat.ku, curdesc.dfqk1new_ch)) goto MEMFAIL;
					tndown_(curdesc.dfqk1_ch, curdesc.n->ndat.HPhi, curdesc.n->ndat.a, &(curdesc.kv),
						&(curdesc.n->ndat.ku), &(t->ndat.ku), &(p->ndat.ku), curdesc.dfqk1new_ch);
					for (curdesc.p = curdesc.n->chd; curdesc.p; curdesc.p = curdesc.p->nxtsb) {
						if (j >= PTSTACKLEN)             goto STACKFAIL;
						stdesc[j++] = curdesc;
						curdesc.kv=curdesc.n->ndat.ku;  curdesc.dfqk1_ch=curdesc.dfqk1new_ch;
						curdesc.n =curdesc.p;           curdesc.p=NULL;
						curdesc.dfqk1new_ch=NULL;
						goto DOWNDESC;
UPDESC:
						curdesc = stdesc[--j];
					}
					deldfqk(curdesc.dfqk1new_ch);
					free(curdesc.dfqk1new_ch);
				}
				if (j) goto UPDESC;
			}
			free(stdesc);
			deldfqk(dfqk1new_ch);
			free(dfqk1new_ch);
		}
		initgbk(&gbk, t, p, maxdim(t));
		wkret = 0;
		/* In OpenMP 3.1 specification, it's not explicitly written that there is an implicit
		   flush at taskwait, but there is a flush at new task starting. So we need to flush
		   ourselves. Also, calling flush without argument means that the malloc'd areas are
		   also all flushed, but when called with argument only the pointers to malloc'd memory
		   is flushed. */
		__PRAGMA__("omp parallel shared(wkret) num_threads(nthd)")
		{
			thrpv_bilinmat = NULL; /* Thread private. */
			if (dir) {
				if (! (thrpv_bilinmat = malloc(ndir * ndir * sizeof(double)))) {
					__ATOMIC_WRITE__
					wkret = 3;
				} else {
					dzero(thrpv_bilinmat, ndir*ndir);
				}
			}
			__PRAGMA__("omp flush")
			__PRAGMA__("omp barrier")
			/* If some threads failed their allocation then all threads should finish itself up, ending the parallel region.  */
			if (wkret == 3) goto END_THREAD;
			__PRAGMA__("omp master")
			{
				int interrupted = 0;
				struct node *q;
				for (q = p->chd; q; q = q->nxtsb) {
					wkret=hessglobwk(q, p, &gbk, x0, t, VwPhi_L, get_VwPhi, wsp, swsp, lwsp, &interrupted, extrmem, dir, ndir);
					__PRAGMA__("omp flush");
					if (wkret || interrupted) break;
				}
				/* Now wait for everything to finish... */
				__PRAGMA__("omp taskwait")
			}
			__PRAGMA__("omp flush")
			__PRAGMA__("omp barrier")
			if (dir) {
				__PRAGMA__("omp critical")
				{
					int k;
					for (k=0; k<ndir*ndir; ++k) {
						extrmem[k] += thrpv_bilinmat[k];
					}
				}
			}
END_THREAD:
			free(thrpv_bilinmat);
			__PRAGMA__("omp barrier")
		}
		delgbk(gbk);
		if (wkret) break;
	} /* for struct node *p... */
	return wkret;
MEMFAIL:
	return 3;
STACKFAIL:
	return 2;
}

void delgbk(struct hessglbbk gbk) {
	freellst(gbk.fmlfm);    freellst(gbk.qm);
	freellst(gbk.fm);	freellstptr(gbk.a);
}
void initgbk(struct hessglbbk *gbk, struct node *rt, struct node *p, int maxdim) {
	gbk->mdim = maxdim;
	if ( !(gbk->fmlfm = calloc(1, sizeof(struct llst) + gbk->mdim*gbk->mdim*sizeof(double))) /* Yes, use mdim */
	     || !(gbk->fm = calloc(1, sizeof(struct llst) + gbk->mdim*gbk->mdim*sizeof(double)))
	     || !(gbk->qm = calloc(1, sizeof(struct llst) + gbk->mdim * sizeof(double)))
	     || !(gbk->a  = calloc(1, sizeof(struct llstptr))))
		goto MEMFAIL;
	gbk->fmlfm->siz = p->ndat.ku;     /* Note that `siz` has different interpretations */
	gbk->qm->siz    = p->ndat.ku;
	gbk->fm->siz    = rt->ndat.ku;    /* Store the second dimension cus the first is always passed around */
	gbk->a->siz     = p->ndat.ku;
	gbk->a->dat     = p->ndat.a;
	memcpy(gbk->fmlfm->dat, p->ndat.Lamb, (p->ndat.ku)*(p->ndat.ku)  * sizeof(double));
	memcpy(gbk->qm->dat,    p->ndat.a,    (p->ndat.ku)               * sizeof(double));
	memcpy(gbk->fm->dat,    p->ndat.HPhi, (p->ndat.ku)*(rt->ndat.ku) * sizeof(double));
	return;
MEMFAIL:
	error("initgbk(): Error allocating memory for internal book-keeping.");
}

void llstcpy(struct llst **dst, const struct llst *src, int blksiz) {
	int siz = sizeof(struct llst) + blksiz*sizeof(double);
AGAIN:
	*dst = malloc(siz);
	if (! *dst) goto MEMFAIL;
	memcpy(*dst, src, siz);
	if (src->nxt) {
		dst = &((*dst)->nxt);
		src = src->nxt;
		goto AGAIN;
	}
	return;
MEMFAIL:
	error("llstcpy(): Error allocating memory.");
}
void llstptrcpy(struct llstptr **dst, const struct llstptr *src) {
AGAIN:
	*dst = malloc(sizeof(struct llstptr));
	if (! *dst) goto MEMFAIL;
	memcpy(*dst, src, sizeof(struct llstptr));
	if (! src->nxt) return;
	dst = &((*dst)->nxt);
	src = src->nxt;
	goto AGAIN;
MEMFAIL:
	error("llstptrcpy(): Error allocating memory.");
}
void gbkcpy(struct hessglbbk **dst, const struct hessglbbk *src) {
	*dst = malloc(sizeof(struct hessglbbk));
	if (! *dst) goto MEMFAIL;
	llstcpy    (&((*dst)->fmlfm), src->fmlfm, (src->mdim)*(src->mdim));
	llstcpy    (&((*dst)->fm),    src->fm,    (src->mdim)*(src->mdim));
	llstcpy    (&((*dst)->qm),    src->qm,    src->mdim);
	llstptrcpy (&((*dst)->a),     src->a);
	(*dst)->mdim = src->mdim;
	return;
MEMFAIL:
	error("gbkcpy(): Error allocating memory.");
}

/* BOTTLE NECK */
/* Compute Hessians of parameters of outside-m's subtree wrt to m */
void walk_alpha (struct node *pv_rt, double *pv_x0, int pv_i, struct node **pv_ancestry,
		 void *pv_starters, int pv_kv, int pv_mdim, int *interrupted, double *extrmem, double *dir, int ndir) {
	/* Performance note:
	   1. Putting the parallelisation pragma at the for loop below will slow down the compuation by half.
	   2. I have no idea why PGI sucks so much with this piece of code. Usually PGI is quite fast,
	      but at least the numerical output is correct and precise.
	*/
	struct {                        /* States of the inner stack */
		int kv;
		struct node *n, *p;
		struct dfdqdk *dfqk1_ch;
		struct dfdqdk *dfqk1new_ch;
	} *stalpha;                        /* Each k has different stack states */
	struct {
		size_t hessptr;
		struct node *n;
		int knv;
		size_t lblk;
	} *pushback;
	size_t lpushback, pushbackptr;
	double *hessmem; size_t lhessmem, lblk, hessptr;
	int q, yes;
	struct node *z;
	size_t swsp_a, lwsp_a;
	void *wsp_a;
	int c;
	size_t b;
	int k,j;
	for (k=0; k < pv_i; ++k) {          /* Now run the recursion for each k (for each beta, that is) */
		wsp_a = NULL;
		lpushback = 0; pushbackptr = 0;
		lhessmem = 0; lblk = 0; hessptr = 0;

		swsp_a=0; lwsp_a=0;
		sumnode_siz_fixed(pv_ancestry[k], 0, &lwsp_a, sizeof(*stalpha));
		swsp_a += lwsp_a; /* Stack pointer advanced to a size enough for walking the whole tree.
				     Stack_size/sizeof(*stalpha) <= ndesc, mathematically guaranteed. */
		lwsp_a = 0;
		sumnode_siz_fixed(pv_ancestry[k], 0, &lwsp_a, DFQKSIZ+sizeof(*stalpha));
#define PAIR_HESS_SIZE(KNV,KNU,KMV,KMU) (((KNU*(KNU+1))/2)*((KMU*(KMU+1))/2)+ KNV*KNU*((KMU*(KMU+1))/2) + KNU*(KMU*(KMU+1))/2 +\
					 ((KNU*(KNU+1))/2)* KMV*KMU         + KNV*KNU*KMV*KMU           + KNU*KMV*KMU         +\
					 ((KNU*(KNU+1))/2)* KMU             + KNV*KNU*KMU               + KNU*KMU)
		sumnode_siz_fixed(pv_ancestry[k], 0, &lhessmem,
				  sizeof(double)*PAIR_HESS_SIZE(pv_mdim,pv_mdim,pv_mdim,pv_mdim));
		lwsp_a += lhessmem;
		sumnode_siz_fixed(pv_ancestry[k], 0, &lpushback, sizeof(*pushback));
		lwsp_a += lpushback;
		
		/* In C11, POSIX.1-2008, glibc >= 2.15, musl, and Microsoft, malloc() and free() is thread safe.
		   Everybody is assuming this nowadays. But don't use something like PGI's -Mipa=inline etc.
		   to mess with the libc binary...  */
		if (! (wsp_a = malloc(lwsp_a))) {
			/* What to do when out of memory here...? */
			return;
		}
		memset(wsp_a, 0, lwsp_a);
		stalpha  =wsp_a;
		hessmem  =(void*)((char*)wsp_a+swsp_a); swsp_a +=lhessmem; /* Skip swsp_a to leave room for the stack. */
		pushback =(void*)((char*)wsp_a+swsp_a); swsp_a +=lpushback;
		/* Start recursion to compute hessian values */
		stalpha[0].dfqk1_ch = (void*)((char*)wsp_a+swsp_a); swsp_a+=DFQKSIZ;
		/* 
		   The POSIX standard says memcpy MUST BE thread-safe:
		   
		   https://pubs.opengroup.org/onlinepubs/9699919799/functions/V2_chap02.html#tag_15_09_01
		   
		   Microsoft website doesn't say anything about thread safety, but I can't imagine their employees
		   can write a memcpy() routine using static global tmp variables in that kind of a coorporate
		   culture... BTW the StackOverflow crowd are funny... :)
		   
		   https://stackoverflow.com/questions/15145152/is-memcpy-process-safe
		 */
		memcpy(stalpha[0].dfqk1_ch, (char*)pv_starters+DFQKSIZ*k, DFQKSIZ);
		q = 0;
		/* Among all beta's children there is one that is identical to the next beta (pv_ancestry[k+1]).
		   We only loop through children which is at the right-hand-side of the the next beta. Correctness
		   proof:

		   Denote, respectively, the set of all nodes which is in some of the left, and right, sibling
		   subtrees of beta in a walk_alpha(global=m, iterate=alpha) as L_ma and R_ma. Note that
		   L_ma = R_am and L_am = R_ma. Only the pairs {m,alpha} where alpha in R_ma is computed in
		   walk_alpha(global=m, iterate=alpha), and because R_ma = L_am, the process
		   walk_alpha(iterate=alpha, global=m) did not touch the pair {m,alpha}. In other words, 
		   {m,alpha} for alpha in R_ma is visited once and only once. Similarly, {m,alpha} for alpha in L_ma
		   is not visited by walk_alpha(global=m, iterate=alpha); but it is visited exactly once in 
		   walk_alpha(global=alpha, iterate=m) because L_ma = R_am. So {m,alpha} is visited exactly once
		   globally.
		*/
		for (z = pv_ancestry[k]->chd; z != pv_ancestry[k+1]; z = z->nxtsb) continue;
		for (z = z->nxtsb; z; z = z->nxtsb) {
			/* Invariants: stalpha[0].dfqk1_ch never changes from before the for loop to after. */
			stalpha[q].kv = pv_ancestry[k]->ndat.ku; stalpha[q].n = z;
DOWNALPHA:
#define KR  (pv_rt->ndat.ku)
#define KNV (stalpha[q].kv)
#define KNU (stalpha[q].n->ndat.ku)
#define KMV (pv_kv)
#define KMU (pv_ancestry[pv_i]->ndat.ku)
			lblk = PAIR_HESS_SIZE(KNV,KNU,KMV,KMU);
			if (pushbackptr % 64) {
				__ATOMIC_READ__
					yes = *interrupted;
				if (yes) {
					free(wsp_a);
					for (j=0; j<pv_i; ++j) deldfqk((void*)((char*)pv_starters+DFQKSIZ*j));
					free(pv_starters);
					free(pv_ancestry);
					return;
				}
			}
			tntmthrfast_(&KR, &KNV, &KNU, &KMV, &KMU, stalpha[q].dfqk1_ch,
				     stalpha[q].n->ndat.dodv, stalpha[q].n->ndat.dodphi, stalpha[q].n->ndat.dgamdv, stalpha[q].n->ndat.dgamdw,
				     stalpha[q].n->ndat.dgamdphi, pv_x0, hessmem+hessptr, &lblk);
			pushback[pushbackptr].hessptr = hessptr;
			pushback[pushbackptr].n       = stalpha[q].n;
			pushback[pushbackptr].knv     = KNV;
			pushback[pushbackptr].lblk    = lblk;
			++pushbackptr;
			hessptr += lblk;
			if (stalpha[q].n->chd) {
				stalpha[q].dfqk1new_ch = (void*)((char*)wsp_a+swsp_a); swsp_a+=DFQKSIZ;
				allocdfqk(pv_rt->ndat.ku, stalpha[q].n->ndat.ku, stalpha[q].kv,
				          pv_ancestry[pv_i]->ndat.ku, pv_kv, stalpha[q].dfqk1new_ch);
				tndown_(stalpha[q].dfqk1_ch, stalpha[q].n->ndat.HPhi, stalpha[q].n->ndat.a,
				        &(stalpha[q].kv), &(stalpha[q].n->ndat.ku), &pv_kv, &(pv_ancestry[pv_i]->ndat.ku),
				        stalpha[q].dfqk1new_ch);
				/* 
				   PGI 2019 Compiler bug
				   ---------------------

				   If you put `stalpha[q].p = stalpha[q].n->chd` inside the for loop initialiser instead of
				   in an ugly separate line, the assembly will spit out non-sense like this:
					   
				   %z = alloca %struct.node*, align 8
				   %yes = alloca i32, align 4
				   %p = alloca void
				   %c = alloca i32, align 4
				   %b = alloca i32, align 4
					   
				   `%p = alloca void`... You know what the compiler wants...
				*/
				stalpha[q].p = stalpha[q].n->chd;
				for (; stalpha[q].p; stalpha[q].p = stalpha[q].p->nxtsb) {
					c = q++;
					stalpha[q].kv= stalpha[c].n->ndat.ku;   stalpha[q].dfqk1_ch= stalpha[c].dfqk1new_ch;
					stalpha[q].n = stalpha[c].p;            stalpha[q].p = NULL;
					stalpha[q].dfqk1new_ch = NULL;
					goto DOWNALPHA;
				UPALPHA:
					--q;
				}
				deldfqk(stalpha[q].dfqk1new_ch);
				swsp_a-=DFQKSIZ;
			}
			if (q) goto UPALPHA;
		}
		for (b = 0; b < pushbackptr; ++b) {
			/* HESS_WRITE */
			if (dir) {
				/* Update the bilinear forms in a thread-local storage. The values will be summed up
				   after all threads have finished. */
				tntmcpydir_(&(pushback[b].knv), &(pushback[b].n->ndat.ku), &KMV, &KMU, hessmem+(pushback[b].hessptr), &(pushback[b].lblk),
					    thrpv_bilinmat, dir, &ndir, &(pv_rt->u.rbk.nparam), &(pv_ancestry[pv_i]->u.hnbk.Phi),
					    &(pv_ancestry[pv_i]->u.hnbk.V), &(pv_ancestry[pv_i]->u.hnbk.w),
					    &(pushback[b].n->u.hnbk.Phi), &(pushback[b].n->u.hnbk.V), &(pushback[b].n->u.hnbk.w));
			} else {
				/* Because loop only visits the subtrees of the right-hand-side sibilings of beta, different threads writes to different
				   elements of `hessflat'. Therefore no locking is needed. */
				tntmcpy_(&(pushback[b].knv), &(pushback[b].n->ndat.ku), &KMV, &KMU, hessmem+(pushback[b].hessptr), &(pushback[b].lblk),
					 pv_rt->u.rbk.hessflat, &(pv_rt->u.rbk.nparam), &(pv_ancestry[pv_i]->u.hnbk.Phi),
					 &(pv_ancestry[pv_i]->u.hnbk.V), &(pv_ancestry[pv_i]->u.hnbk.w),
					 &(pushback[b].n->u.hnbk.Phi), &(pushback[b].n->u.hnbk.V), &(pushback[b].n->u.hnbk.w));
			}
		}
#undef KR
#undef KNV
#undef KNU
#undef KMV
#undef KMU
		free(wsp_a);
	}/* for each k */
	for (j=0; j<pv_i; ++j) deldfqk((void*)((char*)pv_starters+DFQKSIZ*j));
	free(pv_starters);
	free(pv_ancestry);
}

int hessglobwk(struct node *m, struct node *parent, struct hessglbbk *gbk,
	       double *x0, struct node *rt, SEXP VwPhi_L, fn_getvwphi get_VwPhi, void *wsp,
	       size_t swsp, size_t lwsp, int *interrupted, double *extrmem, double *dir, int ndir) {
	int istip;
	struct dfdqdk *dfqk1_ch, *dfqk1new_ch;
	double *K;
	struct llstptr *l, *a_new;
	struct llst *fmlfm_new, *fm_new, *qm_new;
	double *w;
	char err = 0;
	struct {  /* We manually manage our recursion stacks instead of the compiler's built-in one
		     because this way, we can loop through the stack conveniently without passing
		     even more stuff around function calls or mess with the stack frame addresses. */
		struct node *m, *v;
		struct hessglbbk *gbk, *gbk_new;
		int kv;
	} *stglob, curglob;
	struct node **ancestry;
	int ndesc_sum __UNUSED;
	int i=1;
	int mdim;
	int depth = 0;
	stglob = NULL;
	fmlfm_new = NULL;
	fm_new = NULL;
	qm_new = NULL;
	a_new  = NULL;
	K = NULL;
	dfqk1_ch = NULL;
	dfqk1new_ch = NULL;
	if (!(stglob = malloc(sizeof(*stglob) * PTSTACKLEN))) goto MEMFAIL;
	memset(stglob, 0, sizeof(*stglob) * PTSTACKLEN);
	curglob.kv = parent->ndat.ku;
	curglob.m = m; curglob.gbk = gbk;
	stglob[0].m = parent;	/* Artificially push the parent into the stack, since the parent here must be a direct child
				   of the root by the way this function is called. This is for making the outside-m's-subtree
				   walk easier. */
DOWNGLOB:
	++depth;
	stglob[i]  = curglob; 
	get_VwPhi(VwPhi_L, curglob.m, curglob.kv, NULL, &w, NULL, (char*)wsp+swsp, lwsp-swsp);
	istip = (int)(!(curglob.m->chd));
	if (! (K           = malloc((curglob.kv)*(curglob.kv)*sizeof(double))))            goto MEMFAIL;
	if (! (dfqk1_ch    = calloc(DFQKSIZ,1)))                                           goto MEMFAIL;
	if (! (dfqk1new_ch = calloc(DFQKSIZ,1)))                                           goto MEMFAIL;
	dzero(K, (curglob.kv)*(curglob.kv));
	if (!allocdfqk(rt->ndat.ku, curglob.kv, curglob.m->ndat.ku, curglob.m->ndat.ku, curglob.kv, dfqk1_ch)) goto MEMFAIL;
	/* HESS_WRITE */
	if (dir) {
		tmtmdir_(&(rt->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku), &(curglob.gbk->fmlfm), &(curglob.gbk->qm), &(curglob.gbk->fm), &(curglob.gbk->a),
			 curglob.m->ndat.dodv, curglob.m->ndat.dodphi, curglob.m->ndat.dgamdv, curglob.m->ndat.dgamdw, curglob.m->ndat.dgamdphi,
			 dfqk1_ch, K, x0, &(istip),
			 (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.invVLsO),
			 (istip ? curglob.m->u.hnbk.u.hsbktip.invVPhi   : curglob.m->u.hnbk.u.hsbkgen.invVLsOPhi),
			 (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.VmVLV),
			 (istip ? curglob.m->u.hnbk.u.hsbktip.invVxw    : curglob.m->u.hnbk.u.hsbkgen.invVLb),
			 (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.Hto),
			 thrpv_bilinmat, dir, &ndir, &(rt->u.rbk.nparam), &(curglob.m->u.hnbk.Phi), &(curglob.m->u.hnbk.V), &(curglob.m->u.hnbk.w));
	} else {
		tmtm2_(&(rt->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku), &(curglob.gbk->fmlfm), &(curglob.gbk->qm), &(curglob.gbk->fm), &(curglob.gbk->a),
		       curglob.m->ndat.dodv, curglob.m->ndat.dodphi, curglob.m->ndat.dgamdv, curglob.m->ndat.dgamdw, curglob.m->ndat.dgamdphi,
		       dfqk1_ch, K, x0, &(istip),
		       (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.invVLsO),
		       (istip ? curglob.m->u.hnbk.u.hsbktip.invVPhi   : curglob.m->u.hnbk.u.hsbkgen.invVLsOPhi),
		       (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.VmVLV),
		       (istip ? curglob.m->u.hnbk.u.hsbktip.invVxw    : curglob.m->u.hnbk.u.hsbkgen.invVLb),
		       (istip ? curglob.m->ndat.invV                  : curglob.m->u.hnbk.u.hsbkgen.Hto),
		       rt->u.rbk.hessflat, &(rt->u.rbk.nparam), &(curglob.m->u.hnbk.Phi), &(curglob.m->u.hnbk.V), &(curglob.m->u.hnbk.w));
	}
	/* Invariants for m:

	   1. curglob.gbk->fmlfm, fm, qm contains *_m and m > 1 so they aren't NULL or diagonal.
	   2. curglob.gbk->a contains all a_i to the current m
	   3. (1)/.m->j and (2)/.m->j are true in stglob[j] for all j <= m-1
	   4. i = 1
	   5. stglob[0].m is a direct child of the root but stglob[0].other_things are all undefined.
	 */
	struct hessglbbk kbk_beta;
	void *starters;
	initgbk(&kbk_beta, rt, stglob[0].m, curglob.gbk->mdim);                 /* Start kbk_beta using one of the direct children of the root. */
	if (!(starters = calloc(DFQKSIZ,i))) goto MEMFAIL;
	mdim = curglob.gbk->mdim;
	for (int k=0; k <= i-1; ++k) {                   /* Walk from the direct kid of the root to prepare for the beta-alpha walk. */
		struct llst *falfm_new;                  /* New block to be added into the linked list. */
		if (k == 0) {
			initfalfm_beta_(&(kbk_beta.fmlfm), &(curglob.gbk->fm), &(stglob[0].m->ndat.ku), &(curglob.kv));
		} else {
			if ( !(falfm_new = calloc(1, sizeof(struct llst) + mdim*mdim*sizeof(double))) ) {
				goto MEMFAIL;
			}
			falfm_new->siz = stglob[k].m->ndat.ku;
			memcpy(falfm_new->dat, stglob[k].m->ndat.Lamb, (stglob[k].m->ndat.ku)*(stglob[k].m->ndat.ku) * sizeof(double));
			betadown_(&(kbk_beta.fmlfm), &falfm_new, &(curglob.gbk->fm), kbk_beta.fm->dat, kbk_beta.qm->dat, stglob[k].m->ndat.HPhi,
				  stglob[k].m->ndat.a, &k, &mdim, &(rt->ndat.ku),
				  &(stglob[k].m->ndat.ku), &(stglob[k-1].m->ndat.ku), &(curglob.kv));
		}
		if( !allocdfqk(rt->ndat.ku, stglob[k].m->ndat.ku, curglob.m->ndat.ku, curglob.m->ndat.ku, curglob.kv, (struct dfdqdk*)(((char*)starters)+DFQKSIZ*k)) )
			goto MEMFAIL;
		/* TO OPTIMISE: The following function walks through fmlfm, qm, fm etc. to calculates (F,q,K) of each beta and allocate big chunks of memory in the heap.
		                Possible to avoid all these by updating? */
		initfqk4b_((void*)((char*)starters+DFQKSIZ*k), &(curglob.gbk->fm), &(curglob.gbk->qm), &(kbk_beta.fmlfm), kbk_beta.fm->dat, kbk_beta.qm->dat,
			   &(curglob.gbk->a), &k, &(rt->ndat.ku), &(stglob[k].m->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku),
			   curglob.m->ndat.dodv, curglob.m->ndat.dodphi, curglob.m->ndat.dgamdv, curglob.m->ndat.dgamdw,
			   curglob.m->ndat.dgamdphi);
	}
	delgbk(kbk_beta);
	if (! (ancestry = malloc(sizeof(struct node *) * (i+1)))) goto MEMFAIL;
	ndesc_sum = 0;
	for (int k=0; k <= i; ++k) {
		ancestry[k] = stglob[k].m;
		ndesc_sum += ancestry[k]->ndat.ndesc;
	}
	/* Implicit everything-flush by OpenMP 3.1 specification. */
	__PRAGMA__("omp task firstprivate(rt,x0,i,ancestry,starters,curglob,mdim,extrmem,interrupted,dir,ndir) if (ndesc_sum > 40)")
	walk_alpha (rt, x0, i, ancestry, starters, curglob.kv, mdim, interrupted, extrmem, dir, ndir);
	if (err) goto MEMFAIL;
	if (!(curglob.m->chd)) {
		/*__PRAGMA__("omp taskwait") */ /* DEBUG CENTOS 8. */
		if (i != 1) goto UPGLOB;    /* "Return" to the upper-level recursion */
		else        goto DONE;	    /* This is the top level, quit entire the function */
	}
	if ((depth%10) && my_rchk()) goto MASTER_INTERRUPTED;
	/* Now walk down the n, fixing m. The following line does the first step downward. */
	/* if(!allocdfqk(rt->ndat.ku, curglob.m->ndat.ku, curglob.m->ndat.ku, curglob.m->ndat.ku, curglob.kv, dfqk1new_ch)) */
	if(!allocdfqk(rt->ndat.ku, curglob.m->ndat.ku, curglob.kv, curglob.m->ndat.ku, curglob.kv, dfqk1new_ch))
		goto MEMFAIL;
	tndown1st_(dfqk1_ch, K, curglob.m->ndat.H, curglob.m->ndat.HPhi, w, curglob.m->ndat.a,
		   curglob.gbk->fm->dat, curglob.gbk->qm->dat, curglob.m->ndat.Lamb, curglob.m->ndat.invV,
		   curglob.m->u.hnbk.u.hsbkgen.invVLsOPhi,
		   &(rt->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku), dfqk1new_ch);
	/* __PRAGMA__("omp taskwait") */ /* DEBUG CENTOS 8. Move this above tndown1st_ to see what happens.
	                                If BLAS calls in tndown1st_ is replaced by matmul etc. results
	                                indicating something thread-wrong is going on when using their
	                                OpenBLAS .so file. */
	free(K);           K=NULL;
	deldfqk(dfqk1_ch);
	free(dfqk1_ch);    dfqk1_ch=NULL;
	{			/* Now really walk the subtree of curglob.m. */
		struct {	/* States of the stack */
			int kv;
			struct node *n, *p;
			struct dfdqdk *dfqk1_ch;
			struct dfdqdk *dfqk1new_ch;
		} *stdesc, curdesc;
		int j;		/* Counter */
		size_t stsiz = 0;
		stack_siz_fixed(curglob.m, 0, &stsiz, sizeof(*stdesc));
		if (!(stdesc = malloc(stsiz))) goto MEMFAIL;
		for (struct node *n = curglob.m->chd; n; n = n->nxtsb) {
			curdesc.kv=curglob.m->ndat.ku;  curdesc.n=n;  curdesc.dfqk1_ch = dfqk1new_ch;
			j = 0;
DOWNDESC:
			/* HESS_WRITE */
			if (dir) {
				/* This may be the culprit!!? But it's thread private to the master thread! */
				/* printf("NODE_ID: %d-%d (E)\n", curglob.m->id+1, curdesc.n->id+1); */
				tntmdir_(&(rt->ndat.ku), &(curdesc.kv), &(curdesc.n->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku), curdesc.dfqk1_ch,
					 curdesc.n->ndat.dodv, curdesc.n->ndat.dodphi, curdesc.n->ndat.dgamdv, curdesc.n->ndat.dgamdw,
					 curdesc.n->ndat.dgamdphi, x0,
					 thrpv_bilinmat, dir, &ndir, &(rt->u.rbk.nparam), &(curglob.m->u.hnbk.Phi), &(curglob.m->u.hnbk.V), &(curglob.m->u.hnbk.w),
					 &(curdesc.n->u.hnbk.Phi), &(curdesc.n->u.hnbk.V), &(curdesc.n->u.hnbk.w));
			} else {
				tntm_(&(rt->ndat.ku), &(curdesc.kv), &(curdesc.n->ndat.ku), &(curglob.kv), &(curglob.m->ndat.ku), curdesc.dfqk1_ch,
				      curdesc.n->ndat.dodv, curdesc.n->ndat.dodphi, curdesc.n->ndat.dgamdv, curdesc.n->ndat.dgamdw,
				      curdesc.n->ndat.dgamdphi, x0,
				      rt->u.rbk.hessflat, &(rt->u.rbk.nparam), &(curglob.m->u.hnbk.Phi), &(curglob.m->u.hnbk.V), &(curglob.m->u.hnbk.w),
				      &(curdesc.n->u.hnbk.Phi), &(curdesc.n->u.hnbk.V), &(curdesc.n->u.hnbk.w));
			}
			if (curdesc.n->chd) {
				if (! (curdesc.dfqk1new_ch = calloc(DFQKSIZ,1))) goto MEMFAIL;
				if( !allocdfqk(rt->ndat.ku, curdesc.n->ndat.ku, curdesc.kv,
					       curglob.m->ndat.ku, curglob.kv, curdesc.dfqk1new_ch))
					goto MEMFAIL;
				tndown_(curdesc.dfqk1_ch, curdesc.n->ndat.HPhi, curdesc.n->ndat.a,
				        &(curdesc.kv), &(curdesc.n->ndat.ku),
				        &(curglob.kv), &(curglob.m->ndat.ku), curdesc.dfqk1new_ch);
				for (curdesc.p = curdesc.n->chd; curdesc.p; curdesc.p = curdesc.p->nxtsb) {
					stdesc[j++] = curdesc;
					curdesc.kv=curdesc.n->ndat.ku;  curdesc.dfqk1_ch=curdesc.dfqk1new_ch;
					curdesc.n=curdesc.p;  curdesc.p=NULL;  curdesc.dfqk1new_ch=NULL;
					goto DOWNDESC;
UPDESC:
					curdesc = stdesc[--j];
				}
				deldfqk(curdesc.dfqk1new_ch);
				free(curdesc.dfqk1new_ch);   curdesc.dfqk1new_ch=NULL;
			}
			if (j) goto UPDESC;
		}
		free(stdesc);      stdesc = NULL;
		deldfqk(dfqk1new_ch);
		free(dfqk1new_ch); dfqk1new_ch = NULL;
	}

	/* 2. Global walk down. */
	if ( !(fmlfm_new = calloc(1, sizeof(struct llst) + (curglob.gbk->mdim)*(curglob.gbk->mdim)*sizeof(double)))
	     || !(fm_new = calloc(1, sizeof(struct llst) + (curglob.gbk->mdim)*(curglob.gbk->mdim)*sizeof(double)))
	     || !(qm_new = calloc(1, sizeof(struct llst) + curglob.gbk->mdim * sizeof(double)))
	     || !(a_new  = calloc(1, sizeof(struct llstptr))))
		goto MEMFAIL;
	gbkcpy(&(curglob.gbk_new), curglob.gbk);
	updategbk_( &(curglob.kv), &(curglob.m->ndat.ku),
		    &(curglob.gbk_new->fmlfm), &(fmlfm_new), &(curglob.gbk_new->fm), &(fm_new), &(curglob.gbk_new->qm), &(qm_new),
		    curglob.m->ndat.Lamb,                    curglob.m->ndat.HPhi,              curglob.m->ndat.a, &(curglob.gbk->mdim));
	for (l=curglob.gbk_new->a; l->nxt; l=l->nxt) continue;
	l->nxt     = a_new;
	a_new->siz = curglob.m->ndat.ku;
	a_new->dat = curglob.m->ndat.a;
	for (curglob.v = curglob.m->chd; curglob.v; curglob.v = curglob.v->nxtsb) {
		if (i >= PTSTACKLEN)             goto STACKFAIL;
		stglob[i++] = curglob;
		curglob.kv = curglob.m->ndat.ku; curglob.m=curglob.v;  curglob.gbk = curglob.gbk_new;
		goto DOWNGLOB;
UPGLOB:
		curglob=stglob[--i];
	}
	delgbk(*(curglob.gbk_new));
	free(curglob.gbk_new); curglob.gbk_new =NULL;
	if (i!=1) goto UPGLOB;		/* It's one but not zero cus the bottom of the stack is the parent. */
DONE:
	free(stglob);
	goto SUCCESS;
MEMFAIL:
	{ /* You have to wrap this line with that curly braces to stop GCC from complaining. */
		__PRAGMA__("omp flush")
		__PRAGMA__("omp taskwait")
	}
	free(stglob);
	free(K);
	deldfqk(dfqk1new_ch);
	free(dfqk1new_ch);
	return 3;
STACKFAIL:
	{
		__PRAGMA__("omp flush")
		__PRAGMA__("omp taskwait")
	}
	free(stglob);
	free(K);
	deldfqk(dfqk1new_ch);
	free(dfqk1new_ch);
	return 2;
MASTER_INTERRUPTED:
	/* Send out signal to shutdown every other threads. */
	__ATOMIC_WRITE__
		*interrupted = 1;
	__PRAGMA__("omp flush")
	__PRAGMA__("omp taskwait")
	/* Free up everything left in the stack. */
	for (; i>=2; --i) delgbk(*(stglob[i].gbk));
	free(stglob);
	free(K);
	deldfqk(dfqk1new_ch);
	free(dfqk1new_ch);
	return 1;
SUCCESS:
	return 0;
}


void hessselftop(struct node *m, int kv,
		 int ictx, int i, int j, int p, int q, double *x0, struct node *rt,
		 double *hessflat, double *dir, int ndir) {
	long didx1, didx2;
	double dl_ijpq=0, dl_ijqp=0, dl_jipq=0, dl_jiqp=0, dl;
	
	/* Precondition: For V, i>=j and p>=q. Symmetry is handled in here so caller should loop on U not V. */

#define DIFFGEN(ICTX,I,J,P,Q,RES) dbledifftopgen_(&(ICTX),&(I),&(J),&(P),&(Q),&(rt->ndat.ku),&kv,&(m->ndat.ku),\
					       m->u.hnbk.u.hsbkgen.invVLsO, m->u.hnbk.u.hsbkgen.invVLsOPhi, \
					       m->u.hnbk.u.hsbkgen.VmVLV, m->u.hnbk.u.hsbkgen.invVLb, \
					       m->u.hnbk.u.hsbkgen.Hto, x0, &(RES))
#define	DIFFTIP(ICTX,I,J,P,Q,RES) dbledifftoptip_(&(ICTX),&(I),&(J),&(P),&(Q),&(rt->ndat.ku),&kv,&(m->ndat.ku), \
					       m->ndat.invV, m->u.hnbk.u.hsbktip.invVPhi, m->u.hnbk.u.hsbktip.invVxw, \
					       x0, &(RES));
	switch (ictx) {
	case IVV:
		didx1 = m->u.hnbk.V + iijtouplolidx_(&(m->ndat.ku), &i, &j); /* Fortran idx. */
		didx2 = m->u.hnbk.V + iijtouplolidx_(&(m->ndat.ku), &p, &q);
		if (! m->ndat.x) {
			DIFFGEN(ictx,i,j,p,q, dl_ijpq);
			DIFFGEN(ictx,j,i,p,q, dl_jipq);
			DIFFGEN(ictx,j,i,q,p, dl_jiqp);
			DIFFGEN(ictx,i,j,q,p, dl_ijqp);
		} else {
			DIFFTIP(ictx,i,j,p,q, dl_ijpq);
			DIFFTIP(ictx,j,i,p,q, dl_jipq);
			DIFFTIP(ictx,j,i,q,p, dl_jiqp);
			DIFFTIP(ictx,i,j,q,p, dl_ijqp);
		}
		symhessvv_(&i,&j,&p,&q,&dl_ijpq,&dl_jipq,&dl_jiqp,&dl_ijqp,&dl);
		break;
	case IVPHI:
		didx2 = m->u.hnbk.Phi + (q-1) * m->ndat.ku + p;
		goto DOIVANY;
	case IVW:
		didx2 = m->u.hnbk.w + p;
DOIVANY:
		didx1 = m->u.hnbk.V + iijtouplolidx_(&(m->ndat.ku), &i, &j);
		if (! m->ndat.x) {
			DIFFGEN(ictx,i,j,p,q, dl_ijpq);
			DIFFGEN(ictx,j,i,p,q, dl_jipq);
		} else {
			DIFFTIP(ictx,i,j,p,q, dl_ijpq);
			DIFFTIP(ictx,j,i,p,q, dl_jipq);
		}
		symhessvany_(&i,&j, &dl_ijpq,&dl_jipq, &dl);
		break;
	case IPHIPHI:
		didx1 = m->u.hnbk.Phi + (j-1) * m->ndat.ku + i;
		didx2 = m->u.hnbk.Phi + (q-1) * m->ndat.ku + p;
		goto DIFSIM;
	case IPHIW:
		didx1 = m->u.hnbk.w + p;
		didx2 = m->u.hnbk.Phi + (j-1) * m->ndat.ku + i;
		goto DIFSIM;
	case IWW:
		didx1 = m->u.hnbk.w + i;
		didx2 = m->u.hnbk.w + p;
DIFSIM:
		if (! m->ndat.x)  DIFFGEN(ictx,i,j,p,q, dl);
		else              DIFFTIP(ictx,i,j,p,q, dl);
		break;
	default:
		error("Bug in hessselftop(): default case");
	}
	if (didx1 < didx2)  error("Bug in hessselftop(): wrong indicies");
	/* HESS_WRITE */
	if (dir) {
		bilinupdt_(&dl, hessflat, &(rt->u.rbk.nparam), &didx1, &didx2, dir, &ndir);
	} else {
		hessflat[ijtouplolidx_(&(rt->u.rbk.nparam), &didx1, &didx2)-1] = dl;
	}
#undef DIFFTIP
#undef DIFFGEN
}

void dndgcgod (struct node *t, SEXP VwPhi_L, int kv, double *c, double *gam, double *o, double *d,
	       fn_getvwphi get_VwPhi, fn_tcgod tcgod, fn_merg merg, void *wsp, size_t swsp, size_t lwsp,
	       int *info) {
	/* Precondition: cgod contains either the sum of all previous siblings of
	                 t, excluding t, or zero if non-existent.
	   Post-condition: cgod contains sum of all previous sibilings of t, including t. */
	struct node *p;
	size_t nbytes;
	double *v, *w, *phi, *c1, *d1, *gam1, *o1;

	if ((nbytes=get_VwPhi(VwPhi_L, t, kv, &v, &w, &phi, (char*)wsp+swsp, lwsp-swsp)) == 0) {
		*info = -99;
		return;
	}
	swsp += nbytes;
	if (t->ndat.x) {
		tcgod(t, kv, v, w, phi, c, gam, o, d, info);
		if (*info != 0)	 {
			Rprintf("Problematic node: the tip #%d\n", t->id+1);
			*info = -2;
			*info = -1;
			return;
		}
	} else {	
		ZEROCGOD((char*)wsp+swsp, c1, gam1, o1, d1, t->ndat.ku);
		swsp += CGODBYTES(t->ndat.ku);
		for (p = t->chd; p; p = p->nxtsb) {
			dndgcgod(p, VwPhi_L, t->ndat.ku, c1, gam1, o1, d1, get_VwPhi, tcgod, merg, wsp, swsp, lwsp, info);
			if (*info != 0) return;
		}
		merg(t, kv, v, w, phi, c1, gam1, o1, d1, c, gam, o, d, info);
		if (*info != 0) {
			Rprintf("Problematic lineage: the branch that leads to node #%d\n", t->id+1);
			*info = -2;
			return;
		}
	}
	*info = 0;
	return;
}

void hgcgod (struct node *t, SEXP VwPhi_L, int kv, double *c, double *gam, double *o, double *d,
	     fn_getvwphi get_VwPhi, void *wsp, size_t swsp, size_t lwsp, int *info) {
	struct node *p;
	double *v, *w, *phi, *d1;
	size_t nbytes;
	if ((nbytes=get_VwPhi(VwPhi_L, t, kv, &v, &w, &phi, (char*)wsp+swsp, lwsp-swsp)) == 0) {
		*info = -99;
		return;
	}
	swsp += nbytes;
	if (t->ndat.x) {
		c_htcgod(t, kv, v, w, phi, c, gam, o, d, info);
		if (*info != 0)	 {
			*info = -1;
			return;
		}
	} else {
		d1=(double*)((char*)wsp+swsp);  *d1=0;  swsp+=sizeof(double);
		for (p = t->chd; p; p = p->nxtsb) {
			hgcgod(p, VwPhi_L, t->ndat.ku, t->ndat.sc, t->ndat.sgam, t->ndat.so, d1,
			       get_VwPhi, wsp, swsp, lwsp, info);
			if (*info != 0) return;
		}
		c_hmerg(t, kv, v, w, phi, t->ndat.sc, t->ndat.sgam, t->ndat.so, d1, c, gam, o, d, info);
		if (*info != 0)	 {
			*info = -2;
			return;
		}
	}
	*info = 0;
	return;
}



/* TODO: Check for NA, NaN, Inf, -Inf and so on. But tbh the users is sort of
         responsible for that...  */
int chk_VwPhi_listnum2(struct node *t, SEXP VwPhi_L, int kv, int *mode, int *errcode) {
	SEXP dim, VwPhi, V, w, Phi;
	int ans, nprot;
	VwPhi = PROTECT(VECTOR_ELT(VwPhi_L, t->id));
	V = R_NilValue; w = R_NilValue; Phi = R_NilValue;  /* Make GCC shut up... */
	nprot = 1;
	if (*mode == -1) {	/* detect if strings should be used as key or numbers */
		SEXP names = PROTECT(getAttrib(VwPhi, R_NamesSymbol));    ++nprot;
		if (isNull(names)) {     goto MODE2; }
		V = PROTECT(Rlistelem(VwPhi, "V"));                       ++nprot;
		if (isNull(V))     {     goto MODE2; }
		w = PROTECT(Rlistelem(VwPhi, "w"));                       ++nprot;
		if (isNull(w))     {     goto MODE2; }
		Phi = PROTECT(Rlistelem(VwPhi, "Phi"));                   ++nprot;
		if (isNull(Phi))     {   goto MODE2; }
		goto MODE1;
	}
MODE1:  /* String indexing */
	*mode = 1;
	if (length(VwPhi) != 3)                      { *errcode = 91; UNPROTECT(nprot); return -(t->id); }
	goto MORECHECKS;
MODE2:  /* Numeric indexing */
	*mode = 2;
	if (length(VwPhi) != 3)                      { *errcode = 92; UNPROTECT(nprot); return -(t->id); }
	V   = PROTECT(VECTOR_ELT(VwPhi, 0));
	w   = PROTECT(VECTOR_ELT(VwPhi, 1));
	Phi = PROTECT(VECTOR_ELT(VwPhi, 2));
	nprot += 3;
MORECHECKS:
	if (TYPEOF(V) != REALSXP)   { *errcode = 10; UNPROTECT(nprot); return -(t->id); }
	if (TYPEOF(w) != REALSXP)   { *errcode = 20; UNPROTECT(nprot); return -(t->id); }
	if (TYPEOF(Phi) != REALSXP) { *errcode = 30; UNPROTECT(nprot); return -(t->id); }

	dim = PROTECT(getAttrib(V, R_DimSymbol));
	if (length(dim) != 2)                                                   { *errcode=11; UNPROTECT(nprot+1); return -(t->id); }
	if ((INTEGER(dim)[0] != t->ndat.ku) || (INTEGER(dim)[1] != t->ndat.ku)) { *errcode=12; UNPROTECT(nprot+1); return -(t->id); }
	dim = PROTECT(getAttrib(w, R_DimSymbol));
	if (!(length(dim)==0 || length(dim)==1 || length(dim)==2))              { *errcode=21; UNPROTECT(nprot+2); return -(t->id); }
	if ((length(dim)==2) && (INTEGER(dim)[1] != 1))                         { *errcode=22; UNPROTECT(nprot+2); return -(t->id); }
	if (length(w) != t->ndat.ku)                                            { *errcode=23; UNPROTECT(nprot+2); return -(t->id); }
	dim = PROTECT(getAttrib(Phi, R_DimSymbol));
	if (length(dim)!=2)                                                     { *errcode=31; UNPROTECT(nprot+3); return -(t->id); }
	if ((INTEGER(dim)[0] != t->ndat.ku) || (INTEGER(dim)[1] != kv))         { *errcode=32; UNPROTECT(nprot+3); return -(t->id); }
	UNPROTECT(nprot+3);
	for (struct node *p = t->chd; p; p=p->nxtsb)
		if ((ans = chk_VwPhi_listnum2(p, VwPhi_L, t->ndat.ku, mode, errcode)) != 1)
			return ans;
	return 1;
}
/* Return 1 if success. Otherwise return the negative INTERNAL
   (start from zero) node ID which fails the check.
 */
int chk_VwPhi_listnum(struct node *t, SEXP VwPhi_L, int *mode, int *errcode) {
	int ans;
	struct node *p;
	if (! isNull(PROTECT(VECTOR_ELT(VwPhi_L, t->id)))) { UNPROTECT(1); return -(t->id); }
	*mode = -1;
	UNPROTECT(1);
	for (p = t->chd; p; p=p->nxtsb) 
		if ((ans=chk_VwPhi_listnum2(p, VwPhi_L, t->ndat.ku, mode, errcode)) != 1)
			return ans;
	return 1;
}

fn_getvwphi chk_VwPhi(struct node *t, SEXP VwPhi_L) {
	int chk_ans, mode, errcode;
	switch (TYPEOF(VwPhi_L)) {
	case REALSXP:
		/* It must be of length nparam */
		if (! (length(VwPhi_L) == t->u.rbk.nparam))
			error("The VwPhi parameters should be %ld dimensional but we've got %d dimensions",
				   t->u.rbk.nparam, (int)(length(VwPhi_L)));
		return &getvwphi_vec;
	case VECSXP:
		if (length(VwPhi_L) != t->ndat.ndesc+1)
			error("VwPhi parameters is a list but its length is not equal to the number of nodes");
		if ((chk_ans = chk_VwPhi_listnum(t, VwPhi_L, &mode, &errcode))!=1)
			error("Malformed VwPhi parameter at node #%d, err. code=%d", -chk_ans+1, errcode);
		return mode == 1 ? &getvwphi_liststr : &getvwphi_listnum;
	default:
		error("VwPhi parameters must either be a list or numeric vector with mode 'double'");
	}
}
SEXP Rndphylik(SEXP p, SEXP VwPhi_L, SEXP x0, SEXP k) {
	struct node *t;
	SEXP l;
	t = R_ExternalPtrAddr(p);
	if (!(t->u.rbk.xavail)) error("Cannot compute likelihood or its gradient/Hessian using empty tip values");
	l = PROTECT(allocVector(REALSXP, 1));
	ndphylik((struct node *) t, VwPhi_L, REAL(x0), (INTEGER(k))[0], REAL(l), chk_VwPhi(t, VwPhi_L));
	UNPROTECT(1);
	return l;
}
SEXP Rdphylik(SEXP p, SEXP VwPhi_L, SEXP x0, SEXP k, SEXP nthd) {
	struct node *t;
	SEXP l;
	t = R_ExternalPtrAddr(p);
	if (!(t->u.rbk.xavail)) error("Cannot compute likelihood or its gradient/Hessian using empty tip values");
	l = PROTECT(allocVector(REALSXP, 1));
	dphylik((struct node *) t, VwPhi_L, REAL(x0), (INTEGER(k))[0], REAL(l), chk_VwPhi(t,VwPhi_L), *(INTEGER(nthd)));
	UNPROTECT(1);
	return l;
}

SEXP Rhphylik_dir(SEXP p, SEXP VwPhi_L, SEXP x0, SEXP k, SEXP dir, SEXP nthd) {
	struct node *t;
	SEXP l, Rndirdim, Rres, Rres_dim;
	int *ndirdim, ndir, *res_dim;
	t = R_ExternalPtrAddr(p);
	if (!(t->u.rbk.xavail)) error("Cannot compute likelihood or its gradient/Hessian using empty tip values");
	if (TYPEOF(dir) != REALSXP) error("Directions must be a double precision matrix but you have passed me something else\n");
	Rndirdim = getAttrib(dir, R_DimSymbol);
	if (isNull(Rndirdim) || (length(Rndirdim) != 2)) error("Directions must be a matrix");
	ndirdim  = INTEGER(Rndirdim);
	if (ndirdim[0] != (int) t->u.rbk.nparam) 
		error("Directions must have the same amount of columns as the number of underlying Gaussian parameters");
	ndir     = ndirdim[1];
	Rres = PROTECT(allocVector(REALSXP, ndir * ndir));
	dzero(REAL(Rres), ndir*ndir);
	Rres_dim = PROTECT(allocVector(INTSXP, 2));
	res_dim = INTEGER(Rres_dim);
	res_dim[0] = ndir;
	res_dim[1] = ndir;
	setAttrib(Rres, R_DimSymbol, Rres_dim);
	l = PROTECT(allocVector(REALSXP, 1));
	hphylik((struct node *) t, VwPhi_L, REAL(x0), INTEGER(k)[0], REAL(l), chk_VwPhi(t, VwPhi_L), REAL(Rres), REAL(dir), ndir, *(INTEGER(nthd)));
	t->u.rbk.hessflat_needs_free = 0;
	t->u.rbk.hessflat            = NULL;
	UNPROTECT(3);
	return Rres;
}

SEXP Rhphylik(SEXP p, SEXP VwPhi_L, SEXP x0, SEXP k, SEXP nthd) {
	struct node *t;
	SEXP l;
	t = R_ExternalPtrAddr(p);
	if (!(t->u.rbk.xavail)) error("Cannot compute likelihood or its gradient/Hessian using empty tip values");
	l = PROTECT(allocVector(REALSXP, 1));
	hphylik((struct node *) t, VwPhi_L, REAL(x0), INTEGER(k)[0], REAL(l), chk_VwPhi(t, VwPhi_L), NULL, NULL, 0, *(INTEGER(nthd)));
	UNPROTECT(1);
	return l;
}
SEXP Rhphylik_big(SEXP p, SEXP VwPhi_L, SEXP x0, SEXP k, SEXP hessfp, SEXP nthd) {
	struct node *t;
	SEXP l;
	double *hessflat;
	t        = R_ExternalPtrAddr(p);
	hessflat = R_ExternalPtrAddr(hessfp);
	if (!(t->u.rbk.xavail)) error("Cannot compute likelihood or its gradient/Hessian using empty tip values");
	l = PROTECT(allocVector(REALSXP, 1));
	hphylik((struct node *) t, VwPhi_L, REAL(x0), INTEGER(k)[0], REAL(l), chk_VwPhi(t, VwPhi_L), hessflat, NULL, 0, *(INTEGER(nthd)));
	UNPROTECT(1);
	return l;
}
void extractderiv(struct node *t, int kv, SEXP x);
SEXP Rextractderiv(SEXP tr, SEXP nr) {
	/* Returns a R-list x, in which the x[[i]] contains
	   a list of dlikdv, dlikdw, dlikdphi of the node with
	   ape-id i. n is the total number of nodes, internals + tip + root. */
	SEXP x;
	struct node *p, *t;
	int n;
	n = INTEGER(nr)[0];
	t = (struct node *) R_ExternalPtrAddr(tr);
	x = PROTECT(allocVector(VECSXP, n));
	for (p = t->chd; p; p = p->nxtsb)  extractderiv(p, t->ndat.ku, x);
	UNPROTECT(1);
/*	UNPROTECT(1 + (n-1) * 4); */
	return x;
}

void extractderiv(struct node *t, int kv, SEXP x) {
	SEXP d; SEXP c; double *rptr;
	struct node *p;

	if (! t) return;
	
	d = PROTECT(allocVector(VECSXP, 3));
	c = PROTECT(allocMatrix(REALSXP, t->ndat.ku, t->ndat.ku));
	rptr = REAL(c);
	memcpy(rptr, t->ndat.dlikdv, (t->ndat.ku) * (t->ndat.ku) * sizeof(double));
	SET_VECTOR_ELT(d, 0, c);
	c = PROTECT(allocVector(REALSXP, t->ndat.ku));
	rptr = REAL(c);
	memcpy(rptr, t->ndat.dlikdw, t->ndat.ku * sizeof(double));
	SET_VECTOR_ELT(d, 1, c);
	c = PROTECT(allocMatrix(REALSXP, t->ndat.ku, kv));
	rptr = REAL(c);
	memcpy(rptr, t->ndat.dlikdphi, kv * (t->ndat.ku) * sizeof(double));
	SET_VECTOR_ELT(d, 2, c);
	SET_VECTOR_ELT(x, t->id, d);
	
	for (p = t->chd; p; p = p->nxtsb) extractderiv(p, t->ndat.ku, x);
	UNPROTECT(4);
	return;
}

void extractderivvec(struct node *t, int kv, double *dptr) {
	struct node *p;
	memcpy(dptr + t->u.hnbk.Phi, t->ndat.dlikdphi, (t->ndat.ku) * kv * sizeof(double));
	memcpy(dptr + t->u.hnbk.w, t->ndat.dlikdw, (t->ndat.ku) * sizeof(double));
	gesylcpy_(dptr + t->u.hnbk.V, t->ndat.dlikdv, &(t->ndat.ku));
	for (p = t->chd; p; p = p->nxtsb) extractderivvec(p, t->ndat.ku, dptr);
}
SEXP Rextractderivvec(SEXP tr) {
	SEXP d;   double *dptr;   struct node *p, *t;
	t = (struct node *) R_ExternalPtrAddr(tr);
	d = PROTECT(allocMatrix(REALSXP, t->u.rbk.nparam, 1));
	dptr = REAL(d);
	for (p = t->chd; p; p = p->nxtsb)  extractderivvec(p, t->ndat.ku, dptr);
	UNPROTECT(1);
	return d;
}
SEXP Rextracthessuplol(SEXP tr) {
	SEXP d;   double *dptr;   struct node *t;  long siz;
	t = (struct node *) R_ExternalPtrAddr(tr);
	siz = ((t->u.rbk.nparam) * ((t->u.rbk.nparam)+1))/2;
	d = PROTECT(allocMatrix(REALSXP, siz, 1));
	dptr = REAL(d);
	memcpy(dptr, t->u.rbk.hessflat, siz * sizeof(double));
	UNPROTECT(1);
	return d;
}
SEXP Rextracthessall(SEXP tr) {
	SEXP d;   double *dptr;   struct node *t;
	t = (struct node *) R_ExternalPtrAddr(tr);
	d = PROTECT(allocMatrix(REALSXP, t->u.rbk.nparam, t->u.rbk.nparam));
	dptr = REAL(d);
	lsylgecpy_(dptr, t->u.rbk.hessflat, &(t->u.rbk.nparam));
	UNPROTECT(1);
	return d;
}
SEXP Rnparams(SEXP Rtr) {
	struct node *t;
	SEXP Rn; double *n;
	t = R_ExternalPtrAddr(Rtr);
	Rn = PROTECT(allocVector(REALSXP, 1));
	n = REAL(Rn);
	*n = (double) t->u.rbk.nparam;
	UNPROTECT(1);
	return Rn;
}
SEXP Rndesc(SEXP Rtr) {
	struct node *t;
	SEXP Rn; double *n;
	t = R_ExternalPtrAddr(Rtr);
	Rn = PROTECT(allocVector(REALSXP, 1));
	n = REAL(Rn);
	*n = (double) t->ndat.ndesc;
	UNPROTECT(1);
	return Rn;
}
SEXP Rxavail(SEXP Rtr) {
	struct node *t;
	SEXP Rn; int *n;
	t = R_ExternalPtrAddr(Rtr);
	Rn = PROTECT(allocVector(LGLSXP, 1));
	n = INTEGER(Rn);
	*n = t->u.rbk.xavail;
	UNPROTECT(1);
	return Rn;
}

SEXP Rtagreg(SEXP p, SEXP Rnnode, SEXP regspec);
SEXP Rdeschpos(SEXP tr, SEXP Rx, SEXP Ry);
void tagreg2(struct node *t, int nnode, int *v, int lenv, int *res, int curreg);
void tagreg(struct node *t, int nnode, int *v, int lenv, int *res);
void findhpos_wk(struct node *t, int kv, long target, int *nodeid, int *vwphi);

/* Only the id and topology of the tree is used. Every other things are not even read. */
SEXP Rtagreg(SEXP p, SEXP Rnnode, SEXP regspec) {
	/* regspec: a vector of ape-IDs of roots of regions, plus one cell of 'working area'.
	   Returns a vector of region IDs, which are just the R indices of regspec.
	 */
	struct node *t;
	int *v;
	int nnode;
	size_t lenv;
	int ilenv;
	SEXP res;
	t = (struct node *)R_ExternalPtrAddr(p);
	v = INTEGER(regspec);
	lenv = length(regspec);
	ilenv = (int) lenv; /* If the user passes in 2.147 billions of regions it will fail... */
	nnode = INTEGER(Rnnode)[0];
	res = PROTECT(allocVector(INTSXP, nnode));
	tagreg(t, nnode, v, ilenv, INTEGER(res));
	UNPROTECT(1);
	return res;
}
void tagreg(struct node *t, int nnode, int *v, int lenv, int *res) {
	int j = 0;
	iset(res, -1, nnode);
	v[lenv-1] = t->id+1;
	while (v[j++] != t->id+1);
	if (j>=lenv) j = -1;
	res[t->id] = -1;	/* Root branch doesn't exist. */
	for (t = t->chd; t; t = t->nxtsb) tagreg2(t, nnode, v, lenv, res, j);
	return;
}
void tagreg2(struct node *t, int nnode, int *v, int lenv, int *res, int curreg) {
	int j = 0;
	v[lenv-1] = t->id+1;
	while (v[j++] != t->id+1);
	res[t->id] = j >= lenv ? curreg : (curreg = j);
	if (curreg < 0) error("tagreg(): Failed to find the evolutionary region of node %d", t->id+1);
	for (t = t->chd; t; t=t->nxtsb) tagreg2(t, nnode, v, lenv, res, curreg);
}

void vwphi_simulwk(struct node *t, int ntip, double *dpar, double *daddy, int kv, double *wsp, size_t swsp, SEXP out, int *info);
void vwphi_simul(struct node *t, int ntip, double *dpar, double *x0, double *wsp, SEXP out, int *info);
SEXP Rvwphi_simul(SEXP Rctree, SEXP Rntip, SEXP Rdimtab, SEXP Rpar, SEXP Rnsamp, SEXP Rx0) {
	struct node *t;
	int nsamp, *dimtab, ntip, mdim;
	double *dpar, *x0;
	int info;
	SEXP out;
	double *wsp;
	size_t lwsp;
	
	t     = (struct node *)R_ExternalPtrAddr(Rctree);
	dpar  = REAL(Rpar);
	x0    = REAL(Rx0);
	nsamp = INTEGER(Rnsamp)[0];
	dimtab= INTEGER(Rdimtab);
	ntip  = INTEGER(Rntip)[0];
	
	mdim = maxdim(t);
	out = PROTECT(allocVector(VECSXP, nsamp));
	for (int i=0; i<nsamp; ++i) {
		SEXP xi;
		xi = PROTECT(allocVector(VECSXP, ntip));
		SET_VECTOR_ELT(out, i, xi);
		UNPROTECT(1);
		for (int j=0; j<ntip; ++j) {
			SEXP xij;
			xij = PROTECT(allocVector(REALSXP, dimtab[j]));
			SET_VECTOR_ELT(xi, j, xij);
			UNPROTECT(1);
		}
	}
	lwsp = 0;	stack_siz_fixed(t, 0, &lwsp, mdim*sizeof(double));
	if (!(wsp = malloc(lwsp))) goto MEMFAIL;
	GetRNGstate();
	for (int i=0; i<nsamp; ++i) {
		vwphi_simul(t, ntip, dpar, x0, wsp, PROTECT(VECTOR_ELT(out, i)), &info);
		UNPROTECT(1);
		if (info != 0) {
			free(wsp);
			goto CHOLFAIL;
		}
	}
	free(wsp);
	PutRNGstate();
	UNPROTECT(1);
	return out;

MEMFAIL:
	PutRNGstate();
	error("Rvwphi_simul(): failed to allocate memory.");
CHOLFAIL:
	PutRNGstate();
	if (info > 0) error("Rvwphi_simul(): the `V` in node #%d is not positive definite", info);
	else          error("Rvwphi_simul(): congratulation! you have found a bug in the package... (cholesky of node #%d)", info);
}
void vwphi_simul(struct node *t, int ntip, double *dpar, double *x0, double *wsp, SEXP out, int *info) {
	for (struct node *p = t->chd; p; p=p->nxtsb) {
		vwphi_simulwk(p, ntip, dpar, x0, t->ndat.ku, wsp, 0, out, info);
		if (*info != 0) break;
	}
}
extern void vwphisimstep_(double *Phi, double *w, double *V, double *daddy, int *kv, int *ku, double *out, int *info);
void vwphi_simulwk(struct node *t, int ntip, double *dpar, double *daddy, int kv, double *wsp, size_t swsp, SEXP out, int *info) {
	for (int j=0; j<(t->ndat.ku); ++j)
		wsp[swsp+j]= rnorm(0.0, 1.0); /* norm_rand(); */
	vwphisimstep_(dpar+(t->u.hnbk.Phi), dpar+(t->u.hnbk.w), dpar+(t->u.hnbk.V), daddy, &kv, &(t->ndat.ku), wsp+swsp, info);
	if (*info != 0) {
		if      (*info > 0)  *info = t->id+1;
		else if (*info < 0)  *info = -(t->id+1);
		return;
	}
	if (t->id < ntip) {
		memcpy(REAL(PROTECT(VECTOR_ELT(out, t->id))), wsp+swsp, (t->ndat.ku)*sizeof(double));
		UNPROTECT(1);
	} else {
		size_t newswsp;
		newswsp = swsp + (size_t)(t->ndat.ku);
		for (struct node *p = t->chd; p; p=p->nxtsb)
			vwphi_simulwk(p, ntip, dpar, wsp+swsp, t->ndat.ku, wsp, newswsp, out, info);
	}
}



void unpack_gauss(struct node *t, int kv, double *par, SEXP x);
SEXP Runpack_gauss(SEXP Rctree, SEXP Rn, SEXP Rpar) {
	SEXP x;
	struct node *t, *p;
	int n;
	double *dpar;
	t = (struct node *)R_ExternalPtrAddr(Rctree);
	dpar = REAL(Rpar);
	n = INTEGER(Rn)[0];
	x = PROTECT(allocVector(VECSXP, n));
	for (p = t->chd; p; p = p->nxtsb) unpack_gauss(p, t->ndat.ku, dpar, x);
	UNPROTECT(1);
	return x;
}
void unpack_gauss(struct node *t, int kv, double *par, SEXP x) {
	SEXP d; SEXP c; SEXP names;
	struct node *p;
	if (! t) return;
	d = PROTECT(allocVector(VECSXP, 3));
	c = PROTECT(allocMatrix(REALSXP, t->ndat.ku, kv)); /* Phi */
	memcpy(REAL(c), par + t->u.hnbk.Phi, (t->ndat.ku)*kv*sizeof(double));
	SET_VECTOR_ELT(d, 0, c);
	c = PROTECT(allocVector(REALSXP, t->ndat.ku));     /* w */
	memcpy(REAL(c), par + t->u.hnbk.w, (t->ndat.ku)*sizeof(double));
	SET_VECTOR_ELT(d, 1, c);
	c = PROTECT(allocMatrix(REALSXP, t->ndat.ku, t->ndat.ku)); /* V */
	sylgecpy_(REAL(c), par + t->u.hnbk.V, &(t->ndat.ku));
	SET_VECTOR_ELT(d, 2, c);
	names = PROTECT(allocVector(VECSXP, 3));
	SET_VECTOR_ELT(names, 0, install("Phi"));
	SET_VECTOR_ELT(names, 1, install("w"));
	SET_VECTOR_ELT(names, 2, install("V"));
	setAttrib(d, R_NamesSymbol, names);
	SET_VECTOR_ELT(x, t->id, d);
	UNPROTECT(5);
	for (p = t->chd; p; p = p->nxtsb) unpack_gauss(p, t->ndat.ku, par, x);
	return;
}

/* Just for debugging */
void findhpos(struct node *t, long target, int *nodeid, int *vwphi) {
	for (struct node *p = t->chd; p; p = p->nxtsb)
		findhpos_wk(p, (long)(t->ndat.ku), target, nodeid, vwphi);
}
void findhpos_wk(struct node *t, int kv, long target, int *nodeid, int *vwphi) {
	if (target >= t->u.hnbk.Phi && target < t->u.hnbk.w) {
		*nodeid = t->id;	*vwphi  = 3;
	} else if (target >= t->u.hnbk.w && target < t->u.hnbk.V) {
		*nodeid = t->id;	*vwphi  = 2;
	} else if (target >= t->u.hnbk.V && target < t->u.hnbk.V + (long)((t->ndat.ku) * (t->ndat.ku))) {
		*nodeid = t->id;	*vwphi  = 1;
	} else for (struct node *p = t->chd; p; p = p->nxtsb)
		       findhpos_wk(p, t->ndat.ku, target, nodeid, vwphi);
}
SEXP Rdeschpos(SEXP tr, SEXP Rx, SEXP Ry) {
	/* Returns a R-list x, in which the x[[i]] contains a list of dlikdv, dlikdw, dlikdphi of the node with
	   ape-id i. n is the total number of nodes, internals + tip + root. */
	int x, y;
	long xl, yl;
	struct node *t;
	SEXP desc;
	int *dptr;

	x = INTEGER(Rx)[0];    y = INTEGER(Ry)[0];
	xl = (long) x;         yl = (long) y;
	t = (struct node *) R_ExternalPtrAddr(tr);
	desc = PROTECT(allocMatrix(INTSXP, 4, 1));
	dptr = INTEGER(desc);
	dptr[0] = dptr[1] = dptr[2] = dptr[3] = -1;
	findhpos(t, xl-1, dptr, dptr+1);
	findhpos(t, yl-1, dptr+2, dptr+3);
	UNPROTECT(1);
	return desc;
}

SEXP Rtested(void) {
	SEXP ret; int *x;
	ret = PROTECT(allocVector(INTSXP, 1));
	x = INTEGER(ret);
	*x = __TESTED__;
	UNPROTECT(1);
	return ret;
}

/*
    tr: just a 'template tree' and only the ID and topology of the tree is
        used and other attributes are not read at all.

    tipmiss: the missingness of the trait, a matrix such that each column
             a missing-ness vector.
 */
void tagmiss(struct node *t, int *TM, int maxdim, int ntips, int nnodes, int *M);
SEXP Rtagmiss(SEXP Rtr, SEXP Rnnodes, SEXP Rtipmiss) {
	struct node *t;
	int *TM, *M, maxdim, nnodes;
	SEXP RM, dim;
	t = (struct node *) R_ExternalPtrAddr(Rtr);
	TM = INTEGER(Rtipmiss);
	dim = PROTECT(getAttrib(Rtipmiss, R_DimSymbol));
	maxdim = INTEGER(dim)[0];
	nnodes = INTEGER(Rnnodes)[0];
	RM = PROTECT(allocMatrix(INTSXP, maxdim, nnodes));
	M = INTEGER(RM);
	for (int j=0; j<maxdim*nnodes; ++j ) M[j]=1;
        tagmiss(t, TM, maxdim, INTEGER(dim)[1], nnodes, M);
#define _M(q, dim)   M[((q)->id)*(maxdim)+(dim)]
	for (int d=0; d<maxdim; ++d)
		if (_M(t,d) != 1)
			error("Some dimensions has NaN on all tips!");
	UNPROTECT(2);
	return RM;
}

void tagmiss(struct node *t, int *TM, int maxdim, int ntips, int nnodes, int *M) {
#define _TM(q, dim) TM[((q)->id)*(maxdim)+(dim)]
	int d;
	if (t->id < ntips) for (d=0; d<maxdim; ++d) _M(t,d) = _TM(t,d);
	else {
		for (struct node *p = t->chd; p; p=p->nxtsb) {
			tagmiss(p, TM, maxdim, ntips, nnodes, M);
			for (d=0; d<maxdim; ++d) _M(t,d) *= !(_M(p,d));
		}
		for (d=0; d<maxdim; ++d) _M(t,d) = !_M(t,d);
	}
#undef _M
#undef _TM
}

int chkusrhess_VwOrPhi(SEXP Robj, int VwOrPhi, int nparregime, int ku, int kv) {
	int ld, retval; SEXP Rdim;
	ld = 0;         /* Just to make GCC shut up. */
	switch (VwOrPhi) {
	case 2: 		/* V */
		ld = (ku*(ku+1))/2;   break;
	case 1:			/* w */
		ld = ku;              break;
	case 0:			/* Phi */
		ld = ku*kv;
	}
	Rdim = PROTECT(getAttrib(Robj, R_DimSymbol));
	retval= (TYPEOF(Robj) == REALSXP
		&& (!isNull(Rdim))
		&& TYPEOF(Rdim) == INTSXP
		&& length(Rdim) == 3
		&& (INTEGER(Rdim))[0] == ld
		&& (INTEGER(Rdim))[1] == nparregime
		&& (INTEGER(Rdim))[2] == nparregime);
	UNPROTECT(1);
	return(retval);
}
void chkusrhess (SEXP Robj, int nparglobal, int nparregime, int nid, int pid, int ku, int kv) {
	SEXP RVans, Rwans, RPhians;
	if (TYPEOF(Robj) != VECSXP)
		error("curvifyhess(): User-supplied Hessian function for the user-specified "
			  "parameterisation returned a non-list on node ID #%d (mother node is #%d).",
			   nid+1, pid+1);
	if (length(Robj) != 3)
		error("curvifyhess(): User-supplied Hessian function for the user-specified "
			  "parameterisation returned a wrong-formatted list on node ID #%d. "
			   "(mother node is #%d). The list should contains exactly three elements "
			   "with names `V', `w', and `Phi'",
			   nid+1, pid+1);
	RVans   = PROTECT(Rlistelem(Robj, "V"));
	Rwans   = PROTECT(Rlistelem(Robj, "w"));
	RPhians = PROTECT(Rlistelem(Robj, "Phi"));
	if (! chkusrhess_VwOrPhi(RVans, 2, nparregime, ku, kv))
		error("curvifyhess(): User-supplied Hessian function for the user-specified "
			   "parameterisation returned an wrong object on the `V' part of the returned "
			   "list on node ID #%d (mother node is #%d). For this particular node, "
			   "I expect that ans[['V']] "
			   "is a %d-by-%d-by-%d array of double precision real numbers.",
			   nid+1, pid+1, (ku*(ku+1))/2, nparregime, nparregime);
	if (! chkusrhess_VwOrPhi(Rwans, 1, nparregime, ku, kv))
		error("curvifyhess(): User-supplied Hessian function for the user-specified "
			   "parameterisation returned an wrong object on the `w' part of the returned "
			   "list on node ID #%d (mother node is #%d). For this particular node, "
			   "I expect that ans[['w']] "
			   "is a %d-by-%d-by-%d array of double precision real numbers.",
			   nid+1, pid+1, ku, nparregime, nparregime);
	if (! chkusrhess_VwOrPhi(RPhians, 0, nparregime, ku, kv))
		error("curvifyhess(): User-supplied Hessian function for the user-specified "
			   "parameterisation returned an wrong object on the `Phi' part of the returned "
			   "list on node ID #%d (mother node is #%d). For this particular node, "
			   "I expect that ans[['Phi']] "
			   "is a %d-by-%d-by-%d array of double precision real numbers.",
			   nid+1, pid+1, ku*kv, nparregime, nparregime);
	UNPROTECT(3);
}

SEXP Rchkusrhess(SEXP Robj, SEXP Rnparglobal, SEXP Rnparregime, SEXP Rnid, SEXP Rpid, SEXP Rku, SEXP Rkv) {
	chkusrhess(Robj,
		   INTEGER(Rnparglobal)[0], INTEGER(Rnparregime)[0],
		   INTEGER(Rnid)[0], INTEGER(Rpid)[0],
		   INTEGER(Rku)[0],  INTEGER(Rkv)[0]);
	return R_NilValue;
}

void curvifyhess(double *H, struct node *t, int npar, int kv, SEXP fnh, SEXP env,
		 double *wsp, SEXP Rpar) {
	SEXP Rans, RVans, Rwans, RPhians, Rnodeidcell, Rf_call;
	struct node *p;
	int *nodeid;
	Rnodeidcell = PROTECT(allocVector(INTSXP, 1));
	nodeid = INTEGER(Rnodeidcell);
	*nodeid = (t->id)+1;
	Rf_call = PROTECT(lang3(fnh, Rnodeidcell, Rpar));
	Rans = PROTECT(eval(Rf_call, env)); /* Trusted because the user function was wrapped and checked */
	RVans = PROTECT(Rlistelem(Rans, "V"));
	Rwans = PROTECT(Rlistelem(Rans, "w"));
	RPhians = PROTECT(Rlistelem(Rans, "Phi"));
	curvifyupdate_( H, REAL(RVans), REAL(Rwans), REAL(RPhians),
			&(npar), &(t->ndat.ku), &(kv),
			t->ndat.dlikdv, t->ndat.dlikdw, t->ndat.dlikdphi,
			wsp);
	for (p = t->chd; p; p = p->nxtsb)
		curvifyhess(H, p, npar, t->ndat.ku, fnh, env, wsp, Rpar);
	UNPROTECT(6);
}

/*
  Before:
  
  RH:   a write-only Hessian matrix to be updated
  Rpar: parameters (in the user parameter space) at which the Hessian is evaluated
  tr:   a tree which is decorated with dlikdv dlikdw dlikdphi.
  fnh:  a function f, such that for each node n, f(n) returns
        a list of three 3-D arrays, such that ans[['V']][Vij,,] is the
        Hessian of V_ij wrt. to the user parametrisation; and so on
        for 'w' and 'Phi'.

  After:
  RH is updated.
  
*/
SEXP Rcurvifyhess(SEXP RH, SEXP Rpar, SEXP tr, SEXP fnh, SEXP env) {
	/* R caller should check the type, mode and dimension of RH, tr and so on. */
	double *wsp;
	int npar;
	struct node *t, *p;
	t = (struct node *) R_ExternalPtrAddr(tr);
	npar = INTEGER(PROTECT(getAttrib(RH, R_DimSymbol)))[0];
	if (!(wsp = malloc((2 * npar * npar + 1) * sizeof(double))))
		error("Rcurvifyhess(): failed in malloc()");
	dzero(wsp, 2*npar*npar+1);
	for (p = t->chd; p; p = p->nxtsb)
		curvifyhess(REAL(RH), p, npar, t->ndat.ku, fnh, env, wsp, Rpar);
	free(wsp);
	UNPROTECT(1);
	return R_NilValue;
}

/* --- Utility "proper" interfaces to some Fortran functions ----- */

/* Just sylgecpy_. v MUST be double and k MUST be integer. */
SEXP Rsylgecpy(SEXP Rv, SEXP Rk) {
	SEXP Rout; int *k;
	k = INTEGER(Rk);
	Rout = PROTECT(allocVector(REALSXP, (*k)*(*k)));
	dzero(REAL(Rout), (*k)*(*k));
	sylgecpy_(REAL(Rout), REAL(Rv), k);
	UNPROTECT(1);
	return Rout;
}

SEXP Rparamrestrict(SEXP Rcmdstr, SEXP Rpar, SEXP Rk, SEXP Rfixedpart) {
	SEXP Rout;
	const char *cmdstr;
	double *in, *out;
	int len_par, len_out, k;
        /* 1: Expects output type or termination;
           2: Expects input type for 'M';
           3: Expects input type for 'V';
           4: Expects input type for 'L';
           (5: Expects input type argument.) */
	int curstate = 1, i, iout, iin;
	int cnt_M = 0; int cnt_V = 0; int cnt_L = 0;
	int fixed_ptr = 0;
	int fixed_chked = 0;
	
	cmdstr = CHAR(PROTECT(STRING_ELT(Rcmdstr,0)));
	in     = REAL(Rpar);
	len_par= length(Rpar);
	k      = INTEGER(Rk)[0];
	/* Count output length. */
	for (i=0; cmdstr[i]; ++i) {
		switch (cmdstr[i]) {
		case 'M':       /* Matrix  */
			++cnt_M;
			break;
		case 'V':       /* Vector  */
			++cnt_V;
			break;
		case 'L':	/* Lower triangular  */
			++cnt_L;
		}
	}
	if (i == 0) error("Rparamrestrict(): parameter restriction does not contain any 'M', 'v', or 'L'.");
	len_out = cnt_M*k*k + cnt_V*k + cnt_L*(k*(k+1))/2;
	/* Allocate the output according to len_out */
	Rout = PROTECT(allocVector(REALSXP, len_out));
	out  = REAL(Rout);
	dzero(out, len_out);
	/* Now parse the string. */
	i = 0;
	iout = 0;
	iin  = 0;
	while (cmdstr[i]) {
		switch (curstate) {
		case 1: /* Expects output type or termination; */
			if (cmdstr[i] == 'M')          curstate = 2;
			else if (cmdstr[i] == 'V')     curstate = 3;
			else if (cmdstr[i] == 'L')     curstate = 4;
			else {
				error("Expected output type declaration at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 2: /* Expects input argument for 'M'; */
			if (cmdstr[i] == 's') {                /* symmetric */
				sylgecpy_(out+iout,in+iin,&k);
				iin  += (k*(k+1))/2;
				iout += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'l') {        /* Log cholesky */
				double *wsp;    int lwsp = k*k;    int info = 0;
				if (!(wsp = malloc(lwsp*sizeof(double))))
					error("Failed to allocate memory");
				lnunchol_(in+iin, &k, wsp, &lwsp, out+iout, &info);
				free(wsp);
				if (info) {
					error("Failed to revert logged-diagonal cholesky. INFO=%d", info);
				}
				iin  += (k*(k+1))/2;
				iout += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'c') { /* cholesky */
				double *wsp;    int lwsp = k*k;    int info = 0;
				if (!(wsp = malloc(lwsp*sizeof(double))))
					error("Failed to allocate memory");
				unchol_(in+iin, &k, wsp, &lwsp, out+iout, &info);
				free(wsp);
				if (info)
					error("Failed to revert logged-diagonal cholesky. INFO=%d", info);
				iin  += (k*(k+1))/2;
				iout += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'd') { /* diagonal */
				/* Construct a flattened diagonal matrix. */
				int jdiag=0;/* Diagonal counter */
				if (iin+k > len_par)
					error("Passed in parameter vector is too short");
				for (int jcol=0; jcol < k; ++jcol) { /* for each column */
					int jrow=0;
					while (jrow < jdiag) { out[iout++]=0.0; ++jrow; }
					out[iout++] = in[iin++];
					++jrow;
					while (jrow < k) { out[iout++]=0.0; ++jrow; };
					++jdiag;
				}
				curstate = 1;
			} else if (cmdstr[i] == 'e') { /* log-diagonal */
				/* Construct a flattened diagonal matrix. */
				int jdiag=0;/* Diagonal counter */
				if (iin+k > len_par)
					error("Passed in parameter vector is too short");
				for (int jcol=0; jcol < k; ++jcol) { /* for each column */
					int jrow=0;
					while (jrow < jdiag) { out[iout++]=0.0; ++jrow; }
					out[iout++] = exp(in[iin++]);
					++jrow;
					while (jrow < k) { out[iout++]=0.0; ++jrow; };
					++jdiag;
				}
				curstate = 1;
			} else if (cmdstr[i] == '0') { /* zero out */
				for (int j=0; j<k*k; ++j) out[iout++] = 0.0;
				curstate = 1;
			} else if (cmdstr[i] == 'f') { /* fixed, but non-zero */
				double *fixed;
				SEXP Rfixed_elt;
				if (!fixed_chked) {
					fixed_chked = 1;
					if (TYPEOF(Rfixedpart) != VECSXP)
						error("Fixed parameter parts should be a list of double precision vectors");
				}
				if (fixed_ptr >= length(Rfixedpart)) {
					error("The list of fixed parameters is too short");
				}
				Rfixed_elt = PROTECT(VECTOR_ELT(Rfixedpart, fixed_ptr++));
				if (length(Rfixed_elt) != k*k)
					error("The length of the %d-th fixed parameter part is incorrect: should be %d but I've got a vector of length %d",
					      fixed_ptr, k*k, length(Rfixed_elt));
				if (TYPEOF(Rfixed_elt) != REALSXP)
					error("The %d-th fixed parameter is not a double precision numeric vector", fixed_ptr);
				fixed = REAL(Rfixed_elt);
				for (int j=0; j<k*k; ++j) out[iout++] = fixed[j];
				UNPROTECT(1);
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				/* Copy the entire kxk matrix */
				if (iin+k*k > len_par)
					error("Passed in parameter vector is too short");
				for (int j=0; j<k*k; ++j) out[iout++] = in[iin++];
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 3: /* Expects input argument for 'V'; */
			if (cmdstr[i] == '0') { /* zero out */
				for (int j=0; j<k; ++j) out[iout++] = 0.0;
				curstate = 1;
			} else if (cmdstr[i] == 'f') {
				double *fixed;
				SEXP Rfixed_elt;
				if (!fixed_chked) {
					fixed_chked = 1;
					if (TYPEOF(Rfixedpart) != VECSXP) {
						UNPROTECT(1);
						error("Fixed parameter parts should be a list of double precision vectors");
					}
				}
				if (fixed_ptr >= length(Rfixedpart))
					error("The list of fixed parameters is too short");
				Rfixed_elt = PROTECT(VECTOR_ELT(Rfixedpart, fixed_ptr++));
				if (length(Rfixed_elt) != k)
					error("The length of the %d-th fixed parameter part is incorrect: should be %d but I've got a vector of length %d", fixed_ptr, k, length(Rfixed_elt));
				if (TYPEOF(Rfixed_elt) != REALSXP)
					error("The %d-th fixed parameter is not a double precision numeric vector", fixed_ptr);
				fixed = REAL(Rfixed_elt);
				for (int j=0; j<k; ++j) out[iout++] = fixed[j];
				UNPROTECT(1);
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				/* Copy a k-vector */
				if (iin+k > len_par)
					error("Passed in parameter vector is too short");
				for (int j=0; j<k; ++j) out[iout++] = in[iin++];
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 4: /* Expects input argument for 'L' */
			if (cmdstr[i] == 'k') { /* keep the same */
				if (iin+(k*(k+1))/2 > len_par)
					error("Passed in parameter vector is too short");
				for (int j=0; j<(k*(k+1))/2; ++j) {
					out[iout++] = in[iin++];
				}
				curstate = 1;
			} else if (cmdstr[i] == 'd') { /* Diagonal restriction of lower-triangular matrices */
				if (iin+k > len_par) {
					UNPROTECT(1);
					error("Passed in parameter vector is too short");
				}
				diag2ltri_(in+iin,&k,out+iout);
				iin  += k;
				iout += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'f') {
				double *fixed;
				SEXP Rfixed_elt;
				if (!fixed_chked) {
					fixed_chked = 1;
					if (TYPEOF(Rfixedpart) != VECSXP)
						error("Fixed parameter parts should be a list of double precision vectors");
				}
				if (fixed_ptr >= length(Rfixedpart))
					error("The list of fixed parameters is too short");
				Rfixed_elt = PROTECT(VECTOR_ELT(Rfixedpart, fixed_ptr++));
				if (length(Rfixed_elt) != (k*(k+1))/2)
					error("The length of the %d-th fixed parameter part is incorrect: should be %d but I've got a vector of length %d", fixed_ptr, (k*(k+1))/2, length(Rfixed_elt));
				if (TYPEOF(Rfixed_elt) != REALSXP)
					error("The %d-th fixed parameter is not a double precision numeric vector", fixed_ptr);
				fixed = REAL(Rfixed_elt);
				for (int j=0; j<(k*(k+1))/2; ++j) out[iout++] = fixed[j];
				UNPROTECT(1);
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			// THIS BREAK STATEMENT WILL BUG OUT A BIG-NAME COMMERCIAL COMPILER.
			//break;
		}
		++i;
	}
	UNPROTECT(2);
	if (iin != len_par) {
		if (iin < len_par)
			error("Passed in parameter vector is too long (=%d) compared to what the command string specifies (=%d)",
			      len_par, iin);
		if (iin > len_par)
			error("Passed in parameter vector is "
			      "too short (=%d) compared to what the command string specifies (=%d)",
			      len_par, iin);
	}
	return Rout;
}

/* Post-process jacobian_original(restricted_par) so that new Jacobian is
   the Jacobian of the parfn_original(restricted_par). */
SEXP Rpostjacrestrict(SEXP Rcmdstr, SEXP Rpar, SEXP Rjac, SEXP Rk) {
	SEXP Rjacout, Rdim_jacin, Rdim_jacout;
	const char *cmdstr;
	double *parin, *jacout, *jacin;
	int len_parin, len_jacout, len_rng, len_jaccolorig, k, *dim_jacout;
        /* 1: Expects output type or termination;
           2: Expects input type for 'M';
           3: Expects input type for 'V';
           4: Expects input type for 'L';
           (5: Expects further argument. Not yet implemented.) */
	int curstate = 1, i, ijacout, ijacin;
	int cnt_M = 0; int cnt_V = 0; int cnt_L = 0;
	cmdstr   = CHAR(PROTECT(STRING_ELT(Rcmdstr,0)));
	parin    = REAL(Rpar);
	jacin    = REAL(Rjac);
	len_parin= length(Rpar);
	k        = INTEGER(Rk)[0];
	/* Count output length. */
	for (i=0; cmdstr[i]; ++i) {
		switch (cmdstr[i]) {
		case 'M':       /* Matrix  */
			++cnt_M;
			break;
		case 'V':       /* Vector  */
			++cnt_V;
			break;
		case 'L':	/* Lower triangular  */
			++cnt_L;
		}
	}
	if (i == 0) error("parameter restriction does not contain any 'M', 'v', or 'L'.");
	/* How many dimension is the jac function's range? */
	Rdim_jacin = PROTECT(getAttrib(Rjac, R_DimSymbol));
	len_rng  = INTEGER(Rdim_jacin)[0];
	len_jaccolorig = cnt_M*k*k + cnt_V*k + cnt_L*(k*(k+1))/2; /* Need to check counters against this*/
	if (INTEGER(Rdim_jacin)[1] != len_jaccolorig) {
		error("The passed-in Jacobian should have %d rows but it has %d", len_jaccolorig, INTEGER(Rdim_jacin)[1]);
	}
	len_jacout = len_rng*len_parin;
	/* Allocate the output according to len_out */
	Rjacout = PROTECT(allocVector(REALSXP, len_jacout));
	jacout  = REAL(Rjacout);
	
	for (i = 0; i<len_jacout; ++i) jacout[i] = 0.0;

	/* Now parse the string. */
	i = 0;
	ijacout = 0;
	ijacin  = 0;
	while (cmdstr[i]) {
		switch (curstate) {
		case 1: /* Expects output type or termination; */
			if (cmdstr[i] == 'M')          curstate = 2;
			else if (cmdstr[i] == 'V')     curstate = 3;
			else if (cmdstr[i] == 'L')     curstate = 4;
			else
				error("Expected output type declaration at position %d but got '%c'", i, cmdstr[i]);
			break;
		case 2: /* Expects input argument for 'M'; */
			if (cmdstr[i] == 's') {                /* symmetric */
				/* Sums up the lower and upper part to make the new lower part */
				int ioutidx, ifinx, ifiny;
				for (int iiny = 0; iiny < k; ++iiny) {
					int iinx;
					ifiny = iiny+1;
					for (iinx = 0; iinx < iiny; ++iinx) {
						ifinx = iinx+1;
						ioutidx = ijacout+iijtouplolidx_(&k, &ifiny, &ifinx)-1; /* x<y */
						for (int jrng=0; jrng < len_rng; ++jrng)
							jacout[jrng+ioutidx*len_rng]+= jacin[jrng+ijacin*len_rng];
						++ijacin;
					}
					for (; iinx < k; ++iinx) {
						ifinx = iinx+1; ifiny = iiny+1;
						ioutidx = ijacout+iijtouplolidx_(&k, &ifinx, &ifiny)-1; /* y<x */
						for (int jrng=0; jrng < len_rng; ++jrng)
							jacout[jrng+ioutidx*len_rng]+= jacin[jrng+ijacin*len_rng];
						++ijacin;
					}
				}
				ijacout += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'l') { /* Log cholesky */
				dlnchnunchol_(jacin+ijacin*len_rng, parin+ijacin, &len_rng, &k, jacout+ijacout);
				ijacin  += k*k;
				ijacout += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'c') { /* cholesky */
				dchnunchol_(jacin+ijacin*len_rng, parin+ijacin, &len_rng, &k, jacout+ijacout);
				ijacin  += k*k;
				ijacout += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'd') { /* diagonal */
				int jdiag=0;/* Diagonal counter */
				for (int ipcol=0; ipcol < k; ++ipcol) { /* for each column */
					ijacin += jdiag;
					for (int jrng=0; jrng < len_rng; ++jrng)
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng];
					ijacin += k - jdiag;
					++ijacout;
					++jdiag;
				}
				curstate = 1;
			} else if (cmdstr[i] == 'e') { /* log-diagonal */
				int jdiag=0;/* Diagonal counter */
				for (int ipcol=0; ipcol < k; ++ipcol) { /* for each column */
					ijacin += jdiag;
					for (int jrng=0; jrng < len_rng; ++jrng)
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng] * exp(parin[ijacout]);
					ijacin += k - jdiag;
					++ijacout;
					++jdiag;
				}
				curstate = 1;
			} else if (cmdstr[i] == '0') { /* zero out */
				ijacin += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'f') { /* Fixed, but not zero */
				ijacin += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				/* Copy the entire chunk of Jacobian */
				for (int j=0; j<k*k; ++j) {
					for (int jrng=0; jrng < len_rng; ++jrng) {
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng];
					}
					++ijacout;
					++ijacin;
				}
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 3: /* Expects input argument for 'V'; */
			if (cmdstr[i] == '0') { /* zero out */
				ijacin += k;
				curstate = 1;
			} else if (cmdstr[i] == 'f') { /* Fixed, but not zero */
				ijacin += k;
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				/* Copy the entire chunk of Jacobian */
				for (int j=0; j<k; ++j) {
					for (int jrng=0; jrng < len_rng; ++jrng)
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng];
					++ijacout;
					++ijacin;
				}
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 4: 	/* Expects input argument for 'L' */
			if (cmdstr[i] == 'k') { /* keep the same */
				/* Copy the entire chunk of Jacobian */
				for (int j=0; j<(k*(k+1))/2; ++j) {
					for (int jrng=0; jrng < len_rng; ++jrng)
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng];
					++ijacout;
					++ijacin;
				}
				curstate = 1;
			} else if (cmdstr[i] == 'd') { /* Diagonal restriction for lower triangular matrices */
				/* Copy only the diagonal par and skip the rest */
				int jdiag=0;/* Diagonal counter */
				for (int ipcol=0; ipcol < k; ++ipcol) { /* for each column */
					for (int jrng=0; jrng < len_rng; ++jrng)
						jacout[jrng+ijacout*len_rng]= jacin[jrng+ijacin*len_rng];
					ijacin += k - jdiag;
					++ijacout;
					++jdiag;
				}
			} else if (cmdstr[i] == 'f') { /* Fixed, but not zero */
				ijacin += (k*(k+1))/2;
				curstate = 1;
			} else
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
		}
		++i;
	}
	/* Add dim attributes to the output */
	Rdim_jacout = PROTECT(allocVector(INTSXP, 2));
	dim_jacout = INTEGER(Rdim_jacout);
	dim_jacout[0] = len_rng;
	dim_jacout[1] = len_parin;
	setAttrib(Rjacout, R_DimSymbol, Rdim_jacout);
	UNPROTECT(4);
	return Rjacout;
}


/* 
   Same as Rpostjacrestrict but for Hessian. Last two arguemnts can be NULL if not needed.
   Rhess is a Hessian, not a named list.
*/
SEXP Rposthessrestrict(SEXP Rcmdstr, SEXP Rpar, SEXP Rhess, SEXP Rk,
		       SEXP Rjaclower, SEXP Rjloweroffset, SEXP Rjacthis, SEXP Rjthisoffset_r, SEXP Rjthisoffset_c) {
	SEXP Rhessout, Rdim_hessin, Rdim_hessout;
	const char *cmdstr;
	double *parin, *hessout, *hessin, *jaclower, *hessouttmp, *jacthis;
	int len_parin, len_hessout, len_rng, len_hesscolorig, k, *dim_hessout,
		jloweroffset, ld_jaclower, ld_jacthis, len_hesscurr, jthisoffset_r, jthisoffset_c;
        /* 1: Expects output type or termination;
           2: Expects input type for 'M';
           3: Expects input type for 'V';
           4: Expects input type for 'L';
           (5: Expects further argument. Not yet implemented.) */
	int curstate = 1, i, isquashed;
	int ihessin __UNUSED;
	int chked_jaclower = 0;
	int chked_jacthis = 0;
	int cnt_M = 0; int cnt_V = 0; int cnt_L = 0;
	jacthis = NULL;    /* Just to make GCC shut up. */
	jaclower  = NULL;  /* Just to make GCC shut up. */
	ld_jacthis = 0;    /* Same */
	jthisoffset_c = 0; /* Same */
	jthisoffset_r = 0; /* Same */
	cmdstr   = CHAR(PROTECT(STRING_ELT(Rcmdstr,0)));
	parin    = REAL(Rpar);
	hessin    = REAL(Rhess);
	len_parin= length(Rpar);
	k        = INTEGER(Rk)[0];
	/* Count output length. */
	for (i=0; cmdstr[i]; ++i) {
		switch (cmdstr[i]) {
		case 'M':       /* Matrix  */
			++cnt_M;
			break;
		case 'V':       /* Vector  */
			++cnt_V;
			break;
		case 'L':	/* Lower triangular  */
			++cnt_L;
		}
	}
	if (i == 0) error("parameter restriction does not contain any 'M', 'v', or 'L'.");
	/* How many dimension is the Hessian function's range? */
	Rdim_hessin = PROTECT(getAttrib(Rhess, R_DimSymbol));
	len_rng  = INTEGER(Rdim_hessin)[0];
	len_hesscolorig = cnt_M*k*k + cnt_V*k + cnt_L*(k*(k+1))/2;
	if (INTEGER(Rdim_hessin)[1] != len_hesscolorig || INTEGER(Rdim_hessin)[2] != len_hesscolorig)
		error("The passed-in Hessian has a wrong dimension.");
	len_hessout = len_rng*len_parin*len_parin;
	/* Allocate the temporary storage for each step and copy the original hessian into here. */
	if (!(hessouttmp = malloc(len_rng*len_hesscolorig*len_hesscolorig*sizeof(double))))
		error("Failed to allocate memory.");
	memcpy(hessouttmp, hessin, len_rng*len_hesscolorig*len_hesscolorig*sizeof(double));
	/* Allocate the output according to len_out */
	Rhessout = PROTECT(allocVector(REALSXP, len_hessout));
	hessout  = REAL(Rhessout);
	len_hesscurr = len_hesscolorig; /* Will be decreased step-by-step until it reaches len_parin. */
	for (i = 0; i<len_hessout; ++i) hessout[i] = 0.0;
	/* Now parse the string. */
	i = 0;
	isquashed= 0;
	ihessin  = 0;
	while (cmdstr[i]) {
		switch (curstate) {
		case 1: /* Expects output type or termination; */
			if      (cmdstr[i] == 'M')     curstate = 2;
			else if (cmdstr[i] == 'V')     curstate = 3;
			else if (cmdstr[i] == 'L')     curstate = 4;
			else    error("Expected output type declaration at position %d but got '%c'", i, cmdstr[i]);
			break;
		case 2: /* Expects input argument for 'M'; */
			if (cmdstr[i] == 's') {        /* symmetric */
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip = k*k-(k*(k+1))/2; /* NOTE! Can be zero, if and only if k=1. */
				if (nskip == 0) {
					/* do nothing */
					ihessin += 1;
					isquashed+= 1;
				} else {
					len_hesscurr_new = len_hesscurr - nskip;
					if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
						goto MEMFAIL;
					houchnsymh_(hessouttmp, &len_rng, &k, &len_hesscurr, &isquashed, hessouttmp_new);
					free(hessouttmp);
					hessouttmp   = hessouttmp_new;
					len_hesscurr = len_hesscurr_new;
					ihessin += k*k;
					isquashed += (k*(k+1))/2;
				}
				curstate = 1;
			} else if (cmdstr[i] == 'l') { /* Log cholesky */
				double *hessouttmp_new;
				int len_hesscurr_new;
				if (isNull(Rjaclower)) { error("Log-Cholesky 2nd-order chain rule required but Rjaclower is NULL"); }
				if (!chked_jaclower) {
					SEXP Rdim_jaclower;
					if (TYPEOF(Rjaclower) != REALSXP)
						error("Cholesky 2nd-order chain rule required but Rjaclower is NULL");
					Rdim_jaclower = getAttrib(Rjaclower, R_DimSymbol);
					if (TYPEOF(Rdim_jaclower) != INTSXP || length(Rdim_jaclower) != 2)
						error("Rjaclower must be two-dimensional");
					if (INTEGER(Rdim_jaclower)[1] != len_hesscolorig)
						error("ncol(Rjaclower) mismatches the original Hessian");
					if (TYPEOF(Rjloweroffset) != INTSXP || length(Rjloweroffset) != 1)
						error("Rjloweroffset is not an integer. Please as.integer() if called from R.");
					jloweroffset = INTEGER(Rjloweroffset)[0];
					ld_jaclower = INTEGER(Rdim_jaclower)[0];
					if (jloweroffset + len_rng > ld_jaclower || jloweroffset < 0)
					        error("Rjloweroffset is too big or negative");
					jaclower = REAL(Rjaclower);
					chked_jaclower = 1;
				}
				len_hesscurr_new = len_hesscurr - k*k + (k*(k+1))/2;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				dzero(hessouttmp_new, len_rng*len_hesscurr_new*len_hesscurr_new);
				houlnspdh_(hessouttmp, parin+isquashed, jaclower, &ld_jaclower, &jloweroffset,
					   &len_rng, &k, &len_hesscurr, &len_hesscurr_new, &isquashed, hessouttmp_new);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += k*k;
				isquashed += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'c') { /* cholesky */
				double *hessouttmp_new;
				int len_hesscurr_new;
				if (isNull(Rjaclower)) error("Cholesky 2nd-order chain rule required but Rjaclower is NULL");
				if (!chked_jaclower) {
					SEXP Rdim_jaclower;
					if (TYPEOF(Rjaclower) != REALSXP)
						error("Cholesky 2nd-order chain rule required but Rjaclower is NULL");
					Rdim_jaclower = getAttrib(Rjaclower, R_DimSymbol);
					if (TYPEOF(Rdim_jaclower) != INTSXP || length(Rdim_jaclower) != 2)
						error("Rjaclower must be two-dimensional");
					if (INTEGER(Rdim_jaclower)[1] != len_hesscolorig)
						error("ncol(Rjaclower) mismatches the original Hessian");
					if (TYPEOF(Rjloweroffset) != INTSXP || length(Rjloweroffset) != 1)
						error("Rjloweroffset is not an integer. Please as.integer() if called from R.");
					jloweroffset = INTEGER(Rjloweroffset)[0];
					ld_jaclower = INTEGER(Rdim_jaclower)[0];
					if (jloweroffset + len_rng > ld_jaclower || jloweroffset < 0)
					        error("Rjloweroffset is too big or negative");
					jaclower = REAL(Rjaclower);
					chked_jaclower = 1;
				}
				len_hesscurr_new = len_hesscurr - k*k + (k*(k+1))/2;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				dzero(hessouttmp_new, len_rng*len_hesscurr_new*len_hesscurr_new);
				houspdh_(hessouttmp, parin+isquashed, jaclower, &ld_jaclower, &jloweroffset,
					 &len_rng, &k, &len_hesscurr, &len_hesscurr_new, &isquashed, hessouttmp_new);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += k*k;
				isquashed += (k*(k+1))/2;
				curstate = 1;
			} else if (cmdstr[i] == 'd') { /* diagonal */
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip = k*k-k; /* NOTE! Can be zero, if and only if k=1. */
				if (nskip == 0) {
					/* do nothing */
					ihessin += 1;
					isquashed+= 1;
				} else {
					len_hesscurr_new = len_hesscurr - nskip;
					if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
						goto MEMFAIL;
					hesschopnondiag_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr, &len_rng, &isquashed, &k);
					free(hessouttmp);
					hessouttmp   = hessouttmp_new;
					len_hesscurr = len_hesscurr_new;
					ihessin += k*k;
					isquashed += k;
				}
				curstate = 1;
			} else if (cmdstr[i] == 'e') { /* log-diagonal */
				int len_hesscurr_new;
				double *hessouttmp_new;
				if (isNull(Rjacthis)) error("Log-diagonal 2nd-order chain rule required but Rjacthis is NULL");
				if (!chked_jacthis) {
					SEXP Rdim_jacthis;
					if (TYPEOF(Rjacthis) != REALSXP)
						error("Log diagonal 2nd-order chain rule required but Rjacthis is NULL");
					Rdim_jacthis = PROTECT(getAttrib(Rjacthis, R_DimSymbol));
					if (TYPEOF(Rdim_jacthis) != INTSXP || length(Rdim_jacthis) != 2)
						error("Rjacthis must be two-dimensional");
					if (TYPEOF(Rjthisoffset_c) != INTSXP || length(Rjthisoffset_c) != 1)
						error("Rjthisoffset_c is not an integer. Please as.integer() if called from R.");
					if (TYPEOF(Rjthisoffset_r) != INTSXP || length(Rjthisoffset_r) != 1)
						error("Rjthisoffset_r is not an integer. Please as.integer() if called from R.");
					jthisoffset_c = INTEGER(Rjthisoffset_c)[0];
					jthisoffset_r = INTEGER(Rjthisoffset_r)[0];
					ld_jacthis    = INTEGER(Rdim_jacthis)[0];
					if (jthisoffset_r + len_rng > ld_jacthis || jthisoffset_r < 0)
					        error("Rjthisoffset_r is too big or negative");
					if (jthisoffset_c + k > (INTEGER(Rdim_jacthis)[1]) || jthisoffset_c < 0)
					        error("Rjthisoffset_c is too big or negative, or Rjacthis has too few columns.");
					jacthis = REAL(Rjacthis);
					UNPROTECT(1);
					chked_jacthis = 1;
				}
				len_hesscurr_new = len_hesscurr - k*k+k;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				hchnlndiag_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr,
					    parin+isquashed, jacthis+ld_jacthis*jthisoffset_c, &ld_jacthis, &jthisoffset_r,
					    &len_rng, &isquashed, &k);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += k*k;
				isquashed += k;
				curstate = 1;
			} else if (cmdstr[i] == '0' || cmdstr[i] == 'f') { /* zero out or fixed */
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip = k*k;
				len_hesscurr_new = len_hesscurr - nskip;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				hesscpyskip_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr, &len_rng, &isquashed, &nskip);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += k*k;
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				ihessin += k*k;
				isquashed+= k*k;
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 3: /* Expects input argument for 'V'; */
			if (cmdstr[i] == '0' || cmdstr[i] == 'f') { /* zero out or fixed */
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip;
				nskip = k;
				len_hesscurr_new = len_hesscurr - nskip;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				hesscpyskip_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr, &len_rng, &isquashed, &nskip);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += k;
				curstate = 1;
			} else if (cmdstr[i] == 'k') { /* keep the same */
				ihessin += k;
				isquashed+= k;
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
			break;
		case 4: 	/* Expects input argument for 'L' */
			if (cmdstr[i] == 'k') { /* keep the same */
				ihessin   += (k*(k+1))/2;
				isquashed += (k*(k+1))/2;
				curstate   = 1;
			} else if (cmdstr[i] == 'd') {
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip;
				nskip = (k*(k+1))/2 - k;
				len_hesscurr_new = len_hesscurr - nskip;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				hessdiag2ltri_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr, &len_rng, &k, &isquashed);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin   += (k*(k+1))/2;
				isquashed += k;
			} else if (cmdstr[i] == 'f') {
				double *hessouttmp_new;
				int len_hesscurr_new;
				int nskip;
				nskip = (k*(k+1))/2;
				len_hesscurr_new = len_hesscurr - nskip;
				if (!(hessouttmp_new=malloc(len_rng*len_hesscurr_new*len_hesscurr_new*sizeof(double))))
					goto MEMFAIL;
				hesscpyskip_(hessouttmp_new, &len_hesscurr_new, hessouttmp, &len_hesscurr, &len_rng, &isquashed, &nskip);
				free(hessouttmp);
				hessouttmp   = hessouttmp_new;
				len_hesscurr = len_hesscurr_new;
				ihessin += (k*(k+1))/2;
				curstate = 1;
			} else {
				error("Expected input type at position %d but got '%c'", i, cmdstr[i]);
			}
		}
		++i;
	}
	/* Add dim attributes to the output */
	if (len_hesscurr != len_parin)
		error("Input parameter length mismatches what the command string describes");
	memcpy(hessout, hessouttmp, len_rng*len_parin*len_parin*sizeof(double));
	Rdim_hessout = PROTECT(allocVector(INTSXP, 3));
	dim_hessout = INTEGER(Rdim_hessout);
	dim_hessout[0] = len_rng;
	dim_hessout[1] = len_parin;
	dim_hessout[2] = len_parin;
	setAttrib(Rhessout, R_DimSymbol, Rdim_hessout);
	UNPROTECT(4);
	return Rhessout;
MEMFAIL:
	error("Failed to allocate memory");
	return R_NilValue;	/* Unreachable. */
}


extern void (glinvtestfloatIEEE01_)(double *, double*);
extern SEXP glinvtestfloatIEEE02 (SEXP);
extern void (glinvtestfloatIEEE03)(double *, double *);
static const R_CallMethodDef callMethods[]  = {
	{"Rparamrestrict",       (DL_FUNC) &Rparamrestrict,       4},
	{"Rpostjacrestrict",     (DL_FUNC) &Rpostjacrestrict,     4},
	{"Rposthessrestrict",    (DL_FUNC) &Rposthessrestrict,    9},
	{"Rcurvifyhess",         (DL_FUNC) &Rcurvifyhess,         5},
	{"Rchkusrhess",          (DL_FUNC) &Rchkusrhess,          7},
	{"Rextractderivvec",     (DL_FUNC) &Rextractderivvec,     1},
	{"Rdphylik",             (DL_FUNC) &Rdphylik,             5},
	{"Rhphylik",             (DL_FUNC) &Rhphylik,             5},
	{"Rhphylik_dir",         (DL_FUNC) &Rhphylik_dir,         6},
	{"Rextracthessall",      (DL_FUNC) &Rextracthessall,      1},
	{"Rndesc",               (DL_FUNC) &Rndesc,               1},
	{"Rnparams",             (DL_FUNC) &Rnparams,             1},
	{"Rxavail",              (DL_FUNC) &Rxavail,              1},
	{"Rsettip",              (DL_FUNC) &Rsettip,              3},
	{"Rvwphi_simul",         (DL_FUNC) &Rvwphi_simul,         6},
	/*{"R_clone_tree",         (DL_FUNC) &R_clone_tree,         1},*/
	{"Rnewnode",             (DL_FUNC) &Rnewnode,             3},
	{"Rndphylik",            (DL_FUNC) &Rndphylik,            4},
	{"Rvwphi_paradr",        (DL_FUNC) &Rvwphi_paradr,        1},
	{"Rtagmiss",             (DL_FUNC) &Rtagmiss,             3},
	{"Rtagreg",              (DL_FUNC) &Rtagreg,              3},
	{"Rtested",              (DL_FUNC) &Rtested,              0},
	{"Risnullptr",           (DL_FUNC) &Risnullptr,           1},
	{"Rgetroot",             (DL_FUNC) &Rgetroot,             1},
	{"glinvtestfloatIEEE02", (DL_FUNC) &glinvtestfloatIEEE02, 1},
	{NULL, NULL, 0}
};
struct fortcplx_t {double dr; double di;}; /* Needed just for GCC's LTO warning... */
typedef struct fortcplx_t fortcplx_t;
extern void (d0geouvwphi_)(double *,int *,double *,double *,double *,double *,double *,double *,fortcplx_t *,fortcplx_t *,
	fortcplx_t *,double *,int *,fortcplx_t *,int *,int *,int *);
static R_NativePrimitiveArgType d0geouvwphi_type[] = {
	REALSXP,INTSXP,REALSXP,REALSXP,REALSXP,REALSXP,REALSXP,REALSXP,CPLXSXP,CPLXSXP,
	CPLXSXP,REALSXP,INTSXP,CPLXSXP,INTSXP,INTSXP,INTSXP};
extern void (ougejac_)(double *,int *,double *,fortcplx_t *,fortcplx_t *,fortcplx_t *,double *,int *,fortcplx_t *,int *,int *,double *,int *);
static R_NativePrimitiveArgType ougejac_type[] = {
	REALSXP,INTSXP,REALSXP,CPLXSXP,CPLXSXP,CPLXSXP,REALSXP,INTSXP,CPLXSXP,INTSXP,INTSXP,REALSXP,INTSXP};
static R_NativePrimitiveArgType lnunchol_type[] = {
	REALSXP, INTSXP, REALSXP, INTSXP, REALSXP, INTSXP};
extern void (hphiha_)(double *,int *,fortcplx_t *,fortcplx_t *,fortcplx_t *,double *,fortcplx_t *,int *,int *);
static R_NativePrimitiveArgType hphiha_type[] = {
	REALSXP,INTSXP,CPLXSXP,CPLXSXP,CPLXSXP,REALSXP,CPLXSXP,INTSXP,INTSXP};
extern void (hvha_)(double *,double *,double *,int *,fortcplx_t *,fortcplx_t *,fortcplx_t *,double *,double *,int *,fortcplx_t *,int *,int *,int *);
static R_NativePrimitiveArgType hvha_type[] = {
	REALSXP,REALSXP,REALSXP,INTSXP,CPLXSXP,CPLXSXP,CPLXSXP,REALSXP,REALSXP,INTSXP,CPLXSXP,INTSXP,INTSXP,INTSXP};
extern void (hvdadl_)(double *,double *,int *,double *,fortcplx_t *,fortcplx_t *,fortcplx_t *,double *,double *,int *,fortcplx_t *,int *,int *);
static R_NativePrimitiveArgType hvdadl_type[] = {
	REALSXP,REALSXP,INTSXP,REALSXP,CPLXSXP,CPLXSXP,CPLXSXP,REALSXP,REALSXP,INTSXP,CPLXSXP,INTSXP,INTSXP};
extern void (hvhl_)(double *, int *, double *, fortcplx_t *, fortcplx_t *, fortcplx_t *, double *, int *, fortcplx_t *, int *, double *);
static R_NativePrimitiveArgType hvhl_type[] = {
	REALSXP, INTSXP, REALSXP, CPLXSXP, CPLXSXP, CPLXSXP, REALSXP, INTSXP, CPLXSXP, INTSXP, REALSXP};
extern void (hwdthetada_)(int *, double *, double *);
static R_NativePrimitiveArgType hwdthetada_type[] = { INTSXP, REALSXP, REALSXP };
extern void (dphida_)(double *, int *, fortcplx_t *, fortcplx_t *, fortcplx_t *, double *, fortcplx_t *, int *);
static R_NativePrimitiveArgType dphida_type[] = {
	REALSXP,INTSXP,CPLXSXP,CPLXSXP,CPLXSXP,REALSXP,CPLXSXP,INTSXP};
extern void (hwha_)(int *, double*, double *, double *);
static R_NativePrimitiveArgType hwha_type[] = {INTSXP,REALSXP,REALSXP,REALSXP};
static R_NativePrimitiveArgType glinvtestfloatIEEE01_type[] = {REALSXP,REALSXP};
static R_NativePrimitiveArgType glinvtestfloatIEEE03_type[] = {REALSXP,REALSXP};
static const R_CMethodDef cMethods[] = {
	{"d0geouvwphi_",          (DL_FUNC) &d0geouvwphi_,          17, d0geouvwphi_type           },
	{"ougejac_",              (DL_FUNC) &ougejac_,              13, ougejac_type               },
	{"lnunchol_",             (DL_FUNC) &lnunchol_,              6, lnunchol_type              },
	{"hphiha_",               (DL_FUNC) &hphiha_,                9, hphiha_type                },
	{"hvha_",                 (DL_FUNC) &hvha_,                 14, hvha_type                  },
	{"hvdadl_",               (DL_FUNC) &hvdadl_,               13, hvdadl_type                },
	{"hvhl_",                 (DL_FUNC) &hvhl_,                 11, hvhl_type                  },
	{"hwdthetada_",           (DL_FUNC) &hwdthetada_,            3, hwdthetada_type            },
	{"dphida_",               (DL_FUNC) &dphida_,                8, dphida_type                },
	{"hwha_",                 (DL_FUNC) &hwha_,                  4, hwha_type                  },
	{"glinvtestfloatIEEE01_", (DL_FUNC) &glinvtestfloatIEEE01_,  2, glinvtestfloatIEEE01_type  },
	{"glinvtestfloatIEEE03",  (DL_FUNC) &glinvtestfloatIEEE03,   2, glinvtestfloatIEEE03_type  },
	{NULL, NULL, 0, NULL}
};

void R_init_glinvci(DllInfo *info) {
	R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
}
