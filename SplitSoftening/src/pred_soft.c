#include "R.h"

struct ctx_s {
	int index;
	double *data;
	int ndata;
	int treesize;
	int *var;
	double *splits;
	int *ncat;
	double *lb;
	double *ub;
	int *childref;

	int *treerep;
	double *presence;
};

int resolve_categorial_branching(struct ctx_s *context, int node)
{
	unsigned int v = (unsigned int)context->data[context->ndata*context->var[node]+context->index];
	unsigned int b = (1<<(v-1));
	if ( b & (unsigned int)context->lb[node] )
		return context->treerep[node];
	if ( b & (unsigned int)context->ub[node] )
		return context->treerep[context->treesize+node];
	return node;
}

void fall_down(struct ctx_s *context, int node)
{
	double s;
	double v;
	int next;
	switch ( context->ncat[node] )
	{
		case 0:
			return;
		case 1:
		case -1:
			v = context->data[context->ndata*context->var[node]+context->index];
			if ( v <= context->lb[node] ) {
				next = context->treerep[node];
				context->presence[next] = context->presence[node];
				fall_down( context, next );
			} else if ( v >= context->ub[node] ) {
				next = context->treerep[context->treesize+node];
				context->presence[next] = context->presence[node];
				fall_down( context, next );
			} else {
				s = ( v <= context->splits[node] )
					? 1.0-(v-context->lb[node])/(2*(context->splits[node]-context->lb[node]))
					: (v-context->ub[node])/(2*(context->splits[node]-context->ub[node]));
				next = context->treerep[node];
				context->presence[next] = s * context->presence[node];
				context->presence[node] -= context->presence[next];
				fall_down( context, next );
				next = context->treerep[context->treesize+node];
				context->presence[next] = context->presence[node];
				fall_down( context, next );
			}
			context->presence[node] = 0.0;
			return;
		default:
			next = resolve_categorial_branching( context, node );
			if ( next != node ) {
				context->presence[next] = context->presence[node];
				fall_down( context, next );
				context->presence[node] = 0.0;
			}
			return;
	}
}

void pred_ss(double *data, int *ndata, int *dim, int *treesize, int *var,
               double *splits, int *ncat, double *lb, double *ub,
               int *childref, double *yval, int *nclass, double *prob)
{
	struct ctx_s ctx;
	ctx.data=data;
	ctx.ndata=*ndata;
	ctx.treesize=*treesize;
	ctx.var=var;
	ctx.splits=splits;
	ctx.ncat=ncat;
	ctx.lb=lb;
	ctx.ub=ub;
	ctx.childref=childref;

	ctx.treerep=(int*)R_alloc(*treesize*2, sizeof(int));
	for (int i = 0; i < *treesize; ++i ) {
		if ( 0 != ncat[i] ) {
			int sw = ( 1 == ncat[i] ) ? 1 : 0;
			int la = childref[i] + sw;
			int ha = la + 1 - 2*sw;
			ctx.treerep[i] = la - 1;
			ctx.treerep[*treesize+i] = ha - 1;
		}
	}

	ctx.presence=(double*)R_alloc(*treesize, sizeof(double));

	int root = 0;
	for ( ctx.index = 0; ctx.index < *ndata; ++ctx.index ) {
		for (int i = 0; i < *treesize; ++i)
			ctx.presence[i] = 0.0;
		ctx.presence[root] = 1.0;
		fall_down(&ctx, root);

		for ( int c=0; c<*nclass; ++c ) {
			double *target = prob + *ndata*c+ctx.index;
			*target = 0.0;
			for ( int n=0; n < *treesize; ++n ) {
				*target += yval[*treesize*c+n] * ctx.presence[n];
			}
		}
	}
}

