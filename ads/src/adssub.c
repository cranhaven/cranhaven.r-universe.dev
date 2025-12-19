#include "adssub.h"
#include <math.h>

double Pi(void) {
	return 2*acos(0);
}

void progress(int i,int *l, int max) {
	int nb=20;
	int p=i*(nb+1)/max;
	int j;

	if(p>*l){
		for(j=*l;j<p;j++) {
			if(j==nb) {
				Rprintf("ok\n");
			}
			else {
				Rprintf(".");
			}
		}
		*l=p;
	}
}

/*double alea () {
    double w;
    w = ((double) rand())/ (double)RAND_MAX;
    return (w);
}*/

/***********************************************************************/
/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
void freeintvec (int *vec) {
	free((char *) vec);

}

/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
void freetab (double **tab) {
    int     i, n;
    n = *(*(tab));
    for (i=0;i<=n;i++) {
            free((char *) *(tab+i) );
    }
    free((char *) tab);
}

/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
void freevec (double *vec) {
    free((char *) vec);
}

/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
void taballoc (double ***tab, int l1, int c1) {
    int i, j;
	if ( (*tab = (double **) calloc(l1+1, sizeof(double *))) != 0) {
        for (i=0;i<=l1;i++) {
            if ( (*(*tab+i)=(double *) calloc(c1+1, sizeof(double))) == 0 ) {
                return;
                for (j=0;j<i;j++) {
                    free(*(*tab+j));
                }
            }
        }
    }
    **(*tab) = l1;
    **(*tab+1) = c1;
}

/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau
* d'entiers (l1, c1)
--------------------------------------------------*/
void tabintalloc (int ***tab, int l1, int c1) {
    int     i, j;
    *tab = (int **) calloc(l1+1, sizeof(int *));
    if ( *tab != NULL) {
        for (i=0;i<=l1;i++) {
            *(*tab+i)=(int *) calloc(c1+1, sizeof(int));
            if ( *(*tab+i) == NULL ) {
                for (j=0;j<i;j++) {
                    free(*(*tab+j));
                }
                return;
            }
        }
    } else return;
    **(*tab) = l1;
    **(*tab+1) = c1;
    for (i=1;i<=l1;i++) {
        for (j=1;j<=c1;j++) {
            (*tab)[i][j] = 0;
        }
    }
}

/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau
--------------------------------------------------*/
void freeinttab (int **tab) {
    int     i, n;
    n = *(*(tab));
    for (i=0;i<=n;i++) {
            free((char *) *(tab+i) );
    }
    free((char *) tab);
}

/*--------------------------------------------------
* Allocation de memoire pour un vecteur de longueur n
--------------------------------------------------*/
void vecalloc (double **vec, int n) {
    if ( (*vec = (double *) calloc(n+1, sizeof(double))) != 0) {
        **vec = n;
        return;
    } else {
        return;
    }
}

/*--------------------------------------------------
* Allocation de memoire pour un vecteur d'entiers de longueur n
--------------------------------------------------*/
void vecintalloc (int **vec, int n) {
    if ( (*vec = (int *) calloc(n+1, sizeof(int))) != NULL) {
        **vec = n;
        return;
    } else {
        return;
    }
}

/*pour les triangles a exclure*/
double bacos(double a) {
	double b;
	if (a>=1)
		b=0;
	else if (a<=-1)
		b=Pi();
	else
		b=acos(a);
	return b;
}

/*Decale les valeurs de v de la valeur val*/
void decalVal(double *v, int n, double val) {
	int i;
	for(i=0;i<n;i++) {
		v[i]=v[i]+val;
	}
}

/*Decale les points et la fenetre rectangulaire*/
void decalRect(int point_nb,double *x, double *y,double *xmin, double *xmax, double *ymin, double *ymax) {
	if(*xmin<0) {
		decalVal(x,point_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y,point_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

/*Decale les points et la fenetre circulaire*/
void decalCirc(int point_nb,double *x, double *y,double *x0, double *y0, double r0) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;
	if(xmin<0) {
		decalVal(x,point_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y,point_nb,-ymin);
		*y0=*y0-ymin;
	}
}

/*Decale les points et la fenetre rectangulaire + triangles*/
void decalRectTri(int point_nb,double *x, double *y,double *xmin, double *xmax, double *ymin, double *ymax,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	if(*xmin<0) {
		decalVal(x,point_nb,-*xmin);
		decalVal(ax,tri_nb,-*xmin);
		decalVal(bx,tri_nb,-*xmin);
		decalVal(cx,tri_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y,point_nb,-*ymin);
		decalVal(ay,tri_nb,-*ymin);
		decalVal(by,tri_nb,-*ymin);
		decalVal(cy,tri_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

/*Decale les points et la fenetre circulaire + triangles*/
void decalCircTri(int point_nb,double *x, double *y,double *x0, double *y0, double r0,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;
	if(xmin<0) {
		decalVal(x,point_nb,-xmin);
		decalVal(ax,tri_nb,-xmin);
		decalVal(bx,tri_nb,-xmin);
		decalVal(cx,tri_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y,point_nb,-ymin);
		decalVal(ay,tri_nb,-ymin);
		decalVal(by,tri_nb,-ymin);
		decalVal(cy,tri_nb,-ymin);
		*y0=*y0-ymin;
	}
}

/*Decale les points et la fenetre rectangulaire (semis bivarie)*/
void decalRect2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *xmin, double *xmax, double *ymin, double *ymax) {
	if(*xmin<0) {
		decalVal(x1,point_nb1,-*xmin);
		decalVal(x2,point_nb2,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y1,point_nb1,-*ymin);
		decalVal(y2,point_nb2,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

/*Decale les points et la fenetre circulaire (semis bivarie)*/
void decalCirc2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *x0, double *y0, double r0) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;
	if(xmin<0) {
		decalVal(x1,point_nb1,-xmin);
		decalVal(x2,point_nb2,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y1,point_nb1,-ymin);
		decalVal(y2,point_nb2,-ymin);
		*y0=*y0-ymin;
	}
}

/*Decale les points et la fenetre rectangulaire + triangles (semis bivarie)*/
void decalRectTri2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *xmin, double *xmax, double *ymin, double *ymax,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	if(*xmin<0) {
		decalVal(x1,point_nb1,-*xmin);
		decalVal(x2,point_nb2,-*xmin);
		decalVal(ax,tri_nb,-*xmin);
		decalVal(bx,tri_nb,-*xmin);
		decalVal(cx,tri_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y1,point_nb1,-*ymin);
		decalVal(y2,point_nb2,-*ymin);
		decalVal(ay,tri_nb,-*ymin);
		decalVal(by,tri_nb,-*ymin);
		decalVal(cy,tri_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

/*Decale les points et la fenetre circulaire + triangles (semis bivarie)*/
void decalCircTri2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *x0, double *y0, double r0,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;
	if(xmin<0) {
		decalVal(x1,point_nb1,-xmin);
		decalVal(x2,point_nb2,-xmin);
		decalVal(ax,tri_nb,-xmin);
		decalVal(bx,tri_nb,-xmin);
		decalVal(cx,tri_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y1,point_nb1,-ymin);
		decalVal(y2,point_nb2,-ymin);
		decalVal(ay,tri_nb,-ymin);
		decalVal(by,tri_nb,-ymin);
		decalVal(cy,tri_nb,-ymin);
		*y0=*y0-ymin;
	}
}

/*Decale les points d'echantillonnages (pour density)*/
void decalSample(int sample_nb,double *x, double *y, double xmin, double ymin) {
	if(xmin<0) {
		decalVal(x,sample_nb,-xmin);
	}
	if(ymin<0) {
		decalVal(y,sample_nb,-ymin);
	}
}

/*memory allocation for a table with variable row length*/
double** taballoca(int a,int *b)
{
    double **t;
    int i;
    t = (double ** ) malloc (a * sizeof (double*));
    for (i=0;i<a;i++)
    {
		t[i]=(double *)malloc(b[i+1] * a * sizeof(double));
    }
    return t;
}

/*create a table with variable row length*/
void complete_tab(int point_nb,double **xx,double **yy,int *type,int *compt,int *l, double *x,double *y){
    int i;
    for(i=0;i<point_nb;i++)
   	{
   	    xx[type[i]-1][compt[type[i]]]=x[i];
   	    yy[type[i]-1][compt[type[i]]]=y[i];
   	    //Rprintf("%d,%d ::: %f, %f\n",type[i]-1,compt[type[i]],x[i],y[i]);
   	    compt[type[i]]++;
   	}
	return ;
}



/*test wether points are inside a polygon (points on boundaries are considered outside)*/
void pnpoly(double *testx, double *testy, double *vertx, double *verty, int *npts, int *nvert, double *xmi, double *ymi, double *pxr, double *pyr, double *score) {
  
	int i, j, k;
	int nedg=*nvert;

	/*so as to shift all coordinates to positive values only*/
/*	if(*xmi<0) {
		for(i=0;i<*npts;i++) {
			testx[i]=testx[i]-*xmi;
		}
		for(i=0;i<*nvert;i++) {
			vertx[i]=vertx[i]-*xmi;
		}
		for(i=0;i<2;i++) {
			pxr[i]=pxr[i]-*xmi;
		}
		//decalVal(testx,*npts,-*xmi);
		//decalVal(vertx,*nvert,-*xmi);
		//decalVal(pxr,2,-*xmi);
	}
	if(*ymi<0) {
		for(i=0;i<*npts;i++) {
			testy[i]=testy[i]-*ymi;
		}
		for(i=0;i<*nvert;i++) {
			verty[i]=verty[i]-*ymi;
		}
		for(i=0;i<2;i++) {
			pyr[i]=pyr[i]-*ymi;
		}
		//decalVal(testy,*npts,-*ymi);
		//decalVal(verty,*nvert,-*ymi);
		//decalVal(pyr,2,-*ymi);
	}*/

  	for(k=0;k<*npts;k++) {
		score[k]=0;
		//for (i=0, j=(nedg-1); i <nedg; j = i++) {
		j=(nedg-1);
		for (i=0; i <nedg; i++) {
				
			if ((((verty[i] <= testy[k]) && (testy[k] < verty[j])) || ((verty[j] <= testy[k]) && (testy[k] < verty[i]))) && (testx[k] < (vertx[j] - vertx[i]) * (testy[k] - verty[i]) / (verty[j] - verty[i]) + vertx[i]))
				{score[k] = 1;}	
			j=i;
			
  		}
		// We're outside the polygon!
		/*if (testx[k] <= pxr[1] || testx[k] >= pxr[2] || testy[k] <= pyr[1] || testy[k] >= pyr[2]) 
		{score[k]=0;}*/
	}	
}
