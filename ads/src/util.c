#include "adssub.h"
#include "triangulate.h"

int triangulate(int *npoly, int *tabpt, int *nptTot,double *vertX, double *vertY, int *ntri,
double *X1, double *Y1,double *X2, double *Y2,double *X3, double *Y3) {
	int i,j,k,l;
	int **triangles;
	double **vertices;
	double *x,*y;
	tabintalloc(&triangles,*ntri,3);
	taballoc(&vertices,*nptTot+1,2);
	l=0;
	for(i=0;i<*npoly;i++) {
		int npt=tabpt[i];
		vecalloc(&x,npt+1);
		vecalloc(&y,npt+1);
		for(j=1;j<=npt;j++) {
			k=j+l-1;
			x[j]=vertX[k];
			y[j]=vertY[k];
		}
		if(i==0) {
			if(testclock(x,y,npt)) { /*clockwise order*/
				k=npt;
				for(j=1;j<=npt;j++) {
					vertices[j+l][0]=x[k];
					vertices[j+l][1]=y[k];
					k--;
				}
			}
			else { /*anti-clockwise order*/
				for(j=1;j<=npt;j++) {
					vertices[j+l][0]=x[j];
					vertices[j+l][1]=y[j];
				}
			}
		}
		else {
			if(!testclock(x,y,npt)) { /*anti-clockwise order*/
				k=npt;
				for(j=1;j<=npt;j++) {
					vertices[j+l][0]=x[k];
					vertices[j+l][1]=y[k];
					k--;
				}
			}
			else { /*clockwise order*/
				for(j=1;j<=npt;j++) {
					vertices[j+l][0]=x[j];
					vertices[j+l][1]=y[j];
				}
			}
		}
		l+=npt;
		freevec(x);
		freevec(y);
	}

	/*Test de l'unicite des points*/
	for(i=2;i<=*nptTot;i++) {
		for(j=1;j<i;j++) {
			if((vertices[i][0]==vertices[j][0])&&(vertices[i][1]==vertices[j][1])) {
				Rprintf("Error : Duplicate input vertices\n");
				return -1;
			}
		}
	}
	triangulate_polygon(*npoly,tabpt,vertices,triangles);
	for(i=0;i<*ntri;i++) {
		X1[i]=vertices[triangles[i][2]][0];
		Y1[i]=vertices[triangles[i][2]][1];
		X2[i]=vertices[triangles[i][1]][0];
		Y2[i]=vertices[triangles[i][1]][1];
		X3[i]=vertices[triangles[i][0]][0];
		Y3[i]=vertices[triangles[i][0]][1];
	}
	freeinttab(triangles);
	freetab(vertices);
	return 0;
}
