#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

// m is (n x 3)-matrix. The first col includes x-coordinates, the second y-coordinates
// and the third one includes place -holder for the cluster-id
// n is the length of the list (the number of rows of the matrix, to be precise)
// l ist the cluster distance

void ccaRev(double *m, int *n, double *l, int *w)
{	
	w[0] = 0;
	int zeros, i, k, j, mm, cc;
	zeros = 0;
	i=0;
	mm=1;
	cc=1;
	double dist;
	
	while(zeros<*n){
		k = w[i];
		if(m[2* *n+ k]==0){
			m[2* *n + k] = cc;
			zeros++;
		}
		if(k>0)
			for(j=k-1; j>=0 && (m[k]-m[j])<=*l; j--){
				if(m[2* *n + j]==0 && fabs(m[*n+k]-m[*n+j])<=*l){
					dist = sqrt(((m[k]-m[j])*(m[k]-m[j]))+((m[*n+k]-m[*n+j])*(m[*n+k]-m[*n+j])));
					if(dist<=(double)*l){
						w[mm] = j;
						m[2* *n+j] = cc;
						mm++;
						zeros++;
					}
						
				}	
			}
		if(k<*n-1)
			for(j=k+1; j<*n && (m[j]-m[k])<=*l; j++){
				if(m[2* *n + j]==0 && fabs(m[*n+k]-m[*n+j])<=*l){
					dist = sqrt(((m[k]-m[j])*(m[k]-m[j]))+((m[*n+k]-m[*n+j])*(m[*n+k]-m[*n+j])));
					if(dist<=(double)*l){
						w[mm] = j;
						m[2* *n+j] = cc;
						mm++;
						zeros++;
					}	
				}	
			}
		i++;
		if(zeros==*n){ break;}
		if(w[i]==0){
			cc++;
			k = 0;
			while(m[2* *n+ k]!=0) k++;
			w[i] = k;
			mm = i+1;
		}
	}
}



void ccaSum(double *m, int *m3, double *mm, int *n){
	int i;
	for(i=0; i<*n; i++)
		mm[m3[i]-1] += 1;
}

void ccaSumT(double *m, int *m3, double *mm, int *n){
	int i;
	for(i=0; i<*n; i++)
		mm[m3[i]-1] += cos(m[*n+i]);
}

void ccaRevT(double *m, int *n, double *l, int *step_w, int *step_h, double *res_x, double *res_y, int *w)
{	
	w[0] = 0;
	int zeros, i, k, j, mm, cc;
	zeros = 0;
	i=0;
	mm=1;
	cc=1;
	double dist;
	
	while(zeros<*n){
		k = w[i];
		if(m[2* *n+ k]==0){
			m[2* *n + k] = cc;
			zeros++;
		}
		if(k>0)
			for(j=k-1; j>=0 && fabs((m[*n+k]-m[*n+j])/ *res_y)<=*step_h; j--){
				if(m[2* *n + j]==0 && fabs((m[k]-m[j])/ *res_x)<=*step_w){
					dist = acos(sin(m[*n+k])*sin(m[*n+j])+cos(m[*n+k])*cos(m[*n+j])*cos(m[k]-m[j]))*6371000;
					if(dist<=(double)*l){
						w[mm] = j;
						m[2* *n+j] = cc;
						mm++;
						zeros++;
					}
						
				}	
			}
		if(k<*n-1)
			for(j=k+1; j<*n && fabs((m[*n+j]-m[*n+k])/ *res_y)<=*step_h; j++){
				if(m[2* *n + j]==0 && fabs((m[k]-m[j])/ *res_x)<=*step_w){ 
					dist = acos(sin(m[*n+k])*sin(m[*n+j])+cos(m[*n+k])*cos(m[*n+j])*cos(m[k]-m[j]))*6371000;
					if(dist<=(double)*l){
						w[mm] = j;
						m[2* *n+j] = cc;
						mm++;
						zeros++;
					}	
				}	
			}

		i++;
		if(zeros==*n){ break;}
		if(w[i]==0){
			cc++;
			k = 0;
			while(m[2* *n+ k]!=0) k++;
			w[i] = k;
			mm = i+1;
		}
	}
}

double SCMakse(int *m, int *n, double *theta_0, double *xq, double *l)
{
	int k, j, delt=0;
	double dist, zsum=0;
	
	for(k=0; k<*n; k++){
	
		if(k>0)
			for(j=k-1; j>=0 && (m[k]-m[j])<=*l; j--){
				if(m[2* *n + j]==0 && abs(m[*n+k]-m[*n+j])<=*l){
					dist = sqrt(((m[k]-m[j])*(m[k]-m[j]))+((m[*n+k]-m[*n+j])*(m[*n+k]-m[*n+j])));
					if(dist==(double)*l){
						delt++;
						zsum += (m[3* *n+j]-*xq)*(m[3* *n+k]-*xq);
						
					}
						
				}	
			}
		if(k<*n-1)
			for(j=k+1; j<*n && (m[j]-m[k])<=*l; j++){
				if(m[2* *n + j]==0 && abs(m[*n+k]-m[*n+j])<=*l){
					dist = sqrt(((m[k]-m[j])*(m[k]-m[j]))+((m[*n+k]-m[*n+j])*(m[*n+k]-m[*n+j])));
					if(dist==(double)*l){
						delt++;
						zsum += (m[3* *n+j]-*xq)*(m[3* *n+k]-*xq);
					}	
				}	
			}
	}
	return (zsum/(delt* *theta_0));
}



int min(int a, int b)
{
	if(a<=b) return(a);
	else return(b);
}

int max(int a, int b)
{
	if(a>b) return(a);
	else return(b);
}

void ccaBuffED(int *m, int *nr, int *nc, int *sz)
{
	int i,j,k,l,s=*sz;
		for(i=0; i<*nc; i++){
			for(j=0; j<*nr; j++){
					if(m[i**nr+j]==1)
						for(k=max(0,i-s); k<=min(*nc-1,i+s); k++)
							for(l=max(0,j-s); l<=min(*nr-1,j+s); l++)
								if(sqrt(((k-i)*(k-i))+((l-j)*(l-j)))<=s && m[k**nr+l]==0)
									m[k**nr+l] = -1;
			}
		}
}

// Replaced by ccaBuffEDszN and ccaBuffEDszNN
void ccaBuffEDsz(int *m, int *nr, int *nc, int *sz, int *nz)
{
  int i,j,k,l,s,c;
  c=0;
  for(s=1; s<*sz; s++){
    Rprintf("width: %i\n",s);
    Rprintf("zeros: %i\n",*nz-c);
    if( c >= *nz) {
      Rprintf("terminate");
      /* terminate the loop if all zeros are replaced */
      break;
      }
    for(i=0; i<*nc; i++){
      for(j=0; j<*nr; j++){
        if(m[i**nr+j]==1)
          for(k=max(0,i-s); k<=min(*nc-1,i+s); k++)
            for(l=max(0,j-s); l<=min(*nr-1,j+s); l++)
              if(sqrt(((k-i)*(k-i))+((l-j)*(l-j)))<=s && m[k**nr+l]==0){
                m[k**nr+l] = -s;
                c = c+1;
              }
      }
    }
  }
}

// Queens neighbourhood fo of ccaBuffEDsz (euclidean) - not in use
void ccaBuffEDszS(int *m, int *nr, int *nc, int *sz, int *nz)
{
  int i,j,k,l,s,c;
  c=0;
  for(s=1; s<*sz; s++){
    Rprintf("width: %i\n",s);
    Rprintf("zeros: %i\n",*nz-c);
    if( c >= *nz) {
      Rprintf("terminate");
      /* terminate the loop if all zeros are replaced */
      break;
    }
     for(i=0; i<*nc; i++){
      for(j=0; j<*nr; j++){
        if( (s==1 && m[i**nr+j]==1) || ( m[i**nr+j]==-s+1 && s>1)){
        //printf("do\n");
        //printf("s: %i \n", s);
        //printf("m: %i \n", m[i**nr+j]);
          for(k=max(0,i-1); k<=min(*nc-1,i+1); k++)
            for(l=max(0,j-1); l<=min(*nr-1,j+1); l++)
              if(m[k**nr+l]==0){
                 m[k**nr+l] = -s;
                  c = c+1;
              }
            }
          }
      }

  }
}

void ccaBuffEDszN(int *m, int *nr, int *nc, int *sz)
{
  int i,j,k,l,s;
  //Rprintf("Test");
    for(i=0; i<*nc; i++){
      for(j=0; j<*nr; j++){
       if(m[i**nr+j]==1){
         //Rprintf("i=%i j=%i m=%i", i,j,m[i**nr+j]);
         for(k=max(0,i-*sz); k<=min(*nc-1,i+*sz); k++){
           for(l=max(0,j-*sz); l<=min(*nr-1,j+*sz); l++){
              s = ceil(sqrt(((k-i)*(k-i))+((l-j)*(l-j))));
              //Rprintf("k=%i l=%i s=%i", k,l,s);
              if( ( s < abs(m[k**nr+l])  || m[k**nr+l]==0)  && m[k**nr+l]!=1 && s <= *sz){
                //Rprintf("Test in loop");
                m[k**nr+l] = -s;
              }
            }
          }
        }
      }
    }
}


void ccaBuffEDszNN(int *m, int *nr, int *nc, int *sz)
{
  int i,j,s,k,l,d,dt,f,smx,smn,smn1;
  smn1 = 1;
  smx = ceil(sqrt((*nr**nr)+(*nc**nc)));
  //Rprintf("Test");
  for(i=0; i<*nc; i++){
    smn = smn1;
    //smn = 1;
    for(j=0; j<*nr; j++){
      if(m[i**nr+j]==0){
        //if(i==0 && j==19)Rprintf("i=%i j=%i m=%i smn=%i; smx=%i\n", i,j,m[i**nr+j],smn,smx);
        int found[4];
        found[0]=*nr**nc;found[1]=*nr**nc;found[2]=*nr**nc;found[3]=*nr**nc;
        f=0;
        dt=*nr**nc;
        for(s=smn; s<smx; s++){
          // upper row
          if(j-s>=0){
            for(k=max(0, i-s); k <= min(*nc-1, i+s); k++){
              if(m[k**nr+(j-s)]==1){
                d = ceil(sqrt(((k-i)*(k-i))+(s*s)));
                found[0] = min(found[0], d);
                f = f+1;
                }
              }
            }
          // lower row
          if(j+s < *nr){
            for(k=max(0, i-s); k <= min(*nc-1, i+s); k++){
              if(m[k**nr+(j+s)]==1){
                d = ceil(sqrt(((k-i)*(k-i))+(s*s)));
                found[1] = min(found[1], d);
                f = f+1;
              }
            }
          }
          // left col
          if(i-s>=0){
            for(l=max(0, j-s); l <= min(*nr-1, j+s); l++){
               if(m[(i-s)**nr+l]==1){
                d = ceil(sqrt(((s)*(s))+((l-j)*(l-j))));
                found[2] = min(found[2], d);
                f = f+1;
              }
            }
          }
          // right col
          if(i+s < *nc){
            for(l=max(0, j-s); l <= min(*nr-1, j+s); l++){
              if(m[(i+s)**nr+l]==1){
                d = ceil(sqrt(((s)*(s))+((l-j)*(l-j))));
                found[3] = min(found[3], d);
                f = f+1;
              }
            }
          }
//          Rprintf("f=%i\n",f);
          if(f>0){
//            if(i==0 && j==19){
//              Rprintf("\nFound >1 in s=%i\n",s);
//              Rprintf("smn=%i; smx=%i\n", smn,smx); 
//              Rprintf("f0=%i, f0=%i, f0=%i, f0=%i\n",found[0],found[1],found[2],found[3]);
//            }
            if(dt==*nr**nc){
              smn = max(1,s-1);
              if(j==0){smn1=max(1,s-1);} 
            }
            if(dt > min(min(found[0],found[1]),min(found[2],found[3]))){
  //            if(i==0 && j==19)Rprintf("New is smaller: old=%i new=%i s=%i, smn=%i\n",dt, min(min(found[0],found[1]),min(found[2],found[3])),s,smn);
              dt = min(min(found[0],found[1]),min(found[2],found[3]));
    //          if(i==0 && j==19)Rprintf("i=%i j=%i dt=%i smn=%i; smx=%i\n", i,j,dt,smn,smx);
            }
          }
          if(dt<=s){
            m[i**nr+j] = -dt;
            break;
          }
        }
      }
    }
  }
}
