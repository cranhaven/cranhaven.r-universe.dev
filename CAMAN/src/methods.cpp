//#include <windows.h>
#include "methods.h" 

extern "C" {
const double PII=3.14159265358979323846264338327950288419716939937510;


void caman_boot(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, int * NUMBOOT, int * KBOOT, double * LL_k1, int * IS_META){
double *para;
//double *LLstore1, *LLstore2;
int i,nBoot, position, maxn;
nBoot = *NUMBOOT;
para = (double *) R_alloc(2, sizeof(double));
Rprintf("%s \n", "starting the bootstrap:");
maxn=0;
for (i=0;i<nBoot; i++) {
  if (NROWx[i] >= maxn) maxn = NROWx[i];  //find max value for n to correctly allocate memory
}

position = 1;

MixMod *mix=(MixMod *)R_alloc(1, sizeof(MixMod));
 *mix=MixMod(STARTK, DENS, NUMSTEP, &maxn);
mix->ismeta = *(IS_META) > 0;
MixMod *mix0=(MixMod *)R_alloc(1, sizeof(MixMod));
 *mix0=MixMod(&position, DENS, NUMSTEP, &maxn);
mix0->ismeta = *(IS_META) > 0;

mix->limit=*LIMIT;   // Max. limit to combine subpopulations
mix->acc= *ACC;      // Max. level of accuracy
mix->maxstep = *NUMSTEP;   // Max. number of iterations


//char * buffer; // removed by PD because sprintf caused an error on Solaris (pointed out by Brian Ripley)
/*LLstore1 = (double *) R_alloc(nBoot, sizeof(double));  //max no. of bootstraps is 5.000
LLstore2 = (double *) R_alloc(nBoot, sizeof(double));*/
position = 0;

//buffer = (char *) R_alloc(255, sizeof(char)); // removed by PD because sprintf caused an error on Solaris (pointed out by Brian Ripley)


for(i=0; i< nBoot; i++){
//MessageBox (0, "BERECHNUNG STARTET", "START", MB_ICONINFORMATION);
  //if (i % int(nBoot /10) == 9) Rprintf ("|");
  mix->Init(&DATa[position], &DATb[position], &DATc[position], &DATd[position], &NROWx[i]); // initialisation of data
  mix0->Init(&DATa[position], &DATb[position],&DATc[position], &DATd[position], &NROWx[i]); // initialisation of data
  position += NROWx[i];
  if (*STARTK < NROWx[i]) mix->k = *STARTK;  //TODO ??
  else mix->k =  NROWx[i];
  //MessageBox (0, "Compute startet", "START", MB_ICONINFORMATION);  

  mix->Compute(NUMK, LL, P, T, COMP_VAR);

  LL[i] = mix->likelihood ();
  KBOOT[i] = NUMK[0];  //in Compute, the number of Components was saved in NUMK[0].

  
  //Compute LL of NullModel (onecomponent, p=1, t=0
  para[0]=1.; //p
  para[1]=0.; //t
  
  
  mix0->p[0] = para[0];
  mix0->t[0] = para[1];            
  mix0->k = 1;
  mix0->EM(mix->numstep); 
  LL_k1[i] = mix0->likelihood ();
  //if (i % int(nBoot /10) == 9) Rprintf ("|");
//  snprintf (buffer, (size_t)255, "%d", *(NROWx + i));// removed by PD because sprintf caused an error on Solaris (pointed out by Brian Ripley)
  //MessageBox (0, buffer, "START", MB_ICONINFORMATION);  
}


//delete mix;
}


void caman_C(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, double * VEM_DETAILS, double * EM_DETAILS, int * IS_META){
int i;
//MessageBox (0, "BERECHNUNG STARTET", "START", MB_ICONINFORMATION);
MixMod *mix=(MixMod *)R_alloc(1, sizeof(MixMod));
 *mix=MixMod(STARTK, DENS, NUMSTEP, NROWx);
mix->limit=*LIMIT;   // Max. limit to combine subpopulations
mix->acc= *ACC;      // Max. level of accuracy
mix->maxstep = *NUMSTEP;   // Max. number of iterations
mix->ismeta = *(IS_META) > 0;

mix->Init(DATa, DATb, DATc, DATd, NROWx); // initialisation of data
//MessageBox (0, "COMPUTE STARTET", "START", MB_ICONINFORMATION);
mix->Compute(NUMK, LL, P, T, COMP_VAR); // compute mixture model
//MessageBox (0, "BERECHNUNG WURDE BERECHNET!!", "BEENDET", MB_ICONINFORMATION);
for (i=0; i< (2*int(mix->vem_details[0]) +2); i++) VEM_DETAILS[i] = mix->vem_details[i];
NUMSTEP[0] = mix->VEMStepsDone; //VEM
EM_DETAILS[0] = mix->em_details[0];  //EM acc
EM_DETAILS[1] = mix->em_details[1]; //numiter
//delete mix;
}

void mixalg_sub(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, int * ADD_INFO, int * IS_META, double * GRADIENT){
    int i;
    MixMod *mix=(MixMod *)R_alloc(1, sizeof(MixMod));
     *mix=MixMod(STARTK, DENS, NUMSTEP, NROWx);
    mix->limit=*LIMIT;   // Max. limit to combine subpopulations
    mix->acc= *ACC;      // Max. level of accuracy
    mix->maxstep = *NUMSTEP;   // Max. number of iterations
    mix->ismeta = *(IS_META) > 0;
            mix->Init(DATa, DATb, DATc, DATd, NROWx);

     if (ADD_INFO[0] == 0){ //perform only VEM and return t & p                              
            mix->Grid();   // construct grid of potential subpopulation means 
            mix->CalcMat(); // calculation of mixing kernel density
            mix->vem();   // VEM algorithm
            ACC[0] = mix->vem_details[1];
            NUMSTEP[0] = mix->VEMStepsDone;
            for (i=0;i< NUMK[0];i++){
              P[i] = mix->p[i];
              T[i] = mix->t[i];
              GRADIENT[i] = mix->vem_details[i+2+mix->k+mix->k];
            }            
     }
     else if (ADD_INFO[0] == 1){ //perform EM and use manual defined p, t, k
             mix->p = P;
             mix->t = T;            
             mix->EM(mix->numstep); // refiend soultion with EM algorithm
           NUMSTEP[0] = mix->em_details[1];
             ACC[0] = mix->em_details[0];
             if (mix->dens==0) COMP_VAR[0] = mix->compvar;
             for (i=0;i< NUMK[0];i++){
                P[i] = mix->p[i];
                T[i] = mix->t[i];
            }                                             
     }
	LL[0] = mix->likelihood ();
	//delete mix;
     }

MixMod::MixMod(int * STARTK, int * DENS, int * NUMSTEP, int * NROWx)
{
int i;

 n = *NROWx;
 startk = *STARTK;
 k=startk;  // set the number of compononts k to start k
 dens=*DENS; // mixing kernel 
 numstep = *NUMSTEP;
 VEMStepsDone = 0;
 // now, array have length k and not kk
p=(double *) R_alloc(startk, sizeof(double)); // mixing weights
t=(double *) R_alloc(startk, sizeof(double));     // subpopulation means
y_grid = (double *) R_alloc(n, sizeof(double));
grad=(double *) R_alloc(startk, sizeof(double)); 

w=(double *) R_alloc(n, sizeof(double));
s1=(double *) R_alloc(n, sizeof(double));
ht=(double *) R_alloc(n, sizeof(double));
xf=(double **) R_alloc(n,sizeof(double *));
for (i=0;i<n;i++) xf[i]=(double *) R_alloc(k, sizeof(double));
x=(double **) R_alloc(n,sizeof(double *));
for (i=0;i<n;i++) x[i]=(double *) R_alloc(4, sizeof(double));            

vem_details = (double *) R_alloc(((3*k)+2), sizeof(double));
em_details = (double *) R_alloc(2, sizeof(double));
em_details[1] = 0.;
}

void MixMod::Init(double * data, double * datb, double * datc, double * datd, int * nrowx)
{
double s=0.;
int i;
n = *nrowx;
for(i=0; i< n; i++)
  {
    x[i][0] = *(data+i); // counts        / ln(or)
    x[i][2] = *(datc+i); //  pop. at risk 
    x[i][1] = *(datb + i);
    s += x[i][1];
    x[i][3]= *(datd + i); // var ln(or)
    
  }
 for(i=0;i<n;i++)
 {
   w[i]=x[i][1]/s;     // set appropirate weights
 }
 

}



void MixMod::Compute(int * NUMK, double * LL, double * P, double * T, double * COMP_VAR)
{ 
 int numk,i;
// MessageBox (0, "0", "START", MB_ICONINFORMATION);
 Grid();   // construct grid of potential subpopulation means 
 CalcMat(); // calculation of mxing kernel density
 vem();   // VEM algorithm
 Update(); // Find subpoplations with positve weight, i.e. p > 0.01
 EM(numstep); // refiend soultion with EM algorithm

 numk=Combine(); // final solution: combine close components
 if (dens==0) COMP_VAR[0] = compvar;
 NUMK[0] = numk;
 LL[0] = ll;
 for(i=0;i<numk;i++){
 P[i] = p[i];
 T[i] = t[i];      
 }
 
}


void MixMod::Grid()
{
 double min,max,diff,a,b=1.0;
 int i;

 for(i=0;i<n;i++)y_grid[i]=x[i][0]/x[i][2];
 qsort( (void *) y_grid,(size_t) n, sizeof( double),(fcmp) cmpgle );
min=y_grid[0];
max=y_grid[n-1];

	diff=max-min;
	for(i=0;i <k;i++)
	{
	a=i*diff;
	if (k>1) a=a/(k-1);
	t[i]=min+a;
	p[i]=b/k;
 	}
}

void MixMod::CalcMat()
{
int i,j;


 for(j=0;j<k;j++)
  {
   for(i=0;i<n;i++)
   {
     switch (dens)
     {
     case 0: // normal density
      {
     xf[i][j]=normal (x[i][0],t[j],x[i][3]);
      break;
      }
     case 1:  // poisson density
      {
      xf[i][j]=poisson(x[i][0],t[j]*x[i][2]);
      break;
      }
     case 2: // binomial density
      {
      xf[i][j]=binomial (x[i][0],x[i][2],t[j]);
      break;
      }
    }  // end of switch
   }  // end of i-loop
  } // end if j-loop
             //
 }

 double  normal (double x,double t,double var)
{
 double std,z,y,value;
 std=sqrt(var);
 z=(x - t)/std;
 y=-.5*z*z;
 value=exp(y)/(std*sqrt(2.*PII));
 return value;
}



void MixMod::Gradient (void)
{
int i,j;
double s;

  for (i=0;i<n;i++)
  {
   s1[i]=0.;
   s=0.;
    for (j=0;j<k;j++)
    {
    s=s+p[j]*xf[i][j];
    }
    s1[i]=s;
  }


  for(j=0;j<k;j++)
  {
   s=0;
    for(i=0;i<n;i++)
    {
     if(s1[i] >1.E-13)s=s+w[i]*xf[i][j]/s1[i];
    }
   grad[j]=s;
 }
}

int MixMod::maxderiv(double & ymax)
{
int i;
int imax=1;
double max=0.;

for(i=0;i<k;i++)
 {
   if(max < grad[i])
  {
   imax=i;
    max=grad[i];
  }
}
ymax=max;
return imax;
}


void MixMod::vem (void)
{
int i, mycounter;
double step,xs,ymax;
int imin,imax,icount;
   for(icount=0;icount<maxstep;icount++)
   {
    Gradient ( );
    imax=maxderiv(ymax);
    imin=minderiv();
    for(i=0;i<n;i++)
    {
	    ht[i]=(xf[i][imax]-xf[i][imin]);
	    ht[i]=ht[i]*p[imin];
    }
    step=stepsize ();
    xs=step*p[imin];
    p[imin]=p[imin]-xs;
    p[imax]=p[imax]+xs;
    VEMStepsDone = VEMStepsDone + 1;
    if (((ymax-1.0 )< acc) || (icount == (maxstep-1))) {	//stopping criterion is fulfilled!
		vem_details[0] = k+0.0; //save results of VEM-algorithm in vem_details
		vem_details[1] = ymax-1.0; //final accurancy of vem...
		mycounter=0;	
		for (i=2; i<(k+2); i++){
			vem_details[i] = p[mycounter];
			mycounter++;
		}
		mycounter=0; 
		for (i=(2+k); i<((2*k)+2); i++){
			vem_details[i] = t[mycounter];
			mycounter++;
		}
		mycounter=0;
		for (i=(2+2*k); i<((3*k)+2); i++){
			vem_details[i] = grad[mycounter];
			mycounter++;
		}
		// grad
	break;
	}
  }

}


double MixMod::likelihood ()
{
int i;
double value=0.;
 for(i=0;i<n;i++)
 {
   value=value+x[i][1]*g(s1[i]);
 }
 return value;
}

double MixMod::g(double s11)
{
//double value=0.;
//if ( s11 < 1.E-13) return value;
//else
//value=log(s11);
//return value;
return(log(s11));   
}

int MixMod::Update()
{
 int i,j;
 std::vector<double> tempt(k);
 std::vector<double> tempp(k);


 for (i=0;i<k;i++)
 {
  tempt[i]=0.;
  tempp[i]=0.;
  }
   j=0;
	for(i=0;i<k;i++)
	{
	 if (p[i] > 1.E-3)
	 {
	 tempp[j]=p[i];
	 tempt[j]=t[i];
         j++;
	 }
        }
	
  //reset weights and parameters
 	for (i=0;i<k;i++)
       {
  	 p[i]=0.;
         t[i]=0.;
       }
	for (i=0;i<j;i++)
        {
	 p[i]=tempp[i];
	 t[i]=tempt[i];
  	}
        k=j;
  	return j;
}




double MixMod::stepsize()
 {
int i,j;
double s0,s11,sl,a,step,oldstep,grad1s,grad2s,b;
s0=0.;
s11=0.;

sl=0.;

   for(i=0;i<n;i++)
   {
   a=0;
   if(fabs(s1[i]) >1.E-7)a=ht[i]/(s1[i]);
   b=1.+a;
   if( fabs(b) >1.E-7)  sl=ht[i]/b;
   s11=s11+sl* w[i];
   s0=s0+w[i]*ht[i];
 }



 ////   Optimal step-length procedure

	step=0.;
        oldstep=0.;
//cccc    begin of iteration
        	for(j=0;j<50;j++)
         {
          grad1s=0.;
          grad2s=0.;
//ccc     computation of derivatives
	for (i=0;i<n;i++)
        {
	a=s1[i]+step*ht[i];
	 if (fabs(a)>1.E-7)
	 {
           b=ht[i]/a;
	   grad1s=grad1s+w[i]*b;
	   grad2s=grad2s-(w[i]*b*b);
         }
        }

	if( fabs(grad2s) > 1.E-10) step=step-grad1s/grad2s;
         if ( (oldstep>1.00) && (step>oldstep) 	)
	{

        step=1.;
        break;
        }
	if(grad1s <1.E-7 )  break;


	oldstep=step;
        }

	if (step >1.0)
	 step=1.;
	 
        return step;

 }


int  MixMod::minderiv()
{
int i;
double ymin=1.E+7;
int imin=1;
 for(i=0;i<k;i++)
 {
    if(p[i] > 1.E-8)
    {
    if(ymin > grad[i]){
      imin=i;
      ymin=grad[i];
    }
    }
 }
 return imin;
}

void MixMod::EM(int nstep)
{
int i,j,icount;
double s11,s12,s13=0.,ymax;
int l;

   l=k-1;

    if(k==1)
    {
     s11=0.;
     s12=0.;
     for (i=0;i<n;i++)
     {
      s11=s11+w[i]*x[i][0]/x[i][3];
      s12=s12+w[i]*x[i][2]/x[i][3];
     }  // end for
    t[l]=s11/s12;
    p[l]=1.;
    CalcMat ();
    Gradient();
    icount=1;
    maxderiv(ymax);
     return;
    }    // end if
   for(icount=0;icount<nstep;icount++)
   {
    CalcMat();
    Gradient();
        
    double su=0;
    for(i=0;i<l;i++)
    {
    p[i]=p[i]*grad[i];
    su=su+p[i];
    }
    p[l]=1.0-su;
       s13=0.;
	for (j=0;j<k;j++)
	{
	 s11=0.;
	 s12=0.;

	  for (i=0;i<n;i++)
	  {

	   if(s1[i] > 1.E-10)
	   {
        s11=s11+w[i]*x[i][0]/x[i][3]*xf[i][j]/s1[i];
	    s12=s12+w[i]*x[i][2]*(xf[i][j]/x[i][3])/s1[i];
	    if (dens==0) s13=s13+w[i]*pow( (x[i][0]-t[j]),2)*p[j]*xf[i][j]/s1[i];
	   }

	  }  // end i

	  if(s12 > 1.E-12) t[j]=s11/s12;
	}
       maxderiv (ymax);
       em_details[0] = fabs(ymax-1.0);
       em_details[1] = (em_details[1] + 1.) ;
	  if ((dens==0) && (!ismeta)){
		  for (i=0;i<n;i++)
		  {
		  	x[i][3] = s13;	
		  }
	  }
      if ( (em_details[0] < acc) && (icount > 10 )) break;
  }
     compvar = s13;
 } 

 double MixMod::LRS()
{
double lrs,llnull;
int tmpnumstep=1;
k=1;
EM(tmpnumstep);
llnull=likelihood();
lrs=-2.0*(llnull-ll);
if(lrs < 0.) lrs=0.;
return lrs;
}



 int MixMod::Combine()
{
 int i,j,jj,tempk,tmpnumstep=1;
 std::vector<int>count(k);
 std::vector<double>tempp(k);
 std::vector<double>tempt(k);
 double diff;

	for (i=0;i<k;i++)
	{
	tempt[i] =1.E+8;
	tempp[i] =0.;
        count[i]=-1;
        }


	jj=-1;

//  find identical parameters and store their index
	for (i=0;i<k-1;i++)
        {
	diff=t[i+1]-t[i];
 	 if(fabs(diff)<limit)
	 {
	  jj++;
	  count[jj]=i+1;
	 }
	   }
           jj=jj+1;
         tempk=k-jj;
	j=0;
	jj=-1;
/*cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      Find remaining different parameters  and update mixing weights
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc*/


	for (i=0;i<k;i++)
	{

	 if ( i==count[j] )
	 {
	  j++;
	  tempp[jj]=tempp[jj]+p[i];
          } // end if
	  else
	  {
	  jj++;
	  tempt[jj]=t[i];
	  tempp[jj]=p[i];
          }         // end else

        } // end for loop

/*cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    done!
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc*/
	k=tempk;
        
	for (i=0;i<k;i++)
        {
	p[i]=tempp[i];
	t[i]=tempt[i];
          
	}
    CalcMat();
    Gradient();
    EM(tmpnumstep);
    ll=likelihood();
    return tempk;

}



MixMod::~MixMod()
{
//delete [] p;
//delete [] t;
/*
delete [] grad;
delete [] ht;
delete [] s1;

if(longmode){
    for (int i = 0; i < nnlong;  i++) {
    delete[] xf[i];          // STEP 1: DELETE THE COLUMNS
    delete[] x[i];          // STEP 1: DELETE THE COLUMNS
    }
} 
for (int i = 0; i < n;  i++) {
    delete[] xf[i];          // STEP 1: DELETE THE COLUMNS
    delete[] x[i];          // STEP 1: DELETE THE COLUMNS
    }
delete[] xf;         // STEP 2: DELETE THE ROWS
delete[] x;         // STEP 2: DELETE THE ROWS

delete [] w;
//delete [] vem_details;
//delete [] em_details;
*/

}


    double binomial (double  k,double  n, double p)
	{
	double ak,an,alfact,albinc,binom;

	 if ( (p<1.E-8)&& (k<1.E-8) ) binom=one;
	 else if ( ( fabs(k-n) < 1.e-8 ) && ( (one-p)< 1.E-8)  ) binom=one;
	 else if ( (p < 1.E-8) ||( (one-p)< 1.E-8) ) binom=0.;

	 else
	 {

	  ak=k;
	  an=n;
	  alfact=0.;
	  albinc=glngam(an+one)-glngam(ak+one)-glngam(an-ak+one);
          alfact=log(p)*ak+log(one-p)*(an-ak);
          binom=exp(alfact+albinc);
         }
          return binom;
        }

	double  glngam (double x)
   {
     double c[6],xx,xh,xgh,s,anum,g,value;
     int reflec;
	c[0]=76.18009173;
	c[1]= -86.50532033;
	c[2]= 24.01409822;
	c[3]=-1.231739516;
	c[4]= 0.12085003;
	c[5]=-0.536382;

	if ( x >= one)
	{
	 reflec=0;
	 xx=x-one;
        }
	else
        {
	 reflec=1;
	 xx=one-x;
        }

        xh=xx+half;
	xgh=xx+double(5.5);
	s=one;
	anum=xx;
	for (int i=0;i<6;i++)
        {
	 anum=anum+one;
	 s=s+c[i]/anum;
        }

	s=s*rtwopi;
	g=xh*log(xgh)+log(s)-xgh;
	if (reflec==1 )
        value=log(PII*xx)-g-log( sin(PII*xx));
	else
	value=g;


	return value;
        }


double poisson (double x,double lambda)
  {
   double pois,s,value;
   int k;
   long int i;
   value=0.;
   if(fabs(lambda)<1.e-8 && fabs(x)<1.e-2) 
     {
      value=1.0;
      return value;
     }
   if(lambda < 1.E-10) return value;
   pois=0.0;
   k=int(x);
   s=0.;
   if(k > 0)
   {
    for(i=1;i<=k; i++) s=s+log(double(i));
    
   }
   pois=-lambda+x*log(lambda);
   value=pois-s;
   value=exp(value);
   return value;
  } 
      


int cmpgle( double *arg1, double *arg2 )
{
    if( *arg1 > *arg2 )
	return 1;
    else if( *arg1 < *arg2 )
	return -1;
    else
	return 0;
}






void MixMod::EMCG()
{
int i,icount,numem=5,nump,ii;
double s,s11,ymax,b,alpha,beta=0.;
std::vector<double>change(k);
std::vector<double>gradq(k);
std::vector<double>grad1q(k);
std::vector<double>d(k);
int l;


// Number of mixing weights
l=k-1;

// Number of parameters in the model
nump=2*k-1;


initchange(change);
EM(numem);
getchange(change);
numem=1;  // Perform one EM-iteration

 // initialise delta 
 for(i=0;i<nump;i++)
   {
d[i]=change[i];
   }
 // start CG-algorithm

 for(icount=0;icount<maxstep;icount++)
 {
	// delta Psi also serves as starting value fro the direction d

// end of delta PSI


// 1. Compute the gradient of  LogL() 



gradcg(gradq,p,t);
alpha=stepjj(gradq,d);

// end of step-length choice

// 3. Update of PSI = PSI+alpha*d
s=0.;
ii=l;
s=0.;
for(i=0;i<l;i++)
{
 p[i]=p[i]+alpha*d[i];
 if(p[i]<1.E-4)p[i]=0;
 s=s+p[i];
}

p[l]=1.-s;
 ii=l;
   for(i=0;i<k;i++)
  {
   t[i]=t[i]+alpha*d[ii];
   ii++;
  }
// end of update
// Using the updated parameter vector get the result of the nextEM-step
initchange(change);
EM(numem);
getchange(change); 
// 4. compute beta
gradcg(grad1q,p,t); // get the gradient for the new parameter vector
s=0.;
s11=0.;

for(i=0;i<nump;i++)
 { 
  b=grad1q[i]-gradq[i];
  s=s+b*change[i];
  s11=s11+b*d[i];
 }
if(fabs(s11) > 1.E-10) beta=s/s11;

  
//Compute the direction d

if(icount%nump ==0)
{
 for(i=0;i<nump;i++)d[i]=change[i];
   }
else
  {
s=0.;
for(i=0;i<nump;i++)
  {
 d[i]=change[i]-beta*d[i]; 
 s=s+d[i];
  }
  }
if(fabs(s) > 1.E-1) for(i=0;i<nump;i++)d[i]=change[i];
    ll=likelihood();
           maxderiv (ymax);
	    if ( (fabs(double(ymax-1.0)) <acc) && (icount > 2 )) break;
  } 
 } 



void MixMod::initchange (std::vector<double> cx)
{
int l,i,ii;
 l=k-1;
   for(i=0;i<l;i++) cx[i]=p[i];
   ii=l;
   for(i=0;i<k;i++)
   {
   cx[ii]=t[i];
   ii++;
   }

}

void MixMod::getchange(std::vector<double> cx)
{
int l,i,ii;
l=k-1;

   for(i=0;i<l;i++) cx[i]=cx[i]-p[i];
   ii=l;
   for(i=0;i<k;i++)
  {
   cx[ii]=cx[ii]-t[i];
   ii++;
  }

}


 

double MixMod::stepcg(std::vector<double> gradcg,double *d)
{
double alpha,a,galphab,c,gstrich,test,alphab;
int i,nump;
nump=2*k-1;
alpha=0.;
alphab=2;
a=likelihood();
gstrich=0.;

for(i=0;i<nump;i++) gstrich=gstrich+gradcg[i]*d[i];

for(i=0;i<20;i++)
 {
  galphab=loglike1(alphab,d);
c=(galphab-a-gstrich*alphab)/(alphab*alphab);
alpha=-gstrich/(2.0*c);
test=loglike1(alpha,d);
if(test > a) break;
 alphab = alphab/2.;
}
//temporary matrix of function values
if(test< a)alpha=0.;
return alpha;
}


double MixMod::loglike1(double alphab,double *d)
{
double value,s,ad,sp;
std::vector<double> td(k);
std::vector<double> pd(k);
int i,j,l,ii;
double * s1t;
//if (longmode) s1t = new double [nnlong];
//else s1t = new double [nn];
s1t = (double *) R_alloc(n, sizeof(double));
l=k-1;
   sp=0.;
   for(i=0;i<l;i++)
   {
    pd[i]=p[i]+alphab*d[i];
    sp=sp+pd[i]; 
   }
   pd[l]=1.-sp;
   ii=l;
   for(i=0;i<k;i++)
  {
   td[i]=t[i]+alphab*d[ii];
   ii++;
  }
   sp=0.;
   for(j=0;j<k;j++)
   { 
     
     sp=sp+pd[j];
   }

    for(i=0;i<n;i++)
   {
    s=0.;
       for(j=0;j<k;j++)
      {
       ad=poisson(x[i][0],td[j]);
       s=s+pd[j]*ad;
      }
     s1t[i]=s;
  }


value=0.;
 for(i=0;i<n;i++)
 {
 value=value+x[i][1]*g(s1t[i]);
 }

 return value;


}
 
void MixMod::gradcg(std::vector<double> gradq,double *p1,double *t1)
{
//int nump;
int i,j,ii;
double s,b;
double **pij, **xft;
double *s1t;
/*if (longmode) {
   pij = new double *[kk];
   xft = new double *[kk];
   for (i=0;i<kk;i++) {xft[i]=new double[nnlong];
                      pij[i] = new double [nnlong];}
   s1t = new double[nnlong];}
else {   
   pij = new double *[kk];
   xft = new double *[kk];
   for (i=0;i<kk;i++) {xft[i]=new double[nn];
                      pij[i] = new double [nn];}
   s1t = new double[nn];}
   */
//new
   pij = (double **) R_alloc(k, sizeof(double *));
   xft =(double **) R_alloc(k, sizeof(double *));
   for (i=0;i<k;i++) {xft[i]=(double *) R_alloc(n, sizeof(double));
                      pij[i] = (double *) R_alloc(n, sizeof(double));}
   s1t = (double *) R_alloc(n, sizeof(double));


int l;


// Number of mixing weights
l=k-1;


// Number of parameters in the model
//nump=2*k-1;

// temporary matrix of function values
  for(i=0;i<n;i++)
   {
    s=0.;
       for(j=0;j<k;j++)
      {
       xft[i][j]=poisson(x[i][0],t1[j]);
       s=s+p1[j]*xft[i][j];
      }
     s1t[i]=s;
  }

	/* Computation of Q�(theta,theta�) 
	   which gives the score of the complete 
	   data likelihood */


// E-Step  Posteriori expectation of component membership
  
   for(i=0;i<n;i++)
   {
       for(j=0;j<k;j++)
      {
       xft[i][j]=poisson(x[i][0],t1[j]);
       pij[i][j]=0.;
       if(s1[i] > 1.E-12)
       pij[i][j]=p1[j]*xft[i][j]/s1t[i];
      } 
   }
 

   // Gradient of LogL()

// First part: The k-1 mixing weights using the constraint that sum p_j =1

       l=k-1;
       for(j=0;j<l;j++)
       {
        s=0.;
        for(i=0;i<n;i++)
	{
                         
         b=xft[i][j]-xft[i][l]; // Contribution of the last component to the gradient
         if(s1t[i] > 1.E-12) s=s+x[i][1]*b/s1t[i];
	}  
	gradq[j]=s;
       }
      
       // Second part: Population parameters 
       ii=l;
        for(j=0;j<k;j++)
       {
        s=0.;
        for(i=0;i<n;i++)
       {
       b=0.;
       if(fabs(t1[j]) > 1.E-10)  b=(x[i][0]-t1[j])/t1[j];
       s=s+x[i][1]*pij[i][j]*b;
   
       }
     
        gradq[ii]=s;
        ii++;
     }
}

// The function gradcg is overloaded to avoid trouble with
// variable length arrays that the strict CRAN policy does not
// allow
void MixMod::gradcg(std::vector<double> gradq,std::vector<double> p1,
                    std::vector<double>t1)
{
//int nump;
int i,j,ii;
double s,b;
double **pij, **xft;
double *s1t;
/*if (longmode) {
   pij = new double *[kk];
   xft = new double *[kk];
   for (i=0;i<kk;i++) {xft[i]=new double[nnlong];
                      pij[i] = new double [nnlong];}
   s1t = new double[nnlong];}
else {   
   pij = new double *[kk];
   xft = new double *[kk];
   for (i=0;i<kk;i++) {xft[i]=new double[nn];
                      pij[i] = new double [nn];}
   s1t = new double[nn];}
   */
//new
   pij = (double **) R_alloc(k, sizeof(double *));
   xft =(double **) R_alloc(k, sizeof(double *));
   for (i=0;i<k;i++) {xft[i]=(double *) R_alloc(n, sizeof(double));
                      pij[i] = (double *) R_alloc(n, sizeof(double));}
   s1t = (double *) R_alloc(n, sizeof(double));


int l;


// Number of mixing weights
l=k-1;


// Number of parameters in the model
//nump=2*k-1;

// temporary matrix of function values
  for(i=0;i<n;i++)
   {
    s=0.;
       for(j=0;j<k;j++)
      {
       xft[i][j]=poisson(x[i][0],t1[j]);
       s=s+p1[j]*xft[i][j];
      }
     s1t[i]=s;
  }

  /* Computation of Q�(theta,theta�) 
	   which gives the score of the complete 
	   data likelihood */


// E-Step  Posteriori expectation of component membership
  
   for(i=0;i<n;i++)
   {
       for(j=0;j<k;j++)
      {
       xft[i][j]=poisson(x[i][0],t1[j]);
       pij[i][j]=0.;
       if(s1[i] > 1.E-12)
       pij[i][j]=p1[j]*xft[i][j]/s1t[i];
      } 
   }
 

   // Gradient of LogL()

// First part: The k-1 mixing weights using the constraint that sum p_j =1

       l=k-1;
       for(j=0;j<l;j++)
       {
        s=0.;
        for(i=0;i<n;i++)
	{
                         
         b=xft[i][j]-xft[i][l]; // Contribution of the last component to the gradient
         if(s1t[i] > 1.E-12) s=s+x[i][1]*b/s1t[i];
	}  
	gradq[j]=s;
       }
      
       // Second part: Population parameters 
       ii=l;
        for(j=0;j<k;j++)
       {
        s=0.;
        for(i=0;i<n;i++)
       {
       b=0.;
       if(fabs(t1[j]) > 1.E-10)  b=(x[i][0]-t1[j])/t1[j];
       s=s+x[i][1]*pij[i][j]*b;
   
       }
     
        gradq[ii]=s;
        ii++;
     }
}







double MixMod::stepjj(std::vector<double> gradq, std::vector<double> d)
 {
 double alpha0=0.,alpha1=2.,FNull,s,Falpha,astar=0.;
 std::vector<double>pd(k);
 std::vector<double>td(k);
 double sp,Fstart,diff;
 int i,j,nump,inum,l,ii;
 l=k-1;
 nump=2*k-1;

 s=0.;
 for(i=0;i<nump;i++)
 {
   s=s+d[i]*gradq[i];
 }
 FNull=s;
 Fstart=FNull*0.1;

 for(inum=0;inum<2;inum++)
 {
  sp=0.;
 for(i=0;i<l;i++)
   {
    pd[i]=p[i]+alpha1*d[i];
    sp=sp+pd[i];
   }
   pd[l]=1.-sp;
   ii=l;
   for(i=0;i<k;i++)
  {
   td[i]=t[i]+alpha1*d[ii];
   ii++;
  }
   sp=0.;
   for(j=0;j<k;j++)
   {      
     sp=sp+pd[j];
   }
 gradcg(gradq,pd,td);
 s=0.;
 for(i=0;i<nump;i++)
   {
    s=s+d[i]*gradq[i];
   } 
 Falpha=s;
 diff=FNull-Falpha;
 if(diff > 1.E-10)astar= (alpha1*FNull-alpha0*Falpha)/diff;
 alpha0=alpha1;
 FNull=Falpha;
 alpha1=astar;
 if(inum > 0 && fabs(Falpha) < fabs(Fstart)) break; 
 }
if(astar > 50) astar=0.;
 return astar; 
}

void
R_init_mylib(DllInfo *info)
{
/* Register routines, allocate resources. */
}
void
R_unload_mylib(DllInfo *info)
{
/* Release resources. */
}

}
