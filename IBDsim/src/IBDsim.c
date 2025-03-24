#include <R.h>
#include <Rinternals.h>
//#include <Rmath.h>

void recombine(double *p1, int *a1, int *N1, double *p2, int *a2, int *N2, double *cross, int *ncross, double *p3, int *a3) {
	int ic = 0, i1, i2 = 1, i3 = 0;
	double cx;
	
	if(*ncross == 0) {
		for(i1 = 0; i1 < *N1; i1++) {p3[i1] = p1[i1]; a3[i1] = a1[i1];}
		return;
	}
    for(i1 = 0; (i1 < *N1) && (p1[i1] < cross[0]); i1++) {  //til f?rste break
		p3[i3] = p1[i1]; 
		a3[i3] = a1[i1]; //Rprintf("str1, satt inn f?rste: pos=%f,allel=%d\n",p1[i1],a1[i1]);
		i3++;
	}
	while(ic < *ncross - 1) {
		cx = cross[ic]; //i f?rste loop: cx er fremdeles f?rste break
		for(i2 = i2-1; (i2 < *N2) && (p2[i2] <= cx); i2++)  { //Rprintf("skipper str2, pos=%f\n",p2[i2]); //skip rows with pos < break (cx)
		}
		p3[i3] = cx;   
		a3[i3] = a2[i2-1];
		i3++; //Rprintf("str2, satt inn cx=%f,allel=%d\n",cx,a2[i2-1]);
		
		cx = cross[ic+1];  //set next break point
		for(int j = i2; (j < *N2) && (p2[j] < cx); j++) { //Rprintf("str2, setter inn pos=%f,allel=%d\n",p2[i2],a2[i2]);
			i2 = j;
		  p3[i3] = p2[i2]; 
			a3[i3] = a2[i2]; 
			i3++;
		}
		
		for(i1 = i1-1; (i1 < *N1) && (p1[i1] <= cx); i1++)  {}
		p3[i3] = cx;   
		a3[i3] = a1[i1-1];	//Rprintf("str1, satt inn cx=%f,allel=%d\n",cx,a1[i1-1]);
		i3++;
		
        if(ic < *ncross - 2) {
            for(int j = i1; (j < *N1) && (p1[j] < cross[ic+2]); j++) { 
                i1 = j;
                p3[i3] = p1[i1]; 
                a3[i3] = a1[i1]; //Rprintf("str1, setter inn pos=%f,allel=%d\n",p1[i1],a1[i1]);
                i3++;
            }
        }
        
		ic += 2;
	}
	if(ic == *ncross - 1) {
		cx = cross[ic];  //siste break
		for(i2 = i2-1; (i2 < *N2) && (p2[i2] <= cx); i2++)  {} //skip rows with pos < break (cx)
		p3[i3] = cx;   
		a3[i3] = a2[i2-1];
		i3++;  //Rprintf("str2, satt inn siste break, cx=%f,allel=%d\n",cx,a2[i2-1]);
		for(int j = i2; j < *N2; j++) {
			i2 = j;
			p3[i3] = p2[i2]; 
			a3[i3] = a2[i2]; 
			i3++;  //Rprintf("str2, satt inn siste: pos=%f,allel=%d\n",p2[i2],a2[i2]);
		}
    }
	else {
		for(int j = i1; j < *N1; j++) {
			i1 = j;
		  p3[i3] = p1[i1]; 
			a3[i3] = a1[i1]; 
			i3++; //Rprintf("str1, satt inn siste: pos=%f,allel=%d\n",p1[i1],a1[i1]);
		}
	}
}


// void geneticMap(double *mapPhys, double *mapCm, double *cmLookup, int *n, double *res)
// {
// int i,j, st=0;
// for(j = 0; j < *n; j++) {
  // for(i = st; mapCm[i] < cmLookup[j]; i++) {
	// }
  // res[j] = mapPhys[i-1] + (mapPhys[i]-mapPhys[i-1])*(cmLookup[j]-mapCm[i-1])/(mapCm[i]-mapCm[i-1]);
  // st = i-1;
// }
// }

// void crossovers(double *L_cM, double *cross, int *ncross) {
// GetRNGstate();
// int i, m = 4, ncr = 0;
// const int nC = rpois(*L_cM / 100 * 2 * (m + 1));
// double C_ev[nC], *ip, r;	//Rprintf("nC= %d\n",nC);
 
// //pick nC random numbers uniformly in (0,LcM) and sort them
// for(ip = &C_ev[0]; ip < &C_ev[nC]; ip++) 	*ip = runif(0, *L_cM);
// R_rsort(&C_ev[0], nC); //Rprintf("sorted OK\n");

// //select every 5th (starting randomly among the first 5) and thin with probability 0.5
// for(i = runif(0, m+1); i < nC; i+=(m+1)) {
	// r = unif_rand();
	// if(r > 0.5) 	cross[ncr++]=C_ev[i];	//Rprintf("index i = %d; r=%3.1f; teller = %d; cross = %4.4f\n", i, r, ncross, C_ev[i]);	
// }
// *ncross = ncr;
// //for(i = 0; i < ncr+1; i++)		Rprintf("cross__crossover = %4.2f\n", cross[i]);
// PutRNGstate();
// }

// void meiosis(int *p1, int *a1, int *N1, int *p2, int *a2, int *N2, double *mapphys, double *mapcm, int *Nmap, int *pos, int *allel) {
// double L_cM = mapcm[*Nmap-1];
// int i, maxcross = 20, ncross = 0;
// double cross[maxcross];
// //for(i = 0; i < maxcross; i++)		Rprintf("cross = %4.2f\n", cross[i]);
// crossovers(&L_cM, &cross[0], &ncross);
// // for(i = 0; i < maxcross; i++)		Rprintf("cross = %4.2f\n", cross[i]);

// double cross_cm[ncross], cross_phys[ncross];
// int cross_int[ncross];
// for(i = 0; i < ncross; i++)	cross_cm[i] = cross[i];
// //Rprintf("sizeof cm= %d\n", sizeof(cross_cm)/sizeof(cross_cm[0]));
	
// geneticMap(mapphys, mapcm, &cross_cm[0], &ncross, &cross_phys[0]);
	// //for(i = 0; i < ncross; i++)		Rprintf("cross_phys = %4.2f\n", cross_phys[i]);
// for(i=0; i<ncross; i++) cross_int[i] = cross_phys[i]+0.5;
	// //for(i = 0; i < ncross; i++)		Rprintf("cross_int = %d\n", cross_int[i]);

// recombine(p1, a1, N1, p2, a2, N2, &cross_int[0], &ncross, pos, allel);
// }




// void meio(int *p1, int *a1, int *N1, int *p2, int *a2, int *N2, double *mapphys, double *mapcm, int *Nmap, int *p3, int *a3) {
// GetRNGstate();
// int i, ncross = 0, m = 4;
// double LcM = mapcm[*Nmap-1];
// const int nC = rpois(LcM / 100 * 2 * (m + 1));
// double C_ev[nC], *ip, Cx[nC], r;

// //pick nC random numbers uniformly in (0,LcM) and sort them
// for(ip = &C_ev[0]; ip < &C_ev[nC]; ip++) 	*ip = runif(0, LcM);
// R_rsort(&C_ev[0], nC); 

// //select every 5th (starting randomly among the first 5) and thin with probability 0.5
// for(i = runif(0, m+1); i < nC; i+=(m+1)) {
	// r = unif_rand();
	// if(r > 0.5) 	Cx[ncross++]=C_ev[i];		Rprintf("index i = %d; r=%3.1f; teller = %d; cross = %4.4f\n", i, r, ncross, C_ev[i]);	
// }
// Rprintf("LcM=%f, nC=%d, ncross=%d \n",LcM,nC,ncross);

// //Cx now contains trailing 0's, create new vector without these
// double cross[ncross]; int cross_ph[ncross];
// for(i = 0; i < ncross; i++) 	cross[i]=Cx[i];

// //find physical positions corresponding to the cross positions
// geneticMap(&mapphys[0], &mapcm[0], &cross[0], &ncross, &cross_ph[0]);

// for(i = 0; i < ncross; i++)		Rprintf("cross = %4.2f\n", cross_ph[i]);
// PutRNGstate();

// //recombine(p1, a1, N1, p2, a2, N2, &cross_ph[0], &ncross, p3, a3);

// }
