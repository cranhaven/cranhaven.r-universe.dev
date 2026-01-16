# include "kml.h"



#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))

/*void R_init_kml(DllInfo *info){
    printMatrix = R_GetCCallable("longitudinalData","printMatrix");
    printMatrixInt = R_GetCCallable("longitudinalData","printMatrixInt");
    }*/

/* ****************************************************
********************** DISTANCES **********************
**************************************************** */



void printMatrix(double *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %f",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}



void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %i",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}




/* *****************************************************
****************** Distances for traj ******************
***************************************************** */

static double euclideanTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int nbNA=0, i=0;
    double difference=0.0;

    for(i=0 ; i<*taille ; i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = x[i]-y[i];
	    if(!ISNAN(difference)){
		dist += difference * difference;
	    }else{}
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    dist = pow(dist,0.5);
    return(dist);
}


void calculMean(double *traj, int *nbInd, int *nbTime, int *clusterAffectation, int *nbClusters, double *trajMean){

    int i=0,j=0;
    int nbEachCluster[*nbClusters * *nbTime];

    /* Rprintf("Traj =\n");
    printMatrix(traj,nbTime,nbInd);
    Rprintf("\n\nnbInd=%i nbTime=%i nbClusters=%i",*nbInd,*nbTime,*nbClusters);
    Rprintf("\nclusterAffectation=%i %i %i",clusterAffectation[0],clusterAffectation[1],clusterAffectation[2]);
    Rprintf("\n\ntrajMean =\n");
    printMatrix(trajMean,nbTime,nbClusters); */

    // Initialisation de mTrajMean
    for (i=0 ; i < *nbClusters * *nbTime ; i++){
	trajMean[i] = 0;
	nbEachCluster[i] = 0;
    };

    for (i=0 ; i < *nbInd ; i++){
        for(j=0 ; j < *nbTime ; j++){
	    if(R_FINITE(traj[i * *nbTime + j]) && clusterAffectation[i]!=NA_INTEGER ){
		trajMean[(clusterAffectation[i]-1) * *nbTime + j] += traj[i * *nbTime + j];
		nbEachCluster[(clusterAffectation[i]-1) * *nbTime + j] += 1;
		/*  Rprintf("\n\nXXXXXXXX\ni=%i j=%i indice=%i trajMean=%f nbEach=%i",i,j,(vClusterAffectation[i]-1) * *iNbInd + j,
		    mTrajMean[(vClusterAffectation[i]-1) * *iNbTime + j],
		    nbEachCluster[(vClusterAffectation[i]-1) * *iNbTime + j]
		);
            Rprintf("\n\nTrajMean =\n");
            printMatrix(mTrajMean,iNbTime,iNbClusters);*/
	    }else{};
	};
    }
    for (i=0 ; i < *nbClusters ; i++){
        for(j=0 ; j < *nbTime ; j++){
	    trajMean[i * *nbTime + j] /= nbEachCluster[i * *nbTime + j];
	}
    }
/* Rprintf("\n\nTrajMean (from calculMean)=\n");
   printMatrix(mTrajMean,iNbTime,iNbClusters);*/
}


void affecteIndiv(double *traj, int *nbInd, int *nbTime, double *trajMean, int *nbClusters, int *clusterAffectation){

    int i=0,j=0 ;
    double best=0;
    double *dist=malloc(sizeof(double)); // here some valgrind trouble ? Why ?
    *dist=0;
/*    Rprintf("Pre=%p,P=%p, Dist=%f\n",&dist,dist,*dist);
    Rprintf("\nTraj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);

    Rprintf("\n\nTrajMean =\n");
    printMatrix(mTrajMean,iNbTime,iNbClusters);
*/
    for (i=0 ; i<*nbInd ; i++){
	*dist = euclideanTraj(&traj[i * *nbTime], &trajMean[0], nbTime);
        // Rprintf("-- 0 -- %f %f %i %f %f\n",mTraj[i * *iNbTime], mTrajMean[0], *iNbTime, *power, *dist);
	best = *dist;
	// distance(double *x,double *y,int *taille, double *power, double *dist)
	// Rprintf("\nBest=%f \n",best);
	clusterAffectation[i] = 1 ;

	for(j=2 ; j<*nbClusters+1 ; j++){
	*dist = euclideanTraj(&traj[i * *nbTime], &trajMean[(j-1) * *nbTime], nbTime);
        // Rprintf("-- j -- %f %f %i %f %f\n",mTraj[i * *iNbTime], mTrajMean[0], *iNbTime, *power, *dist);

	// R_distance2(&mTraj[i * *iNbTime], &mTrajMean[(j-1) * *iNbTime], iNbTime, distance, power, &dist);
	// Rprintf("\nDist=%f \n",dist);
	    if(*dist<best){
		best = *dist;
		clusterAffectation[i] = j ;
	    }else{};
	    // Rprintf("\nBest2=%f \n",best);
	}
    }
    free(dist);
    
}



void kml1(double *traj, int *nbInd, int *nbTime, int *nbClusters, int *maxIt, int *clusterAffectation1, int *convergenceTime){

    int i=0,iter=0;
    int *clusterAffectation2=malloc(*nbInd * sizeof(int));
    double *trajMean=malloc(*nbClusters * *nbTime * sizeof(double));

    for(i = 0; i < *nbClusters * *nbTime; i++){trajMean[i] = 0.0;};
    for(i = 0; i < *nbInd; i++){clusterAffectation2[i] = 0;};

    /*    Rprintf("Traj =\n");
    printMatrix(traj,nbTime,nbInd);
    Rprintf("\n\nnbInd=%i nbTime=%i nbClusters=%i maxIt=%i ",*nbInd,*nbTime,*nbClusters,*maxIt);
    Rprintf("\nclusterAffectation1=%i %i %i",clusterAffectation1[0],clusterAffectation1[1],clusterAffectation1[2]);
    Rprintf("\nclusterAffectation2=%i %i %i",clusterAffectation2[3],clusterAffectation2[4],clusterAffectation2[5]);
    Rprintf("\nConvergenceTime=%i \n **************************",*convergenceTime);
    */


    //    printMatrix(trajMean,iNbTime,iNbClusters);

//    for(iNbId = 0; iNbId < nbId; iNbId++) vClusterAffectation[iNbId] = 1;
//    Rprintf("\n\nDDD mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
    //  Rprintf("\n DDD mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
    //    for(iter = 0; iter < *maxIt; iter+=2){
    for(iter = 0; iter < *maxIt; iter+=2){
      //	Rprintf("+++++++++++++ ITER1=%i +++++++++++++\n",iter);
      //	printMatrix(trajMean,nbTime,nbClusters);
	calculMean(traj,nbInd,nbTime,clusterAffectation1,nbClusters,trajMean);
	//	Rprintf("traj mean\n");
        // printMatrix(trajMean,nbTime,nbClusters);
       	affecteIndiv(traj,nbInd,nbTime,trajMean,nbClusters,clusterAffectation2);
	//	for(int j = 0; j < *nbInd; j++){Rprintf("%i",clusterAffectation1[j]);};	Rprintf("\n");
	// for(int j = 0; j < *nbInd; j++){Rprintf("%i",clusterAffectation2[j]);};	Rprintf("\n");

	i = 0;
	while(i<*nbInd && clusterAffectation1[i]==clusterAffectation2[i]){i++;};
	if(i == *nbInd){
	    *convergenceTime = iter + 1;
	    break;
	}else{};
	//	Rprintf("+++++++++++++ ITER2=%i +++++++++++++\n",iter);
	//printMatrix(trajMean,nbTime,nbClusters);
	calculMean(traj,nbInd,nbTime,clusterAffectation2,nbClusters,trajMean);
	//Rprintf("traj mean\n");
	//printMatrix(trajMean,nbTime,nbClusters);
	affecteIndiv(traj,nbInd,nbTime,trajMean,nbClusters,clusterAffectation1);
	//for(int j = 0; j < *nbInd; j++){Rprintf("%i",clusterAffectation1[j]);};	Rprintf("\n");
	//for(int j = 0; j < *nbInd; j++){Rprintf("%i",clusterAffectation2[j]);};	Rprintf("\n");

	i = 0;
	while(i<*nbInd && clusterAffectation1[i]==clusterAffectation2[i]){i++;};
	if(i == *nbInd){
	    *convergenceTime = iter + 2;
      	    break;
	}else{};
    }
    free(clusterAffectation2);
    free(trajMean);
}

