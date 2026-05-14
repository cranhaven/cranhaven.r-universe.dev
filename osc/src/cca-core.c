
#include <R.h>
#include <math.h>
#include <stdio.h>

//Required 
void getrow(int *x, int *xmax, int *ymax, int *data, int *ret) {

	int y;
	for(y=0; y<= *ymax; y++){
		ret[y] = data[*x * *ymax + y];
	}
}
void getcol(int *y, int *xmax, int *ymax, int *data, int *ret) {
	int x;
	for(x=0; x<= *xmax; x++){
		ret[x] = data[x * *ymax + *y];
	}
}
void getblock(int *y, int *x, int *dy, int *dx, int *xmax, int *ymax, int *data, int *ret) {
	int e, f;
	for(e=0; e< *dx; e++){
		for(f=0; f< *dy; f++){
			ret[f * *dy + e] = data[f * *ymax + e];
		}
	}
}


void burnn(int *x,int *y,int *c, int *xmax, int *ymax, int *data,int *clu) {
	// burn nearest neigbors
	//Rprintf("xycrxmaxymax\t%i\t%i\t%i\t%i\t%i\t%i\n",*x,*y,*c,1,*xmax,*ymax);
	int d, e,f,g;
	d=*x;					// steps to the left
	while(d>=0 && data[d * *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		d--;

	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		e++;
	}
	//Rprintf("  de data\t%i\t%i\t", d,e);
	for(f=*y+1;f>=*y-1;f=f-2){// one step up or down
		if(f<*ymax && f>=0){				
			for(g=d+1; g<e; g++){ //I think we must not include d and e to avoid diagonals
				if(clu[g * *ymax + f]==0&&data[g * *ymax + f]>0) {
					burnn(&g,&f,c,xmax,ymax,data,clu);
				}
			}
		}
	}
}

void burns(int *data,int *clu,int *x,int *y,int *c,int *s, int *xmax, int *ymax) {
	// burn s shells
	//Rprintf("xycrxmaxymax\t%i\t%i\t%i\t%i\t%i\t%i\n",*x,*y,*c,*s,*xmax,*ymax);
	long a,b;
	int dx,dy;
	int d, e,g;
	d=*x;					// steps to the left
	while(d>=0 && clu[d* *ymax + *y]==0 && data[d* *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		d--;

	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&clu[e * *ymax + *y]==0&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		e++;
	}
	for(g=d+1; g<e; g++){ // go allong all marked cells and use algorithm from diego
		for(a=-*s;a<=*s;a++) {
			dx=g+a;
			//Rprintf("dx\t%i\n",dx);
			if(dx>=0&&dx<*xmax) {
				for(b=-*s;b<= *s;b++) {
					dy=*y+b;
					//Rprintf("dy\t%i\n",dy);
					if(dy>=0&&dy<*ymax) {
						//Rprintf("clu[%i,%i], data[%i,%i]\t%i\t%i\n",dx, dy, dx,dy,clu[dx * *ymax +dy],data[dx * *ymax +dy]);
						if(clu[dx * *ymax +dy]==0&&data[dx * *ymax +dy]>0) {
							burns(data,clu,&dx,&dy,c,s,xmax,ymax);
						}
					}
				}
			}
		}
	}
}

void burnr(int *data,int *clu,int *x,int *y,int *c,int *r, int *xmax, int *ymax) {
	// burn with radius r
	// r=1 should correspond burnn
	long a,b;
	int dx,dy;
	double rd;
	int d, e,g;
	d=*x;					// steps to the left
	while(d>=0 && clu[d* *ymax + *y]==0 && data[d* *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		d--;

	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&clu[e * *ymax + *y]==0&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		e++;
	}
	for(g=d+1; g<e; g++){ // go allong all marked cells and use algorithm from diego

		for(a=-*r;a<=*r;a++) {
			for(b=-*r;b<=*r;b++) {
				rd=sqrt((double)a*(double)a+(double)b*(double)b);
				if(rd<=*r) {
					dx=g+a;
					dy=*y+b;
					if((dx>=0&&dx<*xmax)&&(dy>=0&&dy<*ymax)) {
						if(clu[dx * *ymax + dy]==0&& data[dx * *ymax + dy]>0) {
							burnr(data,clu,&dx,&dy,c,r, xmax, ymax);
						}
					}
				}
			}
		}
	}
}

 /* Note: R fills matrices per column, so first index is row,
     second is column */
void callburn(int *s, int *xmax, int *ymax, int *mode, int *data,int *clu) {
	int a,b;
	int c;
	c=0;
	for(a=0;a<*xmax;a++) {
		for(b=0;b<*ymax;b++) {
			if(data[a* *ymax + b]>0&&clu[a* *ymax + b]==0) {
				c++;
				if(*mode == 1){
					burnn(&a,&b,&c,xmax,ymax,data,clu);		// burn nearest
				} else if(*mode == 2){
					burns(data,clu,&a,&b,&c,s,xmax,ymax);		// burn shells
				} else if(*mode == 3){
					burnr(data,clu,&a,&b,&c,s,xmax,ymax);		// burn radius
				} else {
					Rprintf("unknown mode: %d\n", *mode);
				}
			}
		}
	}
}

			


void burnn_count(int *x,int *y,int *c, int *xmax, int *ymax, int *data,int *clu, int *count) {
	// burn nearest neigbors
	//Rprintf("xycrxmaxymax\t%i\t%i\t%i\t%i\t%i\t%i\n",*x,*y,*c,1,*xmax,*ymax);
	int d, e,f,g;
	d=*x;					// steps to the left
	while(d>=0 && data[d * *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		count[*c-1]++;
		d--;
	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		count[*c-1]++;
		e++;
	}
	//Rprintf("  de data\t%i\t%i\t", d,e);
	for(f=*y+1;f>=*y-1;f=f-2){// one step up or down
		if(f<*ymax && f>=0){				
			for(g=d+1; g<e; g++){
				if(clu[g * *ymax + f]==0&&data[g * *ymax + f]>0) {
					burnn_count(&g,&f,c,xmax,ymax,data,clu,count);
				}
			}
		}
	}
}

void burns_count(int *data,int *clu,int *x,int *y,int *c,int *s, int *xmax, int *ymax, int *count) {
	// burn s shells
	//Rprintf("xycrxmaxymax\t%i\t%i\t%i\t%i\t%i\t%i\n",*x,*y,*c,*s,*xmax,*ymax);
	long a,b;
	int dx,dy;
	int d, e,g;
	d=*x;					// steps to the left
	while(d>=0 && clu[d* *ymax + *y]==0 && data[d* *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		count[*c-1]++;
		d--;

	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&clu[e * *ymax + *y]==0&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		count[*c-1]++;
		e++;
	}
	for(g=d+1; g<e; g++){ // go allong all marked cells and use algorithm from Diego
		for(a=-*s;a<=*s;a++) {
			dx=g+a;
			//Rprintf("dx\t%i\n",dx);
			if(dx>=0&&dx<*xmax) {
				for(b=-*s;b<= *s;b++) {
					dy=*y+b;
					//Rprintf("dy\t%i\n",dy);
					if(dy>=0&&dy<*ymax) {
						//Rprintf("clu[%i,%i], data[%i,%i]\t%i\t%i\n",dx, dy, dx,dy,clu[dx * *ymax +dy],data[dx * *ymax +dy]);
						if(clu[dx * *ymax +dy]==0&&data[dx * *ymax +dy]>0) {
							burns_count(data,clu,&dx,&dy,c,s,xmax,ymax,count);
						}
					}
				}
			}
		}
	}
}

void burnr_count(int *data,int *clu,int *x,int *y,int *c,int *r, int *xmax, int *ymax, int *count) {
	// burn with radius r
	// r=1 should correspond burnn
	long a,b;
	int dx,dy;
	double rd;
	int d, e,g;
	d=*x;					// steps to the left
	while(d>=0 && clu[d* *ymax + *y]==0 && data[d* *ymax + *y]>0) {
		clu[d * *ymax + *y]=*c;
		count[*c-1]++;
		d--;

	}
	e=*x+1;					// steps to the right
	while(e<*xmax&&clu[e * *ymax + *y]==0&&data[e * *ymax + *y]>0) {
		clu[e * *ymax + *y]=*c;
		count[*c-1]++;
		e++;
	}
	for(g=d+1; g<e; g++){ // go allong all marked cells and use algorithm from diego

		for(a=-*r;a<=*r;a++) {
			for(b=-*r;b<=*r;b++) {
				rd=sqrt((double)a*(double)a+(double)b*(double)b);
				if(rd<=*r) {
					dx=g+a;
					dy=*y+b;
					if((dx>=0&&dx<*xmax)&&(dy>=0&&dy<*ymax)) {
						if(clu[dx * *ymax + dy]==0&& data[dx * *ymax + dy]>0) {
							burnr_count(data,clu,&dx,&dy,c,r, xmax, ymax, count);
						}
					}
				}
			}
		}
	}
}

 /* Note: R fills matrices per column, so first index is row,
     second is column */
void callburn_count(int *s, int *xmax, int *ymax, int *mode, int *data,int *clu, int *count, int *countmax) {
	int a,b;
	int c;
	c=0;
	for(a=0;a<*xmax;a++) {
		for(b=0;b<*ymax;b++) {
			if(data[a* *ymax + b]>0&&clu[a* *ymax + b]==0) {
				c++;
				if(*countmax <= c){
				    Rprintf("count.max is too little\n");
				    return;
                                }
				if(*mode == 1){
					burnn_count(&a,&b,&c,xmax,ymax,data,clu,count);			// burn nearest
				} else if(*mode == 2){
					burns_count(data,clu,&a,&b,&c,s,xmax,ymax,count);		// burn shells
				} else if(*mode == 3){
					burnr_count(data,clu,&a,&b,&c,s,xmax,ymax,count);		// burn radius
				} else {
					Rprintf("unknown mode: %d\n", *mode);
				}
			}
		}
	}
}

