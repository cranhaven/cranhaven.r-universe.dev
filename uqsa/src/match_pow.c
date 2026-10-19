#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
typedef SEXP Rdata;

struct stack {
	char *buffer;
	size_t size;
	size_t n;
};

#ifndef _GNU_SOURCE
void *mempcpy(void *dest, const void *src, size_t len){
	return (char*) memcpy (dest, src, len) + len; /* the cast is to allow pointer arithmetic */
}
#endif

const char *pow_str[]={"gsl_pow_","gsl_pow_int","pow",NULL};
enum pow_type {small_integer_power, integer_power, double_power};

int word(char c){
	if (c >= 'a' && c <= 'z') return 1;
	if (c >= 'A' && c <= 'Z') return 1;
	if (c >= '0' && c <= '9') return 1;
	if (c=='_' || c=='.') return 1;
	return 0;
}

int find(const char *s, char c){
	int i;
	for (i=0;i<strlen(s);i++){
		if (s[i]==c) break;
	}
	return i;
}

/* remove whitespace and one layer of parenthese*/
void tighten(char **left, char **right){
	while (**left==' ') (*left)++;
	while (**right==' ') (*right)--;
	if (**left=='(' && **right==')'){
		(*left)++;
		(*right)--;
	}
}

/* does this: (var1 + (var2 / var3))         */
/*            ^boundary            ^start    */
/* either in forward or backward direction   */
char* find_boundary(const char* src, char *start, int direction){
	char *ptr=start;
	if (direction==0) return ptr;
	int n=0; /* parnethesis counter */
	do {
		switch(*ptr){
		case ')': n-=direction; break;
		case '(': n+=direction; break;
		}
		ptr+=direction;
	} while (ptr>=src && (word(*ptr) || n>0));
	return ptr-direction;
}

int count(const char *s, char c){
	const char *p=s;
	int z=0;
	while (*p){
		z+=(c==*p);
		p++;
	}
	return z;
}

/* check that all n characters are numerical digits [0-9] */
int digit(const char *str, size_t n){
	int i;
	for (i=0;i<n;i++){
		if (str[i]<'0' || str[i]>'9') return 0;
	}
	return 1;
}

/* replaces a^b with pow(a,b) */
void replace(char *buffer, const char *src){
	char *ptr;
	char *out;
	char *b_open, *b_close, *e_open, *e_close;
	/* b - base; e - exponent; */
	const char *ps;
	size_t n;
	enum pow_type t=double_power;
	const char *hat=strchr(src,'^');
	if (hat){
		ptr = (char*) hat-1;
		while (*ptr==' ') ptr--;
		/* find base: */
		b_close= ptr;
		b_open = find_boundary(src,ptr,-1);
		/* copy initial part, if any, to output buffer */
		*(out=(char*) mempcpy(buffer,src,b_open-src))='\0';
		/* find the exponent */
		ptr  = (char*) hat+1;
		while (*ptr==' ') ptr++;
		e_open = ptr;
		e_close= find_boundary(src,ptr,+1);
		ptr=e_close+1;
		tighten(&b_open,&b_close);
		tighten(&e_open,&e_close);
		/* add power functin to buffer: */
		if (e_open==e_close && digit(e_open,1)){
			t=small_integer_power;
		} else if (digit(e_open,1+(e_close-e_open))) {
			t=integer_power;
		}
		ps=pow_str[t];
		n=strlen(ps);
		switch (t){
		case small_integer_power:
			*(out=(char*) mempcpy(out,ps,n))='\0';
			*(out=(char*) mempcpy(out,e_open,1+(e_close-e_open)))='\0';
			*(out=(char*) mempcpy(out,"(",1))='\0';
			*(out=(char*) mempcpy(out,b_open,1+(b_close-b_open)))='\0';
			*(out=(char*) mempcpy(out,")",1))='\0';
			break;
		default:
			*(out=(char*) mempcpy(out,ps,n))='\0';
			*(out=(char*) mempcpy(out,"(",1))='\0';
			*(out=(char*) mempcpy(out,b_open,1+(b_close-b_open)))='\0';
			*(out=(char*) mempcpy(out,", ",2))='\0';
			*(out=(char*) mempcpy(out,e_open,1+(e_close-e_open)))='\0';
			*(out=(char*) mempcpy(out,")",1))='\0';
		}
		*(out=(char*) mempcpy(out,ptr,strlen(ptr)))='\0';
	}
}

Rdata replace_pow(Rdata str){
	int i,j,N=length(str);
	Rdata w = PROTECT(NEW_CHARACTER(N)); /* return value */
	const char *input;
	int n;
	const char max_pow_str[]="gsl_pow_int(,)"; /* maximum possible extra content per caret (^) in input */
	size_t max_l=strlen(max_pow_str);
	char *buffer; /* output buffer */
	char *src;    /* copy of the input */
	int m;
	for (i=0;i<N;i++){
		input = CHAR(STRING_ELT(str,i));
		n=count(input,'^');
		m=1+strlen(input)+max_l*n;
		buffer=malloc(m);
		src=malloc(m);
		strcpy(src,input);
		strcpy(buffer,input);
		for (j=0;j<n;j++){
			replace(buffer,src);
			strcpy(src,buffer);
		}
		SET_STRING_ELT(w, i, mkChar(buffer));
	}
	UNPROTECT(1);
	return w;
}
