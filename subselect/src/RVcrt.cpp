#include <cmath>
#include "Sscma.h"
#include "Vsmabo.h"
#include "RVcrt.h"

namespace extendedleaps {

#ifdef COUNTING  
extern int fpcnt1;
#endif

rvgdata::rvgdata(vind nvariables)
  :   p(nvariables), s2(0)
{
	s2 = new symtwodarray(p);
}

rvgdata::~rvgdata()
{
	delete s2;
}

partialrvdata::partialrvdata(vind nvariables)
  :   p(nvariables)
{
	tmpv.resize(p);
	cndv.resize(p);
	for (int i=0;i<p;i++) {	
		tmpv[i] = 0.;
		cndv[i] = 0.;
	}
	vin.resize(p);
	m1t.assign(p,vector<real>(p));
}

rvdata::rvdata(vind lastvariab,vind nvtopiv,vind tnv,rvgdata *data,const deque<bool>& active,vind *origvarlist,real criterion)
  :  lastv(lastvariab), p(tnv), k(nvtopiv), crt(criterion), varin(active), orgvar(origvarlist), e(0), gdt(data), rpl(0), unreliable(false)
{
	try {
		if (k > 0)  {
			ivct.assign(p,0);
			e = new symtwodarray(k);
			for (vind i=0;i<p;i++) {
				if (i+k >= lastv) ivct[i] = new matvectarray(k,e,i-(lastv-k));
				else ivct[i] = new matvectarray(k,0,0);
			}
		}
		s2m1.assign(p,vector<real>(p));
		rpl = new real *[2*p+2];
	}
	catch (...)   {
		delete e;
		{ for (unsigned i=0;i<ivct.size();i++) delete ivct[i]; }
		delete[] rpl;
		throw;
	}
}

rvdata::~rvdata()
{
	for (unsigned i=0;i<ivct.size();i++) delete ivct[i]; 
	delete e;
	delete[] rpl;
}

void  rvdata::getpdata(partialdata* pd)  
{ 
	vind oi,oj;
	partialrvdata *pdasrv = static_cast<partialrvdata *>(pd);    
	
	/* Attention: pd MUST point to partialrvdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */

/*	partialrvdata *pdasrv = dynamic_cast<partialrvdata *>(pd);
	assert(pdasrv);                                               */

	setcriterion(pdasrv->getcrt());
	{ for (vind j=0;j<p;j++) varin[j] = pdasrv->vin[j]; }
	for (vind i=0;i<p;i++) if (varin[oi=orgvar[i]])
		for (vind j=0;j<p;j++)  if (varin[oj=orgvar[j]])  s2m1[i][j] = pdasrv->m1t[oi][oj]; 
}

real rvdata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{ 
	if (mmind.direct()) return updatecrt(dir,*(mmind.idpm()),*(mmind.idfm()),var,pdt,reliable,tol); 
	else return updatecrt(dir,*(mmind.iipm()),*(mmind.iifm()),var,pdt,reliable,tol); 
}


void rvdata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) pivot(dir,*(mmind.idpm()),*(mmind.idfm()),vp,t,pdt,fdt,last,reliable,tol); 
	else pivot(dir,*(mmind.iipm()),*(mmind.iifm()),vp,t,pdt,fdt,last,reliable,tol); 
}


real rvdata::updatecrt(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind var,partialdata* newdtpnt,bool& reliable,const double tol) const
{
	partialrvdata *newdata = static_cast<partialrvdata *>(newdtpnt);    
	
	/* Attention: newdtpnt MUST point to partialrvdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialrvdata *newdata = dynamic_cast<partialrvdata *>(newdtpnt);
	assert(newdata);                                                         */

	vind cnt(0);
	vind varind = prtmmit[var-1];
	real newcrt,e1 = (*e)(varind,varind);
	real *cv = newdata->getcndv(),iv1;
	deque<bool>& vin = newdata->vin;
	deque<bool>	mpvin(p);	// mapped vin - a version of vin with indices sorted according to the current variable ordering 

	{ for (vind i=0;i<p;i++) mpvin[i] = newdata->vin[orgvar[i]]; }

	reliable = true;
	rpl[0] = &e1;	vin = varin;
	if (dir == forward) vin[orgvar[var-1]] = true;
	else vin[orgvar[var-1]] = false;
	{ for (vind i=0;i<p;i++) mpvin[i] = vin[orgvar[i]]; }
	fmmind.reset();
	for (vind i=0;i<p;fmmind++,i++)	{
		if (mpvin[i] && (i!=var-1) )  {
			rpl[cnt+1] = &(iv1 = (*ivct[fmmind()])[varind]);
			rpl[cnt+2] = &(cv[orgvar[i]] = iv1 / e1);
			cnt += 2;
		}
	}
	if (dir == forward) rpl[(cnt+=1)] = &(cv[orgvar[var-1]] = 1./e1);
	cmpts2sm1(prtmmit,fmmind,newdata,newdata->getm1t(),orgvar,var,&mpvin[0],&mpvin[0],false);
	newcrt = frobenius(newdata->getm1t(),&vin[0]);
	rpl[cnt+1] = &newcrt;
	reliable = errcheck(rpl,tol,cnt+1);

	#ifdef COUNTING  
	fpcnt1 += p;
	#endif

	newdata->setpivotval(e1);
	newdata->setcrt(newcrt);
	return newcrt;
}

real rvdata::updatecrt(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind var,partialdata* newdtpnt,bool& reliable,const double tol) const
{
	partialrvdata *newdata = static_cast<partialrvdata *>(newdtpnt);    
	
	/* Attention: newdtpnt MUST point to partialrvdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialrvdata *newdata = dynamic_cast<partialrvdata *>(newdtpnt);
	assert(newdata);                                                         */
	
	vind cnt(0);
	vind varind = prtmmit[var-1];
	real newcrt,e1 = (*e)(varind,varind);
	real *cv = newdata->getcndv(),iv1;
	deque<bool>& vin = newdata->vin;
	deque<bool>	mpvin(p);	// mapped vin - a version of vin with indices sorted according to the current variable ordering

	{ for (vind i=0;i<p;i++) mpvin[i] = newdata->vin[orgvar[i]]; }
	
	reliable = true;
	rpl[0] = &e1;	vin = varin;
	if (dir == forward) vin[orgvar[var-1]] = true;
	else vin[orgvar[var-1]] = false;
	{ for (vind i=0;i<p;i++) mpvin[i] = vin[orgvar[i]]; }
	fmmind.reset();

	for (vind i=0;i<p;fmmind++,i++)	{
		if (mpvin[i] && (i!=var-1) )  {
		rpl[cnt+1] = &(iv1 = (*ivct[fmmind()])[varind]);
		rpl[cnt+2] = &(cv[orgvar[i]] = iv1 / e1);
		cnt += 2;
		}
	}

	if (dir == forward) rpl[(cnt+=1)] = &(cv[orgvar[var-1]] = 1./e1);
	cmpts2sm1(prtmmit,fmmind,newdata,newdata->getm1t(),orgvar,var,&mpvin[0],&mpvin[0],false);
	newcrt = frobenius(newdata->getm1t(),&vin[0]);
	rpl[cnt+1] = &newcrt;
	reliable = errcheck(rpl,tol,cnt+1);

	#ifdef COUNTING  
	fpcnt1 += p;
	#endif

	newdata->setpivotval(e1);
	newdata->setcrt(newcrt);
	return newcrt;
}

void rvdata::pivot(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,
			bool& reliable,const double tol)
{
	vind fpivotind = fmmind[vp-1];              

	partialrvdata* pdata = static_cast<partialrvdata *>(newpdtpnt);    
	rvdata* newdata = static_cast<rvdata *>(newfdtpnt);    
	
	/* Attention: pdtpnt and newdttpnt MUST point to partialrvdata and rvdata objects !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert       */
	
/*	partialrvdata* pdata = dynamic_cast<partialrvdata *>(newpdtpnt);
	rvdata* newdata = dynamic_cast<rvdata *>(newfdtpnt);
	assert(pdata && newdata);                                  */

	real pivotval = pdata->getpivotval();
	real *cv = pdata->getcndv();
	deque<bool>& colin = pdata->vin;
	deque<bool>	mprowin(p);	// mapped rowin - a version of rowin with indices sorted according to the current variable ordering
	deque<bool>	mpcolin(p);	// mapped colin - a version of colin with indices sorted according to the current variable ordering

	{ for (vind i=0;i<p;i++) { 
		mprowin[i] = newdata->varin[orgvar[i]];
		mpcolin[i] = colin[orgvar[i]];
	} } 

	symatpivot(prtmmit,pivotval,*e,*(newdata->e),vp,t,reliable,tol);
	fmmind.reset();

	for (vind i=0;i<vp;fmmind++,i++)  
		if (mprowin[i])  {
			vectorpivot(prtmmit,*ivct[fmmind()],*newdata->ivct[i],*e,cv[orgvar[i]],vp,t,reliable,tol);
			newdata->ivct[i]->switchtoowndata();
	} 
	if (dir == forward)  {
		prtmmit.reset(vp);
		for (vind j=vp;j<vp+t;prtmmit++,j++)   
			newdata->ivct[vp-1]->setvalue(j-vp,-(*ivct[fpivotind])[prtmmit()]/pivotval);  
		#ifdef COUNTING  
		fpcnt += t;
		#endif
		newdata->ivct[vp-1]->switchtoowndata();
	}
	fmmind.reset(vp+t);
	{ for (vind i=vp+t;i<p;fmmind++,i++)  
		if (mprowin[i])  {
			vectorpivot(prtmmit,*ivct[fmmind()],*newdata->ivct[i],*e,cv[orgvar[i]],vp,t,reliable,tol);
			newdata->ivct[i]->switchtoowndata();
		} 
	}

	{ for (vind j=0;j<p;j++)
		if (j+1 > vp && j+1 <= vp+t && !mpcolin[j]) mpcolin[j] =  true;
		else mpcolin[j] = false;
	}
	cmpts2sm1(prtmmit,fmmind,pdata,newdata->s2m1,orgvar,vp,&mprowin[0],&mpcolin[0],true);
}

void rvdata::pivot(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,
			bool& reliable,const double tol)
{
	vind fpivotind = fmmind[vp-1];              

	partialrvdata* pdata = static_cast<partialrvdata *>(newpdtpnt);    
	rvdata* newdata = static_cast<rvdata *>(newfdtpnt);    
	
	/* Attention: pdtpnt and newdttpnt MUST point to partialrvdata and rvdata objects !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert       */
	
/*	partialrvdata* pdata = dynamic_cast<partialrvdata *>(newpdtpnt);
	rvdata* newdata = dynamic_cast<rvdata *>(newfdtpnt);
	assert(pdata && newdata);                                  */

	real pivotval = pdata->getpivotval();
	real *cv = pdata->getcndv();
	deque<bool>& colin = pdata->vin;
	deque<bool>	mprowin(p);	// mapped rowin - a version of rowin with indices sorted according to the current variable ordering
	deque<bool>	mpcolin(p);	// mapped colin - a version of colin with indices sorted according to the current variable ordering

	{ for (vind i=0;i<p;i++) { 
		mprowin[i] = newdata->varin[orgvar[i]];	 
		mpcolin[i] = colin[orgvar[i]]; 
	} }
	symatpivot(prtmmit,pivotval,*e,*(newdata->e),vp,t,reliable,tol);
	fmmind.reset();

	for (vind i=0;i<vp;fmmind++,i++)  
		if (mprowin[i])  {
			vectorpivot(prtmmit,*ivct[fmmind()],*newdata->ivct[i],*e,cv[orgvar[i]],vp,t,reliable,tol); 
			newdata->ivct[i]->switchtoowndata();
	} 
	if (dir == forward)  {
		prtmmit.reset(vp);
		for (vind j=vp;j<vp+t;prtmmit++,j++)   
			newdata->ivct[vp-1]->setvalue(j-vp,-(*ivct[fpivotind])[prtmmit()]/pivotval);  
		#ifdef COUNTING  
		fpcnt += t;
		#endif
		newdata->ivct[vp-1]->switchtoowndata();
	}
	fmmind.reset(vp+t);
	{ for (vind i=vp+t;i<p;fmmind++,i++)  
		if (mprowin[i])  {
			vectorpivot(prtmmit,*ivct[fmmind()],*newdata->ivct[i],*e,cv[orgvar[i]],vp,t,reliable,tol); 
			newdata->ivct[i]->switchtoowndata();
		} 
	}

	{ for (vind j=0;j<p;j++)
		if (j+1 > vp && j+1 <= vp+t && !mpcolin[j]) mpcolin[j] =  true;	
		else mpcolin[j] = false;
	}
	cmpts2sm1(prtmmit,fmmind,pdata,newdata->s2m1,orgvar,vp,&mprowin[0],&mpcolin[0],true);
}

void rvdata::cmpts2sm1(lagindex<d>&,itindex<d>&,partialrvdata* pdata,twodarray& outmat,vind* orgvlst,vind vp,bool* rowlst,bool* collst,bool reorder) const
{
	vind fvarind=lastv-k,pivotind=vp-fvarind-1,ri,ci;
	real *tv=pdata->gettmpv(),*fl=pdata->getcndv();

	for (vind j=0;j<p;j++) if (collst[j] ) {
		tv[j] = 0.;
		for (vind a=0;a<p;a++) { 
			if ( !rowlst[a] || a+1 == vp ) continue;
			tv[j] += -(*ivct[a])[pivotind]*gdt->gets2(orgvlst[a],orgvlst[j]);  
			#ifdef COUNTING  
			fpcnt++;
			#endif
		}
	} 

	if (rowlst[vp-1]) {
		for (vind i=0;i<p;i++)  {  
			if ( !rowlst[i] || i+1 == vp ) continue;
			for (vind j=0;j<p;j++)  if (collst[j] ) {
				if (reorder) { ri=i; ci=j; }	
				else { ri=orgvlst[i]; ci=orgvlst[j]; }	
				outmat[ri][ci] = s2m1[i][j] +  fl[orgvlst[i]] * ( gdt->gets2(orgvlst[vp-1],orgvlst[j]) - tv[j] );
				#ifdef COUNTING  
				fpcnt++;
				#endif
			}
		} 
		for (vind j=0;j<p;j++)  if (collst[j] ) {
			if (reorder) { ri=vp-1; ci=j; }	
			else { ri=orgvlst[vp-1]; ci=orgvlst[j]; }
			outmat[ri][ci] = fl[orgvlst[vp-1]] * ( gdt->gets2(orgvlst[vp-1],orgvlst[j]) - tv[j] );
			#ifdef COUNTING  
			fpcnt++;
			#endif
		} 
	}
	else {
		for (vind i=0;i<p;i++)  {  
			if (!rowlst[i] ) continue;
			for (vind j=0;j<p;j++)  if (collst[j] ) {
				if (reorder) { ri=i; ci=j; }
				else { ri=orgvlst[i]; ci=orgvlst[j]; }
				outmat[ri][ci] = s2m1[i][j] +
					  (*ivct[i])[pivotind] * gdt->gets2(orgvlst[j],orgvlst[vp-1]) - fl[orgvlst[i]]*tv[j];
				#ifdef COUNTING  
				fpcnt += 2;
				#endif
			}
		}
	}
}

void rvdata::cmpts2sm1(lagindex<i>& prtmmit,itindex<i>& fmmind,partialrvdata* pdata,twodarray& outmat,vind* orgvlst,vind vp,bool* rowlst,bool* collst,bool reorder) const
{
	real *tv=pdata->gettmpv(),*fl=pdata->getcndv();
	vind inrowi,ri,ci;
	vind pivotind=prtmmit[vp-1];
	itindex<i> rowind(fmmind);	
	itindex<i> colind(fmmind);	

	for (vind j=0;j<p;j++) if (collst[j] ) {
		tv[j] = 0.;
		rowind.reset();
		for (vind a=0;a<p;rowind++,a++) { 
			if ( !rowlst[a] || a+1 == vp ) continue;
			tv[j] += -(*ivct[rowind()])[pivotind]*gdt->gets2(orgvlst[a],orgvlst[j]);  
			#ifdef COUNTING  
			fpcnt++;
			#endif
		}
	} 

	if (rowlst[vp-1]) {
		rowind.reset();
		for (vind i=0;i<p;rowind++,i++)  {  
			if ( !rowlst[i] || i+1 == vp ) continue;
			inrowi = rowind();
			colind.reset();
			for (vind j=0;j<p;colind++,j++)  if (collst[j] ) {
				if (reorder) { ri=i; ci=j; }
				else { ri=orgvlst[i]; ci=orgvlst[j]; }
				outmat[ri][ci] = s2m1[inrowi][colind()] +  fl[orgvlst[i]] * ( gdt->gets2(orgvlst[vp-1],orgvlst[j]) - tv[j] );
				#ifdef COUNTING  
				fpcnt++;
				#endif
			}
		} 
		for (vind j=0;j<p;j++)  if (collst[j] ) {
			if (reorder) { ri=vp-1; ci=j; }	
			else { ri=orgvlst[vp-1]; ci=orgvlst[j]; }
			outmat[ri][ci] = fl[orgvlst[vp-1]] * ( gdt->gets2(orgvlst[vp-1],orgvlst[j]) - tv[j] );

			#ifdef COUNTING  
			fpcnt++;
			#endif
		} 
	}

	else {
		rowind.reset();	
		for (vind i=0;i<p;rowind++,i++)  {  
			if (!rowlst[i] ) continue;
			inrowi = rowind();
			colind.reset();
			for (vind j=0;j<p;colind++,j++)  if (collst[j] ) {
				if (reorder) { ri=i; ci=j; }
				else { ri=orgvlst[i]; ci=orgvlst[j]; }
				outmat[ri][ci] = s2m1[inrowi][colind()] + 
					  (*ivct[inrowi])[pivotind] * gdt->gets2(orgvlst[j],orgvlst[vp-1]) - fl[orgvlst[i]]*tv[j];
				#ifdef COUNTING  
				fpcnt += 2;
				#endif
			}
		}
	}
}

real rvdata::frobenius(twodarray& m,bool *inlst) const
{
	real tmp = 0.;

	for (vind i=0;i<p;i++)  if (inlst[i]) {
		tmp += pow(m[i][i],2);   
		#ifdef COUNTING  
		fpcnt++;
		#endif
		for (vind j=0;j<i;j++)  if (inlst[j])  { 
			tmp += 2*m[i][j]*m[j][i];  
			#ifdef COUNTING  
			fpcnt++;
			#endif
		}
	}
	return tmp;
}

}

