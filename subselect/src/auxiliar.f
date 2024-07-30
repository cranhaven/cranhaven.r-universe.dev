* SUBPROGRApsMS:
* Included in the R/S package "subselect", available on CRAN.
********************************************************************
       subroutine newinicializar(criterio,p,s,svector,sq,nfora,
     +  fora,ndentro,dentro,fica,tracos,tracosq,vecp,poriginal,vecvecp,
     +   h,hvector,rh)
**********************************************************************
* performs some initial chores: converts svector into a matrix s; computes
* the square of that matrix, sq, and the traces of both s (for RM) and sq 
* (for RV); converts the eigenvectors of s (for GCD), calculated within R 
* and passed on as vector vecvecp, into a matrix. All this is done taking 
* into account the variables that are to be 
* forcibly excluded (vector fora) or included (vector dentro).
*
*
* INPUT:
*
* criterio - integer variable indicating the criterion for the quality of 
*            the k-subset.
*    p     - integer variable, giving the number of original variables.
* svector  - double precision vector, giving the full covariance (or 
*            correlation) matrix of the p variables (given as a vector
*            for convenience in passing from R to Fortran). 
*  nfora   - integer variable indicating the number of original variables 
*            that are to be forcefully excluded from the k-subset.
*   fora   - integer vector, indicating the variable numbers associated
*            with the nfora variables that are to be excluded by force.
* ndentro  - integer variable indicating the number of original variables 
*            that are to be forcefully included in the k-subset.
*  dentro  - integer vector, indicating the variable numbers associated
*            with the ndentro variables that are to be included by force.
* vecvecp  - double precision vector, giving the eigenvectors of the 
*            covariance (or correlation) matrix of the p variables (given 
*            as a vector for convenience in passing from R to Fortran).
* hvector  - double precision vector, giving the full effect descrption 
*            matrix (H) of the p variables (given as a vector for
*            convenience in passing from R to Fortran). 
*   rh     - H matrix rank (expected)
*
* OUTPUT:
*
*   p      - integer variable indicating the number of admissible variables,
*            i.e., the original number of variables p minus nfora.
*   s      - double precision array (pxp) giving the covariance (or 
*            correlation) matrix of the admissible variables.
*   sq     - double precision array (pxp) giving the square of matrix s.
*  fica    - integer vector giving the original variable numbers of the 
*            admissible variables.
* tracos   - double precision variable giving the trace of matrix s.
* tracosq  - double precision variable giving the trace of matrix sq.
*  vecp    - double precision array (pxp) giving the eigenvectors of s (in
*            the same column-order as the eigenvalues).
* poriginal - integer variable, indicating the original number of variables
*             (poriginal = p + nfora)
*   h       - double precision array (pxp) giving the full effect descrption 
*            matrix (H)  
*

* general declarations
      integer criterio,i,iaux,j,nfora,ndentro,nfica
      integer p,poriginal
      integer fora(0:nfora),fica(0:p),dentro(0:ndentro)
      double precision s(p,*),sq(p,*),svector(*)
      double precision vecvecp(*)
* declaration only for the RM criterion
      double precision tracos
* declarations only for the RV criterion
      double precision tracosq
* declarations only for the GCD criterion
      double precision vecp(p,*)
* declarations only for the tau2,xi2,zeta2 and ccr12 criteria
      integer rh
      double precision hvector(*),h(p,*)

      external dprodmat

* conversion of svector into a matrix (s); if GCD, initialization of the vecp
* matrix which will hold the eigenvectors of s (computed in R and passed 
* in as vector vecvecp)
       do j=1,p
         DO i= 1,p
           s(i,j) = svector(i+(j-1)*p)
           if (criterio.eq.3) then 
              vecp(i,j) = vecvecp(i+(j-1)*p)
           end if 
         END DO
      end do

       If (rh .GT. 0) then
* conversion of hvector into a matrix (h) 
       do j=1,p
         DO i= 1,p
           h(i,j) = hvector(i+(j-1)*p)
          END DO
        end do
        end if


* Defines matrix sq := s x s. Computes the traces of s (for RM) and 
* sq (for RV), and the eigendecomposition of s (for GCD)


      call dprodmat(p,p,s,p,s,sq)


      if (criterio .eq. 1) then 
         tracos=0.0D0
         DO j=1,p
           tracos=tracos+s(j,j)
         END DO
      end if
      if (criterio .eq. 2) then 
         tracosq=0.0D0
         DO j=1,p
           tracosq=tracosq+sq(j,j)
         END DO
      end if


* Re-defines the matrices s, sq and h (if r>0), excluding the rows/columns that the 
* user has requested be excluded from the final solution. 
* p is redefined as p-nfora, where nfora is the number of variables that 
* are to be excluded. The original variable-number for each of the surviving
* variables is preserved in vector fica(j), j=1,p-nfora.
* Warning: the original matrices s and sq are destroyed.

      poriginal=p
      p=p-nfora
      fora(0)=0
      nfica=0
      fica(0)=0
      do j=0,nfora-1
         do i=fora(j)+1,fora(j+1)-1
            nfica=nfica+1
            fica(nfica)=i
         end do
      end do
      iaux=fora(nfora)
      do j=iaux+1,poriginal
         nfica=nfica+1
         fica(nfica)=j
      end do

      if(nfora .GT. 0) then
         do i=1,p-1
            do j=i+1,p
              s(i,j)=s(fica(i),fica(j))
              s(j,i)=s(i,j)
              sq(i,j)=sq(fica(i),fica(j))
              sq(j,i)=sq(i,j)
              if(rh .GT. 0) then
		    h(i,j)=h(fica(i),fica(j))
                    h(j,i)=h(i,j)
	      end if
            end do
            s(i,i)=s(fica(i),fica(i))
            sq(i,i)=sq(fica(i),fica(i))
		    if(rh .GT. 0) then
		       h(i,i)=h(fica(i),fica(i))
                    end if
         end do
         s(p,p)=s(fica(p),fica(p))
         sq(p,p)=sq(fica(p),fica(p))
         if(rh .GT. 0) then
		    h(p,p)=h(fica(p),fica(p))
          end if 
      end if

* The vector of variables that are forced to be included in the solution 
* is redefined, taking into account that nfora variables (may) have been 
* excluded.

      if((ndentro.GT.0) .and. (nfora.GT.0)) then
         i=1
         j=1
         do while(j.LE.ndentro)
           if(dentro(j).EQ.fica(i)) then
             dentro(j)=i
             j=j+1
           end if
           i=i+1
         end do
      end if
      return
      end
**********************************************************************

*********************************************************************
      subroutine randsk1(n,k,sk,pp)
**********************************************************************
* generates a subset of k random integers from the set {1,2,...,n}. 
* Warning: uses function randint.

*  INPUT: 
*    iseed
*    n   - integer variable, largest integer in the set.
*    k   - integer variable, giving the number of random integers that
*                are to be generated
*    pp  - integer vector of n elements, used to store temporary results
* 
*  OUTPUT: 
*   sk   - logical vector defining the generated subset 
*               (sk(i)=.true. iff i belongs to sk, i=1,...,n)

       integer i,k,n,nalt,randint
       integer pp(*)
       logical sk(*)

       do i=1,n
        sk(i)=.false.
        pp(i)=i
       end do
       do i=1,k
        nalt=randint(i,n)
        sk(pp(nalt))=.true.
        pp(nalt)=pp(i)
       end do
       return 
      end
***********************************************************************

************************************************************************
      integer function randint(esq,dto)
**********************************************************************
* randomly generates a number in the interval [esq,dir].
*
* WARNING: Uses a call to R's random number generator (as described
*  in pages 48 and 50-51 of the "Writing R Extensions" manual, version
*  1.5).
*
*INPUT: 
       
*        esq  - integer variable, giving lower bound of interval.
*        dto  - integer variable, giving upper bound of interval.

       external unifrnd, rndstart, rndend
       
       double precision ran, unifrnd
       integer esq,dto

       call rndstart()
       ran = unifrnd()
       randint=esq+int(ran*(dto-esq+1))
       call rndend()
       return 
      end
*********************************************************************

*********************************************************************
      subroutine dprodmat(n,m,a,r,b,prod)
**********************************************************************
* calculates the matrix product of two matrices
*
* INPUT : n - integer, number of rows of matrix a
*         m - integer, number of columns of matrix a (= number of rows
*                matrix b)
* 
*         a - double precision array (nxm)
*         r - integer, number of columns of matrix b
*         b - double precision array (mxr)
* OUTPUT: 
*   prod - double precision array (nxr), the product a x b.

       INTEGER i,j,l,m,r
       DOUBLE PRECISION a(n,*),b(m,*),prod(n,*),soma

       DO i=1,n
         DO j=1,r
           soma=0.0D0 
        	DO l=1,m
	          soma=soma+a(i,l)*b(l,j)
	        END DO
	        prod(i,j)=soma
         END DO
       END DO
       RETURN
      END
**********************************************************************

**********************************************************************
       subroutine dcorrigesk(ndentro,dentro,p,setk,poriginal)
**********************************************************************
* subroutine randsk1, randomly selects (with uniform 
* distribution) a subset (setk) with cardinality k-ndentro  of the set 
* {1,2,...,p-ndentro}. This routine takes that subset and merges it with 
* the ndentro variables indicated in vector dentro to produce a subset of
* cardinality k=|setk|+ndentro, whose variable numbers are in accordance with 
* the original variable numbers. The vector setk is re-defined accordingly.
* Thus, the two subroutines randsk1and dcorrigesk randomly select a subset
* of cardinality k from the set {1,2,...,p}, ensuring that the ndentro 
* variables specified by dentro are included.
*
* INPUT: 
*
* ndentro - integer variable giving the size of vector dentro.
* dentro  - integer vector giving the variable numbers of variables that are
*           to be forcefully included in the k-subset.
*   p     - number of elements in the full set (excluding 'exclude' variables).
*  setk   - logical vector with setk(j)=.true. iff j belongs to the  
*           subset of {1,2,...,p-ndentro}
* poriginal - number of variables originally (equal to 'p', except when
*              the 'exclude' option has been used
* 
* OUTPUT: 
* 
*  setk   - logical vector with setk(j)=.true. iff j belongs to the  
*           subset of {1,2,...,p}

       integer i,j,naux,ndentro,p,poriginal
       integer dentro(0:ndentro)
       logical setk(*)

       dentro(0)=0
       naux=ndentro
       i=p-ndentro
       j=p
       do while(j.GE.1)
         if(dentro(naux).EQ.j) then
           setk(j)=.true.
	   naux=naux-1
	 else
	   setk(j)=setk(i)
	   i=i-1
         end if
         j=j-1
       end do
       return
      end
**********************************************************************

************************************************************************
	subroutine dannealing(criterio,p,kmax,setk,vactual,ndentro,
     +                dentro,niter,beta,temp,coolfreq,k,s,tracos,sq,
     +                tracosq,nqsi,qsi,valp,vecp,fica,poriginal,
     +                h,rh,skinput,hkinput,ekinput,egval,work,workmat,
     +                randsk1pp,setint,checksg,numtol,numprb)
**********************************************************************

* 
* Applies simulated annealing to a subset setk of the original variables. 
*
*
* INPUT : 
*
*criterio - integer variable indicating the criterion of subset quality
*            requested.
*   p     - integer variable giving the number of original variables (minus
*               ndentro).
* kmax    - integer variable, giving the largest cardinality
*   k     - integer variable, cardinality of subset.
*   s     - double precision matrix of covariances of all p variables.
*   sq    - double precision matrix, the square of s. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*ndentro  - integer variable, giving the number of variables that are to
*          be forcefully included in the output subset.
* dentro  - integer vector, indicating the numbers associated with the 
*          ndentro variables that are to be forced into the subsets.
* niter   - integer variable, number of simulated annealing iterations 
*           requested for each solution.
*  beta   - double precision variable,(between 0 and 1) indicating the
*           geometric cooling factor for simulated annealing.
* temp   - double precision variable, indicating the initial temperature
*           to e used in the simulated annealing algorithm.
* coolfreq - integer indicating the cooling frequency, i.e., the number 
*           of iterations that are to be carried out for each fixed
*           temperature 
* tracos  - double precision variable, the trace of the covariance (or
*           correlation) matrix s. (For RM criterion only).
* tracosq - double precision variable, the trace of the matrix sq. (For RV
*           criterion only).
*  nqsi   - integer variable, indicating the dimension of the Principal 
*           Subspace with which the subspace spanned by the k-subset is
*            to be compared. (For GCD criterion only).
*  qsi    - integer vector, giving the ranks of the Principal Components 
*           which span the Principal Subspace described above (see nqsi).
*           (For GCD criterion only).
*  valp   - double precision vector giving the eigenvalues of s, in 
*            decreasing order. (For GCD criterion only).
*  vecp   - double precision array (pxp) giving the eigenvectors of s, in
*            the same column-order as the values of valp. (For GCD only).
*  fica   - integer vector giving the original variable numbers of the 
*            admissible variables.
* poriginal - 
*
*   h     - double precision matrix giving the full effect descrption 
*            matrix (H)  
*
*   rh     - integer variable giving the expected rank of H matrix.
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal 
*           elements of E Choleski decomposition

*
* OUTPUT: 
*
* setk    - logical vector indicating the best subset produced by simulated 
*           annealing after the niter iterations. setk(j)=.true. iff variable 
*           j belongs to the subset. 
*vactual  - double precision variable giving the best value of the criterion 
*           produced throughout the niter iterations.
* numprb -  logical flag set to true if numerical problems precluded some
*           (ill-conditioned) subsets to be evaluated, and to false otherwise.   

*  WORKING ARRAYS:
*
*  skinput   - double precision matrix (kmax*kmax) used to store the submatrices S_k. 
*  hkinput   - double precision matrix (kmax*kmax) used to store the submatrices H_k. 
*  ekinput   - double precision matrix (kmax*kmax) used to store the submatrices E_k. 
*  egval     - double precision array (kmax), used to store eigenvalues.
*  work      - double precision array (6*kmax), to be used internaly by LAPACK routines.
*  workmat   - double precision matrix (kmax*kmax) used to store temporary results.
*  randsk1pp - integer array(p) to be used internaly by the randsk1 routine.
*  setint    - integer array(p) used to store the indices of the variables kept in each subset.
        
* general declarations
        integer coolfreq,criterio,i,inaoconv,iter,j,jentra,jsai,k,kcons
        integer kque,ncons,ndentro,nigual,niter,nque,p,poriginal,kmax
        integer dentro(0:ndentro),que(p),cons(p),randint
	logical checksg,numprb
        logical setk(*),setkmax(poriginal),auxlog(p)
        double precision vactual,vmax,vtroca,dir,numtol,beta,citer,temp
        double precision unifrnd,r
        double precision s(poriginal,*)
* declarations specific to the RM and RV criteria
        double precision sq(poriginal,*),tracos,tracosq,dobjrm,dobjrv
* declarations specific to the GCD criterion
        integer qsi(*),fica(0:p),nqsi
        double precision valp(*)
        double precision vecp(poriginal,*),dobjgcd
* declarations specific to the tau2,xi2,zeta2 and ccr12 criteria
        integer rh
        double precision h(poriginal,*)
        double precision dobjtau2,dobjxi2,dobjzeta2,dobjccr12
* declarations of local arrays and matrices
       INTEGER randsk1pp(*),setint(*)
       DOUBLE PRECISION skinput(kmax,*),hkinput(kmax,*)
       DOUBLE PRECISION ekinput(kmax,*),workmat(kmax,*)
       DOUBLE PRECISION work(*),egval(*)

        external unifrnd, rndstart, rndend

* initializing 
        citer = temp
        vtroca = -1.0D0
        dir = 0.0D0
        numprb=.false.

        do j=1,p
         auxlog(j)=.true.
        end do
        do j=1,ndentro
         auxlog(dentro(j))=.false.
        end do

        ncons=0
        nque=0
        do j=1,p
           if(.not.setk(j)) then
              nque=nque+1
	      que(nque)=j
	   else
	      if(auxlog(j)) then
	        ncons=ncons+1
	        cons(ncons)=j
	      end if
           end if
        end do

        vmax=vactual
        do j=1,p
          setkmax(j)=setk(j)
        end do

* more initializing
        iter=0      
        nigual=0
        inaoconv=0
        do while(iter.LT.niter)
         
 
* generates a (uniform) random integer, kque, in {1,2,...,nque}.
* jentra, the variable which is being considered for inclusion in the subset
* setk, is the element in position kque of the vector que 

         kque=randint(1,nque)
         jentra=que(kque)
  
* generates a (uniform) random integer, kcons, in {1,2,...,kcons}.
* jsai, the variable which is being considered for exclusion from the subset
* setk, is the element in position kcons of the vector cons, i.e., an 
* element of setk not belonging to vector dentro

         kcons=randint(1,ncons)
         jsai=cons(kcons)

* calculates the value of the criterion for solution setk\{jsai}U{jentra}

         setk(jsai)=.false.
         setk(jentra)=.true.
         if (criterio.eq.1) then
	    vtroca=dobjrm(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
         end if
         if (criterio.eq.2) then
           vtroca=dobjrv(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
         end if
         if (criterio.eq.3) then
            vtroca=dobjgcd(k,setk,p,poriginal,kmax,s,workmat,
     +   nqsi,qsi,valp,setint,vecp,fica,skinput)
        end if
         if (criterio.eq.4) then
               vtroca=dobjtau2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
            end if
	   if (criterio.eq.5) then
               vtroca=dobjxi2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if
	   if (criterio.eq.6) then
               vtroca=dobjzeta2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
           end if		 
	  if (criterio.eq.7) then
* tirei rh
               vtroca=dobjccr12(k,setk,p,poriginal,kmax,s,h,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if

         if (checksg.and.criterio.GT.3.and.vtroca.eq.-0.9999D0) then
	   numprb=.true.        
         end if

* if vtroca>vmax, updates vmax and setkmax, which hold the best value and 
* subset found so far.

         if(vtroca.GT.vmax) then
          vmax=vtroca
          do j=1,p
	    setkmax(j)=setk(j)
	  end do
         end if

* The replacement is carried out if vtroca>=vactual or, if not, with 
* probability given by the simulated annealing algorithm, and computed
* by R's Random Number Generator

         call rndstart()
         r = unifrnd()
         call rndend()

         if(vtroca-vactual.GE.0) then 
          dir=0.1D5
         else
          if (criterio.eq.1) then
           dir=exp((dsqrt(vtroca)-dsqrt(vactual))/(dsqrt(tracos)*citer))
          end if
          if (criterio.eq.2) then
          dir=exp((dsqrt(vtroca)-dsqrt(vactual))/(dsqrt(tracosq)*citer))
          end if
          if (criterio.eq.3) then
          dir=exp(((vtroca/dsqrt(DBLE(nqsi*k)))-
     +         (vactual/dsqrt(DBLE(nqsi*k))))/citer)
          end if
	    if (criterio.eq.4) then
          dir=exp((vtroca-vactual)/citer)
          end if
		if (criterio.eq.5) then
          dir=exp((vtroca-vactual)/citer)
          end if
		if (criterio.eq.6) then
          dir=exp((vtroca-vactual)/citer)
          end if
		if (criterio.eq.7) then
          dir=exp((vtroca-vactual)/citer)
          end if
         end if
         if(vtroca.GE.vactual.or.(vtroca.LT.vactual.and.r.LT.dir)) then 
* swap

          vactual=vtroca
          que(kque)=jsai
          cons(kcons)=jentra

           nigual=0
        else 
* reject the swap
           setk(jsai)=.true.
           setk(jentra)=.false.

          nigual=nigual+1 
* counts number of consecutive rejections of a swap
        end if

* Hacking the temperature:
* updates the temperature citer every coolfreq iterations

        if(mod(iter,coolfreq).EQ.0) citer=citer*(1-beta)


* when there are more than p consecutive rejections of a swap and the
* first 20p iterations have not yet been completed, the
* simulated annealing temperature citer will be increased: by a factor 
* of 2 if less than 5p iterations have gone by, a factor of 1.5 if the
* number of iterations is between 5p and 10p, by a factor of 1.1 if
* the current number of iterations is between 10p and 20p. 

       if(nigual.GE.p) then
         if(iter.LE.5*p) then
              citer=citer*2.0D0
	 else
	      if(iter.LE.10*p) then
	        citer=citer*1.5D0
	      else
	        if(iter.LE.20*p) then
	            citer=citer*1.1D0
	        else
		    inaoconv=inaoconv+1
	        end if
	       end if
          end if
          nigual=0
        end if

* inaoconv counts the number of times in which the currently best solution 
* has remained unchanged after the first 20p iterations.

* inaoconv counts the number of consecutive iterations without changes in
* the current solution AFTER THE FIRST 20p ITERATIONS. If this becomes too
* large (the value chosen here was 500), the algorithm stops (a further change
* is unlikely, given the low temperature after so many iterations).

         if (inaoconv.GT.500) iter=niter
         iter=iter+1
      end do
 
* defines the algorithm's solution, vactual, as the best solution obtained
* at any stage of the niter iterations. 

       vactual=vmax
       do i=1,p
          setk(i)=setkmax(i)
       end do

       return
      end
**********************************************************************

**********************************************************************
	subroutine dmelhoramentogen(criterio,p,kmax,setk,vactual,ndentro,
     +                dentro,k,s,sq,nqsi,qsi,valp,vecp,fica,poriginal,
     +                h,rh,skinput,hkinput,ekinput,egval,work,workmat,
     +                randsk1pp,setint,checksg,numtol,numprb)

**********************************************************************
* This subroutine (which is only called by the "improve" subroutine, 
* but also by the other two main routines - "genetic" and "anneal" -  if 
* the logical variable "improvement" is set to .true.) seeks to improve an 
* initial k-subset by a modified local search algorithm, the details of 
* which are as follows. The variables not belonging to this initial subset 
* are placed in a queue ("que"). This subroutine explores the possibility 
* of replacing a variable in the subset with a variable from the queue. 
* More precisely, a variable j is selected and removed from the queue. Each 
* variable i in the subset is, in turn, replaced by variable j and the 
* resulting values of the criterion are computed. If the best of these k 
* new criterion values exceeds the subset's original criterion value, the 
* current solution is updated accordingly. In this case, the variable which
* leaves the subset is added to the queue, but only if it has not previously 
* been in the queue (i.e., no variable can enter the queue twice). The 
* algorithm proceeds until the queue is emptied. 
*
* INPUT: 
*criterio - integer variable indicating the criterion of subset quality
*            requested.
*   p     - integer variable, number of original variables. 
*  kmax   - integer variable, giving the largest cardinality
*           of the subsets of variables that are wanted.
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on exit. 
*vactual  - double precision variable giving the best value of the criterion 
*           for the initial solution. Changed on exit.
*ndentro  - integer variable, giving the number of variables that are to
*          be forcefully included in the output subset.
* dentro  - integer vector, indicating the numbers associated with the 
*          ndentro variables that are to be forced into the subsets.
*   k     - integer, cardinality of subset setk.
*   s     - double precision matrix of covariances of all p variables.
*   sq    - double precision matrix, the square of s.
*  nqsi   - integer variable, indicating the dimension of the Principal 
*           Subspace with which the subspace spanned by the k-subset is
*            to be compared. (For GCD criterion only).
*  qsi    - integer vector, giving the ranks of the Principal Components 
*           which span the Principal Subspace described above (see nqsi).
*           (For GCD criterion only).
*  valp   - double precision vector giving the eigenvalues of s, in 
*            decreasing order. (For GCD criterion only).
*  vecp   - double precision array (pxp) giving the eigenvectors of s, in
*            the same column-order as the values of valp. (For GCD only).
*  fica   - integer vector giving the original variable numbers of the 
*            admissible variables.
* poriginal - 
*
*   h     - double precision matrix giving the full effect descrption 
*            matrix (H)  
*
*   rh     - integer variable giving the expected rank of H matrix.
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements of E Choleski decomposition.
*
*
* OUTPUT: 
*
* setk    - logical vector indicating the best subset after the modified local
*           search, with setk(j)=.true. iff variable j belongs to the subset. 
*vactual  - double precision variable giving the best value of the criterion 
*           after the modified local search.
* numprb -  logical flag set to true if numerical problems precluded some
*           (ill-conditioned) subsets to be evaluated, and to false otherwise.   

*  WORKING ARRAYS:
*
*  skinput   - double precision matrix (kmax*kmax) used to store the submatrices S_k. 
*  hkinput   - double precision matrix (kmax*kmax) used to store the submatrices H_k. 
*  ekinput   - double precision matrix (kmax*kmax) used to store the submatrices E_k. 
*  egval     - double precision array (kmax), used to store eigenvalues.
*  work      - double precision array (6*kmax), to be used internaly by LAPACK routines.
*  workmat   - double precision matrix (kmax*kmax) used to store temporary results.
*  randsk1pp - integer array(p) to be used internaly by the randsk1 routine.
*  setint    - integer array(p) used to store the indices of the variables kept in each subset.


* general declarations
      integer criterio,k,kmax,j,jconsmax,jentra,jmax,jsai
      integer ncons,ndentro,nque,p,poriginal
      integer dentro(0:ndentro),que(p),cons(p)
      logical setk(*),auxlog(p),esteveque(p)
      logical checksg,numprb
      double precision s(poriginal,*)
      double precision vactual,vtroca,vtrocamax,numtol
* declarations specific to the RM and RV criteria
      double precision sq(poriginal,*),dobjrm,dobjrv
* declarations specific to the gcd criteria
        integer qsi(*),fica(0:p),nqsi
        double precision valp(*),vecp(poriginal,*),dobjgcd
* declarations specific to the tau2,xi2,zeta2 and ccr12 criteria
        integer rh
        double precision h(poriginal,*)
        double precision dobjtau2,dobjxi2,dobjzeta2,dobjccr12
* declarations of local arrays and matrices
       INTEGER randsk1pp(*),setint(*)
       DOUBLE PRECISION skinput(kmax,*),hkinput(kmax,*)
       DOUBLE PRECISION ekinput(kmax,*),workmat(kmax,*)
       DOUBLE PRECISION work(*),egval(*)

* initializations

      vtroca = -1.0D0
      jmax = 0 
      jconsmax = 0

      do j=1,p
        auxlog(j) = .true.
      end do
      do j=1,ndentro
        auxlog(dentro(j))=.false.
      end do

      ncons=0
      nque=0
      DO j=1,p
         IF (.not.setk(j)) THEN
           nque=nque+1
	   que(nque)=j
	   esteveque(j)=.true.
         ELSE
	   esteveque(j)=.false.
	   IF (auxlog(j)) then
             ncons=ncons+1
             cons(ncons)=j
	   END IF
         END IF
      END DO

* emptying the queue "que"
       DO WHILE(nque.GT.0 .and. ncons.GT.0)
       jentra=que(nque)
       nque=nque-1
       vtrocamax=0.0D0
       DO j=1,ncons
        jsai=cons(j)
	setk(jsai)=.false.
	setk(jentra)=.true.
        if (criterio.eq.1) then
		vtroca=dobjrm(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
        end if
        if (criterio.eq.2) then
           	vtroca=dobjrv(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
        end if
        if (criterio.eq.3) then
		 vtroca=dobjgcd(k,setk,p,poriginal,kmax,s,workmat,
     +   nqsi,qsi,valp,setint,vecp,fica,skinput)
        end if
        if (criterio.eq.4) then
               vtroca=dobjtau2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
           end if
	   if (criterio.eq.5) then
               vtroca=dobjxi2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if
	   if (criterio.eq.6) then
               vtroca=dobjzeta2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
           end if		 
	  if (criterio.eq.7) then
* tirei rh
               vtroca=dobjccr12(k,setk,p,poriginal,kmax,s,h,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if		 

        if (checksg.and.criterio.GT.3.and.vtroca.eq.-0.9999D0) then
	   	numprb=.true.        
        end if
	
	IF (vtroca.GT.vtrocamax) THEN
           vtrocamax=vtroca
           jmax=jsai
           jconsmax=j
	END IF
	setk(jsai)=.true.
	setk(jentra)=.false.
       END DO
       IF (vtrocamax.GT.vactual) THEN
        vactual=vtrocamax
        setk(jmax)=.false.
	setk(jentra)=.true.
	cons(jconsmax)=jentra
        IF (.not.esteveque(jmax)) THEN
           nque=nque+1
           que(nque)=que(1)
           que(1)=jmax
           esteveque(jmax)=.true.
	END IF
       END IF
       END DO
       RETURN
       END
**********************************************************************

**********************************************************************
      function dobjrm(k,setk,p,poriginal,kmax,s,sq,ski,setint,skinput)
**********************************************************************
* This function computes the "variable part" of the RM criterion. By "variable
* part" is meant that which changes from one given k-subset to the other 
* (the final value of RM is only computed in the main subroutine, for 
* the selected solutions, in order to spare some computations in this
* subroutine, which is called extensively). 
*
* WARNING : This function calls the LAPACK routine DPOSV.
*
*  OUTPUT:
*
* The trace of the matrix inv(S_k) x (S**2)_k, where S_k 
* is the submatrix of the covariance (or correlation) matrix for the
* variables in subset setk, (S**2)_k is its equivalent for the square of
* that covariance/correlation matrix, and "inv" stands for matrix inverse.
*
*  INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision matrix of covariances of all p variables.
*   sq    - double precision matrix, the square of s.
* poriginal - 
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices ski and skinput
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix S_k. 
*  ski       - double precision matrix used to store inverse of the submatrix S_k. 

       integer i,info,j,k,kmax,p,poriginal
       integer setint(*)
       logical setk(*)
       character*1 laux
       double precision s(poriginal,*),sq(poriginal,*)
       double precision ski(kmax,*),skinput(kmax,*),dobjrm

       external dposv

* initializations and call to LAPACK routine DPOSV
* matrix skinput will store the submatrix S_k. Matrix s will initially
* be identity, fed to the LAPACK routine DPOSV, from which it will emerge
* as the inverse of S_k. 

       do i=1,p
          setint(i)=i
       end do

       i=0
       do j=1,p
         if(setk(j)) then
           i=i+1
           setint(i)=j
         end if
       end do

       do i=1,k-1
           do j=i+1,k
              skinput(i,j)=s(setint(i),setint(j))
              skinput(j,i)=skinput(i,j)
              ski(i,j)=0.0D0
              ski(j,i)=0.0D0
           end do
           skinput(i,i)=s(setint(i),setint(i))
           ski(i,i)=1.0D0
         end do

          skinput(k,k)=s(setint(k),setint(k))
          ski(k,k)=1.0D0

          laux='L'
          info=0
          call dposv(laux,k,k,skinput,kmax,ski,kmax,info)

* computes the trace of inv(S_k) x (S**2)_k

          dobjrm=0.0D0
	  do i=1,k
	    do l=1,k
		  dobjrm=dobjrm+ski(i,l)*sq(setint(l),setint(i))
            end do
	  end do
	  return
	end   
**********************************************************************

**********************************************************************
      function dobjrv(k,setk,p,poriginal,kmax,s,sq,ski,setint,skinput)
**********************************************************************
* This function computes the "variable part" of the RV criterion. By "variable
* part" is meant that which changes from one given k-subset to the other 
* (the final value of RV is only computed in the main subroutine, for 
* the selected solutions, in order to spare some computations in this
* subroutine, which is called extensively). 
*
* WARNING : This function calls the LAPACK routine DPOSV.
* 
* OUTPUT:
* The trace of the matrix (inv(S_k) x (S**2)_k)**2, where S_k 
* is the submatrix of the covariance (or correlation) matrix for the
* variables in subset setk, (S**2)_k is its equivalent for the square of
* that covariance/correlation matrix, and "inv" stands for matrix inverse.
*
*  INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision matrix of covariances of all p variables.
*   sq    - double precision matrix, the square of s.
* poriginal -
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices ski and skinput
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix S_k. 
*  ski       - double precision matrix used to store inverse of the submatrix S_k. 

       integer i,info,j,k,kmax,p,poriginal
       integer setint(*)
       logical setk(*)
       character*1 laux
       double precision s(poriginal,*),sq(poriginal,*)
       double precision ski(kmax,*),skinput(kmax,*)
       double precision soma1,soma2,soma,dobjrv

      external dposv

* initializations and call to LAPACK routine DPOSV
* matrix skinput will store the submatrix S_k. Matrix s will initially
* be identity, fed to the LAPACK routine DPOSV, from which it will emerge
* as the inverse of S_k. 
	  
       do i=1,p
          setint(i)=i
       end do


	  i=0
	  do j=1,p
	    if(setk(j)) then
		  i=i+1
       	          setint(i)=j
	    end if
	  end do

	  do i=1,k-1
	    do j=i+1,k
                  skinput(i,j)=s(setint(i),setint(j))
		  skinput(j,i)=skinput(i,j)
	          ski(i,j)=0.0D0
                  ski(j,i)=0.0D0
            end do
            skinput(i,i)=s(setint(i),setint(i))
            ski(i,i)=1.0D0
	  end do
	  skinput(k,k)=s(setint(k),setint(k))
          ski(k,k)=1.0D0

           laux='L'
           info=0
          call dposv(laux,k,k,skinput,kmax,ski,kmax,info)

* computes the trace of (inv(S_k) x (S**2)_k)**2

          dobjrv=0.0D0
          do i=1,k-1
            do j=i+1,k
              soma1=0.0D0
              soma2=0.0D0
	      do l=1,k
	        soma1=soma1+ski(i,l)*sq(setint(l),setint(j))
	        soma2=soma2+ski(j,l)*sq(setint(l),setint(i))
	      end do
	      dobjrv=dobjrv+soma1*soma2
            end do
          end do
          dobjrv=dobjrv*2
          soma=0.0D0
          do i=1,k
              soma=0.0D0
              do l=1,k
	 soma=soma+ski(i,l)*sq(setint(l),setint(i))
              end do
              dobjrv=dobjrv+soma**2.0D0
           end do
       return
       end   
********************************************************************


********************************************************************
      function dobjgcd(k,setk,p,poriginal,kmax,s,ski,nqsi,qsi,valp,
     +   setint,vecp,fica,skinput)
**********************************************************************
* This function computes the "variable part" of the GCD criterion. By "variable
* part" is meant that which changes from one given k-subset to the other 
* (the final value of GCD is only computed in the main subroutine, for 
* the selected solutions, in order to spare some computations in this
* subroutine, which is called extensively).  Specifically, this subroutine
* calculates the trace of the matrix (Sqsi_k) x inv(S_k), where S_k 
* is the submatrix of the covariance (or correlation) matrix for the
* variables in subset setk, "inv" stands for matrix inverse, and Sqsi_k
* 
* WARNING : This function calls the LAPACK routine DPOSV.
*
*
*  OUTPUT:
*    sum_{m in qsi} (valp_m vecp_m^t inv(S_k) vecp_m), where S_k is
*    the submatrix of the covariance/correlation matrix relevant for the 
*    k variables in subset setk, "inv" stands for matrix inverse, valp_m
*    stands for the eigenvalue of the full covariance/correlation 
*    matrix S, associated with the m-th rank (as defined in the set qsi), and
*    vecp_m is the corresponding SUB-eigenvector, defined as the subvector
*    of the eigenvector associated with valp_m, whose elements correspond to
*    positions of the k variables in setk.
*
*  INPUT: 
*
*  nqsi   - integer variable, indicating the dimension of the Principal 
*           Subspace with which the subspace spanned by the k-subset is
*            to be compared.
*  qsi    - integer vector, giving the ranks of the Principal Components 
*           which span the Principal Subspace described above (see nqsi).
*  valp   - double precision vector giving the eigenvalues of s, in 
*            decreasing order.
*  vecp   - double precision array (pxp) giving the eigenvectors of s, in
*            the same column-order as the values of valp.
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision matrix of covariances of all p variables. 
*  fica   - integer vector giving the original variable numbers of the 
*            admissible variables (those that are not forcefully excluded
*            from the subset by the user).
* poriginal -
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices ski and skinput
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix S_k. 
*  ski       - double precision matrix used to store inverse of the submatrix S_k. 


      integer i,info,j,k,kmax,l,m,nqsi,p,poriginal
      integer setint(*),qsi(*)
      integer fica(0:p)
      logical setk(*)
      character*1 laux
      double precision s(poriginal,*),skinput(kmax,*),ski(kmax,*)
      double precision valp(*),vecp(poriginal,*)
      double precision dobjgcd,aux0,aux

      external dposv

* initilizations and call to LAPACK routine DPSOV
* matrix skinput will store the submatrix S_k. Matrix s will initially
* be identity, fed to the LAPACK routine DPOSV, from which it will emerge
* as the inverse of S_k.
  
       do i=1,p
          setint(i)=i
       end do

	  i=0
	  do j=1,p
	    if(setk(j)) then
		  i=i+1
   	      setint(i)=j
	    end if
	  end do

	  do i=1,k-1
	    do j=i+1,k
                  skinput(i,j)=s(setint(i),setint(j))
		  skinput(j,i)=skinput(i,j)
	          ski(i,j)=0.0D0
                  ski(j,i)=0.0D0
            end do
            skinput(i,i)=s(setint(i),setint(i))
            ski(i,i)=1.0D0
	  end do
	  skinput(k,k)=s(setint(k),setint(k))
          ski(k,k)=1.0D0

           laux='L'
           info=0
           call dposv(laux,k,k,skinput,kmax,ski,kmax,info)

* calculates sum_{m in qsi} (valp_m vecp_m^t inv(S_k) vecp_m)

          dobjgcd=0.0D0
	  do m=1,nqsi
	    aux=0.0D0
	    do i=1,k
		  aux0=0.0D0
		  do l=1,k
		    aux0=aux0+ski(i,l)*vecp(fica(setint(l)),qsi(m))
		  end do
                  aux0=aux0*vecp(fica(setint(i)),qsi(m))
    	          aux=aux+aux0	  
	    end do
            dobjgcd=dobjgcd+aux*valp(qsi(m))
	  end do
	  return
	  end   
************************************************************************


************************************************************************
       subroutine checksingl2nrm(k,origmat,ldmat,workmat,egval,work,
     +  numtol,numsing)
************************************************************************
* This function computes checks the singularity of the symmetric square 
* positive-definite (or positive-semidefinite) matrix origmat based on 
* its l2-norm condition number (ratio of largest to smallest eigenvalues).
* WARNING : This function calls the LAPACK routine DSYEV.

*  OUTPUT:
*
*   numsing - integer flag, set to 1 is origmat is numerically singular and 
*              to 0 otherwise

*  INPUT: 
*
*   k       - integer, conceptual dimension of matrix origmat. 
*  origmat  - double precision low-triangle of the positive definite matrix 
*             (or positive-semidefinite) whose singularity is being tested.
*  ldmat    - integer, leading (physical) dimension of the matrix origmat. 
*  numtol   - double precision tolerance level for the ratio of the
*             origmat eigenvalues.

*  WORKING ARRAYS:
*
*  workmat  - double precision matrix with the same dimensions as origmat. 
*  egval    - double precision array, used to store the eigenvalues of origmat.
*  work     - double precision array, to be used internaly by the DSYEV routine.
   
       integer k,info,lwork,numsing,ldmat
       character*1 jobz,laux
       double precision work(*),egval(*)
       double precision origmat(ldmat,*),workmat(ldmat,*)
       double precision numtol

       external dsyev

	numsing = 1 

* Create a copy of matrix origmat in order to keep the original data	

	do i =1,k
	  do j=1,i
		workmat(i,j) = origmat(i,j)
	   end do
	end do	

* call to LAPACK routine dsyev

	   jobz='N'
	   laux='L'
           info=0
	   lwork=6*k
  
           call dsyev(jobz,laux,k,workmat,ldmat,egval,work,lwork,info)

* check the singularity of matrix workmat and return 1 if numerically 
* singular, and 0 otherwise 

	if (info .NE. 0) then 
	  return	
	else
	  if (egval(k) .lt. numtol) then 
		return
	  else
		if (egval(1)/egval(k) .lt. numtol) then 
			return
	  	else
			numsing = 0
		end if
	  end if
	end if

	return
	end

************************************************************************

************************************************************************
       function dobjtau2(k,setk,p,poriginal,kmax,s,h,rh,checksg,numtol,
     +   setint,work,egval,skinput,hkinput,ekinput,workmat)
************************************************************************
* This function computes the tau_2 criterion.
* WARNING : This function calls the LAPACK routine DSYGV.
*
*  OUTPUT:
*
*   The tau^2 index of "effect magnitude".  
*   This criterion  is equivalent to the minimization of 
*   Wilks lambda statistic. tau_2 = 1 - lamlba^(1/l) Where l=min(rh,k)
*   lambda = det(E)/det(T) Where E=T-H.
*   We can also obtain lambda = d1*d2*...*dl, where d1, d2, ... dl are the 
*   eigen values of ET^-1
*
*  INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision low-triangle of the matrix of covariances of all p variables.
* poriginal - 
*   h     - double precision low-triangle of a symmetric positive semi-definite 
*           (or positive definite) matrix 
*   rh    - integer, h matrix rank
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (T being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements
*           of T Choleski decomposition
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices skinput, hkinput, ekinput and workmat
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix T_k. 
*  hkinput   - double precision matrix used to store the submatrix H_k. 
*  ekinput   - double precision matrix used to store the submatrix E_k. 
*  egval     - double precision array used to store the eigenvalues of E_k (T_k)^(-1).
*  work      - double precision array to be used internaly by LAPACK routines.
*  workmat   - double precision matrix to be used internaly by the checksingl2nrm routine.
    
       integer i,info,itype,j,k,lwork,p,poriginal,kmax,numsingular
       integer rh,rhaux
       integer setint(*)
       logical checksg
       logical setk(*)
       character*1 laux,jobz
       double precision numtol,lambda,dobjtau2
       double precision s(poriginal,*),h(poriginal,*)
       double precision skinput(kmax,*),hkinput(kmax,*),ekinput(kmax,*)
       double precision work(*),egval(*),workmat(kmax,*)

       external dsygv

* initializations and call to LAPACK routine dsygv
* matrix skinput will store the submatrix S_k. 
* matrix ekinput will store the submatrix E_K = S_k - H_k. 

       do i=1,p
          setint(i)=i
       end do
       i=0
       do j=1,p
         if(setk(j)) then
           i=i+1
           setint(i)=j
         end if
       end do
       do i=1,k
           do j=1,i
              skinput(i,j)=s(setint(i),setint(j))
           end do
        end do

	if (checksg) then
	   call checksingl2nrm(k,skinput,kmax,workmat,egval,work,
     +  numtol,numsingular)

	if (numsingular.eq.1) then
		dobjtau2 = -0.9999D0
		return
	   end if
        end if

       do i=1,k
           do j=1,i
              hkinput(i,j)=h(setint(i),setint(j))
              ekinput(i,j)=skinput(i,j)-hkinput(i,j)
           end do
        end do

	if (checksg) then
	   call checksingl2nrm(k,ekinput,kmax,workmat,egval,work,
     +  numtol,numsingular)

	if (numsingular.eq.1) then
		dobjtau2 = -0.9999D0
		return
	   end if
        end if

        itype=1
	jobz='N'
	laux='L'
        info=0
	lwork=6*k
  
        call dsygv(itype,jobz,laux,k,ekinput,kmax,skinput,kmax,
     +    egval,work,lwork,info)
	  
	lambda=1.0D0
	do i=1,k
	     lambda=lambda*egval(i)
        end do
	rhaux=rh
	if (rh.GT.k) then 
	      rhaux=k
	end if 
        dobjtau2= 1.0D0 - lambda ** (1.0D0/DBLE(rhaux))
        return
     
        end
************************************************************************

   
************************************************************************
       function dobjxi2(k,setk,p,poriginal,kmax,s,h,rh,checksg,numtol,
     +   setint,work,egval,skinput,hkinput,workmat)
************************************************************************
* This function computes the xi_2 criterion.
* WARNING : This function calls the LAPACK routine DSYGV.
*
*  OUTPUT:
*
*   The criterion xi_2 of "effect magnitude".  
*   This criterion  is equivalent to the maximization of Bartlett-Pillai 
*   trace statistic U = tr(HT^-1),  xi_2 = U/l, Where l=min(rh,k)
*  
*
* INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision low-triangle of the matrix of covariances of all p variables.
* poriginal - 
*   h     - double precision low-triangle of a positive semi-definite 
*           (or positive definite) matrix
*   rh    - integer, h matrix rank
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (T being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements
*           of T Choleski decomposition
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices skinput, hkinput and workmat
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix T_k. 
*  hkinput   - double precision matrix used to store the submatrix H_k. 
*  egval     - double precision array used to store the eigenvalues of H_k (T_k)^(-1).
*  work      - double precision array to be used internaly by LAPACK routines.
*  workmat   - double precision matrix to be used internaly by the checksingl2nrm routine.

   
       integer i,info,itype,j,k,lwork,p,poriginal,kmax,numsingular
       integer rh,rhaux
       integer setint(*)
       logical checksg
       logical setk(*)
       character*1 laux,jobz
       double precision numtol,dobjxi2
       double precision s(poriginal,*),h(poriginal,*)
       double precision skinput(kmax,*),hkinput(kmax,*)
       double precision work(*),egval(*),workmat(kmax,*)

       external dsygv

* initializations and call to LAPACK routine dsygv
* matrix skinput will store the submatrix S_k. 
* matrix hkinput will store the submatrix H_k. 

       do i=1,p
          setint(i)=i
       end do

       i=0
       do j=1,p
         if(setk(j)) then
           i=i+1
           setint(i)=j
         end if
       end do
       do i=1,k
           do j=1,i
              skinput(i,j)=s(setint(i),setint(j))
           end do
        end do

	if (checksg) then
	   call checksingl2nrm(k,skinput,kmax,workmat,egval,work,
     +  numtol,numsingular)
	   if (numsingular .eq. 1) then
		dobjxi2 = -0.9999D0
		return
	   end if
        end if

       do i=1,k
           do j=1,i
              hkinput(i,j)=h(setint(i),setint(j))
           end do
        end do

        itype=1
	jobz='N'
	laux='L'
        info=0
        lwork=6*k

        call dsygv(itype,jobz,laux,k,hkinput,kmax,skinput,kmax,
     +  egval,work,lwork,info)

        dobjxi2=0.0D0
	do i=1,k
	    dobjxi2=dobjxi2+egval(i)
        end do
	rhaux=rh
	if (rh.GT.k) then 
	    rhaux=k
	end if 
	dobjxi2=dobjxi2/DBLE(rhaux)  

        return
	end   
************************************************************************

************************************************************************
       function dobjzeta2(k,setk,p,poriginal,kmax,s,h,rh,checksg,numtol,
     +   setint,work,egval,skinput,hkinput,ekinput,workmat)
************************************************************************
* This function computes the zeta_2 criterion.
* WARNING : This function calls the LAPACK routine DSYGV.
*
*  OUTPUT:
*
*   The criterion zeta_2 of "effect magnitude".  
*   This criterion  is equivalent to the maximization of Hotteling-Lawley
*   trace statistic V = tr(HE^-1),  zeta_2 = V/(V+l), Where l=min(rh,k)
*
*  INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision low-triangle of the matrix of covariances of all p variables.
* poriginal - 
*   h     - double precision low-triangle of a positive semi-definite 
*          (or positive definite) matrix.
*   rh    - integer, h matrix rank
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements
*           of E Choleski decomposition
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices skinput, hkinput, ekinput and workmat
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix S_k. 
*  hkinput   - double precision matrix used to store the submatrix H_k. 
*  ekinput   - double precision matrix used to store the submatrix E_k. 
*  egval     - double precision array used to store the eigenvalues of H_k (E_k)^(-1).
*  work      - double precision array to be used internaly by LAPACK routines.
*  workmat   - double precision matrix to be used internaly by the checksingl2nrm routine.

    
       integer i,info,itype,j,k,lwork,p,poriginal,kmax,numsingular
       integer rh,rhaux
       integer setint(*)
       logical checksg
       logical setk(*)
       character*1 laux,jobz
       double precision numtol,lambda,dobjzeta2
       double precision s(poriginal,*),h(poriginal,*)
       double precision skinput(kmax,*),hkinput(kmax,*),ekinput(kmax,*)
       double precision work(*),egval(*),workmat(kmax,*)

       external dsygv

* initializations and call to LAPACK routine dsygv
* matrix skinput will store the submatrix S_k. 
* matrix ekinput will store the submatrix E_K = S_k - H_k. 

       do i=1,p
          setint(i)=i
       end do
       i=0
       do j=1,p
         if(setk(j)) then
           i=i+1
           setint(i)=j
         end if
       end do
       do i=1,k
           do j=1,i
              skinput(i,j)=s(setint(i),setint(j))
              hkinput(i,j)=h(setint(i),setint(j))
              ekinput(i,j)=skinput(i,j)-hkinput(i,j)
           end do
        end do

	if (checksg) then
	   call checksingl2nrm(k,ekinput,kmax,workmat,egval,work,
     +  numtol,numsingular)

	if (numsingular.eq.1) then
		dobjzeta2 = -0.9999D0
		return
	   end if
        end if

        itype=1
	jobz='N'
	laux='L'
        info=0
	lwork=6*k
  
        call dsygv(itype,jobz,laux,k,hkinput,kmax,ekinput,kmax,
     +    egval,work,lwork,info)

        dobjzeta2=0.0D0
        do i=1,k
	    dobjzeta2 = dobjzeta2 + egval(i)
        end do
	   
	rhaux=rh
	if (rh.GT.k) then 
	    rhaux=k
	end if 
	dobjzeta2=dobjzeta2/(dobjzeta2+DBLE(rhaux))  

        return
	end   
************************************************************************


************************************************************************
       function dobjccr12(k,setk,p,poriginal,kmax,s,h,checksg,numtol,
     +   setint,work,egval,skinput,hkinput,workmat)
************************************************************************
* This function computes the ccr1_2 criterion.
* WARNING : This function calls the LAPACK routine DSYGV.
*
*  OUTPUT:
*
*   The criterion ccr1_2 
*   This criterion  is equivalent to the maximization of Roy first
*   root (lambda1), first eigen value of HE^-1, where E=T-H.
*   ccr1_2 = lambda1/(1+lambda1). 
*  INPUT: 
*
*   k     - integer, cardinality of subset setk. 
* setk    - logical vector, indicating a given subset of variables. 
*           setk(j)=.true. iff variable j belongs to the subset. Changed
*           on output.
*   p     - integer variable, number of original variables.
*   s     - double precision low-triangle of the matrix of covariances 
*           of all p variables.
*   h     - double precision low-triangle of a symmetric positive semi-definite 
*           (or positive definite) matrix. 
* poriginal - 
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements
*           of E Choleski decomposition
*  kmax   - integer variable, giving the physical dimensionality of the 
*           square matrices skinput, hkinput, ekinput and workmat
              
*  WORKING ARRAYS:
*
*  setint    - integer array used to store the indices of the variables 
*              kept in subset setk.
*  skinput   - double precision matrix used to store the submatrix T_k = H_k + E_k. 
*  hkinput   - double precision matrix used to store the submatrix H_k. 
*  egval     - double precision array used to store the eigenvalues of H_k (E_k)^(-1).
*  work      - double precision array to be used internaly by LAPACK routines.
*  workmat   - double precision matrix to be used internaly by the checksingl2nrm routine.

       integer i,info,itype,j,k,lwork,p,poriginal,kmax,numsingular
       integer setint(*)
       logical checksg
       logical setk(*)
       character*1 laux,jobz
       double precision numtol,dobjccr12
       double precision s(poriginal,*),h(poriginal,*)
       double precision skinput(kmax,*),hkinput(kmax,*)
       double precision work(*),egval(*),workmat(kmax,*)
        
      external dsygv

* initializations and call to LAPACK routine dsygv
* matrix skinput will store the submatrix S_k. 
* matrix hkinput will store the submatrix H_k.

       do i=1,p
          setint(i)=i
       end do
       i=0
       do j=1,p
         if(setk(j)) then
           i=i+1
           setint(i)=j
         end if
       end do
       do i=1,k
           do j=1,i
              skinput(i,j)=s(setint(i),setint(j))
           end do
        end do

	if (checksg) then
	   call checksingl2nrm(k,skinput,kmax,workmat,egval,work,
     +  numtol,numsingular)
	   if (numsingular .eq. 1) then
		dobjccr12 = -0.9999D0
		return
	   end if
        end if

       do i=1,k
           do j=1,i
              hkinput(i,j)=h(setint(i),setint(j))
           end do
        end do
          
        itype=1
	jobz='N'
	laux='L'
        info=0
	lwork=6*k

        call dsygv(itype,jobz,laux,k,hkinput,kmax,skinput,kmax,
     +    egval,work,lwork,info)
      
	dobjccr12=egval(k)

        return
	end   
