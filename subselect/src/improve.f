
*#######################################################################
       SUBROUTINE improve(criterio,p,svector,kmin,
     +   kmax,valores,vars,bestval,bestvar,nfora,fora,ndentro,
     +   dentro,nsol,nqsi,qsi,esp,silog,solinit, valp,
     +   vecvecp,hvector,rh,checksg,numtol,numprb)
*#######################################################################

* Restricted improvement algorithm to search for a k-variable subset
* of a set of p original variables, which maximizes one of several criteria.
* (The criteria currently considered are RM, RV, GCD, TAU_2,XI_2,ZETA_2 and 
*  CCR1_2).
* The user may force the inclusion and/or exclusion of certain 
* variables from the k-subset.
* 
* Designed to be called by an R/S function "improve".
*
* WARNING: Uses R's default Random Number Generator.
*
* INPUT : 
*
*criterio  - integer variable indicating which criterion of subset quality
*            was requested.
*   p      - integer variable indicating the number of original variables.
* svector  - double precision vector, giving the full covariance (or 
*            correlation) matrix of the p variables (given as a vector
*            for convenience in passing from R to Fortran). 
*kmin,kmax - integer variables, giving the smallest and largest cardinalities
*           of the subsets of variables that are wanted.
*  nfora   - integer variable indicating the number of original variables 
*            that are to be forcefully excluded from the k-subset.
*   fora   - integer vector, indicating the variable numbers associated
*            with the nfora variables that are to be excluded by force.
* ndentro  - integer variable indicating the number of original variables 
*            that are to be forcefully included in the k-subset.
*  dentro  - integer vector, indicating the variable numbers associated
*            with the ndentro variables that are to be included by force.
*  nsol    - integer indicating how many different solutions are to be 
*            produced by the simulated annealing search.
*  nqsi    - integer variable, indicating the dimension of the Principal 
*           Subspace with which the subspace spanned by the k-subset is
*            to be compared. (For GCD criterion only).
*  qsi     - integer vector, giving the ranks of the Principal Components 
*           which span the Principal Subspace described above (see nqsi).
*           (For GCD criterion only).
*  esp     - logical vector. If .true., indicates that the user has specified
*            a set of Principal Components different from the first k, to 
*            compare with the k-variable subset. (For GCD criterion only).
* silog    - logical variable indicating whether or not the user has specified
*             initial solutions.
* solinit  - integer variable, giving the user-specified initial solutions
*             (if any).
*   valp   - a double precision vector with the eigenvalues of the covariance 
*            (correlation) matrix, computed in R.
* vecvecp  - double precision vector, giving the eigenvectors of the 
*            covariance (or correlation) matrix of the p variables (given 
*            as a vector for convenience in passing from R to Fortran). 
* hvector  - double precision vector, giving the full effect descrption 
*            matrix (H) of the p variables (given as a vector for
*            convenience in passing from R to Fortran). 
*   rh     - integer variable giving the expected rank of H matrix.
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal elements of E Choleski decomposition

* OUTPUT: 

* valores  - double precision variable giving the final values produced by the 
*            restricted local search Algorithm for each of the nsol solutions, 
*            in each cardinality, for the criterion required. Will pass to the
*            R/S function "improve" for conversion to an nsol x (kmax-kmin+1) 
*            matrix within R/S, with each column associated with a cardinality
*            and each row with a different solution. 
*  vars    - integer vector giving the list of variable numbers belonging to 
*            each of the nsol final subsets, for each cardinality (padded 
*            with zeros if k<kmax). Will pass to the R/S function "improve" 
*            for conversion to an nsol x kmax x (kmax-kmin+1) 3-d array.
* bestval - double precision vector with the best value of the criterion
*           obtained by running the routine, for each cardinality (R/S output).
* bestvar - integer vector with the variable numbers of the best subset 
*           obtained, for each cardinality (ouput for R/S).
* numprb -  logical flag set to true if numerical problems precluded some
*           (ill-conditioned) subsets to be evaluated, and to false otherwise.   
*
*

* general declarations
       INTEGER p,criterio,poriginal,rh
       INTEGER fora(0:nfora),fica(0:p),dentro(0:ndentro),auxw(kmax)
       INTEGER vars(nsol*(kmax-kmin+1)*kmax)
       INTEGER bestvar((kmax-kmin+1)*kmax)
       LOGICAL setk(p),setkmax(p)
       DOUBLE PRECISION s(p,p),sq(p,p),svector(p*p),hvector(p*p),h(p,p)
       DOUBLE PRECISION vmax,vactual,numtol
       DOUBLE PRECISION valores((kmax-kmin+1)*nsol)
       DOUBLE PRECISION critvalue,bestval(kmax-kmin+1)
       DOUBLE PRECISION vecvecp(p*p)
       LOGICAL silog,checksg,numprb
       INTEGER solinit(kmax*nsol*(kmax-kmin+1))
* FIM NOVO TESTE
* declarations only for the RM criterion
       DOUBLE PRECISION tracos,dobjrm
* declarations only for the RV criterion
       DOUBLE PRECISION tracosq,dobjrv
* declarations only for the GCD criterion
       DOUBLE PRECISION valp(p),vecp(p,p),dobjgcd
       INTEGER nqsi,qsi(p)
       LOGICAL esp
* declarations for tau2,xi2,zeta2 e ccr12
       DOUBLE PRECISION dobjtau2,dobjxi2,dobjzeta2,dobjccr12
* declarations of local arrays and matrices
       INTEGER randsk1pp(p),setint(p)
       DOUBLE PRECISION skinput(kmax,kmax),hkinput(kmax,kmax)
       DOUBLE PRECISION ekinput(kmax,kmax),workmat(kmax,kmax)
       DOUBLE PRECISION work(6*kmax),egval(kmax)

       external dmelhoramentogen,newinicializar,randsk1,dcorrigesk
       external dobjrm,dobjrv,dobjgcd
       external dobjtau2,dobjxi2,dobjzeta2,dobjccr12

* initializations 
       critvalue = -1.0D0
       numprb=.false.

       call newinicializar(criterio,p,s,svector,sq,nfora,fora,
     +  ndentro, dentro,fica,tracos,tracosq,vecp,poriginal,vecvecp,
     +  h,hvector,rh)

**********************************
* The loop which is to be repeated for each cardinality of subsets, 
* between kmin e kmax starts here and ends together with the subroutine.

      do k=kmin,kmax

* Generates nsol solutions at random, and runs the Restricted Local
* Improvement algorithm on them.

         if (criterio.eq.3) then
             if (.not.esp) then
               nqsi=k
               do m=1,nqsi
                 qsi(m)=m
               end do
             end if
         end if
         vmax=-1
         do ksol=1,nsol

* setting up the inital solutions

           if (silog) then
             do m=1,poriginal
               setk(m)=.false.
             end do           
             do m=1,k
               setk(solinit(nsol*kmax*(k-kmin)+nsol*(m-1)+ksol))=.true.
             end do
             if (nfora.ge.1) then
               do m=1,p
                 setk(m) = setk(fica(m))
               end do
             end if  
           else
               call randsk1(p-ndentro,k-ndentro,setk,randsk1pp)           
               if(ndentro .GT. 0) then 
                  call dcorrigesk(ndentro,dentro,p,setk,poriginal)
               endif
           endif

           if (criterio.eq.1) then
		vactual=dobjrm(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
           end if
           if (criterio.eq.2) then
               vactual=dobjrv(k,setk,p,poriginal,kmax,s,sq,workmat,
     +   setint,skinput)
           end if
           if (criterio.eq.3) then
		vactual=dobjgcd(k,setk,p,poriginal,kmax,s,workmat,
     +   nqsi,qsi,valp,setint,vecp,fica,skinput)
           end if
           if (criterio.eq.4) then
               vactual=dobjtau2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
           end if
	   if (criterio.eq.5) then
               vactual=dobjxi2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if
	   if (criterio.eq.6) then
               vactual=dobjzeta2(k,setk,p,poriginal,kmax,s,h,rh,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,ekinput,workmat)
           end if		 
	  if (criterio.eq.7) then
* tirei rh
               vactual=dobjccr12(k,setk,p,poriginal,kmax,s,h,checksg,
     +   numtol,setint,work,egval,skinput,hkinput,workmat)
           end if

           if (checksg.and.criterio.GT.3.and.vactual.eq.-0.9999D0) then
	       numprb=.true.        
           end if
	   if (vactual.GE.0) then
		call dmelhoramentogen(criterio,p,kmax,setk,vactual,
     +         ndentro,dentro,k,s,sq,nqsi,qsi,valp,vecp,fica,
     +         poriginal,h,rh,skinput,hkinput,ekinput,egval,
     +         work,workmat,randsk1pp,setint,checksg,numtol,numprb)
	   end if

           if (criterio.eq.1) then
              critvalue = dsqrt(vactual/tracos)
           end if
           if (criterio.eq.2) then
              critvalue = dsqrt(vactual/tracosq)
           end if
           if (criterio.eq.3) then
              critvalue = vactual/dsqrt(DBLE(nqsi*k))
           end if
           if (criterio.eq.4) then
              critvalue = vactual
           end if

	     if (criterio.eq.5) then
               critvalue = vactual
           end if
		  if (criterio.eq.6) then
              critvalue = vactual
           end if

	     if (criterio.eq.7) then
               critvalue = vactual
           end if

* preparing the output for R/S

            valores((k-kmin)*nsol+ksol)=critvalue
            jjaux=0
           do i=1,p
             if (setk(i)) then
               jjaux=jjaux+1
		if (critvalue.LT.0.0D0) then 
                   vars(nsol*kmax*(k-kmin)+nsol*(jjaux-1)+ksol) = 0
		else
                   vars(nsol*kmax*(k-kmin)+nsol*(jjaux-1)+ksol) = 
     +           fica(i)
                endif
             end if
           end do  

           if(vactual .GT. vmax) then
              vmax=vactual
              do j=1,p
                 setkmax(j)=setk(j)
              end do
           end if
         end do

         naux=0
         do j=1,p
           if(setkmax(j)) then
              naux=naux+1
              auxw(naux)= fica(j)
           end if
         end do
         if (criterio.eq.1) then
            critvalue=dsqrt(vmax/tracos)
         end if
         if (criterio.eq.2) then
            critvalue=dsqrt(vmax/tracosq)
         end if
         if (criterio.eq.3) then
              critvalue = vmax/dsqrt(DBLE(nqsi*k))
         end if
		if (criterio.eq.4) then
            critvalue=vmax
         end if

         if (criterio.eq.5) then
             critvalue=vmax  
           end if
		 
          if (criterio.eq.6) then
            critvalue=vmax
         end if

         if (criterio.eq.7) then
             critvalue=vmax  
           end if

        bestval(k-kmin+1)=critvalue
        do j=1,k
            bestvar(kmax*(k-kmin)+j) = auxw(j)
        end do
        end do  

* End of the k-loop

      return
      end





