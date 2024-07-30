                
*#######################################################################
       SUBROUTINE anneal(criterio,p,svector,kmin,kmax,valores,vars,
     + bestval,bestvar,nfora,fora,ndentro,dentro,nsol,niter,
     + improvement,beta,temp,coolfreq,nqsi,qsi,esp,silog,solinit,valp,
     + vecvecp,hvector,rh,checksg,numtol,numprb)
*#######################################################################

* Routine to perform a Simulated Annealing search for a k-variable subset
* of a set of p original variables, which maximizes one of several criteria.
* (The criteria currently considered are RM, RV, GCD, TAU_2,XI_2,ZETA_2 and 
*  CCR1_2). The user may force the inclusion and/or exclusion of certain 
*  variables from the k-subset.
* 
* Designed to be called by an R/S function "anneal" (included in the package 
* "subselect", available from CRAN). 
*
* WARNING: Requires the LAPACK routine DPOSV
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
* niter    - integer variable, number of simulated annealing iterations 
*           requested for each solution.
*improvement- logical variable. If true, the best solution produced by 
*            Simulated Annealing will be subjected to a restricted local
*            improvement algorithm. (By default .true.).
*  beta    - double precision variable,(between 0 and 1) indicating the
*           geometric cooling factor for simulated annealing.
* temp    - double precision variable, indicating the initial temperature
*           to be used in the simulated annealing algorithm.
* coolfreq - integer indicating the cooling frequency, i.e., the number 
*           of iterations that are to be carried out for each fixed
*           temperature 
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
*           as a vector for convenience in passing from R to Fortran). 
* hvector  - double precision vector, giving the full effect description 
*            matrix (H) of the p variables (given as a vector for
*            convenience in passing from R to Fortran). 
*   rh     - integer variable giving the expected rank of H matrix.
* checksg - logical, flag indicating if a test for ill-conditioned 
*	    (E being numerically singular) problems should be implemented
*  numtol - double precision tolerance level for the ratio of the diagonal 
*           elements of E Choleski decomposition

* 
*
*
* OUTPUT: 
*
* valores  - double precision variable giving the final values produced by S.  
*            Annealing for each of the nsol solutions, in each cardinality, 
*            for the criterion required. Will pass to the R/S function "anneal"
*            for conversion to an nsol x (kmax-kmin+1) matrix within R/S, 
*            with each column associated with a  cardinality and each row with
*            a different solution. 
*  vars    - integer vector giving the list of variable numbers  belonging 
*            to  each of the nsol final subsets, for each cardinality (padded
*            with zeros if k<kmax). Will pass to the R/S function "anneal" for
*             conversion to an nsol x kmax x (kmax-kmin+1) 3-dimensional array.
* bestval - double precision vector with the best value of the criterion
*           obtained by running the routine, for each cardinality.  If 
*           improvement=.true. (the default), then the value given is the 
*           value resulting from running the restricted local search algorithm
*           on the best solution from Simulated Annealing (output for R/S).
* bestvar - integer vector with the variable numbers of the best subset 
*           obtained, for each cardinality.  If improvement=.true. (the 
*           default), then the subset given results from running the 
*           restricted local search algorithm on the best solution from 
*           Simulated Annealing (output for R/S).
* numprb -  logical flag set to true if numerical problems preclude some
*           (ill-conditioned) subsets to be evaluated, and to false otherwise.   
*

* general declarations
       INTEGER i,j,k,m,p,criterio,kmin,kmax,nfora,ndentro,niter
       INTEGER poriginal,coolfreq 
       INTEGER fora(0:nfora),fica(0:p),dentro(0:ndentro),auxw(kmax)
       INTEGER vars(nsol*(kmax-kmin+1)*kmax)
       INTEGER bestvar((kmax-kmin+1)*kmax)
       INTEGER solinit(kmax*nsol*(kmax-kmin+1))
       DOUBLE PRECISION s(p,p),svector(p*p),vecvecp(p*p)
       DOUBLE PRECISION vmax,vactual,vcorrente,numtol
       DOUBLE PRECISION valores((kmax-kmin+1)*nsol)
       DOUBLE PRECISION beta,temp,critvalue
       DOUBLE PRECISION bestval(kmax-kmin+1)
       LOGICAL silog,improvement,checksg,numprb
       LOGICAL setk(p),setkmax(p)
* declarations only for the RM and RV criteria
       DOUBLE PRECISION tracos,tracosq,dobjrm,dobjrv
       DOUBLE PRECISION sq(p,p)
* declarations only for the GCD criterion
       DOUBLE PRECISION valp(p),vecp(p,p),dobjgcd
       INTEGER nqsi,qsi(p)
       LOGICAL esp
* declarations only for the tau2,xi2,zeta2 and ccr12 criteria
       INTEGER rh
       DOUBLE PRECISION hvector(p*p),h(p,p)
       DOUBLE PRECISION dobjtau2,dobjxi2,dobjzeta2,dobjccr12
* declarations of local arrays and matrices
       INTEGER randsk1pp(p),setint(p)
       DOUBLE PRECISION skinput(kmax,kmax),hkinput(kmax,kmax)
       DOUBLE PRECISION ekinput(kmax,kmax),workmat(kmax,kmax)
       DOUBLE PRECISION work(6*kmax),egval(kmax)

       external randsk1,dobjrm,dobjrv,dobjgcd,dobjtau2,dobjxi2
       external dcorrigesk,dannealing,dobjzeta2,dobjccr12
       external dmelhoramentogen,newinicializar

* initializations 

      call newinicializar(criterio,p,s,svector,sq,nfora,fora,ndentro,
     +  dentro,fica,tracos,tracosq,vecp,poriginal,vecvecp,
     +  h,hvector,rh)
       critvalue = -1.0D0

**********************************
* The loop which is to be repeated for each cardinality of subsets, 
* between kmin e kmax starts here and ends together with the subroutine.

      do k=kmin,kmax

* Generates nsol solutions via Simulated Annealing

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

* determining the initial solution.

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
      
           call dannealing(criterio,p,kmax,setk,vactual,ndentro,
     +                dentro,niter,beta,temp,coolfreq,k,s,tracos,sq,
     +                tracosq,nqsi,qsi,valp,vecp,fica,poriginal,
     +                h,rh,skinput,hkinput,ekinput,egval,work,workmat,
     +                randsk1pp,setint,checksg,numtol,numprb)

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
		end if
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


* If improvement is "true" and a valid solution was found
* runs a restricted local improvement algorithm on the 
* best of the nsol solutions produced by Simulated Annealing.  

         if (improvement.and.vmax.GE.0.D0) then
            vcorrente = vmax

            call dmelhoramentogen(criterio,p,kmax,setk,vactual,ndentro,
     +                dentro,k,s,sq,nqsi,qsi,valp,vecp,fica,poriginal,
     +                h,rh,skinput,hkinput,ekinput,egval,work,workmat,
     +                randsk1pp,setint,checksg,numtol,numprb)

            if (vmax .GT. vcorrente) then 
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
            end if      
         end if

* End of restricted improvement

         bestval(k-kmin+1)=critvalue
         do j=1,k
            bestvar(kmax*(k-kmin)+j)=auxw(j)
         end do
       end do  

* End of the k-loop

      return
      end





