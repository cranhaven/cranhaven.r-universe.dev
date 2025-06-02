      subroutine updatevar(Y,RR,n,TT,r,k,Mu,Si,Y1,Var)

c declare arguments in input
      integer t,TT,n,r,u,i,indt(r)
      integer j,j1,j2,p,k,info,RR(n,TT,r)
c      integer indo1
      logical mask(r)
      double precision Y1(n,TT,r,k),Mu(r,k),Si(r,r)
      double precision Y(n,TT,r),Var(n,TT,r,r)
      integer, allocatable :: indo(:),indm(:)
      double precision, allocatable :: Tmp(:,:),iU(:,:)
      double precision zero
      parameter (zero = 0d0)


      external dportf dtrtri

c check each row
      do j = 1,r
        indt(j) = j
      end do
      do i = 1,n
        do t = 1,TT
          p = sum(RR(i,t,:))
          if(p==0) then
            Y1(i,t,:,:) = Mu
            Var(i,t,:,:) = Si
          end if
          if(p>0 .and. p<r) then
            mask = RR(i,t,:)==1
            indo = pack(indt,mask)
            mask = RR(i,t,:)==0
            indm = pack(indt,mask)
            if(p==1) then
              iU = 1/sqrt(Si(indo,indo))
            else
              iU = Si(indo,indo)
              call dpotrf('U',p,iU,p,info)
              call dtrtri('U','N',p,iU,p,info)
              do j1 = 2,p
                do j2 = 1,j1-1
                  iU(j1,j2) = zero
                end do
              end do
            end if
            Tmp = matmul(Si(indm,indo),matmul(iU,transpose(iU)))
            Var(i,t,indm,indm) = Si(indm, indm)-
     c                           matmul(Tmp,Si(indo,indm))
            do u = 1,k
              Y1(i,t,indm,u) = Mu(indm,u)+
     c                         matmul(Tmp,Y(i,t,indo)-Mu(indo,u))
            end do
          end if
        end do
      end do

      end
