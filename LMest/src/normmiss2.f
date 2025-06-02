      subroutine normmiss2(Y,RR,n,TT,r,k,Mu,Si,Phi)

c declare arguments in input
      integer n,r,u,i,indt(r),j,j1,j2,indo1,p,k
      integer info,t,TT,RR(n,TT,r)
      double precision Y(n,TT,r),Mu(n*TT,r,k),Si(r,r),Phi(n,k,TT)
      double precision f0,f,det,tmp1
      integer, allocatable :: indo(:)
      double precision, allocatable :: tmp(:),iU(:,:)
      external dportf dtrtri
      logical mask(r)
      double precision eps
      parameter (eps = 0.1d0**300d0)
      double precision zero
      parameter (zero = 0d0)
      double precision two
      parameter (two = 2d0)
      double precision twopi
      parameter (twopi = 6.283185307179586231996d0)

c check each row
      do j = 1,r
        indt(j) = j
      end do
      j = 0
      do i = 1,n
        do t = 1,TT
          j = j+1
          p = sum(RR(i,t,:))
          if(p==0) then
            Phi(i,:,t) = 1
          else
            mask = RR(i,t,:)==1
            indo = pack(indt,mask)
            if(p==1) then
              indo1 = indo(1)
              f0 = 1/sqrt(twopi*Si(indo1,indo1))
              do u = 1,k
                tmp1 = ((Y(i,t,indo1)-Mu(j,indo1,u))**two)/
     c                  Si(indo1,indo1)
                f = f0*exp(-tmp1/two)
                Phi(i,u,t) = max(f,eps)
              end do
            else
              iU = Si(indo,indo)
              call dpotrf('U',p,iU,p,info)
              call dtrtri('U','N',p,iU,p,info)
              det = iU(1,1)
              do j1 = 2,p
                do j2 = 1,j1-1
                  iU(j1,j2) = zero
                end do
                det = det*iU(j1,j1)
              end do
              f0 = det/sqrt(twopi**dble(p))
              do u = 1,k
                tmp = matmul(transpose(iU),(Y(i,t,indo)-Mu(j,indo,u)))
                f = f0*exp(-dot_product(tmp,tmp)/two)
                Phi(i,u,t) = max(f,eps)
              end do
            end if
          end if
        end do
      end do

      end
