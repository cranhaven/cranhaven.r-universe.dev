! Even if nothing in this file will be called from OpenMP parallel regions
! we still add recursive to everything guard against compiler-related bugs,
! especially compiling using lesser-used of C & Fortran compilers and
! compiler combinations...
module oumods
  use, intrinsic :: iso_c_binding
  implicit integer(c_int) (i-k), integer(c_int) (m,n), &
       & real(c_double) (a-h), real(c_double) (l), real(c_double) (o-z)
contains
  recursive function mod2small(z)
    integer(c_int)    :: mod2small
    complex(c_double_complex) :: z
    if (abs(dble(real(z))) < 0.000001_c_double .and. abs(dble(aimag(z))) < 0.000001_c_double) then
       mod2small = 1
    else
       mod2small = 0
    end if
  end function

  recursive subroutine zI0(t,c,alpha,beta,r)
    complex(c_double_complex) c,r,    x,z
    if (mod2small(c) == 1) then
       r = beta*r+alpha*cmplx(t,0.0_c_double,kind(1._c_double))
    else
       z = c*t
       x = (2.0_c_double * cosh((z - cmplx(0._c_double,3.14159265358979324_c_double, kind(1._c_double))) &
            & /2._c_double )) / (c / exp((z + cmplx(0._c_double,3.14159265358979324_c_double,kind(1._c_double)))/2._c_double))
       r = beta * r + alpha * x
       !r = beta * r + alpha * ((exp(t*c)-1.0_c_double)/c)
    end if
  end subroutine
  
  recursive subroutine zI1(t,c,alpha,beta,r) bind(C, name='zI1_')
    real(c_double) t, alpha, beta
    complex(c_double_complex) c,r,    x,y,z
    if (mod2small(c) == 1) then
       r = beta*r+alpha*cmplx((t**2.0_c_double)/2.0_c_double,0.0_c_double,kind(1._c_double))
    else
       z = c*t
       y = exp(z)
       x = (2.0_c_double * cosh((z - cmplx(0._c_double,3.14159265358979324_c_double, kind(1._c_double))) &
            & /2._c_double )) / (c / exp((z + cmplx(0._c_double,3.14159265358979324_c_double,kind(1._c_double)))/2._c_double))
       r = beta*r+alpha*((t*y - x)/c)
       !r = beta*r+alpha*(exp(t*c)*(c*t-1.0_c_double_complex)+1.0_c_double_complex)/(c**2.0_c_double)
    end if
  end subroutine

  recursive subroutine zI2(t,c,alpha,beta,r)
    real(c_double) t,alpha,beta
    complex(c_double_complex) c, r,   x,y,z
    if (mod2small(c) == 1) then
       r = beta * r + alpha * ((t**3.0_c_double) / 3.0_c_double)
    else
       z = c*t
       y = exp(z)
       x = (2.0_c_double * cosh((z - cmplx(0._c_double,3.14159265358979324_c_double,kind(1._c_double))) &
            & /2._c_double )) / (c / exp((z + cmplx(0._c_double,3.14159265358979324_c_double,kind(1._c_double)))/2._c_double))
       r = beta * r + alpha * ((y*(t**2._c_double) + 2._c_double*(x-t*y)/c)/c)
    endif
  end subroutine
  
  ! Convert cholesky decomposition to the original matrix
  ! wsp at least k^2
  recursive subroutine dunchol(sig_x,k,wsp,lwsp,sig,info) bind(C,name="unchol_")
    real(c_double) sig_x, wsp, sig
    integer(c_int) :: lwsp, k, info
    dimension sig_x((k*(k+1))/2), wsp(lwsp), sig(k,k)
    target :: wsp
    real(c_double), pointer  :: tmp(:,:)
    external dtpttr, dgemm
    if (lwsp < k*k) then
      call rwarn("dunchol: workspace too small.")
    endif
    ! De-cholesky-ise sig_x
    tmp(1:k,1:k) => wsp(1:(k**2))
    tmp(1:k,1:k) = 0.0_c_double
    call dtpttr('L',k,sig_x,tmp,k,info)
    if (info /= 0) return
    call dgemm('N','T',k,k,k,1.0_c_double,tmp,k,tmp,k,0.0_c_double,sig,k)
    info = 0
  end subroutine

  ! Same as dunchol but the diagonals of sig_x is now parameterised by the logarithm.
  ! wsp at least k^2
  recursive subroutine dlnunchol(sig_x,k,wsp,lwsp,sig,info) bind(C,name="lnunchol_")
    real(c_double) :: sig_x, wsp, sig
    integer(c_int) :: lwsp, k, info
    dimension sig_x((k*(k+1))/2), wsp(lwsp), sig(k,k)
    target :: wsp
    real(c_double), pointer  :: tmp(:,:)
    external dtpttr, dgemm
    if (lwsp < k*k) then
      call rwarn("dlnunchol: workspace too small.")
    endif
    ! De-cholesky-ise sig_x
    tmp(1:k,1:k) => wsp(1:(k**2))
    tmp(1:k,1:k) = 0.0_c_double
    call dtpttr('L',k,sig_x,tmp,k,info)
    if (info /= 0) return
    do i = 1,k
       tmp(i,i) = exp(tmp(i,i))
    enddo
    call dgemm('N','T',k,k,k,1.0_c_double,tmp,k,tmp,k,0.0_c_double,sig,k)
    info = 0
  end subroutine

  ! For A = PLP^{-1}, output P, Lambda, and P^{-1} if A is diagonalisable. Otherwise output
  ! info /= 0 with the content of P, Lambda and invP undefined on exit.
  ! Wsp at least 2*(k**2), zwsp at least 2*(k**2)
  recursive subroutine zeiginv(A,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info) bind(C, name="zeiginv_")
    real(c_double) A, wsp
    complex(c_double_complex) P, invP, Lambda, zwsp
    integer(c_int) k,lwsp,lzwsp,info,ipiv(k)
    dimension A(k,k), zwsp(lzwsp), wsp(lwsp), P(k,k), invP(k,k), Lambda(k), LR(k),LI(k)
    target :: wsp
    target :: zwsp
    real(c_double), pointer :: tmp(:,:), A2(:,:)
    complex(c_double_complex), pointer :: ztmp(:,:)
    external dgeev, zgetrf, zgetri, rwarn
    if (lwsp < 2*k*k) then
      call rwarn("zeiginv: workspace too small.")
    endif
    if (lzwsp < 2*k*k) then
      call rwarn("zeiginv: z-workspace too small.")
    endif
    ! Eigen-decompose the A.
    tmp(1:k,1:k) => wsp
    A2(1:k,1:k)  => wsp((k**2+1):(2*k**2))
    A2 = A
    call dgeev('N','V',k,A2,k,LR,LI,tmp,1,tmp,k,wsp(2*k**2+1),lwsp-2*k**2,info)
    if (info /= 0) then
       return
    end if
    
    ! Copy the eigen values and vectors back to a normal form.
    j = 1
30  Lambda(j) = cmplx(LR(j),LI(j),kind(1._c_double))
    if (LI(j) == 0.0_c_double) then
       P(:,j) = cmplx(tmp(:,j), 0.0_c_double,kind(1._c_double))
    else
       P(:,j)      = cmplx(tmp(:,j),  +tmp(:,j+1),kind(1._c_double))
       P(:,j+1)    = cmplx(tmp(:,j),  -tmp(:,j+1),kind(1._c_double))
       j = j+1
       Lambda(j) = cmplx(LR(j),LI(j),kind(1._c_double))
    end if
    j = j+1
    if (j <= k) goto 30

    ! Invert the eigenvector
    ztmp(1:k,1:k) => zwsp(1:(k*k))
    ztmp(1:k,1:k) = P(:,:)
    call zgetrf(k,k,ztmp,k,ipiv,info)
    if (info /= 0) then
       return
    end if
    call zgetri(k,ztmp,k,ipiv,zwsp(k*k+1),lzwsp-k*k,info)
    if (info /= 0) then
       return
    end if
    invP(:,:) = ztmp(:,:)
    info = 0
    return
  end subroutine


  ! Compute exp(-H * t) using eigen decomposition PLP^-1 of H
  recursive subroutine d0phi (t, k, P, invP, Lambda, Phi, zwsp) bind(C, name="d0phi_")
    real(c_double) t, Phi
    integer(c_int) k
    complex(c_double_complex) P, invP, Lambda, zwsp
    dimension P(k,k), invP(k,k), Lambda(k), Phi(k,k), zwsp(k**2)
    target zwsp
    complex(c_double_complex), pointer :: zout(:,:)
    zout(1:k,1:k) => zwsp(1:(k**2))
    do j = 1,k
       do i = 1,k
          zout(i,j) = P(i,j) * exp(-t*Lambda(j))
       enddo
    enddo
    Phi(:,:) = dble(real(matmul(zout, invP)))
  end subroutine

  ! A, theta and sig_x is our input, V,w,Phi is output, V's initial value is summed into result.
  ! A:     a kxk matrix.
  ! Sig_x: a LOWER TRIANGULAR BLAS-PACKED FORMAT (PCMBase USES UPPER!)
  ! theta: kx1 matrix
  ! V:     lower triangular blas-packed. Output is summed into the result to allow measurement
  !        error `Sigma_e` If on entry V is non-zero, then the content should represent
  !        cholesky(Sigma_e).
  ! w:     kx1 matrix, overridden, output
  ! Phi:   kxk matrix, overridden, output
  ! wsp:   real(c_double)*lwsp. Workspace.
  ! lwsp:  at least 12*k*k
  ! zwsp:  complex(c_double_complex)*lwsp. Workspace
  ! lzwsp: at least 8*k*k
  ! eigavail: 0 or 1. If one, P, invP are recomputed and output. Otherwise they
  !           are read and untouched.
  recursive subroutine d0geouvwphi(A,k,t,theta,sig_x,V,w,Phi,P,invP,Lambda,wsp,lwsp,&
                       & zwsp,lzwsp,eigavail,info) bind(C, name="d0geouvwphi_")
    real(c_double) A,t,theta,sig_x,V,w,Phi,wsp
    complex(c_double_complex) P, invP, Lambda
    integer(c_int) :: info, k, eigavail, lwsp, lzwsp
    dimension A(k,k), wsp(lwsp), V((k*(k+1))/2), w(k), Phi(k,k), Lambda(k), &
         & P(k,k), invP(k,k), sig_x((k*(k+1))/2), theta(k)
    complex(c_double_complex), target :: zwsp(lzwsp)
    target wsp, Phi
    real(c_double), pointer :: tmp(:,:), sig(:,:)
    external dgemv
    if (lwsp < 12*k*k) then
      call rwarn("d0geouvwphi: workspace too small.")
    endif
    if (lzwsp < 8*k*k) then
      call rwarn("d0geouvwphi: z-workspace too small.")
    endif
    if (eigavail==0) then
       call zeiginv(A,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info)
       if (info /= 0) return
    end if
    tmp(1:k,1:k) => wsp(1:(k**2))
    call d0phi(t, k, P, invP, Lambda, tmp, zwsp(1:(k**2)))
    Phi(:,:) = tmp(:,:)
    do j = 1, k
       tmp(j,j) = tmp(j,j) - 1.0_c_double 
    enddo
    call dgemv('N',k,k,-1.0_c_double,tmp,k,theta,1,0.0_c_double,w,1) ! Compute w

    sig(1:k,1:k) => wsp(1:(k*k))
    call dlnunchol(sig_x,k,wsp(k*k+1),lwsp-k*k,sig,info)
    call ouv(t,k,sig,P,invP,Lambda,V,zwsp,lzwsp,wsp(k*k+1),lwsp-k*k) ! V stored in packed 'L' form.
    info = 0
  end subroutine

  ! Copy the real part of the complex symmetric matrix stored in `zA` in standard form
  ! into `rAP` stored in packed form. Strictly upper/lower part of zA is not referenced.
  recursive subroutine z2dtrttp(uplo,k,zA,rAP,wsp)
    complex(c_double_complex) zA
    character uplo
    dimension wsp(k**2), zA(k**2), rAP((k*(k+1))/2)
    external dtrttp
    wsp(1:(k**2)) = dble(real(zA))
    call dtrttp(uplo,k,wsp,k,rAP,info)
  end subroutine

  ! zwsp at least 2*k**2, wsp at least k**2. Output stored in V.
  recursive subroutine ouv(t,k,sig,P,invP,Lambda,V,zwsp,lzwsp,wsp,lwsp) bind(C, name="ouv_")
    real(c_double) t,sig,V,wsp
    complex(c_double_complex) P, invP, Lambda, zwsp
    integer(c_int) k,lzwsp,lwsp
    dimension sig(k,k), P(k,k), invP(k,k), Lambda(k), V((k*(k+1))/2), zwsp(lzwsp), wsp(lwsp)
    target zwsp
    complex(c_double_complex), pointer :: ztmp(:,:), ztmp2(:,:)
    external zwarn
    if (lwsp < k*k) then
      call rwarn("ouv: workspace too small.")
    endif
    if (lzwsp < 2*k*k) then
      call rwarn("ouv: z-workspace too small.")
    endif
    ztmp(1:k,1:k)  => zwsp(1:)
    ztmp2(1:k,1:k) => zwsp((k**2+1):(2*(k**2)))
    ztmp = cmplx(0._c_double, 0._c_double, kind(1._c_double))
    do j=1,k
       do i=1,k
          call zI0(t,-(Lambda(i)+Lambda(j)),1.0_c_double,0.0_c_double,ztmp(i,j))
       enddo
    enddo
    ztmp2 = ztmp*matmul(invP,matmul(sig,transpose(invP)))
    ztmp2 = matmul(P,matmul(ztmp2,transpose(P)))
    call z2dtrttp('L',k,ztmp2,V,wsp)
  end subroutine

  ! Given a Jacobian in a basis defined by P, return the Jacobian in standard basis
  ! zwsp: k^2
  recursive subroutine chgbasis(D,P,invP,k,zwsp,out)
    complex(c_double_complex) P, invP, zwsp, D
    dimension D(k**2,k**2), P(k,k), invP(k,k), zwsp(k**2), out(k**2,k**2)
    external zgeru
    m = 1
    do j=1,k
       do i=1,k
          zwsp = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,i),1,P(j,1),k,zwsp,k)
          out(:,m) = dble(real(matmul(D, zwsp)))
          m = m+1
       end do
    end do
  end subroutine

  ! Same as chgbasis but returns the Jacobian in a packed (lower-triangular) format.
  ! See the dim. of `out'. D needs to be symmetric or result is undefined.
  ! zwsp at least k^2, wsp at least k^2.
  subroutine dpchgbasis(D,P,invP,k,zwsp,wsp,out)
    complex(c_double_complex) P, invP, zwsp, D
    dimension D(k**2,k**2), P(k,k), invP(k,k), zwsp(k**2), wsp(k**2), out((k*(k+1))/2,k**2)
    external zgeru
    m = 1
    do j=1,k
       do i=1,k
          zwsp = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,i),1,P(j,1),k,zwsp,k)
          zwsp = matmul(D, zwsp)
          call z2dtrttp('L',k,zwsp,out(:,m),wsp)
          m = m+1
       end do
    end do
  end subroutine

  ! lzwsp at least 2*k^2. 
  recursive subroutine realhesschgbasis(X,P,invP,m,k,zwsp,lzwsp,out) bind(C, name='realhesschgbasis_')
    real(c_double) out
    complex(c_double_complex) X, P, invP, zwsp, d
    integer(c_int) :: lzwsp, a1, a2, b1, b2, w1, w2, xi1, xi2, j, m, k
    dimension X(m,k**2,k**2), P(k,k), invP(k,k), zwsp(lzwsp), out(m,k**2,k**2)
    target :: zwsp
    complex(c_double_complex), pointer :: solPUBP(:,:), solPUAP(:,:)
    if (lzwsp < 2*k*k) then
      call rwarn("realhesschgbasis: z-workspace too small.")
    endif
    solPUAP(1:k,1:k) => zwsp(1:(k**2))
    solPUBP(1:k,1:k) => zwsp(((k**2)+1):(2*(k**2)))
    out     = 0._c_double
    do a2 = 1,k
       do a1 = 1,k
          solPUAP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,a1),1,P(a2,1),k,solPUAP,k)
          b1 = a1;   b2 = a2;
20        solPUBP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,b1),1,P(b2,1),k,solPUBP,k)
          do xi2 = 1,k
             do xi1 = 1,k
                do w2 = 1,k
                   do w1 = 1,k
                      d = solPUBP(xi1,xi2) * solPUAP(w1,w2)
                      do j = 1,m
                         out(j,a1+(a2-1)*k,b1+(b2-1)*k) = out(j,a1+(a2-1)*k,b1+(b2-1)*k) + &
                              & dble(real(d * X(j,w1+(w2-1)*k,xi1+(xi2-1)*k)))
                      enddo
                   enddo
                enddo
             enddo
          enddo
          if (b1 < k) then
             b1 = b1 + 1
             goto 20
          endif
          if (b2 < k) then
             b1 = 1
             b2 = b2 + 1
             goto 20
          endif
       enddo
    enddo
    ! Copy the values of out(:,(a1,a2),(b1,b2)), for a1*(a2-1)*k <= b1*(b2-1)*k to
    ! out(:,(b1,b2),(a1,a2)) a1*(a2-1)*k > b1*(b2-1)*k
    do a2 = 1,k
       do a1 = 1,k
          b1 = a1;   b2 = a2;
25        b1 = b1 + 1
          if (b1 > k) goto 40
22        do j = 1,m
             out(j,b1+(b2-1)*k,a1+(a2-1)*k) = out(j,a1+(a2-1)*k,b1+(b2-1)*k)
          enddo
          goto 25
40        if (b2 < k) then
             b2 = b2 + 1
             b1 = 1
             goto 22
          endif
       enddo
    enddo
  end subroutine


  ! Same as realhesschgbasis, except that in this function we assume both out(j,:,:) being
  ! a symmetric matrix for each j, and out(:,a,b) is symmetric itself. In the return, out(:,a,b)
  ! is in packed format but out(:,a,b) isn't packed.
  recursive subroutine dprealsymhesschgbasis(X,P,invP,sqrtm,k,zwsp,lzwsp,out) bind(C, name='dprealsymhesschgbasis_')
    real(c_double) out
    complex(c_double_complex) X, P, invP, zwsp, d
    integer(c_int) :: sqrtm, lzwsp, a1, a2, b1, b2, w1, w2, xi1, xi2, i,j,k,c
    dimension X(sqrtm**2,k**2,k**2), P(k,k), invP(k,k), zwsp(lzwsp), out((sqrtm*(sqrtm+1))/2,k**2,k**2)
    target :: zwsp
    complex(c_double_complex), pointer :: solPUBP(:,:), solPUAP(:,:)
    solPUAP(1:k,1:k) => zwsp(1:(k**2))
    solPUBP(1:k,1:k) => zwsp(((k**2)+1):(2*(k**2)))
    out     = 0._c_double
    if (lzwsp < 2*(k**2)) then
      call rwarn("dprealsymhesschgbasis: z-workspace too small.")
    endif
    do a2 = 1,k
       do a1 = 1,k
          solPUAP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,a1),1,P(a2,1),k,solPUAP,k)
          b1 = a1;   b2 = a2;
20        solPUBP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,b1),1,P(b2,1),k,solPUBP,k)
          do xi2 = 1,k
             do xi1 = 1,k
                do w2 = 1,k
                   do w1 = 1,k
                      d = solPUBP(xi1,xi2) * solPUAP(w1,w2)
                      c = 1
                      do j = 1,sqrtm
                         do i = j,sqrtm
                            out(c,a1+(a2-1)*k,b1+(b2-1)*k) = out(c,a1+(a2-1)*k,b1+(b2-1)*k) + &
                                 & dble(real(d * X(i+(j-1)*k,w1+(w2-1)*k,xi1+(xi2-1)*k)))
                            c=c+1
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          enddo
          if (b1 < k) then
             b1 = b1 + 1
             goto 20
          endif
          if (b2 < k) then
             b1 = 1
             b2 = b2 + 1
             goto 20
          endif
       enddo
    enddo
    ! Copy the values of out(:,(a1,a2),(b1,b2)), for a1*(a2-1)*k <= b1*(b2-1)*k to
    ! out(:,(b1,b2),(a1,a2)) a1*(a2-1)*k > b1*(b2-1)*k
    do a2 = 1,k
       do a1 = 1,k
          b1 = a1;   b2 = a2;
25        b1 = b1 + 1
          if (b1 > k) goto 40
22        c = 1
          do j = 1,sqrtm
             do i = j,sqrtm
                out(c,b1+(b2-1)*k,a1+(a2-1)*k) = out(c,a1+(a2-1)*k,b1+(b2-1)*k)
                c=c+1
             enddo
          enddo
          goto 25
40        if (b2 < k) then
             b2 = b2 + 1
             b1 = 1
             goto 22
          endif
       enddo
    enddo
  end subroutine

  ! Same as realhesschgbasis, except that in this function we don't assume the out(j,:,:) being
  ! a symmetric matrix for each j.
  recursive subroutine realdblasymchgbasis(X,P,invP,m,k,zwsp,lzwsp,out) bind(C, name='realdblasymchgbasis_')
    real(c_double) out
    complex(c_double_complex) X, P, invP, zwsp, d
    integer(c_int) :: lzwsp, a1, a2, b1, b2, w1, w2, xi1, xi2, j, m, k
    dimension X(m,k**2,k**2), P(k,k), invP(k,k), zwsp(lzwsp), out(m,k**2,k**2)
    target :: zwsp
    complex(c_double_complex), pointer :: solPUBP(:,:), solPUAP(:,:)
    solPUAP(1:k,1:k) => zwsp(1:(k**2))
    solPUBP(1:k,1:k) => zwsp(((k**2)+1):(2*(k**2)))
    out     = 0._c_double
    do a2 = 1,k
       do a1 = 1,k
          solPUAP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,a1),1,P(a2,1),k,solPUAP,k)
          do b2 = 1,k
             do b1 = 1,k
                solPUBP = cmplx(0._c_double, 0._c_double, kind(1._c_double))
                call zgeru(k,k,cmplx(1.0_c_double, 0._c_double, kind(1._c_double)),invP(1,b1),1,P(b2,1),k,solPUBP,k)
                do xi2 = 1,k
                   do xi1 = 1,k
                      do w2 = 1,k
                         do w1 = 1,k
                            d = solPUBP(xi1,xi2) * solPUAP(w1,w2)
                            do j = 1,m
                               out(j,a1+(a2-1)*k,b1+(b2-1)*k) = out(j,a1+(a2-1)*k,b1+(b2-1)*k) + &
                                    & dble(real(d * X(j,w1+(w2-1)*k,xi1+(xi2-1)*k)))
                            enddo
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine

  subroutine deda(a,b,t,Psi,H,k,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,eigavail,info) bind(C, name="deda_")
    implicit none
    integer(c_int) :: k, lwsp, lzwsp, eigavail, info
    real(c_double) :: a,b,t,Psi,H,out,wsp
    complex(c_double_complex) :: Lambda, P, invP, zwsp
    complex(c_double_complex) :: z, c, q, PsiB
    complex(c_double_complex), pointer :: X(:,:), D(:,:), thisD(:,:), wgt(:)
    dimension H(k,k), P(k,k), invP(k,k), Lambda(k), wsp(lwsp), zwsp(lzwsp), out(k**2,k**2), Psi(k,k), PsiB(k,k)
    target :: zwsp, wsp
    integer(c_int) :: n, m, didx, i, j
    if (eigavail==0) then
       call zeiginv(H,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info)
       if (info /= 0) return
    end if
    D(1:(k**2),1:(k**2))    => zwsp(1:)
    X(1:k,1:k)              => zwsp((k**4+1):)
    wgt(1:(k**2))           => zwsp(((k**4)+k**2+1):)
    didx = 0
    PsiB = matmul(invP,matmul(Psi,P))
    c = cmplx(0._c_double, 0._c_double, kind(1._c_double))
    do n = 1,k
       do m = 1,k
          X(:,:) = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          q = Lambda(n) - Lambda(m)
          do j = 1,k
             z   = a*Lambda(m) + b*Lambda(j)
             if (mod2small(q) == 1) then
                call zI1(t, z, a, 0.0_c_double, c)
                X(m,j) = X(m,j) + PsiB(n,j)*c
             else
                call zI0(t, a*Lambda(n)+b*Lambda(j), 1.0_c_double, 0.0_c_double, c)
                call zI0(t, z,                      -1.0_c_double, 1.0_c_double, c)
                X(m,j) = X(m,j) + PsiB(n,j)*c/q
             end if
          end do
          do i = 1,k
             z   = a*Lambda(i) + b*Lambda(n)
             if (mod2small(q) == 1) then
                call zI1(t, z, b, 0.0_c_double, c)
                X(i,n) = X(i,n) + PsiB(i,m)*c
             else
                call zI0(t, a*Lambda(i)+b*Lambda(m), 1.0_c_double, 0.0_c_double, c)
                call zI0(t, z,                      -1.0_c_double, 1.0_c_double, c)
                X(i,n) = X(i,n) + PsiB(i,m)*c/(-q)
             end if
          end do
          didx = didx + 1
          thisD(1:k,1:k) => D(:,didx)
          thisD = matmul(P, matmul(X, invP))
       end do
    end do
    call chgbasis(D,P,invP,k,wgt,out)
    info = 0
  end subroutine

  !! wsp at least 2*(k^2); zwsp at least (k^4+2*k^2)
  recursive subroutine dvda(t,Psi,H,k,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,eigavail,info) bind(C, name="dvda_")
    real(c_double) t, Psi, H, out, wsp
    integer(c_int) k,lwsp,lzwsp,eigavail,info
    complex(c_double_complex) P,invP,Lambda,zwsp
    dimension Psi(k,k), H(k,k), P(k,k), invP(k,k), Lambda(k), wsp(lwsp), zwsp(lzwsp), out(((k*(k+1))/2),k**2)
    target :: zwsp, wsp
    complex(c_double_complex), pointer :: D(:,:), thisD(:,:), X(:,:), PsiB(:,:)
    complex(c_double_complex) :: z,c
    integer(c_int) a1,a2
    external rwarn
    if (lwsp < 2*k*k) then
      call rwarn("dvda: workspace too small.")
    endif
    if (lzwsp < k*k*k*k+2*k*k) then
      call rwarn("dvda: z-workspace too small.")
    endif
    if (eigavail==0) then
       call zeiginv(H,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info)
       if (info /= 0) return
    end if
    D(1:(k**2),1:(k**2)) => zwsp(1:(k**4))
    X(1:k,1:k)           => zwsp((k**4+1):(k**4+k**2))
    PsiB(1:k,1:k)        => zwsp((k**4+k**2+1):(k**4+2*(k**2)))
    PsiB = matmul(invP, matmul(Psi, transpose(invP)))
    idx = 1
    do a2 = 1,k
       do a1 = 1,k
          X = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          z = Lambda(a1) - Lambda(a2)
          c = cmplx(0._c_double, 0._c_double, kind(1._c_double))
          if (mod2small(z) == 1) then
             do j = 1,k
                call zI1(t, -Lambda(a1)-Lambda(j), 1.0_c_double, 0.0_c_double, c)
                X(a1,j) = PsiB(a2,j)*c
             end do
          else
             do j = 1,k
                call zI0(t, -Lambda(a2)-Lambda(j),  1.0_c_double, 0.0_c_double, c)
                call zI0(t, -Lambda(a1)-Lambda(j), -1.0_c_double, 1.0_c_double, c)
                X(a1,j) = PsiB(a2,j) * c/z
             end do
          end if
          thisD(1:k,1:k) => D(:,idx)
          thisD = - matmul(P, matmul(X + transpose(X), transpose(P)))
          idx = idx + 1
       end do
    end do
    call dpchgbasis(D,P,invP,k,zwsp((k**4+1):),wsp,out)
    info = 0
  end subroutine

  ! wsp: 3*k^2, lzwsp: 2*k^2
  recursive subroutine dvdsigx(t,k,sig_x,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,info) bind(C, name="dvdsigx_")
    real(c_double) t, sig_x, out, wsp
    integer(c_int) k,lwsp,lzwsp,info
    complex(c_double_complex) P,invP,Lambda,zwsp
    dimension sig_x((k*(k+1))/2),P(k,k),invP(k,k),Lambda(k),out((k*(k+1))/2,(k*(k+1))/2),zwsp(lzwsp),wsp(lwsp)
    pointer :: UijLT(:,:), sig_x_unpk(:,:)
    target wsp
    external dtpttr
    if (lwsp < 3*k*k) then
      call rwarn("dvdsigx: workspace too small.")
    endif
    if (lzwsp < 2*k*k) then
      call rwarn("dvdsigx: z-workspace too small.")
    endif
    UijLt(1:k,1:k)      => wsp(1:)
    sig_x_unpk(1:k,1:k) => wsp((k**2+1):)
    sig_x_unpk(1:k,1:k) = 0.0_c_double
    call dtpttr('L',k,sig_x,sig_x_unpk,k,info)
    do i=1,k
       sig_x_unpk(i,i) = exp(sig_x_unpk(i,i))
    enddo
    m=1
    do j=1,k
       do i=j,k
          UijLt(:,:) = 0.0_c_double
          ! Can be made faster... We don't need to unpack -> assign. Only a loop for assignment is needed.
          UijLt(i,:) = sig_x_unpk(:,j)
          UijLt(:,i) = UijLt(:,i) + UijLt(i,:)
          call ouv(t,k,UijLt,P,invP,Lambda,out(1,m),zwsp,lzwsp,wsp(2*k**2+1),lwsp-2*k**2)
          ! Account for the logged diagonal.
          if (i == j) then
             out(:,m) = out(:,m) * sig_x_unpk(i,i)
          endif
          m=m+1
       enddo
    enddo
  end subroutine

  ! wsp at least k**2. zwsp at least k**2.
  recursive subroutine dwdtheta(t,k,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp) bind(C, name="dwdtheta_")
    real(c_double) t, out, wsp
    complex(c_double_complex) P, invP, Lambda, zwsp
    integer(c_int) k, lwsp, lzwsp
    dimension P(k,k), invP(k,k),Lambda(k), wsp(lwsp), out(k,k), zwsp(lzwsp)
    target wsp
    real(c_double), pointer :: tmp(:,:)
    external rwarn
    if (lwsp < k*k) then
      call rwarn("dwdtheta: workspace too small.")
    endif
    if (lzwsp < k*k) then
      call rwarn("dwdtheta: z-workspace too small.")
    endif
    tmp(1:k,1:k) => wsp(1:(k**2))
    tmp(:,:) = 0.0_c_double
    call d0phi(t, k, P, invP, Lambda, tmp, zwsp(1:(k**2)))
    do j = 1,k
       tmp(j,j) = tmp(j,j) - 1.0_c_double 
    enddo
    out(:,:) = -tmp(:,:)
  end subroutine

  ! zwsp at least k^4+k^2+2
  recursive subroutine dphida(t,k,P,invP,Lambda,out,zwsp,lzwsp) bind(C, name="dphida_")
    real(c_double) t, out
    integer(c_int) k, lzwsp
    complex(c_double_complex) P,invP,Lambda,zwsp
    dimension P(k,k), invP(k,k), Lambda(k), zwsp(lzwsp), out(k**2,k**2)
    target zwsp
    complex(c_double_complex), pointer :: D(:,:), thisD(:,:), c, z
    external rwarn, zgeru
    if (lzwsp < k*k*k*k+k*k+2) then
      call rwarn("dphida: z-workspace too small.")
    endif
    D(1:(k**2),1:(k**2)) => zwsp(1:)
    c                    => zwsp(k**4+1)
    z                    => zwsp(k**4+2)
    D(:,:) = cmplx(0._c_double, 0._c_double, kind(1._c_double))
    m = 1
    do j = 1,k
       do i = 1,k
          z = Lambda(i) - Lambda(j)
          if (mod2small(z) == 1) then
             c = t * exp(-Lambda(i)*t)
          else
             c = (exp(-Lambda(j)*t) - exp(-Lambda(i)*t))/z
          endif
          thisD(1:k,1:k) => D(:,m)
          call zgeru(k,k,cmplx(-1.0_c_double, 0._c_double, kind(1._c_double)),P(1,i),1,invP(j,1),k,thisD,k)
          thisD = c*thisD
          m=m+1
       enddo
    enddo
    call chgbasis(D,P,invP,k,zwsp(k**4+3),out)
  end subroutine

  recursive subroutine dwda(k,dphidaout,theta,out) bind(C,name="dwda_")
    real(c_double) dphidaout, theta, out
    integer(c_int) k
    dimension dphidaout(k**2,k**2), theta(k), out(k,k**2)
    external dgemv
    do m=1,k**2
       call dgemv('N',k,k,-1.0_c_double,dphidaout(1,m),k,theta,1,0.0_c_double,out(1,m),1)
    enddo
  end subroutine

  ! Function for calculating the simple OU jacobian.
  recursive subroutine ougejac(t,k,hts,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,eigavail,djac,info) bind(C,name="ougejac_")
    real(c_double) t, hts, wsp,djac
    complex(c_double_complex) P, invP, Lambda, zwsp
    integer(c_int) k,lwsp,lzwsp,eigavail,info
    dimension hts(k**2+k+(k*(k+1))/2), P(k,k), invP(k,k), &
         & Lambda(k), wsp(lwsp), zwsp(lzwsp), djac(k**2+k+(k*(k+1))/2, k**2+k+(k*(k+1))/2)
    target zwsp, wsp, hts
    real(c_double), pointer :: H(:,:), theta(:), sig_x(:)
    H(1:k,1:k)           => hts
    theta(1:k)           => hts((k**2+1):)
    sig_x(1:((k*(k+1))/2)) => hts((k**2+k+1):)
    if (eigavail==0) then
       ! wsp: 2*k^2, zwsp: 2*k^2
       call zeiginv(H,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info)
       if (info /= 0) return
    end if
    djac(:,:) = 0.0_c_double
    ! Output to wsp(1:k**4), zwsp: k^4+k^2+2 > 2*k^2.
    call dphida(t,k,P,invP,Lambda,wsp(1:k**4),zwsp,lzwsp)
    m=1
    do j=1,k**2
       do i=1,k**2
          djac(i,j) = wsp(m)
          m=m+1
       enddo
    enddo
!    do ixx=1,k**2+k+(k*(k+1))/2
!       do iyy=1,k**2+k+(k*(k+1))/2
!          if (ISNAN(djac(ixx,iyy))) print *, "NaN -- 1\n"
!       enddo
!    enddo
    ! dwda: wsp(1:k**4) contains from previous step. out to wsp((k**4+1):(k**4+k*k**2)).
    ! Therefore uses wsp at least of length k**4+k*k**2.
    call dwda(k,wsp(1:k**4),theta,wsp(k**4+1))
    m=1
    do j=1,k**2
       do i=1,k
          djac(k**2+i,j) = wsp(k**4+m)
          m=m+1
       enddo
    enddo
!    do ixx=1,k**2+k+(k*(k+1))/2
!       do iyy=1,k**2+k+(k*(k+1))/2
!          if (ISNAN(djac(ixx,iyy))) print *, "NaN -- 1\n"
!       enddo
!    enddo
    ! output to wsp(1:(k**2)), wsp at least of size k**2 so wsp bumped to 2*k**2.
    ! zwsp at least k**2.
    call dwdtheta(t,k,P,invP,Lambda,wsp,wsp((k**2+1):),lwsp-k**2, zwsp, lzwsp)
    m=1
    do j=1,k
       do i=1,k
          djac(k**2+i,k**2+j) = wsp(m)
          m=m+1
       enddo
    enddo
!    do ixx=1,k**2+k+(k*(k+1))/2
!       do iyy=1,k**2+k+(k*(k+1))/2
!          if (ISNAN(djac(ixx,iyy))) print *, "NaN -- 1\n"
!       enddo
!    enddo
    ! Output to wsp(1:k**2), uses wsp(k**2+1:2*k**2). So wsp at least 2*k**2
    call dlnunchol(sig_x,k,wsp(((k**2)+1):),k**2,wsp(1:(k**2)),info)
    ! zwsp: k^4+2*k^2.
    ! wsp(1:(k**2)) is input, Out to wsp((k**2+1):((k*(k+1))/2)*k**2). tmp of size 2*k^2 after that.
    ! so wsp is at least k^2+((k*(k+1))/2)*k^2+2*k^2.
    call dvda(t,wsp(1:(k**2)),H,k,P,invP,Lambda,wsp((k**2+1):),wsp((k**2+((k*(k+1))/2)*k**2+1):),&
         & lwsp-(k**2+((k*(k+1))/2)*k**2),zwsp,lzwsp,1_c_int,info)
    if (info /= 0) return
    m=1
    do j=1,k**2
       do i=1,((k*(k+1))/2)
          djac(k**2+k+i,j) = wsp(k**2+m)
          m=m+1
       enddo
    enddo
!    do ixx=1,k**2+k+(k*(k+1))/2
!       do iyy=1,k**2+k+(k*(k+1))/2
!          if (ISNAN(djac(ixx,iyy))) print *, "NaN -- 1\n"
!       enddo
!    enddo
    ! output stored at first (k*(k+1)/2)^2 block. tmp needs 3*k^2 so it needs 3*k^2+(k*(k+1)/2)^2
    call dvdsigx(t,k,sig_x,P,invP,Lambda,wsp(1:((k*(k+1))/2)**2),&
         & wsp((((k*(k+1))/2)**2+1):),lwsp-((k*(k+1))/2)**2,zwsp,lzwsp,info)
    m=1
    do j=1,(k*(k+1))/2
       do i=1,((k*(k+1))/2)
          djac(k**2+k+i,k**2+k+j) = wsp(m)
          m=m+1
       enddo
    enddo
!    do ixx=1,k**2+k+(k*(k+1))/2
!       do iyy=1,k**2+k+(k*(k+1))/2
!          if (ISNAN(djac(ixx,iyy))) print *, "NaN -- 1\n"
!       enddo
!    enddo
  end subroutine

  recursive subroutine dchnunchol(DFDH, L, m, k, DFDL) bind(C, name="dchnunchol_")
    real(c_double) DFDH, L, DFDL
    integer(c_int) m, k
    dimension DFDH(m,k**2), L((k*(k+1))/2), DFDL(m,(k*(k+1))/2)
    n=1
    do j=1,k
       do i=j,k
          do ix = j,k
             DFDL(:,n) = DFDL(:,n)+(DFDH(:,ix+(i-1)*k)+DFDH(:,i+(ix-1)*k))*L(iijtouplolidx(k,ix,j))
          enddo
          n=n+1
       enddo
    enddo
  end subroutine
  
  recursive subroutine dlnchnunchol(DFDH, L, m, k, DFDL) bind(C, name="dlnchnunchol_")
    real(c_double) DFDH, L, DFDL
    integer(c_int) m, k
    dimension DFDH(m,k**2), L((k*(k+1))/2), DFDL(m,(k*(k+1))/2)
    n=1
    do j=1,k
       do i=j,k
          do ip=1,m
             DFDL(ip,n) = DFDL(ip,n) + DFDH(ip,i+(j-1)*k)*exp(L(iijtouplolidx(k,j,j)))
             iy = j + 1
11           if (iy > k) goto 10
             DFDL(ip,n) = DFDL(ip,n) + DFDH(ip,i+(iy-1)*k)*L(iijtouplolidx(k,iy,j))
             iy = iy + 1
             goto 11
10           continue

             DFDL(ip,n) = DFDL(ip,n) + DFDH(ip,j+(i-1)*k)*exp(L(iijtouplolidx(k,j,j)))
             ix = j + 1
21           if (ix > k) goto 20
             DFDL(ip,n) = DFDL(ip,n) + DFDH(ip,ix+(i-1)*k)*L(iijtouplolidx(k,ix,j))
             ix = ix + 1
             goto 21
20           continue
          enddo
          if (i == j) then
             DFDL(:,n) = DFDL(:,n) * exp(L(iijtouplolidx(k,i,i)))
          endif
          n=n+1
       enddo
    enddo
  end subroutine

  ! Compute the integral of e^{av} I_0(v,b) I_0(v, c) w.r.t. v within [0,t].
  subroutine zitglei0i0 (t, za, zb, zc, zout) bind(C, name='zitglei0i0_')
    real(c_double) t
    complex(c_double_complex)  :: za, zb, zc, zout
    zout = cmplx(0._c_double,0._c_double,kind(1._c_double))
    if (mod2small(za) == 1) then
       if ((mod2small(zb) == 1) .and. (mod2small(zc) == 1)) then
          zout = (t/1.44224957030740838_c_double)**3.0_c_double
       else if ((mod2small(zb) == 1) .and. (mod2small(zc) == 0)) then
          call zI0(t, zc, 1.0_c_double, 0.0_c_double, zout)
          zout = (zout - t)/zc
       else if ((mod2small(zb) == 0) .and. (mod2small(zc) == 1)) then
          call zI0(t, zb, 1.0_c_double, 0.0_c_double, zout)
          zout = (zout - t)/zb
       else
          zout = t
          call zI0(t, zb+zc, 1.0_c_double, 1.0_c_double, zout)
          call zI0(t, zb,   -1.0_c_double, 1.0_c_double, zout)
          call zI0(t, zc,   -1.0_c_double, 1.0_c_double, zout)
          zout = zout/zb/zc
       endif
    else
       if ((mod2small(zb) == 1) .and. (mod2small(zc) == 1)) then
          call zI2(t, za, 1.0_c_double, 0.0_c_double, zout)
       else if ((mod2small(zb) == 1) .and. (mod2small(zc) == 0)) then
          call zI1(t, za+zc,  1.0_c_double, 0.0_c_double, zout)
          call zI1(t, za,    -1.0_c_double, 1.0_c_double, zout)
          zout = zout/zc
       else if ((mod2small(zb) == 0) .and. (mod2small(zc) == 1)) then
          call zI1(t, za+zb,  1.0_c_double, 0.0_c_double, zout)
          call zI1(t, za,    -1.0_c_double, 1.0_c_double, zout)
          zout = zout/zb
       else
          call zI0(t, za+zb+zc, 1.0_c_double, 0.0_c_double, zout)
          call zI0(t, za,       1.0_c_double, 1.0_c_double, zout)
          call zI0(t, za+zb,   -1.0_c_double, 1.0_c_double, zout)
          call zI0(t, za+zc,   -1.0_c_double, 1.0_c_double, zout)
          zout = zout/zb/zc
       endif
    endif
  end subroutine

  recursive subroutine zK0 (t, za, zb, alpha, beta, out)    bind(C, name="zK0_")
    real(c_double) t, alpha, beta
    complex(c_double_complex) za, zb, out, tmp
    tmp = cmplx(0._c_double,0._c_double,kind(1._c_double))
    if (mod2small(zb) == 1) then
       call zI1(t, za, alpha, beta, out)
    else
       call zI0(t, za+zb,  alpha, 0.0_c_double, tmp)
       call zI0(t, za,    -alpha, 1.0_c_double, tmp)
       out = beta * out + tmp/zb
    endif
  end subroutine

  recursive subroutine zK1 (t, za, zb, alpha, beta, out)    bind(C, name="zK1_")
    real(c_double) t, alpha, beta
    complex(c_double_complex) za, zb, out, tmp
    tmp = cmplx(0._c_double,0._c_double,kind(1._c_double))
    if (mod2small(zb) == 1) then
       call zI2(t, za, alpha/2._c_double, beta, out)
    else
       call zI0(t, za+zb, alpha, 0._c_double, tmp)
       call zI0(t, za,   -alpha, 1._c_double, tmp)
       tmp = tmp/zb
       call zI1(t, za+zb, alpha, -1._c_double, tmp)
       out = beta * out + tmp / zb
    endif
  end subroutine

  ! wsp at least 2*(k**2), zwsp at least (2*(k**2)+3*k)
  recursive subroutine hvhadir (t,Psi,H,k,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,eigavail,info) bind(C, name="hvhadir_")
    real(c_double) t, Psi, H, wsp
    integer(c_int) k,lwsp,lzwsp,eigavail,info
    complex(c_double_complex) P,invP,Lambda,zwsp,out,    d
    dimension Psi(k,k), H(k,k), P(k,k), invP(k,k), Lambda(k), wsp(lwsp), zwsp(lzwsp), &
         & out(k**2,k**2,k**2)
    target :: zwsp, wsp, out
    complex(c_double_complex), pointer :: PsiB(:,:), igterm(:), Ka(:), Kb(:), Z(:,:), thisout(:,:)
    integer(c_int) :: a1, a2, b1, b2
    external rwarn
    if (lwsp < 2*k*k) then
      call rwarn("hvhadir: workspace too small.")
    endif
    if (lzwsp < 2*k*k+3*k) then
      call rwarn("hvhadir: z-workspace too small.")
    endif
    PsiB(1:k,1:k) => zwsp(1:(k**2))
    igterm(1:k)   => zwsp(((k**2)+1):((k**2)+k))
    Ka(1:k)       => zwsp(((k**2)+k+1):((k**2)+2*k))
    Kb(1:k)       => zwsp(((k**2)+2*k+1):((k**2)+3*k))
    Z(1:k,1:k)    => zwsp(((k**2)+3*k+1):(2*(k**2)+3*k))
    if (eigavail==0) then
       call zeiginv(H,k,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,info)
       if (info /= 0) return
    end if
    PsiB = matmul(invP, matmul(Psi, transpose(invP)))
    do a2 = 1,k
       do a1 = 1,k
          do b2 = 1,k
             do b1 = 1,k
                Z(:,:) = cmplx(0._c_double,0._c_double,kind(1._c_double))
                if (a1 == b2) then
                   do j = 1,k
                      call zitglei0i0(t, -Lambda(b1)-Lambda(j), Lambda(b1)-Lambda(b2), Lambda(a1)-Lambda(a2), igterm(j))
                   enddo
                else
                   igterm(:) = cmplx(0._c_double,0._c_double,kind(1._c_double))
                endif
                Kb(:) = cmplx(0._c_double,0._c_double,kind(1._c_double))
                if (a1 == b2) then
                   d = Lambda(a1) - Lambda(b1)
                   if (mod2small(d) == 1) then
                      do j = 1,k
                         call zK1(t, -Lambda(b1)-Lambda(j), Lambda(b1)-Lambda(a2), 1._c_double, 0._c_double, Kb(j))
                      enddo
                   else
                      do j = 1,k
                         call zK0(t, -Lambda(b1)-Lambda(j), Lambda(a1)-Lambda(a2), 1._c_double, 0._c_double, Kb(j))
                         call zK0(t, -Lambda(b1)-Lambda(j), Lambda(b1)-Lambda(a2), -1._c_double, 1._c_double, Kb(j))
                         Kb(j) = Kb(j) / d
                      enddo
                   endif
                endif
                Ka(:) = cmplx(0._c_double,0._c_double,kind(1._c_double))
                if (a2 == b1) then
                   d = Lambda(b2) - Lambda(a2)
                   if (mod2small(d) == 1) then
                      do j = 1,k
                         call zK1(t, -Lambda(a1)-Lambda(j), Lambda(a1)-Lambda(b2), 1._c_double, 0._c_double, Ka(j))
                      enddo
                   else
                      do j = 1,k
                         call zK0(t, -Lambda(a1)-Lambda(j), Lambda(a1)-Lambda(a2), 1._c_double, 0._c_double, Ka(j))
                         call zK0(t, -Lambda(a1)-Lambda(j), Lambda(a1)-Lambda(b2), -1._c_double,1._c_double, Ka(j))
                         Ka(j) = Ka(j) / d
                      enddo
                   endif
                endif
                Z(b1,:)  = Z(b1,:) + PsiB(a2,:) * (igterm - Kb)
                Z(a1,:)  = Z(a1,:) + PsiB(b2,:) * Ka
                call zitglei0i0(t, -Lambda(b1)-Lambda(a1), Lambda(b1)-Lambda(b2), Lambda(a1)-Lambda(a2), igterm(1))
                Z(a1,b1) = Z(a1,b1)+ PsiB(a2,b2) * igterm(1)
                thisout(1:k,1:k) => out(1:(k**2),a1+(a2-1)*k,b1+(b2-1)*k)
                thisout = matmul(P, matmul(Z + transpose(Z), transpose(P)))
             enddo
          enddo
       enddo
    enddo
    info = 0
  end subroutine

  recursive subroutine mydtpttr (X, out, k)
    dimension X((k*(k+1))/2), out(1:k,1:k)
    do j=1,k
       do i=1,k
          out(i,j) = 0.
       enddo
    enddo
    n=1
    do j=1,k
       do i=j,k
          out(i,j) = X(n)
          n=n+1
       enddo
    enddo
  end subroutine

  ! wsp at least 2*(k**2), zwsp at least k^6 + 4*(k**2)+3*k
  recursive subroutine hvha (t,Psi,H,k,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,eigavail,info) bind(C, name="hvha_")
    real(c_double) t, Psi, H, out, wsp
    integer(c_int) k, lwsp, lzwsp, eigavail, info
    complex(c_double_complex) P, invP, Lambda, zwsp
    dimension Psi(k,k), H(k,k), P(k,k), invP(k,k), Lambda(k), wsp(lwsp), zwsp(lzwsp), &
         & out((k*(k+1))/2,k**2,k**2)
    target :: zwsp
    complex(c_double_complex), pointer :: zout(:,:,:)
    external rwarn
    if (lwsp < 2*k*k) then
      call rwarn("hvha: workspace too small.")
    endif
    if (lzwsp < 4*k*k+3*k) then
      call rwarn("hvha: z-workspace too small.")
    endif
    zout(1:(k**2),1:(k**2),1:(k**2)) => zwsp(1:(k**6))
    call hvhadir (t,Psi,H,k,P,invP,Lambda,zout,wsp,lwsp,zwsp(((k**6)+1):lzwsp),lzwsp-(k**6),eigavail,info)
    !   call realhesschgbasis(zout,P,invP,k**2,k,zwsp(((k**6)+1):lzwsp),lzwsp-(k**6),out)
    call dprealsymhesschgbasis(zout, P, invP, k, k, zwsp(((k**6)+1):lzwsp), lzwsp-(k**6), out)
  end subroutine

  ! wsp at least 4*(k^2), lzwsp at least k^4 + 2*k^2
  recursive subroutine hvdadl (t,H,k,sig_x,P,invP,Lambda,out,wsp,lwsp,zwsp,lzwsp,info) bind(C, name='hvdadl_')
    real(c_double) t, H, sig_x, out, wsp
    integer(c_int) k, lwsp, lzwsp, info
    complex(c_double_complex) P, invP, Lambda, zwsp
    dimension sig_x((k*(k+1))/2), H(k,k), P(k,k), invP(k,k), Lambda(k), out((k*(k+1))/2,k*k,(k*(k+1))/2),&
         & wsp(lwsp), zwsp(lzwsp)
    target :: wsp
    real(c_double), pointer :: myPsi(:,:), sig_x_unpk(:,:)
    external dtpttr
    myPsi(1:k,1:k)      => wsp(1:(k**2))
    myPsi(:,:) = 0.0_c_double
    sig_x_unpk(1:k,1:k) => wsp((k**2+1):)
    sig_x_unpk(:,:) = 0.0_c_double
    !
    ! COMPILER BUG IN OPEN64-C + GFORTRAN!! If I print out sig_x everything works.
    ! If I don't then NaN pops up out of nowhere. I don't even know where the NaN
    ! came from because I simply printing it out solves the problem... This is not
    ! a dtpttr blas problem because even if I implement my own dtpttr the problem
    ! is the same. I suspect something is wrong with their implementation of bind(C)?
    ! Or Fortran safe flags?? But if I use gfortran with gcc then everything is fine.
    !
    !    print *, sig_x
    !
    call dtpttr('L',k,sig_x,sig_x_unpk,k,info)
!    call mydtpttr(sig_x,sig_x_unpk,k)
    info = 0
    if (info /= 0) return
    do i=1,k
       sig_x_unpk(i,i) = exp(sig_x_unpk(i,i))
    enddo
    m = 1
    do j=1,k
       do i=j,k
          myPsi(i,:) = sig_x_unpk(:,j)
          myPsi(:,i) = myPsi(:,i) + myPsi(i,:)
          ! zwsp at least k^4+2*k^2
          call dvda(t, myPsi, H, k, P, invP, Lambda, out(:,:,m), wsp((2*(k**2)+1):lwsp), lwsp-(2*(k**2)), &
               & zwsp, lzwsp, 1_c_int, info)
          if (info /= 0) return
          if (i == j) then
             out(:,:,m) = out(:,:,m) * sig_x_unpk(i,i)
          endif
          m=m+1
          myPsi(i,:) = 0.0_c_double
          myPsi(:,i) = 0.0_c_double
       enddo
    enddo
    info = 0_c_int
  end subroutine

  recursive subroutine hwdthetada (k, dphidaout, out) bind(C, name="hwdthetada_")
    integer(c_int) k
    real(c_double) dphidaout, out
    dimension dphidaout(k**2,k**2), out(k,k,k**2)
    out = - reshape(dphidaout, [k,k,k**2])
  end subroutine

  ! lwsp at least (k^6) + 2*(k^2) + 3
  recursive subroutine hphiha (t,ku,P,invP,Lambda,out,zwsp,lzwsp,info) bind(C, name="hphiha_")
    real(c_double) t, out
    integer(c_int) lzwsp,  a1,a2,b1,b2, info, ku
    complex(c_double_complex) P, invP, Lambda, zwsp
    dimension P(ku,ku), invP(ku,ku), Lambda(ku), out(ku**2,ku**2,ku**2), zwsp(lzwsp)
    target zwsp
    complex(c_double_complex), pointer :: dirh(:,:,:), I01, I02, fac1
    external rwarn
    if (lzwsp < ku*ku*ku*ku*ku*ku + 3 + 2*ku*ku) then
      call rwarn("hphiha: z-workspace too small.")
    endif

    dirh(1:(ku**2),1:(ku**2),1:(ku**2)) => zwsp(1:(ku**6))
    dirh = cmplx(0.0_c_double, 0.0_c_double, kind(1._c_double))
    I01  => zwsp((ku**6)+1)
    I01  = cmplx(0.0_c_double, 0.0_c_double, kind(1._c_double))
    I02  => zwsp((ku**6)+2)
    I02  = cmplx(0.0_c_double, 0.0_c_double, kind(1._c_double))
    fac1 => zwsp((ku**6)+3)
    fac1 = cmplx(0.0_c_double, 0.0_c_double, kind(1._c_double))

    do a2 = 1,ku
       do a1 = 1,ku
          do b1 = 1,ku
             !! Assume b2 == a1 here.
             call zI0(t, Lambda(b1)-Lambda(a1), 1.0_c_double, 0.0_c_double, I01)
             call zI0(t, Lambda(a1)-Lambda(a2), 1.0_c_double, 0.0_c_double, I02)
             fac1 = I01 * I02
             call zK0(t, Lambda(b1)-Lambda(a2), Lambda(a1)-Lambda(b1), -1.0_c_double, 1.0_c_double, fac1)
             fac1 = fac1 * exp(-Lambda(b1)*t)
             do j = 1,ku
                do i = 1,ku
                   dirh(i+(j-1)*ku,a1+(a2-1)*ku,b1+(a1-1)*ku) = P(i,b1) * invP(a2,j) * fac1
                enddo
             enddo
          enddo
          do b2 = 1,ku
             !! Assume b1 == a2 here.
             fac1 = cmplx(0.0_c_double, 0.0_c_double, kind(1._c_double))
             call zK0(t, Lambda(a1)-Lambda(b2), Lambda(b2)-Lambda(a2), 1.0_c_double, 0.0_c_double, fac1)
             fac1 = fac1 * exp(-Lambda(a1)*t)
             do j = 1,ku
                do i = 1,ku
                   dirh(i+(j-1)*ku,a1+(a2-1)*ku,a2+(b2-1)*ku) = dirh(i+(j-1)*ku,a1+(a2-1)*ku,a2+(b2-1)*ku) + &
                        P(i,a1) * invP(b2,j) * fac1
                enddo
             enddo
          enddo
       enddo
    enddo
    call realhesschgbasis(dirh, P, invP, ku**2, ku, zwsp(((ku**6)+4):lzwsp), lzwsp-((ku**6)+3), out)
    info = 0_c_int
  end subroutine

  recursive subroutine hwha(k,hphihaout,theta,out) bind(C,name="hwha_")
    real(c_double) hphihaout,theta,out
    integer(c_int) k
    dimension hphihaout(k**2,k**2,k**2), theta(k), out(k,k**2,k**2)
    external dgemv
    do n=1,k**2
       do m=1,k**2
          out(:,m,n) = - reshape(matmul(reshape(hphihaout(:,m,n), [k,k]), reshape(theta, [k,1])), [k])
          !call dgemv('N',k,k,-1.0_c_double,hphihaout(1,m,n),k,theta,1,0.0_c_double,out(1,m,n),1)
       enddo
    enddo
  end subroutine

  ! zwsp at least 2*k^2, wsp at least 4*(k^2)
  recursive subroutine hvhl(t, k, sig_x, P, invP, Lambda, wsp, lwsp, zwsp, lzwsp, out) bind(C,name="hvhl_")
    real(c_double) t, sig_x, wsp, out
    integer(c_int) k,lwsp, lzwsp
    complex(c_double_complex) zwsp, P, invP, Lambda
    dimension sig_x((k*(k+1))/2), out((k*(k+1))/2,(k*(k+1))/2,(k*(k+1))/2), P(k,k), invP(k,k), Lambda(k), &
         & wsp(lwsp), zwsp(lzwsp)
    target :: wsp
    real(c_double), pointer :: myPsi(:,:), sig_x_unpk(:,:), UijLt(:,:)
    external dtpttr
    if (lzwsp < 2*k*k) then
      call rwarn("hvhl: z-workspace too small.")
    endif
    if (lwsp < 4*k*k) then
      call rwarn("hvhl: workspace too small.")
    endif
    myPsi(1:k,1:k)      => wsp(1:(k**2))
    myPsi = 0.0_c_double
    sig_x_unpk(1:k,1:k) => wsp((k**2+1):(2*(k**2)))
    sig_x_unpk = 0.0_c_double
    UijLt(1:k,1:k)      => wsp((2*(k**2)+1):(3*(k**2)))
    call dtpttr('L',k,sig_x,sig_x_unpk,k,info)
    if (info /= 0) return
    do i=1,k
       sig_x_unpk(i,i) = exp(sig_x_unpk(i,i))
    enddo

    iij = 1
    do j=1,k
       do i=j,k
          UijLt(:,:) = 0.0_c_double
          UijLt(i,:) = sig_x_unpk(:,j)
          UijLt(:,i) = UijLt(:,i) + UijLt(i,:)
          imn = 1
          do n=1,k
             do m=n,k
                myPsi(:,:) = 0.0_c_double
                if (j == n) then
                   myPsi(i,m) = 1._c_double
                   myPsi(m,i) = myPsi(m,i) + 1._c_double
                   if (i == j)              myPsi = myPsi * sig_x_unpk(i,i)
                   if (m == n)              myPsi = myPsi * sig_x_unpk(m,m)
                   if (i == n .and. m == n) myPsi = myPsi + UijLt * sig_x_unpk(i,i)
                endif
                call ouv(t,k,myPsi,P,invP,Lambda,out(:,imn,iij),&
                     & zwsp,lzwsp,wsp((3*(k**2)+1):lwsp),lwsp-3*(k**2))
                imn = imn+1
             enddo
          enddo
          iij = iij+1
       enddo
    enddo
  end subroutine

  recursive subroutine houchnsymh (Horig, m, k, nparorig, ithis, out)  bind(C, name='houchnsymh_')
    real(c_double) Horig, out
    integer(c_int) m, k, nparorig, ithis
    dimension Horig(m,nparorig,nparorig), out(m,nparorig-k*k+(k*(k+1))/2,nparorig-k*k+(k*(k+1))/2)
    ! Me versus me
    idxb = 1
    do jb = 1,k
       do ib = jb,k
          idxa = 1
          do ja = 1,k
             do ia = ja,k
                if (ia==ja) then
                   if (ib==jb) then
                      do im = 1,m
                         out(im,ithis+idxa,ithis+idxb) = Horig(im,ithis+ia+(ja-1)*k,ithis+ib+(jb-1)*k)
                      enddo
                   else
                      do im = 1,m
                         out(im,ithis+idxa,ithis+idxb) = Horig(im,ithis+ia+(ja-1)*k,ithis+ib+(jb-1)*k) &
                              & + Horig(im,ithis+ia+(ja-1)*k,ithis+jb+(ib-1)*k)
                      enddo
                   endif
                else
                   if (ib==jb) then
                      do im = 1,m
                         out(im,ithis+idxa,ithis+idxb) = Horig(im,ithis+ia+(ja-1)*k,ithis+ib+(jb-1)*k) &
                              & + Horig(im,ithis+ja+(ia-1)*k,ithis+ib+(jb-1)*k)
                      enddo
                   else
                      do im = 1,m
                         out(im,ithis+idxa,ithis+idxb) = Horig(im,ithis+ia+(ja-1)*k,ithis+ib+(jb-1)*k) &
                              & + Horig(im,ithis+ja+(ia-1)*k,ithis+ib+(jb-1)*k) &
                              & + Horig(im,ithis+ia+(ja-1)*k,ithis+jb+(ib-1)*k) &
                              & + Horig(im,ithis+ja+(ia-1)*k,ithis+jb+(ib-1)*k)
                      enddo
                   endif
                endif
                idxa = idxa + 1
             enddo
          enddo
          idxb = idxb + 1
       enddo
    enddo
    idx = 1_c_int
    
    ! Me versus other
    do j = 1,k
       do i = j,k
          iwherestart = 1_c_int
          iwhereend   = ithis
          idelta = 0_c_int
10        continue
          do n = iwherestart,iwhereend
             if (i == j) then
                do im = 1_c_int,m
                   out(im,ithis+idx,n-idelta) = Horig(im,j+(i-1_c_int)*k,n)
                   out(im,n-idelta,ithis+idx) = out(im,ithis+idx,n-idelta)
                enddo
             else
                do im = 1_c_int,m
                   out(im,ithis+idx,n-idelta) = Horig(im,j+(i-1_c_int)*k,n) + Horig(im,i+(j-1_c_int)*k,n)
                   out(im,n-idelta,ithis+idx) = out(im,ithis+idx,n-idelta)
                enddo
             endif
          enddo
          if (iwhereend /= nparorig) then
             iwherestart = ithis+k*k+1
             iwhereend   = nparorig
             idelta      = k*k-(k*(k+1))/2
             goto 10
          endif
          idx=idx+1_c_int
       enddo
    enddo
    
    ! Other versus other
    jwherestart = 1_c_int
    jwhereend   = ithis
    jdelta = 0_c_int
30  continue
    do jb = jwherestart, jwhereend
       iwherestart = 1_c_int
       iwhereend   = ithis
       idelta = 0_c_int
20     continue
       do ib = iwherestart, iwhereend
          do im = 1_c_int,m
             out(im,ib-idelta,jb-jdelta) = Horig(im,ib,jb)
          enddo
       enddo
       if (iwhereend /= nparorig) then
          iwherestart = ithis+k*k+1
          iwhereend   = nparorig
          idelta      = k*k-(k*(k+1))/2
          goto 20
       endif
    enddo
    if (jwhereend /= nparorig) then
       jwherestart = ithis+k*k+1
       jwhereend   = nparorig
       jdelta      = k*k-(k*(k+1))/2
       goto 30
    endif

  end subroutine
  
  recursive subroutine houspdh(Horig, par, djac,ildjac,joffset, m, k, npar_orig, npar_new, &
                   & ithis, out) bind(C,name='houspdh_')
    real(c_double) Horig, par, djac, out
    integer(c_int) ildjac, joffset, m, k, ithis, npar_orig, npar_new
    dimension Horig(m,npar_orig,npar_orig), par((k*(k+1))/2), djac(ildjac,npar_orig), out(m,npar_new,npar_new)
    nek = 1
    do iell = 1,k
       do kappa = iell,k
          nij = 1
          do j = 1,k
             do i = j,k
                ! This block versus this block
                do im = 1,m
                   dinc = 0._c_double
                   do ixi = j,k
                      do ialpha = iell,k
                         dinc = dinc + par(iijtouplolidx(k,ixi,j))*par(iijtouplolidx(k,ialpha,iell))*( &
                              & Horig(im,ithis+i+(ixi-1)*k,ithis+kappa +(ialpha-1)*k) + &
                              & Horig(im,ithis+i+(ixi-1)*k,ithis+ialpha+(kappa-1) *k) + &
                              & Horig(im,ithis+ixi+(i-1)*k,ithis+kappa +(ialpha-1)*k) + &
                              & Horig(im,ithis+ixi+(i-1)*k,ithis+ialpha+(kappa-1) *k))
                      enddo
                   enddo
                   if (j == iell) then
                      dinc = dinc + &
                           & (djac(joffset+im,ithis+i+(kappa-1)*k) + djac(joffset+im,ithis+kappa+(i-1)*k))
                   endif
                   out(im,ithis+nij,ithis+nek) = out(im,ithis+nij,ithis+nek) + dinc
                enddo
                nij = nij+1
             enddo
          enddo
          ! This block versus outside
          iwherestart = 1_c_int
          iwhereend   = ithis
          idelta = 0_c_int
1         continue
          do it = iwherestart, iwhereend
             do ixi = iell,k
                do im = 1,m
                   out(im,it-idelta,ithis+nek) = out(im,it-idelta,ithis+nek) + &
                        &    par(iijtouplolidx(k,ixi,iell)) &
                        &       *(Horig(im,it,ithis+kappa+(ixi-1)*k) + Horig(im,it,ithis+ixi+(kappa-1)*k))
                   out(im,ithis+nek,it-idelta) = out(im,it-idelta,ithis+nek)
                enddo
             enddo
          enddo
          if (iwhereend /= npar_orig) then
             iwherestart= (k*k+1_c_int)
             iwhereend  = npar_orig
             idelta     = k*k - (k*(k+1))/2
             goto 1
          endif
          nek = nek + 1
       enddo
    enddo
    
    ! Outside versus outside, simply copy
    jwherestart = 1_c_int
    jwhereend   = ithis
    jdelta = 0_c_int
2   continue
    do jt = jwherestart, jwhereend
       iwherestart = 1_c_int
       iwhereend   = ithis
       idelta = 0_c_int
3      continue
       do it = iwherestart, iwhereend
          do im = 1,m
             out(im,it-idelta,jt-jdelta) = Horig(im,it,jt)
          enddo
       enddo
       if (iwhereend /= npar_orig) then
          iwherestart= (k*k+1_c_int)
          iwhereend  = npar_orig
          idelta     = k*k - (k*(k+1))/2
          goto 3
       endif
    enddo
    if (jwhereend /= npar_orig) then
       jwherestart= (k*k+1_c_int)
       jwhereend  = npar_orig
       jdelta     = k*k - (k*(k+1))/2
       goto 2
    endif
  end subroutine

  recursive subroutine houlnspdh(Horig, par,djac,ildjac,joffset, m,k, npar_orig, npar_new, ithis, out) bind(C,name='houlnspdh_')
    real(c_double) Horig, par, djac, out
    integer(c_int) ildjac, joffset, m, k, npar_orig, npar_new, ithis
    dimension Horig(m,npar_orig,npar_orig), par((k*(k+1))/2), djac(ildjac,npar_orig), out(m,npar_new,npar_new)
    !print *,ildjac,joffset,m,k,npar_orig,npar_new,ithis
    nek = 1
    do iell = 1,k
       do kappa = iell,k
          nij = 1
          do j = 1,k
             do i = j,k
                do im = 1,m
                   dinc = 0._c_double
                   do ixi = j,k
                      do ialpha = iell,k
                         if (ixi == j) then
                            dL1 = exp(par(iijtouplolidx(k,ixi,j)))
                         else
                            dL1 = par(iijtouplolidx(k,ixi,j))
                         endif
                         if (ialpha == iell) then
                            dL2 = exp(par(iijtouplolidx(k,ialpha,iell)))
                         else
                            dL2 = par(iijtouplolidx(k,ialpha,iell))
                         endif
                         dinc = dinc + dL1 * dL2 * ( &
                              & Horig(im,ithis+i+(ixi-1)*k,ithis+kappa +(ialpha-1)*k) + &
                              & Horig(im,ithis+i+(ixi-1)*k,ithis+ialpha+(kappa-1) *k) + &
                              & Horig(im,ithis+ixi+(i-1)*k,ithis+kappa +(ialpha-1)*k) + &
                              & Horig(im,ithis+ixi+(i-1)*k,ithis+ialpha+(kappa-1) *k))
                      enddo
                   enddo
                   if (i == j)        dinc = dinc * exp(par(iijtouplolidx(k,i,j)))
                   if (kappa == iell) dinc = dinc * exp(par(iijtouplolidx(k,kappa,iell)))
                   dord2 = 0._c_double
                   if (j == iell) then
                      dord2 = djac(joffset+im, ithis+i+(kappa-1)*k) + djac(joffset+im, ithis+kappa+(i-1)*k)
                      if (i == j)        dord2 = dord2 * exp(par(iijtouplolidx(k,i,j)))
                      if (kappa == iell) dord2 = dord2 * exp(par(iijtouplolidx(k,kappa,iell)))
                   endif
                   dinc = dinc + dord2
                   if (i == j .and. j == kappa .and. kappa == iell) then
                      dLrii = exp(par(iijtouplolidx(k,i,i)))
                      do ixi = j,k
                         if (ixi == j) then
                            dL1 = exp(par(iijtouplolidx(k,ixi,j)))
                         else
                            dL1 = par(iijtouplolidx(k,ixi,j))
                         endif
                         dinc = dinc + &
                              & dL1 * (djac(joffset+im,ithis+i+(ixi-1)*k) + djac(joffset+im,ithis+ixi+(i-1)*k)) * dLrii
                      enddo
                   endif
                   out(im,ithis+nij,ithis+nek) = out(im,ithis+nij,ithis+nek) + dinc
                enddo
                nij = nij+1
             enddo
          enddo
          ! This block versus others
          if (kappa == iell) then
             expterm = exp(par(iijtouplolidx(k,kappa,kappa)))
          else
             expterm = 1.0_c_double
          endif
          iwherestart = 1_c_int
          iwhereend   = ithis
          idelta = 0_c_int
1         continue
          do it = iwherestart, iwhereend
             do ixi = iell,k
                do im = 1,m
                   if (ixi == iell) then
                      dL1 = exp(par(iijtouplolidx(k,ixi,iell)))
                   else
                      dL1 = par(iijtouplolidx(k,ixi,iell))
                   endif
                   out(im,it-idelta,ithis+nek) = out(im,it-idelta,ithis+nek) + &
                   &    expterm * dL1 &
                   &       * (Horig(im,it,ithis+kappa+(ixi-1)*k) + Horig(im,it,ithis+ixi+(kappa-1)*k)) 
                   out(im,ithis+nek,it-idelta) = out(im,it-idelta,ithis+nek)
                enddo
             enddo
          enddo
          if (iwhereend /= npar_orig) then
             iwherestart = ithis+k*k+1_c_int
             iwhereend   = npar_orig
             idelta      = k*k-(k*(k+1))/2
             goto 1
          endif
          nek = nek + 1
       enddo
    enddo

    jwherestart = 1_c_int
    jwhereend   = ithis
    jdelta = 0_c_int
5   continue
    do jt = jwherestart, jwhereend
       iwherestart = 1_c_int
       iwhereend   = ithis
       idelta = 0_c_int
6      continue
       do it = iwherestart, iwhereend
          do im = 1_c_int,m
             ! Other versus others
             out(im,it-idelta,jt-jdelta) = Horig(im,it,jt)
          enddo
       enddo
       if (iwhereend /= npar_orig) then
          iwherestart = ithis+k*k+1_c_int
          iwhereend   = npar_orig
          idelta      = k*k-(k*(k+1_c_int))/2_c_int
          goto 6
       endif
    enddo
    if (jwhereend /= npar_orig) then
       jwherestart = ithis+k*k+1_c_int
       jwhereend   = npar_orig
       jdelta      = k*k-(k*(k+1_c_int))/2_c_int
       goto 5
    endif
  end subroutine

  ! This routine is disgusting... :(
  recursive subroutine hchnlndiag(Hnew, nnew, Hold, nold, par, &
                      & djacthis, ildjac, joffset, m, istart, k) bind(C, name ="hchnlndiag_")
    real(c_double) Hnew, Hold, par, djacthis
    integer(c_int) nnew, nold, ildjac, joffset, m, istart, k
    dimension Hnew(m, nnew, nnew), Hold(m,nold,nold), par(k), djacthis(ildjac,k)
    ijdiag = 0_c_int
    ijo = 1_c_int
    ijn = 1_c_int
1   if (ijo > nold) goto 100
    if (ijo >= istart+1_c_int .and. ijo < istart+k*k)   ijo= ijo+ijdiag
    iidiag = 0_c_int
    iio = 1_c_int
    iin = 1_c_int
2   if (iio > nold) goto 90
    if (iio >= istart+1_c_int .and. iio < istart+k*k)   iio= iio+iidiag
    do im=1,m
       Hnew(im,iin,ijn) = Hold(im,iio,ijo)
       if (ijo >= istart+1_c_int .and. ijo <= istart+k*k) then
          Hnew(im,iin,ijn) = Hnew(im,iin,ijn) * exp(par(ijdiag+1_c_int))
       endif
       if (iio >= istart+1_c_int .and. iio <= istart+k*k) then
          Hnew(im,iin,ijn) = Hnew(im,iin,ijn) * exp(par(iidiag+1_c_int))
       endif
    enddo
    if (ijo >= istart+1_c_int .and. ijo <= istart+k*k .and. iio >= istart+1_c_int .and. iio <= istart+k*k) then
       if (iidiag == ijdiag) then
          do im=1,m
             Hnew(im,iin,ijn) = Hnew(im,iin,ijn) + djacthis(joffset+im, ijdiag+1_c_int)
          enddo
       endif
    endif
    if (iio >= istart+1_c_int .and. iio < istart+k*k) then
       iio = iio + (k-iidiag)
       iidiag = iidiag+1_c_int
    else
       iio = iio + 1_c_int
    endif
    iin = iin+1_c_int
    goto 2
90  continue
    if (ijo >= istart+1_c_int .and. ijo < istart+k*k) then
       ijo    = ijo+ (k-ijdiag)
       ijdiag = ijdiag + 1_c_int
    else
       ijo    = ijo+ 1_c_int
    endif
    ijn = ijn+1_c_int
    goto 1
100 continue
  end subroutine

  subroutine diag2ltri(d, k, out) bind(C, name="diag2ltri_")
    real(c_double) d, out
    integer(c_int) k
    dimension d(k), out((k*(k+1))/2)
    io = 1_c_int
    id = 1_c_int
    do j = 1,k
       out(io) = d(id)
       id = id + 1_c_int
       io = io + 1_c_int
       do i=(j+1),k
          out(io) = 0.0_c_double
          io      = io + 1_c_int
       enddo
    enddo
  end subroutine
end module

