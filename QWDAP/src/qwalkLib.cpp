#include "qwalkLib.h"

ElemType qtoolsTolerance = 5e-16;
QWALK_TYPE qwalk;

Vector initVector(int len){
  Vector res = (ElemType*)malloc(sizeof(ElemType)*len);
  for(int i=0;i<len;i++){
    res[i]=0;
  }
  return res;
}

Matrix initMatrix(int row, int col){
  Matrix res = (ElemType**)malloc(sizeof(ElemType*)*row);
  for(int i=0;i<row;i++){
    res[i]=(ElemType*)malloc(sizeof(ElemType)*col);
    for(int j=0;j<col;j++){
      res[i][j]=0;
    }
  }
  return res;
}

int destroyVector(Vector V){
  free(V);
  return 1;
}

int destroyMatrix(Matrix A, int row){
  if(!A){
    return 1;
  }
  for(int i=0;i<row;i++){
    free(A[i]);
  }
  free(A);
  return 1;
}

Matrix matrixClone(Matrix A, int row, int col){
  Matrix res = (ElemType**)malloc(sizeof(ElemType*)*row);
  for(int i=0;i<row;i++){
    res[i]=(ElemType*)malloc(sizeof(ElemType)*col);
    for(int j=0;j<col;j++){
      res[i][j]=A[i][j];
    }
  }
  return res;
}

Matrix matrixMulNum(const Matrix mat, int row, int col, ElemType num){
  Matrix res = (ElemType**)malloc(sizeof(ElemType*)*row);
  for(int i=0;i<row;i++){
    res[i] = (ElemType*)malloc(sizeof(ElemType)*col);
    for(int j=0;j<col;j++){
      res[i][j]=mat[i][j]*num;
    }
  }
  return res;
}

Matrix dotMMsmall(const Matrix x, const Matrix y, int xrow, int xcol, int ycol){
  Matrix ret = (ElemType**)malloc(sizeof(ElemType*)*xrow);
  for(int i=0;i<xrow;i++){
    ret[i] = (ElemType*)malloc(sizeof(ElemType)*ycol);
  }
  for(int i=xrow-1;i>=0;i--) {
    for(int k=ycol-1;k>=0;k--) {
      int j;
      ElemType woo = x[i][xcol-1] * y[xcol-1][k];
      for(j=xcol-2;j>=1;j-=2) {
        int i0 = j-1;
        woo += x[i][j] * y[j][k] + x[i][i0] * y[i0][k];
      }
      if(j==0) { woo += x[i][0] * y[0][k]; }
      ret[i][k] = woo;
    }
  }
  return ret;
}

Vector getRow(const Matrix A, int row, int col, int loc) {
  if(loc<0||loc>=row){
    return NULL;
  }
  int i;
  Vector x = initVector(col);
  for(i=col-1;i>0;--i) {
    x[i] = A[loc][i];
    --i;
    x[i] = A[loc][i];
  }
  if(i==0) x[0] = A[loc][0];
  return x;
}

int setRow(Matrix A, int row, int col, Vector V, int loc){
  if(loc<0 || loc>row){
    return 0;
  }
  for(int i=0;i<col;i++){
    A[loc][i]=V[i];
  }
  return 1;
}

Vector getCol(const Matrix A, int row, int col, int loc) {
  if(loc<0||loc>=col){
    return NULL;
  }
  int i;
  Vector x=initVector(row);
  for(i=row-1;i>0;--i) {
    x[i] = A[i][loc];
    --i;
    x[i] = A[i][loc];
  }
  if(i==0) x[0] = A[0][loc];
  return x;
}

int setCol(Matrix A, int row, int col, Vector V, int loc){
  if(loc<0 || loc>col){
    return 0;
  }
  for(int i=0;i<row;i++){
    A[i][loc]=V[i];
  }
  return 1;
}

ElemType dotVVtoElem(const Vector x, const Vector y, int n) {
  int i, i1;
  ElemType ret = x[n-1]*y[n-1];
  for(i=n-2;i>=1;i-=2) {
    i1 = i-1;
    ret += x[i]*y[i] + x[i1]*y[i1];
  }
  if(i==0) { ret += x[0]*y[0]; }
  return ret;
}

Matrix dotVVtoMatrix(const Vector x, int xlen, const Vector y, int ylen){
  Matrix res=(ElemType**)malloc(sizeof(ElemType*)*xlen);
  for(int i=0;i<xlen;i++){
    res[i]=(ElemType*)malloc(sizeof(ElemType)*ylen);
    for(int j=0;j<ylen;j++){
      res[i][j]=x[i]*y[j];
    }
  }
  return res;
}

Matrix dotMMbig(const Matrix x, const Matrix y, int xrow, int xcol, int ycol){
  int m = xrow, n = ycol;
  Matrix A = initMatrix(m, n);
  for(int i=n-1 ; i!=-1 ; --i) {
    Vector v= getCol(y, xcol, ycol, i);
    for(int j = m-1 ; j!=-1 ; --j) {
      Vector xj = getRow(x, xrow, xcol, j);
      A[j][i] = dotVVtoElem(xj, v, n);
      destroyVector(xj);
    }
    destroyVector(v);
  }
  return A;
}

Matrix matrixDot(const Matrix x, int xrow, int xcol, const Matrix y, int yrow, int ycol){
  if(xcol!=yrow) return NULL;
  if(xcol < 10){
    return dotMMsmall(x,y,xrow,xcol,ycol);
  }else{
    return dotMMbig(x,y,xrow,xcol,ycol);
  }
  // if(!res){
  //     printf("Error : numeric.dot only works on vectors and matrices");
  // }
}

Matrix matrixSub(const Matrix A, const Matrix B, int row, int col){
  Matrix res=(ElemType**)malloc(sizeof(ElemType*)*row);
  for(int i=0;i<row;i++){
    res[i] = (ElemType*)malloc(sizeof(ElemType)*col);
    for(int j =0;j<col;j++){
      res[i][j]=A[i][j]-B[i][j];
    }
  }
  return res;
}

int matrixAddeq(Matrix x, const Matrix y, int row, int col){
  for(int i=0;i<row;i++){
    for(int j=0;j<col;j++){
      x[i][j]+=y[i][j];
    }
  }
  return 1;
}

int matrixSubeq(Matrix x, const Matrix y, int row, int col){
  for(int i=0;i<row;i++){
    for(int j=0;j<col;j++){
      x[i][j]-=y[i][j];
    }
  }
  return 1;
}

Matrix matrixTranspose(const Matrix A, int row, int col){
  Matrix res=(ElemType**)malloc(sizeof(ElemType*)*col);
  for(int i=0;i<col;i++){
    res[i] = (ElemType*)malloc(sizeof(ElemType)*row);
    for(int j =0;j<row;j++){
      res[i][j]=A[j][i];
    }
  }
  return res;
}

ElemType normVector(Vector x, int len){
  ElemType nor = 0.0;
  for(int i=0;i<len;i++){
    nor+=x[i]*x[i];
  }
  return sqrt(nor);
}

Vector vecDivNum(Vector vec, int len, ElemType num){
  Vector res=initVector(len);
  for(int i=0;i<len;i++){
    res[i]=vec[i]/num;
  }
  return res;
}

Vector vecMulNum(Vector vec, int len, ElemType num){
  Vector res=initVector(len);
  for(int i=0;i<len;i++){
    res[i]=vec[i]*num;
  }
  return res;
}

Vector vecSub(Vector A, Vector B, int len){
  Vector C=initVector(len);
  for(int i=0;i<len;i++){
    C[i]=A[i]-B[i];
  }
  return C;
}

Matrix diag(Vector d, int N){
  int i1,j;
  Matrix A = initMatrix(N,N);
  for(int i=N-1;i>=0;i--) {
    i1 = i+2;
    for(j=N-1;j>=i1;j-=2) {
      A[i][j] = 0;
      A[i][j-1] = 0;
    }
    if(j>i) { A[i][j] = 0; }
    A[i][i] = d[i];
    for(j=i-1;j>=1;j-=2) {
      A[i][j] = 0;
      A[i][j-1] = 0;
    }
    if(j==0) { A[i][0] = 0; }
  }
  return A;
}

Complex* complexAdd(const Complex* a, const Complex* b){
  Complex* cop=(Complex*)malloc(sizeof(Complex));
  cop->real=a->real + b->real;
  cop->imag=a->imag + b->imag;
  return cop;
}

int complexAddeq(Complex* a, const Complex* b){
  a->real += b->real;
  a->imag += b->imag;
  return 1;
}

Complex* complexSub(const Complex* a, const Complex* b){
  Complex* res=(Complex*)malloc(sizeof(Complex));
  res->real=a->real - b->real;
  res->imag=a->imag - b->imag;
  return res;
}

int complexSubeq(Complex* a, const Complex* b){
  a->real -= b->real;
  a->imag -= b->imag;
  return 1;
}

Complex* complexMul(const Complex* a, const Complex* b){
  Complex* cop=(Complex*)malloc(sizeof(Complex));
  cop->real=a->real*b->real - a->imag*b->imag;
  cop->imag=a->real*b->imag + a->imag*b->real;
  return cop;
}

int complexMuleq(Complex* a, const Complex* b){
  Complex cop = *a;
  a->real=cop.real*b->real - cop.imag*b->imag;
  a->imag=cop.real*b->imag + cop.imag*b->real;
  return 1;
}

Complex* complexExp(const Complex* a){
  Complex* cop=(Complex*)malloc(sizeof(Complex));
  cop->real=cos(a->imag)*exp(a->real);
  cop->imag=sin(a->imag)*exp(a->real);
  return cop;
}

ComplexVector initComplexVec(int len){
  ComplexVector res=(Complex*)malloc(sizeof(Complex)*len);
  for(int i=0;i<len;i++){
    res[i].real = res[i].imag = 0;
  }
  return res;
}

int destroyComplexVec(ComplexVector vec){
  if(!vec) return 1;
  free(vec);
  return 1;
}

ComplexMatrix initComplexMat(int row, int col){
  ComplexMatrix res = (Complex**)malloc(sizeof(Complex*)*row);
  for(int i=0;i<row;i++){
    res[i]=(Complex*)malloc(sizeof(Complex)*col);
    for(int j = 0;j<col;j++){
      res[i][j].real = res[i][j].imag = 0;
    }
  }
  return res;
}

int destroyComplexMat(ComplexMatrix A, int row){
  if(!A) return 0;
  for(int i=0;i<row;i++){
    free(A[i]);
  }
  free(A);
  return 1;
}

ComplexMatrix complexMulMat(const Complex* a, const ComplexMatrix Mat, int row, int col){
  ComplexMatrix res = (Complex**)malloc(sizeof(Complex*)*row);
  for(int i=0;i<row;i++){
    res[i]=(Complex*)malloc(sizeof(Complex)*col);
    for(int j=0;j<col;j++){
      Complex * t = complexMul(&Mat[i][j], a);
      res[i][j].real = t->real;
      res[i][j].imag = t->imag;
      free(t);
    }
  }
  return res;
}

int complexMatAddeq(ComplexMatrix matA, const ComplexMatrix matB, int row, int col){
  for(int i=0;i<row;i++){
    for(int j=0;j<col;j++){
      int index = complexAddeq(&matA[i][j], &matB[i][j]);
      if(!index){
        // printf("Complex addeq error!\n");
        return 0;
      }
    }
  }
  return 1;
}

int cdivA(ElemType ar, ElemType ai, ElemType br, ElemType bi, ElemType** A, int in1, int in2, int in3){
  /* Division routine for dividing one complex number into another:
   * This routine does (ar + ai)/(br + bi) and returns the results in the
   * specified elements of the A matrix. */
  
  ElemType s, ars, ais, brs, bis;
  
  s = fabs(br) + fabs(bi);
  ars = ar/s;
  ais = ai/s;
  brs = br/s;
  bis = bi/s;
  s = brs*brs + bis*bis;
  A[in1][in2] = (ars*brs + ais*bis)/s;
  A[in1][in3] = (-(ars*bis) + ais*brs)/s;
  return 1;
}

int hqr2(int N, ElemType** A, ElemType** B, int low, int igh, ElemType* wi, ElemType* wr, int* outEr){
  /* Computes the eigenvalues and eigenvectors of a real upper-Hessenberg Matrix
   * using the QR method. */
  
  ElemType norm = 0.0, p = 0.0, q = 0.0, ra, s = 0.0, sa, t = 0.0, tst1, tst2, vi, vr, r = 0.0, w, x, y, zz = 0.0;
  int k = 0, l, m, mp2;
  int en = igh, incrFlag = 1, its, itn = 30 * N, enm2, na, notlas;
  
  // Store eigenvalues already isolated and compute matrix norm.
  for (int i = 0; i < N; i++) {
    for (int j = k; j < N; j++) {
      norm += fabs(A[i][j]);
    }
    k = i;
    if ((i < low) || (i > igh)) {
      wi[i] = 0.0;
      wr[i] = A[i][i];
    } //End if (i < low or i > igh)
  }  // End for i
  
  // Search next eigenvalues
  while (en >= low) {
    if (incrFlag) {
      // Skip this part if incrFlag is set to 0 at very end of while loop
      its = 0;
      na = en - 1;
      enm2 = na - 1;
    } else {
      incrFlag = 1;
    }
    
    // Look for single small sub-diagonal element for l = en step -1 until low
    
    for (int i = low; i <= en; i++) {
      l = en + low - i;
      if (l == low) {
        break;
      }
      s = fabs(A[l - 1][l - 1]) + fabs(A[l][l]);
      if (s == 0.0) {
        s = norm;
      }
      tst1 = s;
      tst2 = tst1 + fabs(A[l][l - 1]);
      if (tst2 == tst1) {
        break;
      }
    } //End for i
    
    x = A[en][en];
    
    if (l == en) {  //One root found
      wr[en] = A[en][en] = x + t;
      wi[en] = 0.0;
      en--;
      continue;
    } //End if (l == en)
    
    y = A[na][na];
    w = A[en][na]*A[na][en];
    
    if (l == na) {  //Two roots found
      p = (-x + y)/2;
      q = p*p + w;
      zz = sqrt(fabs(q));
      x = A[en][en] = x + t;
      A[na][na] = y + t;
      if (q >= 0.0) {  //Real Pair
        zz = ((p < 0.0) ? (-zz + p) : (p + zz));
        wr[en] = wr[na] = x + zz;
        if (zz != 0.0) {
          wr[en] = -(w/zz) + x;
        }
        wi[en] = wi[na] = 0.0;
        x = A[en][na];
        s = fabs(x) + fabs(zz);
        p = x/s;
        q = zz/s;
        r = sqrt(p*p + q*q);
        p /= r;
        q /= r;
        for (int j = na; j < N; j++){ //Row modification
          zz = A[na][j];
          A[na][j] = q*zz + p*A[en][j];
          A[en][j] = -(p*zz) + q*A[en][j];
        }//End for j
        for (int j = 0; j <= en; j++){ // Column modification
          zz = A[j][na];
          A[j][na] = q*zz + p*A[j][en];
          A[j][en] = -(p*zz) + q*A[j][en];
        }//End for j
        for (int j = low; j <= igh; j++){//Accumulate transformations
          zz = B[j][na];
          B[j][na] = q*zz + p*B[j][en];
          B[j][en] = -(p*zz) + q*B[j][en];
        }  //End for j
      } else {  //else q < 0.0
        wr[en] = wr[na] = x + p;
        wi[na] = zz;
        wi[en] = -zz;
      } //End else
      en--;
      en--;
      continue;
    } //End if (l == na)
    
    if (itn == 0){
      //Set error; all eigenvalues have not converged after 30 iterations.
      *outEr = en + 1;
      return 0;
    }//End if (itn == 0)
    
    if ((its == 10) || (its == 20)) { //Form exceptional shift
      t += x;
      for (int i = low; i <= en; i++) {
        A[i][i] += -x;
      }
      s = fabs(A[en][na]) + fabs(A[na][enm2]);
      y = x = 0.75*s;
      w = -0.4375*s*s;
    } //End if (its equals 10 or 20)
    
    its++;
    itn--;
    
    /* Look for two consecutive small sub-diagonal elements. Do m = en - 2 to l
     * in increments of -1 */
    
    for (m = enm2; m >= l; m--) {
      zz = A[m][m];
      r = -zz + x;
      s = -zz + y;
      p = (-w + r*s)/A[m + 1][m] + A[m][m + 1];
      q = -(zz + r + s) + A[m + 1][m + 1] ;
      r = A[m + 2][m + 1];
      s = fabs(p) + fabs(q) + fabs(r);
      p /= s;
      q /= s;
      r /= s;
      if (m == l) {
        break;
      }
      tst1 = fabs(A[m-1][m-1]) + fabs(zz) + fabs(A[m+1][m+1]);
      tst1 *= fabs(p);
      tst2 = tst1 + fabs(A[m][m - 1]) * (fabs(q) + fabs(r));
      if (tst1 == tst2) {
        break;
      }
    }//End for m
    
    mp2 = m + 2;
    
    for (int i = mp2; i <= en; i++){
      A[i][i - 2] = 0.0;
      if (i == mp2) {
        continue;
      }
      A[i][i - 3] = 0.0;
    }//End for i
    
    /* Double qr step involving rows l to en and columns m to en. */
    
    for (int i = m; i <= na; i++) {
      notlas = ((i != na) ? 1 : 0);
      if (i != m) {
        p = A[i][i - 1];
        q = A[i + 1][i - 1];
        r = ((notlas) ? A[i + 2][i - 1] : 0.0);
        x = fabs(p) + fabs(q) + fabs(r);
        if (x == 0.0) {     //Drop through rest of for i loop
          continue;
        }
        p /= x;
        q /= x;
        r /= x;
      } //End if (i != m)
      
      s = sqrt(p*p + q*q + r*r);
      if (p < 0.0) {
        s = -s;
      }
      if (i != m) {
        A[i][i - 1] = -(s*x);
      } else {
        if (l != m) {
          A[i][i - 1] = -A[i][i - 1];
        }
      }
      
      p += s;
      x = p/s;
      y = q/s;
      zz = r/s;
      q /= p;
      r /= p;
      k = ((i + 3 < en) ? i + 3 : en);
      
      if (notlas){ //Do row modification
        for (int j = i; j < N; j++) {
          p = A[i][j] + q*A[i + 1][j] + r*A[i + 2][j];
          A[i][j] += -(p*x);
          A[i + 1][j] += -(p*y);
          A[i + 2][j] += -(p*zz);
        }//End for j
        
        for (int j = 0; j <= k; j++) {//Do column modification
          p = x*A[j][i] + y*A[j][i + 1] + zz*A[j][i + 2];
          A[j][i] += -p;
          A[j][i + 1] += -(p*q);
          A[j][i + 2] += -(p*r);
        }//End for j
        
        for (int j = low; j <= igh; j++) {//Accumulate transformations
          p = x*B[j][i] + y*B[j][i + 1] + zz*B[j][i + 2];
          B[j][i] += -p;
          B[j][i + 1] += -(p*q);
          B[j][i + 2] += -(p*r);
        } // End for j
      } else {
        for (int j = i; j < N; j++) {//Row modification
          p = A[i][j] + q*A[i + 1][j];
          A[i][j] += -(p*x);
          A[i + 1][j] += -(p*y);
        }//End for j
        
        for (int j = 0; j <= k; j++){//Column modification
          p = x*A[j][i] + y*A[j][i +1];
          A[j][i] += -p;
          A[j][i + 1] += -(p*q);
        }//End for j
        
        for (int j = low; j <= igh; j++){//Accumulate transformations
          p = x*B[j][i] + y*B[j][i +1];
          B[j][i] += -p;
          B[j][i + 1] += -(p*q);
        }//End for j
        
      } //End else if notlas
    }//End for i
    incrFlag = 0;
  }//End while (en >= low)
  
  if (norm == 0.0) {
    return 0;
  }
  
  //Step from (N - 1) to 0 in steps of -1
  for (en = (N - 1); en >= 0; en--){
    p = wr[en];
    q = wi[en];
    na = en - 1;
    
    if (q > 0.0) {
      continue;
    }
    
    if (q == 0.0){//Real vector
      m = en;
      A[en][en] = 1.0;
      
      for (int j = na; j >= 0; j--){
        w = -p + A[j][j];
        r = 0.0;
        for (int ii = m; ii <= en; ii++) {
          r += A[j][ii]*A[ii][en];
        }
        if (wi[j] < 0.0){
          zz = w;
          s = r;
        } else { //wi[j] >= 0.0
          m = j;
          if (wi[j] == 0.0){
            t = w;
            if (t == 0.0){
              t = tst1 = norm;
              do {
                t *= 0.01;
                tst2 = norm + t;
              } while (tst2 > tst1);
            } //End if t == 0.0
            A[j][en] = -(r/t);
          } else { //else wi[j] > 0.0; Solve real equations
            x = A[j][j + 1];
            y = A[j + 1][j];
            q = (-p + wr[j])*(-p + wr[j]) + wi[j]*wi[j];
            A[j][en] = t = (-(zz*r) + x*s)/q;
            A[j+1][en] = (fabs(x) > fabs(zz)) ? -(r+w*t)/x : -(s+y*t)/zz;
          }//End  else wi[j] > 0.0
          
          // Overflow control
          t = fabs(A[j][en]);
          if (t == 0.0) {
            continue; //go up to top of for j loop
          }
          tst1 = t;
          tst2 = tst1 + 1.0/tst1;
          if (tst2 > tst1){
            continue; //go up to top of for j loop
          }
          for (int ii = j; ii <= en; ii++) {
            A[ii][en] /= t;
          }
        }  //End else wi[j] >= 0.0
      }  //End for j
    } else { //else q < 0.0, complex vector
      // Last vector component chosen imaginary so that
      // eigenvector matrix is triangular
      m = na;
      
      if (fabs(A[en][na]) <= fabs(A[na][en])) {
        cdivA(0.0, -A[na][en], -p + A[na][na], q, A, na, na, en);
      } else {
        A[na][na] = q/A[en][na];
        A[na][en] = -(-p + A[en][en])/A[en][na];
      } //End else (fabs(A[en][na] > fabs(A[na][en])
      
      A[en][na] = 0.0;
      A[en][en] = 1.0;
      
      for (int j = (na - 1); j >= 0; j--) {
        w = -p + A[j][j];
        sa = ra = 0.0;
        
        for (int ii = m; ii <= en; ii++) {
          ra += A[j][ii]*A[ii][na];
          sa += A[j][ii]*A[ii][en];
        } //End for ii
        
        if (wi[j] < 0.0){
          zz = w;
          r = ra;
          s = sa;
          continue;
        } //End if (wi[j] < 0.0)
        
        //else wi[j] >= 0.0
        m = j;
        if (wi[j] == 0.0) {
          cdivA(-ra, -sa, w, q, A, j, na, en);
        } else {// else wi[j] > 0.0; solve complex equations
          x = A[j][j + 1];
          y = A[j + 1][j];
          vr = -(q*q) + (-p + wr[j])*(-p + wr[j]) + wi[j]*wi[j];
          vi = (-p + wr[j])*2.0*q;
          if ((vr == 0.0) && (vi == 0.0)){
            tst1 = norm*(fabs(w)+fabs(q)+fabs(x)+fabs(y)+fabs(zz));
            vr = tst1;
            do {
              vr *= 0.01;
              tst2 = tst1 + vr;
            } while (tst2 > tst1);
          } //End if vr and vi == 0.0
          cdivA(-(zz*ra)+x*r+q*sa, -(zz*sa + q*ra)+x*s, vr, vi, A, j, na, en);
          
          if (fabs(x) > (fabs(zz) + fabs(q))){
            A[j + 1][na] = (-(ra + w*A[j][na]) + q*A[j][en])/x;
            A[j + 1][en] = -(sa + w*A[j][en] + q*A[j][na])/x;
          } else {
            cdivA(-(r + y*A[j][na]), -(s + y*A[j][en]), zz, q, A, j + 1, na, en);
          }
        }//End else wi[j] > 0.0
        
        t = (fabs(A[j][na]) > fabs(A[j][en])) ? fabs(A[j][na]) : fabs(A[j][en]);
        
        if (t == 0.0) {
          continue; // go to top of for j loop
        }
        tst1 = t;
        tst2 = tst1 + 1.0/tst1;
        if (tst2 > tst1) {
          continue; //go to top of for j loop
        }
        for (int ii = j; ii <= en; ii++){
          A[ii][na] /= t;
          A[ii][en] /= t;
        } //End for ii loop
        
      } // End for j
      
    }//End else q < 0.0
    
  }//End for en
  
  //End back substitution. Vectors of isolated roots.
  
  for (int i = 0; i < N; i++){
    if ((i < low) || (i > igh)) {
      for (int j = i; j < N; j++) {
        B[i][j] = A[i][j];
      }
    }//End if i
  }//End for i
  
  // Multiply by transformation matrix to give vectors of original full matrix.
  
  //Step from (N - 1) to low in steps of -1.
  
  for (int i = (N - 1); i >= low; i--){
    m = ((i < igh) ? i : igh);
    for (int ii = low; ii <= igh; ii++){
      zz = 0.0;
      for (int jj = low; jj <= m; jj++){
        zz += B[ii][jj]*A[jj][i];
      }
      B[ii][i] = zz;
    }//End for ii
  }//End of for i loop
  return 1;
}

int norVecC(int N, ElemType** Z, const ElemType* wi){
  // This subroutine is based on the LINPACK routine SNRM2,
  // written 25 October 1982, modified
  // on 14 October 1993 by Sven Hammarling of NAG Ltd.
  // It has been further modified for use in this Javascript routine, for use
  // with a column of an array rather than a column vector.
  //
  // Z - eigenvector Matrix
  // wi - eigenvalue vector
  
  ElemType scale, ssq, absxi, dummy, norm;
  
  for (int j = 0; j < N; j++){ //Go through the columns of the vector array
    scale = 0.0;
    ssq = 1.0;
    
    for (int i = 0; i < N; i++){
      if (Z[i][j] != 0){
        absxi = fabs(Z[i][j]);
        dummy = scale/absxi;
        if (scale < absxi){
          ssq = 1.0 + ssq*dummy*dummy;
          scale = absxi;
        } else {
          ssq += 1.0/dummy/dummy;
        }
      } //End if (Z[i][j] != 0)
    } //End for i
    
    if (wi[j] != 0) {
      // If complex eigenvalue, take into account imaginary part of eigenvector
      for (int i = 0; i < N; i++){
        if (Z[i][j + 1] != 0){
          absxi = fabs(Z[i][j + 1]);
          dummy = scale/absxi;
          if (scale < absxi){
            ssq = 1.0 + ssq*dummy*dummy;
            scale = absxi;
          } else {
            ssq += 1.0/dummy/dummy;
          }
        }//End if (Z[i][j + 1] != 0)
      } //End for i
    }//End if (wi[j] != 0)
    
    // This is the norm of the (possibly complex) vector
    norm = scale*sqrt(ssq);
    
    for (int i = 0; i < N; i++) {
      Z[i][j] /= norm;
    }
    if (wi[j] != 0){
      // If complex eigenvalue, also scale imaginary part of eigenvector
      j++;
      for (int i = 0; i < N; i++) {
        Z[i][j] /= norm;
      }
    }//End if (wi[j] != 0)
  }// End for j
  return 1;
}

int calEigSysReal(int N, ElemType** A, ElemType** B, ElemType* wr, ElemType* wi, int* outEr){
  ElemType* scale = (ElemType*) malloc(sizeof(ElemType)*N);    //Contains information about transformations.
  ElemType* trace = (ElemType*) malloc(sizeof(ElemType)*N);    //Records row and column interchanges
  ElemType c,f,g,r,s,radix = 2,b2= radix*radix;
  int ierr = -1, igh, low, k = 0, l = N - 1, noconv;
  
  /* Balance the matrix to improve accuracy of eigenvalues.
   * Introduces no rounding errors, since it scales A by powers of the radix.
   */
  
  //Search through rows, isolating eigenvalues and pushing them down.
  noconv = l;
  
  while (noconv >= 0){
    r = 0;
    for (int j = 0; j <= l; j++) {
      if (j == noconv) continue;
      if (A[noconv][j] != 0.0){
        r = 1;
        break;
      }
    } //End for j
    
    if (r == 0) {
      scale[l] = noconv;
      
      if (noconv != l){
        for (int i = 0; i <= l; i++){
          f = A[i][noconv];
          A[i][noconv] = A[i][l];
          A[i][l] = f;
        }//End for i
        for (int i = 0; i < N; i++){
          f = A[noconv][i];
          A[noconv][i] = A[l][i];
          A[l][i] = f;
        }//End for i
      }//End if (noconv != l)
      
      if (l == 0) {
        break;  //break out of while loop
      }
      noconv = --l;
      
    } else {  //else (r != 0
      noconv--;
    }
  }//End while (noconv >= 0)
  
  if (l > 0) {
    //Search through columns, isolating eigenvalues and pushing them left.
    noconv = 0;
    
    while (noconv <= l){
      c = 0;
      
      for (int i = k; i <= l; i++){
        if (i == noconv) continue;
        if (A[i][noconv] != 0.0){
          c = 1;
          break;
        }
      }//End for i
      
      if (c == 0){
        scale[k] = noconv;
        
        if (noconv != k){
          for (int i = 0; i <= l; i++){
            f = A[i][noconv];
            A[i][noconv] = A[i][k];
            A[i][k] = f;
          }//End for i
          for (int i = k; i < N; i++){
            f = A[noconv][i];
            A[noconv][i] = A[k][i];
            A[k][i] = f;
          }//End for i
        }//End if (noconv != k)
        
        noconv = ++k;
      } else { //else (c != 0)
        noconv++;
      }
    }//End while noconv
    
    //Balance the submatrix in rows k through l.
    
    for (int i = k; i <= l; i++) {
      scale[i] = 1.0;
    }
    //Iterative loop for norm reduction
    do {
      noconv = 0;
      for (int i = k; i <= l; i++) {
        c = r = 0.0;
        for (int j = k; j <= l; j++){
          if (j == i) continue;
          c += fabs(A[j][i]);
          r += fabs(A[i][j]);
        } // End for j
        //guard against zero c or r due to underflow
        if ((c == 0.0) || (r == 0.0)) continue;
        g = r/radix;
        f = 1.0;
        s = c + r;
        while (c < g) {
          f *= radix;
          c *= b2;
        } // End while (c < g)
        g = r*radix;
        while (c >= g) {
          f /= radix;
          c /= b2;
        } // End while (c >= g)
        
        //Now balance
        if ((c + r)/f < 0.95*s) {
          g = 1.0/f;
          scale[i] *= f;
          noconv = 1;
          for (int j = k; j < N; j++){
            A[i][j] *= g;
          }
          for (int j = 0; j <= l; j++) {
            A[j][i] *= f;
          }
        } //End if ((c + r)/f < 0.95*s)
      } // End for i
    } while (noconv);  // End of do-while loop.
  } //End if (l > 0)
  
  low = k;
  igh = l;
  
  //End of balanc
  
  /* Now reduce the real general Matrix to upper-Hessenberg form using
   * stabilized elementary similarity transformations. */
  
  for (int i = (low + 1); i < igh; i++){
    k = i;
    c = 0.0;
    
    for (int j = i; j <= igh; j++){
      if (fabs(A[j][i - 1]) > fabs(c)){
        c = A[j][i - 1];
        k = j;
      }//End if
    }//End for j
    
    trace[i] = k;
    
    if (k != i){
      for (int j = (i - 1); j < N; j++){
        r = A[k][j];
        A[k][j] = A[i][j];
        A[i][j] = r;
      }//End for j
      
      for (int j = 0; j <= igh; j++){
        r = A[j][k];
        A[j][k] = A[j][i];
        A[j][i] = r;
      }//End for j
    }//End if (k != i)
    
    if (c != 0.0){
      for (int m = (i + 1); m <= igh; m++){
        r = A[m][i - 1];
        
        if (r != 0.0){
          r = A[m][i - 1] = r/c;
          for (int j = i; j < N; j++){
            A[m][j] += -(r*A[i][j]);
          }
          for (int j = 0; j <= igh; j++){
            A[j][i] += r*A[j][m];
          }
        }//End if (r != 0.0)
      }//End for m
    }//End if (c != 0)
  }  //End for i.
  
  /* Accumulate the stabilized elementary similarity transformations used in the
   * reduction of A to upper-Hessenberg form. Introduces no rounding errors
   * since it only transfers the multipliers used in the reduction process into
   * the eigenvector matrix. */
  
  for (int i = 0; i < N; i++){ //Initialize B to the identity Matrix.
    for (int j = 0; j < N; j++){
      B[i][j] = 0.0;
    }
    B[i][i] = 1.0;
  } //End for i
  
  for (int i = (igh - 1); i >= (low + 1); i--){
    k = trace[i];
    for (int j = (i + 1); j <= igh; j++){
      B[j][i] = A[j][i - 1];
    }
    if (i == k){
      continue;
    }
    
    for (int j = i; j <= igh; j++){
      B[i][j] = B[k][j];
      B[k][j] = 0.0;
    } //End for j
    
    B[k][i] = 1.0;
  } // End for i
  
  *outEr = ierr;
  // 改变A、B、wi、wr，oPar的值
  hqr2(N, A, B, low, igh, wi, wr, outEr);
  ierr = *outEr;
  
  if (ierr == -1){
    if (low != igh){
      for (int i = low; i <= igh; i++){
        s = scale[i];
        for (int j = 0; j < N; j++){
          B[i][j] *= s;
        }
      }//End for i
    }//End if (low != igh)
    
    for (int i = (low - 1); i >= 0; i--){
      k = scale[i];
      if (k != i){
        for (int j = 0; j < N; j++){
          s = B[i][j];
          B[i][j] = B[k][j];
          B[k][j] = s;
        }//End for j
      }//End if k != i
    }//End for i
    
    for (int i = (igh + 1); i < N; i++){
      k = scale[i];
      if (k != i){
        for (int j = 0; j < N; j++){
          s = B[i][j];
          B[i][j] = B[k][j];
          B[k][j] = s;
        }//End for j
      }//End if k != i
    }//End for i
    
    norVecC(N, B, wi);  //Normalize the eigenvectors
  }//End if ierr = -1
  free(scale);
  free(trace);
  return 1;
}

ComplexMatrix qtoolsQwalk(int N, ComplexVector eigenvalues, ComplexMatrix* eigenprojectors, ElemType t){
  ComplexMatrix U = initComplexMat(N,N);
  Complex *cop=(Complex*)malloc(sizeof(Complex));
  cop->real=0;
  cop->imag=-t;
  for(int i=0;i<N;i++){
    Complex* t = complexMul(cop, &eigenvalues[i]);
    // 此处使用特征值的相关操作
    Complex* c=complexExp(t);
    free(t);
    // 此处使用特征向量进行操作
    // 复数矩阵与复数的乘法
    ComplexMatrix tmp = complexMulMat(c, eigenprojectors[i], N, N);
    complexMatAddeq(U, tmp, N, N);
    free(c);
    destroyComplexMat(tmp, N);
  }
  free(cop);
  return U;
}

/* 说明：
 ** 输入：
 ** 输出：
 */
int qtoolsMGS(Matrix U, int N){
  //float* getV(Matrix A, int N, int i); // getCol
  //int setV(float** A, int N, int i, float* v); // setCol
  for(int i=0;i<N;i++){
    for(int j=0;j<i;j++){
      Vector ui = getCol(U, N, N, i);
      Vector uj = getCol(U, N, N, j);
      Vector tmp= vecMulNum(uj, N, dotVVtoElem(ui,uj,N));
      Vector tmp1=vecSub(ui,tmp,N);
      destroyVector(tmp);
      setCol(U,N,N,tmp1,i);
      destroyVector(tmp1);
    }
    Vector ui = getCol(U,N,N,i);
    Vector tmp = vecDivNum(ui, N, normVector(ui,N));
    setCol(U,N,N,tmp,i);
    destroyVector(ui);
    destroyVector(tmp);
  }
  return 1;
}

/* 说明：
 ** 输入：
 ** 输出：
 */
int qtoolsCorrect(Matrix A, int row, int col){
  for (int i = 0; i < row; i++) {
    for (int j = 0; j < col; j++) {
      if (fabs(A[i][j]) < qtoolsTolerance) {
        A[i][j] = 0;
      }
    }
  }
  return 1;
}

/* 说明：对矩阵进行特征分解
 ** 输入：矩阵
 ** 输出：特征值和特征向量，结构体中indicator为0
 */
int qtoolsEig(Matrix A, int N, Vector eigenvalues, Matrix eigenvectors){
  Matrix B = initMatrix(N,N);
  Vector wr = initVector(N);  // 存储特征向量实部
  Vector wi = initVector(N);  // 存储特征向量虚部
  int outEr = 0;  // 错误码
  Matrix cloneA = matrixClone(A, N, N);
  calEigSysReal(N, cloneA, B, wr, wi, &outEr);
  destroyMatrix(cloneA, N);
  
  /* At this point, the eigenvalues and eigenvectors of A have been computed.
   * The real components of the eigenvalues are in wr, the imaginary components
   * are in wi, the associated eigenvectors are in the B matrix, and oPar.outEr
   * contains the error code. */
  
  if (outEr != -1) {
    // printf("Eigenvector computation failed.\n");
    return -1;
  }
  
  // Since the adjacency matrix is always real and symmetric, the eigenvalues
  // should always be real
  for(int i=0;i<N;i++){
    if(wi[i]>0){
      // printf("Real symmetric matrix should always have real eigenvalues");
      return -2;
    }
  }
  for(int i=0;i<N;i++){
    eigenvalues[i] = (fabs(wr[i])<qtoolsTolerance) ? 0 : wr[i];
    for(int j=0;j<N;j++){
      eigenvectors[i][j] = B[i][j];
    }
  }
  destroyMatrix(B, N);
  destroyVector(wr);
  destroyVector(wi);
  qtoolsMGS(eigenvectors, N);
  qtoolsCorrect(eigenvectors, N, N);
  return 1;
}

/* 说明：将特征向量转换成特征投影
 ** 输入：矩阵
 ** 输出：特征值和特征投影，结构体中indicator为1
 */
int specDecomp(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors){
  Matrix eigenvectors=initMatrix(N,N);
  // int indicator = 0;
  qtoolsEig(A, N, eigenvalues, eigenvectors);
  for(int i=0;i<N;i++){
    // int found=0;
    // for(int j=0; j<indicator;j++){
    //     if(fabs(eigenvalues[1]-eigenvalues[i])<0.0001){
    //         Vector vi=getCol(eigenvectors,N,N,i);
    //         Matrix mi=dotVVtoMatrix(vi,N,vi,N);
    //         destroyVector(vi);
    //         matrixAddeq(eigenprojectors[j],mi,N,N);
    //         free(mi);
    //         found = 1;
    //     }
    // }
    // if(!found){
    Vector vi=getCol(eigenvectors,N,N,i);
    // matrixAddeq(eigenprojectors[indicator++],dotVVtoMatrix(vi,N,vi,N),N,N);
    matrixAddeq(eigenprojectors[i],dotVVtoMatrix(vi,N,vi,N),N,N);
    free(vi);
    // }
  }
  for(int i=0;i<N;i++){
    qtoolsCorrect(eigenprojectors[i],N,N);
  }
  return 1;
}

ElemType normMatrix(Matrix A, int N) {
  Matrix transmat = matrixTranspose(A, N, N);
  Matrix dot = matrixDot(A, N, N, transmat, N, N);
  destroyMatrix(transmat, N);
  Vector eigenvalues = initVector(N);
  Matrix eigenvectors = initMatrix(N, N);
  qtoolsEig(dot, N, eigenvalues, eigenvectors);
  ElemType max = eigenvalues[0];
  for(int i=1;i<N;i++){
    if(eigenvalues[i]>max){
      max=eigenvalues[i];
    }
  }
  destroyMatrix(dot, N);
  destroyVector(eigenvalues);
  destroyMatrix(eigenvectors, N);
  return sqrt(max);
}

int testBasis(int N, Matrix* eigenprojectors){
  Matrix Z=initMatrix(N,N);
  for(int i=0; i<N;i++){
    matrixAddeq(Z, eigenprojectors[i], N, N);
  }
  Vector identity = initVector(N);
  for(int i=0;i<N;i++){
    identity[i]=1;
  }
  Matrix tmp=diag(identity, N);
  matrixSubeq(tmp, Z, N, N);
  qtoolsCorrect(tmp, N, N);
  ElemType error = normMatrix(tmp, N);
  destroyVector(identity);
  destroyMatrix(tmp, N);
  destroyMatrix(Z, N);
  return error<qtoolsTolerance;
}

int testDecomp(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors){
  Matrix Z=initMatrix(N,N);
  for(int i=0;i<N;i++){
    Matrix mul = matrixMulNum(eigenprojectors[i], N, N, eigenvalues[i]);
    matrixAddeq(Z, mul, N, N);
    destroyMatrix(mul, N);
  }
  Matrix sub = matrixSub(A, Z, N, N);
  qtoolsCorrect(sub, N, N);
  ElemType pass=normMatrix(sub, N);
  destroyMatrix(sub, N);
  return pass<qtoolsTolerance;
}

/* 说明：检验谱分解的结果，返回1或者0
 ** 输入：图，
 */
int testSpectralDecomposition(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors, int verbose){
  if(verbose){
    /*for(int i=0;i<N;i++){
     printf("Eigenvalues[%d] = %lf\n",i,eigenvalues[i]);
     printf("Eigenprojectors[%d] = \n", i);
     for(int j=0;j<N;j++){
     for(int k=0;k<N;k++){
     printf("%lf, ",eigenprojectors[i][j][k]);
     }
     putchar('\n');
     }
     }*/
  }
  // printf("Begin Test!\n");
  return testBasis(N, eigenprojectors) && testDecomp(A, N, eigenvalues, eigenprojectors);
}

// 循环得到不同时间的概率值
void qwalkLoop(int N, float scale){
  // 此处根据特征值和特征向量计算各个顶点的概率值
  ComplexMatrix U = qtoolsQwalk(N, qwalk.eigenvalues, qwalk.eigenprojectors, (ElemType)qwalk.curTime);
  for(int i=0;i<N;i++){
    Complex ampl = U[i][qwalk.startIndex];
    ElemType prob = ampl.real*ampl.real+ampl.imag*ampl.imag;
    qwalk.nodes[i] = prob;
  }
  qwalk.curTime += scale;
  destroyComplexMat(U, N);
}

int initQwalk(int N, int startindex){
  // 确认初始顶点参数是否存在
  if(startindex > N || startindex < 0){
    // printf("The initial position is error!\n");
    return 0;
  }
  // 初始化qwalk相关的参数
  qwalk.startIndex = startindex;
  qwalk.curTime = 0;
  qwalk.nodes = (ElemType*)malloc(sizeof(ElemType) * N);
  //qwalk.tolerance = 5e-16;
  qwalk.eigenvalues = nullptr;
  qwalk.eigenprojectors = nullptr;
  qwalk.N = N;
  return 1;
}

int specRun(Matrix graph, int N){
  // 申请N*N的矩阵，矩阵的数据全部都是0，为了存储顶点个数，这个地方采用结构体表示
  Matrix qwalkmat = initMatrix(N,N);
  for(int i=0; i<N; i++){
    for(int j=0; j<N; j++){
      if(i==j){
        continue;
      }
      // 根据坐标找到矩阵中的数值
      qwalkmat[i][j] = graph[i][j]>0 ? 1 : 0;
    }
  }
  // 光谱分解，得到特征值和特征向量
  // 这个地方需要设置一个结构体，特征值和特征向量的表示不一致
  // 其中特征值的部分为一个一维数组，特征向量的部分为二维数组
  Vector eigenvalues = initVector(N);
  Matrix* eigenprojectors = (Matrix*)malloc(sizeof(Matrix)*N);
  for(int i=0;i<N;i++){
    eigenprojectors[i]=initMatrix(N,N);
  }
  specDecomp(qwalkmat, N, eigenvalues, eigenprojectors);
  // 分别使用变量表示这两组数据，分别在每组数据之后加一个虚部
  // 1. 将所有的特征值转换为复数的形式，将特征值的组合放到qwalk.eigenvalues
  if(qwalk.eigenvalues){
    destroyComplexVec(qwalk.eigenvalues);
    qwalk.eigenvalues = nullptr;
  }
  if(qwalk.eigenprojectors){
    for(int i=0;i<N;i++){
      destroyComplexMat(qwalk.eigenprojectors[i],N);
    }
    qwalk.eigenprojectors = nullptr;
  }
  qwalk.eigenvalues = initComplexVec(N);
  for(int i=0;i<N;i++){
    qwalk.eigenvalues[i].real=eigenvalues[i];
    qwalk.eigenvalues[i].imag=0;
  }
  // 2. 将所有的特征投影转成复数形式的矩阵
  qwalk.eigenprojectors = (ComplexMatrix*)malloc(sizeof(ComplexMatrix)*N);
  for(int i=0;i<N;i++){
    qwalk.eigenprojectors[i] = initComplexMat(N,N);
    for(int j=0;j<N;j++){
      for(int k=0;k<N;k++){
        qwalk.eigenprojectors[i][j][k].real=eigenprojectors[i][j][k];
        qwalk.eigenprojectors[i][j][k].imag=0;
      }
    }
  }
  // 检验谱分解的结果
  int istrue = testSpectralDecomposition(qwalkmat, N, eigenvalues, eigenprojectors, 0);
  if(istrue){
    // printf("SUCCESS!\n");
  }else{
    // printf("FAILED!\n");
    return 0;
  }
  // 释放空间
  destroyVector(eigenvalues);
  for(int i=0;i<N;i++){
    destroyMatrix(eigenprojectors[i], N);
  }
  free(eigenprojectors);
  destroyMatrix(qwalkmat, N);
  return 1;
}

/* 说明：此函数为连续时间量子游走的起始函数。
 ** 输入：graph，结构体类型，用于直接读取顶点个数；len，产生数据的长度。
 ** 输出：输出len长度的数据。
 */
Matrix collectData(int N, float scale, int len, int flag, int getfloat, int multiple){
  // 定义一个存储产生数据的矩阵
  Matrix res = (ElemType**)malloc(sizeof(ElemType*)*len);
  int m_Collect = 0;
  for(int i=0; i<len; ){
    // 按照新的
    qwalkLoop(N, scale);
    if( m_Collect == 1 || (getfloat && qwalk.nodes[flag]>1e-5) || (!getfloat && round(qwalk.nodes[flag]*multiple)>0)){
      m_Collect = 1;
      res[i] = (ElemType*)malloc(sizeof(ElemType)*N);
      // 将数据存储到res中
      if(getfloat){
        for(int j=0;j<N;j++){
          res[i][j]=qwalk.nodes[j];
        }
      }else{
        for(int j=0;j<N;j++){
          res[i][j]=round(qwalk.nodes[j]*multiple);
        }
      }
      i+=1;
    }
  }
  return res;
}

// 释放全局变量内存
void releaseMemory(int N){
  if(qwalk.nodes){
    free(qwalk.nodes);
    qwalk.nodes = nullptr;
  }
  if(qwalk.eigenvalues){
    destroyComplexVec(qwalk.eigenvalues);
    qwalk.eigenvalues = nullptr;
  }
  if(qwalk.eigenprojectors){
    for(int i=0;i<N;i++){
      destroyComplexMat(qwalk.eigenprojectors[i],N);
    }
    qwalk.eigenprojectors = nullptr;
  }
  qwalk.curTime = qwalk.N = qwalk.startIndex = 0;
}
