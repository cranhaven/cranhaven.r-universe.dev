#ifndef COEFF_MATRIX_HPP
#define COEFF_MATRIX_HPP

using namespace arma;

// convert the sparse matrix to coordinate form matrix of [row, col, val]
inline mat coordReal(SpMat<double> &x) {

  mat  ans(x.n_nonzero, 3);
  int k = 0;
  sp_mat::const_iterator i;

  for (i = x.begin(); i != x.end(); ++i){
    ans(k, 0) = i.row(); // Row position
    ans(k, 1) = i.col(); // Col position
    ans(k++,2) = *i;
  }


  return(ans);
}

// sp_cx_mat version of coordReal
inline cx_mat coordComplex(sp_cx_mat &x) {
  cx_mat res(x.n_nonzero, 3,fill::zeros);

  sp_cx_mat::const_iterator it     = x.begin();
  sp_cx_mat::const_iterator it_end = x.end();
  //
  int k = 0;
  for(; it != it_end; ++it)
  {
    res.at(k, 0) = (it.row());
    res.at(k, 1) = (it.col());
    res.at(k, 2) = (*it);
    k++;
  }

  return(res);
}

// performs the polynomial multiplication of A,B with the extra +y term
inline SpMat<double> wedge(SpMat<double> &A, SpMat<double> &B){

  Mat<double> baseMat = coordReal(A);
  Mat<double> shiftsMat = coordReal(B);

  int baseRows = baseMat.n_rows;
  int shiftsRows = shiftsMat.n_rows;
  int rowRes = 0;
  int colRes = 0;
  double coeffRes = 0;

  mat resMat(std::ceil(((double)A.n_cols + (double)B.n_cols)/2), A.n_cols + B.n_cols-1,fill::zeros);

  // add the extra + y term
  resMat(1,0) = 1;
  // loop through all combinations of each coordinate list (nonzero elements of each matrix)
  for(int bRow = 0; bRow<baseRows; bRow++){

    for(int sRow = 0; sRow<shiftsRows; sRow++){
      rowRes = baseMat(bRow,0) + shiftsMat(sRow,0);
      colRes = baseMat(bRow,1) + shiftsMat(sRow,1);
      coeffRes = baseMat(bRow,2)*shiftsMat(sRow,2);
      resMat(rowRes,colRes) = resMat(rowRes,colRes) + coeffRes;

    }
  }

  return(sp_mat(resMat));
}

inline sp_cx_mat wedgeTipLabel(sp_cx_mat &A, sp_cx_mat &B){

  cx_mat baseMat = coordComplex(A);
  cx_mat shiftsMat = coordComplex(B);

  int baseRows = baseMat.n_rows;
  int shiftsRows = shiftsMat.n_rows;
  int rowRes = 0;
  int colRes = 0;
  cx_double coeffRes = 0;

  sp_cx_mat resMat(A.n_rows + B.n_rows - 1, A.n_cols + B.n_cols - 1);

  // add the extra + y term
  resMat(0,0) = cx_double(1,1);
  // loop through all combinations of each coordinate list (nonzero elements of each matrix)
  for(int bRow = 0; bRow<baseRows; bRow++){

    for(int sRow = 0; sRow<shiftsRows; sRow++){

      rowRes = real(baseMat(bRow,0)) + real(shiftsMat(sRow,0));
      colRes = real(baseMat(bRow,1)) + real(shiftsMat(sRow,1));
      coeffRes = baseMat(bRow,2)*shiftsMat(sRow,2);
      resMat(rowRes,colRes) += coeffRes;

    }
  }

  return(resMat);
}

inline cx_rowvec wedgeConv(cx_rowvec &A, cx_rowvec &B, cx_double y){
  cx_rowvec res = conv(A, B);
  res[0] += y;
  return(res);
}

// main function to convert a wedge order to a coefficient matrix
// the complex and tip label version use similiar strategy using the different wedge versions
inline mat coeffMatrixReal(std::vector<std::string> wedgeOrder){

  long unsigned int j = 0;
  int subTreeNum = 2;
  std::string op1;
  std::string op2;
  std::string subPattern;

  // this will store the unique coefficient matrices as the total matrix is built
  // intialize with the leaf and cherry matrices
  std::vector<SpMat<double>> subCoeffMats(2);

  SpMat<double> leaf(1,2);
  leaf(0,1) = 1;

  SpMat<double> cherry(2,3);
  cherry(1,0) = 1;
  cherry(0,2) = 1;

  subCoeffMats[0] = leaf;
  subCoeffMats[1] = cherry;

  // each valid string in the wedge order maps to a matrix
  std::map<std::string, int> subCoeffOrder;
  subCoeffOrder.insert(std::make_pair("0", 0));
  subCoeffOrder.insert(std::make_pair("001", 1));

  // loop until the entire wedgeorder is one element
  while(wedgeOrder.size() != 1){
    j = 2;
    // determine the wedge operands
    while(true){
      // RcppThread::checkUserInterrupt();
      if(wedgeOrder[j] == "1"){
        op1 = wedgeOrder[j-2];
        op2 = wedgeOrder[j-1];
        break;
      }
      j++;
    }
    // RcppThread::checkUserInterrupt();

    // insert the new wedge in the map
    subPattern = op1 + op2 + "1";
    subCoeffOrder.insert(std::make_pair(subPattern,subTreeNum));

    // access the two operand matrices and perform the wedge
    SpMat<double> op1Mat = subCoeffMats[subCoeffOrder.find(op1)->second];
    SpMat<double> op2Mat = subCoeffMats[subCoeffOrder.find(op2)->second];
    subCoeffMats.push_back(wedge(op1Mat,op2Mat));

    subTreeNum++;


    // go through and flag any repeats of the current wedge operation
    for(j = 0; j < wedgeOrder.size()-2; j++){

      // account for order swapped option of wedge operands
      if(wedgeOrder[j] == op1 && wedgeOrder[j+1] == op2 && wedgeOrder[j+2] == "1"){

        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";

      } else if(wedgeOrder[j] == op2 && wedgeOrder[j+1] == op1 && wedgeOrder[j+2] == "1"){

        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";

      }
    }

    // erase the repeated locations
    wedgeOrder.erase(std::remove_if(wedgeOrder.begin(),
                                    wedgeOrder.end(),
                                    [](std::string x){return x == " ";}),
                                    wedgeOrder.end());

  }

  return(mat(subCoeffMats.back()));
}

inline cx_rowvec coeffMatrixComplex(std::vector<std::string> wedgeOrder, cx_double y){
  long unsigned int j = 0;
  int subTreeNum = 2;
  std::string op1;
  std::string op2;
  std::string subPattern;


  std::vector<cx_rowvec> subCoeffMats(2);

  cx_rowvec  leaf(2,fill::zeros);
  leaf[1] = cx_double(1,0);


  cx_rowvec cherry(3,fill::zeros);
  cherry[0] = y;
  cherry[2] = cx_double(1, 0);

  subCoeffMats[0] = leaf;
  subCoeffMats[1] = cherry;


  std::map<std::string, int> subCoeffOrder;
  subCoeffOrder.insert(std::make_pair("0", 0));
  subCoeffOrder.insert(std::make_pair("001", 1));


  while(wedgeOrder.size() != 1){

    j = 2;
    while(true){
      // RcppThread::checkUserInterrupt();
      if(wedgeOrder[j] == "1"){
        op1 = wedgeOrder[j-2];
        op2 = wedgeOrder[j-1];
        break;
      }
      j++;
    }


    subPattern = op1 + op2 + "1";

    subCoeffOrder.insert(std::make_pair(subPattern,subTreeNum));




    cx_rowvec  op1Mat = subCoeffMats[subCoeffOrder.find(op1)->second];
    cx_rowvec  op2Mat = subCoeffMats[subCoeffOrder.find(op2)->second];
    subCoeffMats.push_back(wedgeConv(op1Mat,op2Mat,y));



    subTreeNum++;


    for(j = 0; j < wedgeOrder.size()-2; j++){

      if(wedgeOrder[j] == op1 && wedgeOrder[j+1] == op2 && wedgeOrder[j+2] == "1"){


        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";



      } else if(wedgeOrder[j] == op2 && wedgeOrder[j+1] == op1 && wedgeOrder[j+2] == "1"){

        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";

      }
    }

    wedgeOrder.erase(std::remove_if(wedgeOrder.begin(),
                                    wedgeOrder.end(),
                                    [](std::string x){return x == " ";}),
                                    wedgeOrder.end());

  }

  return(subCoeffMats.back());
}

inline sp_cx_mat coeffMatrixTipLabel(std::vector<std::string> wedgeOrder, std::string tipLabA, std::string tipLabB){
  long unsigned int j = 0;
  int subTreeNum = 2;
  std::string op1;
  std::string op2;
  std::string subPattern;


  std::vector<sp_cx_mat> subCoeffMats(2);

  sp_cx_mat  leafA(1,2);
  leafA(0,1) = cx_double(1,0);

  sp_cx_mat  leafB(2,1);
  leafB(1,0) = cx_double(1,0);


  subCoeffMats[0] = leafA;
  subCoeffMats[1] = leafB;

  std::map<std::string, int> subCoeffOrder;
  // use the tip labels to define the two types of leafs
  subCoeffOrder.insert(std::make_pair(tipLabA, 0));
  subCoeffOrder.insert(std::make_pair(tipLabB, 1));


  while(wedgeOrder.size() != 1){

    j = 2;
    while(true){
      // RcppThread::checkUserInterrupt();
      if(wedgeOrder[j] == "1"){
        op1 = wedgeOrder[j-2];
        op2 = wedgeOrder[j-1];
        break;
      }
      j++;
    }


    subPattern = op1 + op2 + "1";

    subCoeffOrder.insert(std::make_pair(subPattern,subTreeNum));




    sp_cx_mat  op1Mat = subCoeffMats[subCoeffOrder.find(op1)->second];
    sp_cx_mat  op2Mat = subCoeffMats[subCoeffOrder.find(op2)->second];
    subCoeffMats.push_back(wedgeTipLabel(op1Mat,op2Mat));



    subTreeNum++;


    for(j = 0; j < wedgeOrder.size()-2; j++){

      if(wedgeOrder[j] == op1 && wedgeOrder[j+1] == op2 && wedgeOrder[j+2] == "1"){

        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";



      } else if(wedgeOrder[j] == op2 && wedgeOrder[j+1] == op1 && wedgeOrder[j+2] == "1"){

        wedgeOrder[j] = subPattern;
        wedgeOrder[j+1] = " ";
        wedgeOrder[j+2] = " ";

      }
    }

    wedgeOrder.erase(std::remove_if(wedgeOrder.begin(),
                                    wedgeOrder.end(),
                                    [](std::string x){return x == " ";}),
                                    wedgeOrder.end());

  }

  return(subCoeffMats.back());
}

#endif //COEFF_MATRIX_HPP
