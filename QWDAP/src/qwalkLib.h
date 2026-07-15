#ifndef WALKLIB_H
#define WALKLIB_H

extern "C"
{

#include <stdlib.h>
#include <math.h>
#include <stddef.h>
  
  typedef double ElemType;
  typedef ElemType* Vector;
  typedef ElemType** Matrix;
  typedef struct{
    ElemType real;
    ElemType imag;
  } Complex;
  typedef Complex* ComplexVector;
  typedef Complex** ComplexMatrix;
  
  typedef struct QWALK_TYPE{
    int startIndex = 0;             // 存储起始顶点
    double curTime = 0;              // 存储当前时间
    ElemType* nodes = nullptr;               // 存储当前所有顶点上的概率
    //double tolerance;            // 存储归零阈值
    ComplexVector eigenvalues = nullptr;       // 存储特征值
    ComplexMatrix* eigenprojectors = nullptr;  // 存储特征投影
    int N = 0;                          // 存储点的个数
  }QWALK_TYPE;
  
  extern QWALK_TYPE qwalk;
  
  
  /* 说明：创建向量
   ** 输入：长度
   ** 输出：一维数组
   */
  Vector initVector(int);
  
  /* 说明：创建二维数组
   ** 输入：高度、宽度
   ** 输出：二维数组
   */
  Matrix initMatrix(int , int );
  
  /* 说明：销毁向量
   ** 输入：数组、高度
   ** 输出：整数
   */
  int destroyVector(Vector V);
  
  /* 说明：销毁二维数组
   ** 输入：数组、高度
   ** 输出：整数
   */
  int destroyMatrix(Matrix A, int row);
  
  /* 说明：矩阵克隆
   **
   */
  Matrix matrixClone(Matrix A, int row, int col);
  
  /* 说明：矩阵乘以数值
   **
   */
  Matrix matrixMulNum(const Matrix mat, int row, int col, ElemType num);
  
  /* 说明：小矩阵（<10）的乘法，此处只实现方针的乘法，直接相乘
   ** 输入：两个矩阵
   ** 输出：矩阵的乘法
   */
  Matrix dotMMsmall(const Matrix x, const Matrix y, int xrow, int xcol, int ycol);
  
  /* 说明：浮点矩阵获取行
   ** 输入：矩阵、长度、列号
   ** 输出：一维数组
   */
  Vector getRow(const Matrix A, int row, int col, int loc);
  
  int setRow(Matrix A, int row, int col, Vector V, int loc);
  
  /* 说明：浮点矩阵获取列
   ** 输入：矩阵、长度、列号
   ** 输出：一维数组
   */
  Vector getCol(const Matrix A, int row, int col, int loc);
  
  int setCol(Matrix A, int row, int col, Vector V, int loc);
  
  /* 说明：
   ** 输入：
   ** 输出：浮点数
   */
  ElemType dotVVtoElem(const Vector x, const Vector y, int n);
  
  Matrix dotVVtoMatrix(const Vector x, int xlen, const Vector y, int ylen);
  
  /* 说明：大矩阵（>=10）的乘法
   ** 输入：两个矩阵
   ** 输出：矩阵的乘法
   */
  Matrix dotMMbig(const Matrix x, const Matrix y, int xrow, int xcol, int ycol);
  
  
  /* 说明：矩阵乘法
   ** 输入：两个矩阵
   ** 输出：乘法的结果
   */
  Matrix matrixDot(const Matrix x, int xrow, int xcol, const Matrix y, int yrow, int ycol);
  
  /* 说明：矩阵减法
   **
   */
  Matrix matrixSub(const Matrix A, const Matrix B, int row, int col);
  
  /* 说明：矩阵自加
   **
   */
  int matrixAddeq(Matrix x, const Matrix y, int row, int col);
  
  int matrixSubeq(Matrix x, const Matrix y, int row, int col);
  
  /* 说明：矩阵转置
   ** 输入：二维数组以及数组的高度和宽度
   ** 输出：转置之后的矩阵
   */
  Matrix matrixTranspose(const Matrix A, int row, int col);
  
  /* 说明：向量的均方根
   ** 输入：数组以及长度
   ** 输出：均方根
   */
  ElemType normVector(Vector x, int len);
  
  /* 说明：一维数组除以数值
   ** 输入：数组、数值
   ** 输出：数组
   */
  Vector vecDivNum(Vector vec, int len, ElemType num);
  
  /* 说明：向量乘以数值
   ** 输入：数组、数值
   ** 输出：数组
   */
  Vector vecMulNum(Vector vec, int len, ElemType num);
  
  /* 说明：两个向量的减法
   ** 输入：两个向量
   ** 输出：向量减的结果
   */
  Vector vecSub(Vector A, Vector B, int len);
  
  Matrix diag(Vector d, int N);
  
  /* 说明：两个复数的加法
   ** 输入：两个复数
   ** 输出：复数加的结果
   */
  Complex* complexAdd(const Complex* , const Complex* );
  
  /* 说明：两个复数的自加
   ** 输入：两个复数
   ** 输出：1表示成功
   */
  int complexAddeq(Complex* , const Complex* );
  
  /* 说明：两个复数的减法
   ** 输入：两个复数
   ** 输出：复数加的结果
   */
  Complex* complexSub(const Complex* , const Complex* );
  
  /* 说明：两个复数的自减
   ** 输入：两个复数
   ** 输出：1表示成功
   */
  int complexSubeq(Complex* , const Complex* );
  
  /* 说明：两个复数的乘法
   ** 输入：两个复数
   ** 输出：复数乘的结果
   */
  Complex* complexMul(const Complex* , const Complex* );
  
  /* 说明：两个复数的自乘
   ** 输入：两个复数
   ** 输出：复数乘的结果
   */
  int complexMuleq(Complex* , const Complex* );
  
  /* 说明：欧拉公式实现e的复数次幂
   ** 输入：复数
   ** 输出：复数
   */
  Complex* complexExp(const Complex* );
  
  /* 说明：向量初始化
   ** 输入：长度
   ** 输出：向量
   */
  ComplexVector initComplexVec(int);
  
  /* 说明：销毁向量
   ** 输入：向量
   ** 输出：代码
   */
  int destroyComplexVec(ComplexVector );
  
  /* 说明：矩阵初始化
   ** 输入：行列
   ** 输出：矩阵
   */
  ComplexMatrix initComplexMat(int , int);
  
  /* 说明：矩阵销毁
   ** 输入：矩阵，行
   ** 输出：代码
   */
  int destroyComplexMat(ComplexMatrix , int);
  
  /* 说明：复数矩阵与复数相乘
   ** 输入：复数矩阵与一个复数
   ** 输出：代码
   */
  ComplexMatrix complexMulMat(const Complex* , const ComplexMatrix , int , int);
  
  /* 说明：复数矩阵自加，结果为第一个参数
   ** 输入：两个复数矩阵
   ** 输出：1表示成功，0表示失败
   */
  int complexMatAddeq(ComplexMatrix , const ComplexMatrix , int ,int);
  
  /* 说明：复数的除法
   ** 会改变A的内容
   */
  int cdivA(ElemType ar, ElemType ai, ElemType br, ElemType bi, ElemType** A, int in1, int in2, int in3);
  
  /* 说明：使用QR分解得到特征值和特征矩阵
   **
   ** 会改变A、B、wi、wr、oPar的值
   */
  int hqr2(int N, ElemType** A, ElemType** B, int low, int igh, ElemType* wi, ElemType* wr, int* outEr);
  
  /* 说明：Normalizes the eigenvectors
   ** 输入：
   ** 输出：一个二维数组
   */
  int norVecC(int N, ElemType** Z, const ElemType* wi);
  
  
  /* 说明：对矩阵进行特征分解
   ** 输入：矩阵
   ** 输出：特征向量、特征值的实部、特征值的虚部、错误代码
   */
  int calEigSysReal(int N, ElemType** A, ElemType** B, ElemType* wr, ElemType* wi, int* outEr);
  
  /* 说明：输入特征值和特征投影，得到具有实部和虚部的数组
   ** 输入：顶点个数，时间
   ** 输出：输出一个复数矩阵
   */
  ComplexMatrix qtoolsQwalk(int N, ComplexVector eigenvectors, ComplexMatrix* eigenprojectors, ElemType t);
  
  /* 说明：
   ** 输入：
   ** 输出：
   */
  int qtoolsMGS(Matrix U, int N);
  
  /* 说明：
   ** 输入：
   ** 输出：
   */
  int qtoolsCorrect(Matrix A, int row, int col);
  
  /* 说明：对矩阵进行特征分解
   ** 输入：矩阵
   ** 输出：特征值和特征向量，结构体中indicator为0
   */
  int qtoolsEig(Matrix A, int N, Vector eigenvalues, Matrix eigenvectors);
  
  /* 说明：将特征向量转换成特征投影
   ** 输入：矩阵
   ** 输出：特征值和特征投影，结构体中indicator为1
   */
  int specDecomp(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors);
  
  /* 说明：norm
   **
   */
  ElemType normMatrix(Matrix A, int N);
  
  int testBasis(int N, Matrix* eigenprojectors);
  
  int testDecomp(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors);
  
  /* 说明：检验谱分解的结果，返回1或者0
   ** 输入：图，
   */
  int testSpectralDecomposition(Matrix A, int N, Vector eigenvalues, Matrix* eigenprojectors, int verbose);
  
  // 循环得到不同时间的概率值
  void qwalkLoop(int N, float scale);
  
  
  // 初始化常量
  int initQwalk(int N, int startindex);
  
  // 进行谱分解，存储到全局变量
  int specRun(Matrix graph, int N);
  
  /* 说明：此函数为连续时间量子游走的起始函数。
   ** 输入：graph，结构体类型，用于直接读取顶点个数；len，产生数据的长度。
   ** 输出：输出len长度的数据。
   */
  Matrix collectData(int N, float scale, int len, int flag, int getfloat, int multiple);
  
  // 释放全局变量内存
  void releaseMemory(int N);
}

#endif /* WALKLIB_H */
