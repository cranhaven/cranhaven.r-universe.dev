


#'Generate black-and-white images
#'
#'Create various images such as circle, rectangle and random dots.
#'@param nx - Width of an image.
#'@param ny - Length of an image.
#'@param Type - Type of an image: 1, 2, and 3 for rectangle, circle, and random dots, respectively.
#'@param bNoise - Option for including noise: TRUE or FALSE.
#'@param sig_noise - Strength of noise: numeric value between 0 and 0.5.
#'@return A list of information of a generated image.
#'\itemize{
#'  \item ImgMat - a matrix whose entries are pixel values of a generated image.
#'  \item S1 - an n1x2 matrix whose entries denote coordinates of white pixels of the image. n1 denotes the number of the white pixels.
#'  \item S2 - an n2x2 matrix whose entries denote coordinates of black pixels of the image. n2 denotes the number of the black pixels.
#'}
#'@examples
#'
#'
#'######## Generate a 10x10 black-and-white rectangle image with some noise
#'nx=10
#'ny=10
#'Type=1
#'bNoise=TRUE
#'sig_noise=0.1
#'lst = GenImg(nx,ny,Type, bNoise, sig_noise)
#'ImgMat = lst$ImgMat
#'image(ImgMat, axes = FALSE, col = grey(seq(0, 1, length = 256)))
#'
#'
#'
#'@export
#'
#'
GenImg = function(nx, ny, Type=1, bNoise=FALSE, sig_noise=0.1){

  Totaln = nx*ny
  n1 = floor(Totaln/2)
  n2 = Totaln - n1

  TS = matrix(0, Totaln, 2)

  for(i in 1:nx){
    for(j in 1:ny){
      nIndex = (i-1) * ny + j
      TS[nIndex, 1] = i
      TS[nIndex, 2] = j
    }


  }



  if(Type==1){

    nl = floor(ny/2)
    nq = floor(ny/3)
    N1 = nq*ny + nq
    N2 = N1+ny
    N3 = N2+ny
    N4 = N3+ny
    N5 = N4+ny

    v1 = N1:(N1+nl-1)
    v2 = N2:(N2+nl-1)
    v3 = N3:(N3+nl-1)
    v4 = N4:(N4+nl-1)
    v5 = N5:(N5+nl-1)

    IndexVec = cbind(t(v1), t(v2), t(v3),t(v4),t(v5) )
    IndexVec = t(IndexVec)


  }else if(Type == 3){

    IndexVec = sample.int(Totaln, size=n1, replace=FALSE)
    IndexVec = sort(IndexVec)
  }


  if(Type!=2){
    S1 = TS[IndexVec,]
    S2 = DiffMatrix2(TS, IndexVec)

  }else{
    lst = GenerateCircle(nx, ny, floor(min(nx,ny)/3))
    S1 = lst[[1]]
    S2 = lst[[2]]
  }

  n1=dim(S1)[1]

  p1 = 1
  p2 = 0


  TrueImgMat = matrix(p2, nx, ny)
  for(i in 1:n1){
    xi = S1[i,1]; yi=S1[i,2]
    TrueImgMat[xi,yi]=p1
  }

  if(sig_noise>0.5){
    sig_noise=0.5
  }
  EpsMat = matrix(rnorm(nx*ny, 0, sig_noise), nx, ny)

  if(bNoise==TRUE){
    TrueImgMat = TrueImgMat+EpsMat
  }

  ans = list(S1=S1, S2=S2, ImgMat = TrueImgMat)
  return(ans)
}


#'Perform image segmentation
#'
#'Seperate an area of white pixels from a given image when there is some noise.
#'@param ImgMat - a matrix whose entries are pixel values of the image.
#'@param p1 - a known value of white pixel (usually 1).
#'@param p2 - a known value of black pixel (usually 0).
#'@return A list of information of a segmented image.
#'\itemize{
#'  \item SegImgMat - a matrix as a result of the image segmentation.
#'  \item Estimated_S1 - an n1x2 matrix whose entries denote estimated coordinates of white pixels, corresponding to p1.
#'  \item Estimated_S2 - an n2x2 matrix whose entries denote estimated coordinates of black pixels, corresponding to p2.
#'}
#'@examples
#'
#'
#'######## Generate a 10x10 black-and-white rectangle image with some noise
#'nx=10
#'ny=10
#'Type=1
#'bNoise=TRUE
#'sig_noise=0.1
#'lst = GenImg(nx,ny,Type, bNoise, sig_noise)
#'ImgMat = lst$ImgMat
#'image(ImgMat, axes = FALSE, col = grey(seq(0, 1, length = 256)))
#'
#'######## Perform image segmentation
#'p1=1     ### value of a white pixel
#'p2=0     ### value of a black pixel
#'
#'
#'lst = GetSegImage(ImgMat, p1, p2)
#'EstImgMat = lst$SegImgMat
#'image(EstImgMat, axes = FALSE, col = grey(seq(0, 1, length = 256)))
#'
#'
#'@export
#'
#'
GetSegImage=function(ImgMat, p1, p2){

  lst = cppGet_Estimated_Img(ImgMat, p1, p2)

  S1 = lst[[1]]
  S2 = lst[[2]]
  SegImgMat = lst[[3]]

  ans = list(Estimated_S1=S1, Estimated_S2=S2, SegImgMat=SegImgMat)
  return(ans)
}




