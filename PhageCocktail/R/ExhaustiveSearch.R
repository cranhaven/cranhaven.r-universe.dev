# ExhaustiveSearch

ExhaustiveSearch<-function(MaxPhage, MaxBacteria,new_matrix,phage_names,limit,file.name,FUN){
  if (MaxPhage == 0 ) {return (0)}
  ResultS1<-Search(1, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet1<-ResultS1[1]
  BestBacteria1<-ResultS1[2]
  rm(ResultS1)
  if (BestBacteria1 == MaxBacteria) {
    return (c(1, TRUE, PhageSet1, BestBacteria1))}

  if (MaxPhage == 1 | limit==1){
    return (c(1, FALSE, PhageSet1, BestBacteria1))}
  ResultS2<-Search(2, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet2<-ResultS2[-3]
  BestBacteria2<-ResultS2[3]

  rm(ResultS2)
  if (BestBacteria2 == MaxBacteria){

    return (c(2, TRUE, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  if (MaxPhage == 2| limit==2){
    return (c(2, FALSE, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}
  ResultS3<-Search(3, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet3<-ResultS3[-4]
  BestBacteria3<-ResultS3[4]

  rm(ResultS3)
  if (BestBacteria3 == MaxBacteria) {
    return (c(3, TRUE, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  if (MaxPhage == 3| limit==3) {
    return (c(3, FALSE, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}
  ResultS4<-Search(4, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet4<-ResultS4[-5]
  BestBacteria4<-ResultS4[5]

  rm(ResultS4)
  if (BestBacteria4 == MaxBacteria) {
    return (c(4, TRUE, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  if (MaxPhage == 4| limit==4) {
    return (c(4, FALSE, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}
  ResultS5<-Search(5, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet5<-ResultS5[-6]
  BestBacteria5<-ResultS5[6]

  rm(ResultS5)
  if (BestBacteria5 == MaxBacteria) {
     return (c(5, TRUE, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  if (MaxPhage == 5| limit==5) {
    return (c(5, FALSE, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}
  ResultS6<-Search(6, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet6<-ResultS6[-7]
  BestBacteria6<-ResultS6[7]

  rm(ResultS6)
  if (BestBacteria6 == MaxBacteria) {
    return (c(6, TRUE, PhageSet6, BestBacteria6, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  if (MaxPhage == 6| limit==6) {
    return (c(6, FALSE, PhageSet6, BestBacteria6, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}
  ResultS7<-Search(7, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet7<-ResultS7[-8]
  BestBacteria7<-ResultS7[8]

  rm(ResultS7)
  if (BestBacteria7 == MaxBacteria) {
    return (c(7, TRUE, PhageSet7, BestBacteria7, PhageSet6, BestBacteria6, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))}

  return (c(7, FALSE, PhageSet7, BestBacteria7, PhageSet6, BestBacteria6, PhageSet5, BestBacteria5, PhageSet4, BestBacteria4, PhageSet3, BestBacteria3, PhageSet2, BestBacteria2, PhageSet1, BestBacteria1))


}
