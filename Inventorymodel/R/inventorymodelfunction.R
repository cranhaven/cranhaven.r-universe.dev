inventorymodelfunction <-
function(model=c("EOQ","EPQ","STI","FOC","MCT","MWHC","MWHC2","MWHCCT"),
n=NA,a=NA,av=NA,d=NA,h=NA,m=NA,r=NA,K=NA,b=NA,c1=NA,c2=NA,cooperation=c(0,1),allocation=c(0,1)){

if (model=="EOQ"){ 

cat("EOQ", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|is.na(h)==TRUE)>=1|length(d)!=n|length(h)!=n|(sum(is.na(d))>=1& sum(is.na(m))>=1)){
sol<-c("Error: invalid data")
}else{

if (cooperation==0){
sol<-EOQ(n,a,d,h,m)
      if (allocation==1){
sol<-list(sol,SOC(n,a,d,h,m,model="EOQ"))
names(sol)<-c("*","SOC rule")
}
}
if (cooperation==1){
sol<-EOQcoo(n,a,d,h,m)
if (allocation==1){
sol<-list(sol,SOC(n,a,d,h,m,model="EOQ"))
names(sol)<-c("*","SOC rule")
}
}
}
}
if (model=="EPQ"){

cat("EPQ", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|is.na(h)==TRUE|length(r)!=n|length(b)!=n|length(d)!=n|length(h)!=n|is.na(r)==TRUE|is.na(r)==TRUE)>=1| (sum(is.na(d))>=1& sum(is.na(m))>=1)){
sol<-c("Error: invalid data")
}else{
if (cooperation==0){
sol<-EPQ(n,a,d,h,m,r,b)
if (allocation==1){
sol<-list(sol,SOC(n,a,d,h,m,r,b,model="EPQ"))
names(sol)<-c("*","SOC rule")
}
}
if (cooperation==1){
sol<-EPQcoo(n,a,d,h,m,r,b)
if (allocation==1){
sol<-list(sol,SOC(n,a,d,h,m,r,b,model="EPQ"))
names(sol)<-c("*","SOC rule")
}
}
}}
if (model=="STI"){
cat("STI", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|length(av)!=n|length(d)!=n|length(h)!=n|is.na(h)==TRUE|is.na(av)==TRUE)>=1| (sum(is.na(d))>=1& sum(is.na(m))>=1)){
sol<-c("Error: invalid data")
}else{
if (cooperation==0){sol<-STI(n,a,av,d,h,m)}
if (cooperation==1){sol<-STIcoo(n,a,av,d,h,m)}
if (allocation==1){
cat("Optimal solution", sep="\n")
sol<-list(sol,linerulecoalitional(n,a,av,d,h,m))
names(sol)<-c("*","Allocation")
}
}
}
if (model=="FOC"){
cat("FOC", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|is.na(d)==TRUE|is.na(K)==TRUE|length(d)!=n|length(K)!=n)>=1){
sol<-c("Error: invalid data")
}else{
sol<-mfoc(n,a,d,K,cooperation)
if (allocation==1){
sol<-list(sol,shapley_mfoc(n,a,d,K))
names(sol)<-c("","Allocation")
}
}}
if (model=="MCT"){
cat("MCT", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|length(av)!=n|length(d)!=n|length(K)!=n|is.na(av)==TRUE|is.na(d)==TRUE|is.na(K)==TRUE)>=1){
sol<-c("Error: invalid data")
}else{
sol<-mct(n,a,av,d,K,cooperation)
if (allocation==1){
sol<-list(sol,twolines(n,a,av,d,K))
names(sol)<-c("Optimal solution","Allocation two-lines rule")
}
}
}
if (model=="MWHC"){
cat("MWHC", sep="\n")
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|is.na(b)==TRUE|length(b)!=n|length(d)!=n|length(K)!=n|is.na(d)==TRUE|is.na(K)==TRUE)>=1){
sol<-c("Error: invalid data")
}else{
sol<-mwhc(n,a,b,d,K,cooperation,allocation)
}
}
if (model=="MWHC2"){
cat("MWHC2C", sep="\n") 
if (sum(is.na(n)==TRUE|is.na(a)==TRUE|is.na(c1)==TRUE|is.na(c2)==TRUE|is.na(b)==TRUE|length(b)!=n|length(d)!=n|length(K)!=n|is.na(d)==TRUE|is.na(K)==TRUE)>=1){
sol<-c("Error: invalid data")
}else{
sol<-mwhc2c(n,a,b,d,K,c1,c2,cooperation,allocation)
}
}
  if (model=="MWHCCT"){
    cat("MWHCCT", sep="\n")
    if (sum(is.na(n)==TRUE|is.na(a)==TRUE|length(av)!=2^n|length(d)!=n|length(K)!=n|is.na(av)==TRUE|is.na(d)==TRUE|is.na(K)==TRUE)>=1){
      sol<-c("Error: invalid data")
    }else{
      sol<-mwhcct(n,a,av,d,K,cooperation,allocation)
      names(sol)<-c("Optimal solution","Allocation two-lines rule")
      
    }
  }
return(sol)}
