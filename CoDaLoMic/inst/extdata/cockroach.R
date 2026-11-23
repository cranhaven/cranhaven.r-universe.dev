#Data extracted from the supplementary material of the article:
#Marín-Miret, J., Pérez-Cobas, A. E., Domínguez-Santos, R., Pérez-Rocher, B., Latorre, A., & Moya, A. (2024). Adaptability of the gut microbiota of the German cockroach Blattella germanica to a periodic antibiotic treatment. Microbiological Research, 287, 127863.
# We can find the article in: https://www.sciencedirect.com/science/article/pii/S0944501324002647
#The data contains the information of the relative abundance of the genera
#present in the gut microbiome of the cockroach K3 (cockroach treated by kanamycin duting three periods of time, days: 1–35,  36–70, 71–105)

Data=read_excel("1-s2.0-S0944501324002647-mmc6.xlsx")

#K3
names=data.frame(Data)[,1]
genera=data.frame(Data[,c("K3a001","K3a002","K3a003","K3a004","K3a005","K3a006","K3a007","K3a008","K3a009","K3a010","K3a011","K3a012","K3a013","K3a014","K3a015","K3a016","K3a017","K3a018","K3a019","K3a020","K3a021","K3a022","K3a023","K3a024","K3a025","K3a026","K3a027","K3a028","K3a029","K3a030","K3a031","K3a032","K3a033","K3a034","K3a035","K3a036","K3a037","K3a038","K3a039","K3a040","K3a041","K3a042","K3a043","K3a044","K3a045","K3a046","K3a047","K3a048","K3a049","K3a050","K3a051","K3a052","K3a053","K3a054","K3a055","K3a056","K3a057","K3a058","K3a059","K3a060","K3a061","K3a062","K3a063","K3a064","K3a065","K3a066","K3a067","K3a068","K3a069","K3a070","K3a071","K3a072","K3a073","K3a074","K3a075","K3a076","K3a077","K3a078","K3a079","K3a080","K3a081","K3a082","K3a083","K3a084","K3a085","K3a086","K3a087","K3a088","K3a089","K3a090","K3a091","K3a092","K3a093","K3a094","K3a095","K3a096","K3a097","K3a098","K3a099","K3a100","K3a101","K3a102","K3a103","K3a104","K3a105")])
rownames(genera)<-names

genera_format=cbind(1:105, t(genera))
colnames(genera_format)<-c("Time", rownames(genera))

cockroach=genera_format

#save(cockroach, file = "data/cockroach.RData",version = 2)
