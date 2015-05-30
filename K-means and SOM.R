library(kohonen)
library(ElemStatLearn)
data(nci)
data<-scale(nci)
set.seed(7)
## k = 2

store.simlar.2<-c()
for(i in c(0.3,0.67,1,2,3)){
  km.2<- kmeans(nci,2)
  som.2<-som(data, grid=somgrid(2,1,"hexagonal"), radius =  i )
  simlar.2<-RRand(km.2$cluster, som.2$unit.classif)$Rand
  store.simlar.2<-c(store.simlar.2,simlar.2)
}
store.simlar.2 # 0.9459029 0.9439690 0.9146640 0.9428658 0.9208210

quartz()
plot(c(0.3,0.67,1,2,3),store.simlar.2,lty=2,type="b")

## k =5

store.simlar.5<-c()
for(i in c(0.3,0.67,1,2,3)){
  km.5<- kmeans(nci,20)
  som.5<-som(data, grid=somgrid(5,1,"hexagonal"), radius =  i )
  simlar.5<-RRand(km.5$cluster, som.5$unit.classif)$Rand
  store.simlar.5<-c(store.simlar.5,simlar.5)
}
store.simlar.5 # 0.6483224 0.6420039 0.6369357 0.6516280 0.6381715

quartz()
plot(c(0.3,0.67,1,2,3),store.simlar.5,lty=2,type="b",main="K=5")

## k = 10

store.simlar.10<-c()
for(i in c(0.3,0.67,1,2,3)){
  km.10<- kmeans(nci,10)
  som.10<-som(data, grid=somgrid(5,2,"hexagonal"), radius =  i )
  simlar.10<-RRand(km.10$cluster, som.10$unit.classif)$Rand
  store.simlar.10<-c(store.simlar.10,simlar.10)
}
store.simlar.10 # 0.7758879 0.8472734 0.7798241 0.7932066 0.8037037

quartz()
plot(c(0.3,0.67,1,2,3),store.simlar.10,lty=2,type="b",main="K=10")

## k = 20

store.simlar.20<-c()
for(i in c(0.3,0.67,1,2,3)){
  km.20<- kmeans(nci,20)
  som.20<-som(data, grid=somgrid(5,4,"hexagonal"), radius =  i )
  simlar.20<-RRand(km.20$cluster, som.20$unit.classif)$Rand
  store.simlar.20<-c(store.simlar.20,simlar.20)
}
store.simlar.20 # 0.8707991 0.8681026 0.8597909 0.8616545 0.8536264

quartz()
plot(c(0.3,0.67,1,2,3),store.simlar.20,lty=2,type="b",main="K=20")
