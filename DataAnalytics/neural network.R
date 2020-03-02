#load packages
setwd("F:/Studies/Semester 4/Final code")
library(neuralnet)
library(ROCR)
library(InformationValue)
library(smbinning)

#Read the data set
data<-read.csv("data.csv",header = T)
str(data)
#Min-Max Normalisation
data$age<-(data$age-min(data$age))/(max(data$age)-min(data$age))
data$tot_bilirubin<-(data$tot_bilirubin-min(data$tot_bilirubin))/(max(data$tot_bilirubin)-min(data$tot_bilirubin))
data$direct_bilirubin<-(data$direct_bilirubin-min(data$direct_bilirubin))/(max(data$direct_bilirubin)-min(data$direct_bilirubin))
data$tot_proteins<-(data$tot_proteins-min(data$tot_proteins))/(max(data$tot_proteins)-min(data$tot_proteins))
data$albumin<-(data$albumin-min(data$albumin))/(max(data$albumin)-min(data$albumin))
data$ag_ratio<-(data$ag_ratio-min(data$ag_ratio))/(max(data$ag_ratio)-min(data$ag_ratio))
data$sgpt<-(data$sgpt-min(data$sgpt))/(max(data$sgpt)-min(data$sgpt))
data$sgot<-(data$sgot-min(data$sgot))/(max(data$sgot)-min(data$sgot))
data$alkphos<-(data$alkphos-min(data$alkphos))/(max(data$alkphos)-min(data$alkphos))
hist(data$age)
#create data set 
set.seed(240)
ind<-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train = data[ ind==1, ]
test = data[ ind==2, ] 
#build model.
set.seed(340)
train[is.na(train)]<-0
n<-neuralnet(is_patient~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos, data = train, hidden = 1,linear.output = FALSE)
plot(n)
output<-compute(n, train[,-1])
head(output$net.result)
head(train[1,])
in4<-0.77047+(-1.225*0.7093023256)+(-0.27142*0.004021447721)+(-3.20717*0.06057645335)+(-17.16156*0.003015075377)+(-0.91994*0.001626346818)+(-2.09198*0.5942028986)+(1.33009*0.5217391304)
out4<-1/(1+exp(-in4))
out4
in5<-(-4.14241)+(10.35961*out4)
out5<-1/(1+exp(-in5))
out5
  
#principal Component anaysis
#library(psych)
#pairs.panels(train[,-11],gap=0,pch=21)
#pc<-princomp(train[,-11])

kl<-cbind(age,gender,tot_bilirubin,direct_bilirubin,tot_proteins,albumin,ag_ratio,sgpt,sgot)
cor(kl)
pcal<-princomp(kl,scores=TRUE,cor=TRUE)
summary(pcal)
loadings(pcal)
plot(pcal)
screeplot(pcal,type="line",main="Scree Plot")
biplot(pcal)
pcal$scores[1:10,]




