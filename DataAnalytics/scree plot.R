data<-read.csv("eig.csv",TRUE)
c<-cov(data)
e<-eigen(c)
j<-e$values
i<-j
scatter.hist(j,xlab="factor",ylab="eigen value",col="red")
screeplot(i,npcs=9,type="lines", main="screeplot")