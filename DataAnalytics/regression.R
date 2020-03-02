data<-read.csv('mulr.csv')
attach(data)
y<-matrix(c(y1,y2,y3),19,3)
x<-matrix(c(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),x1,x2,x3),19,4)
xt<-t(x)
xt
xtx<-xt%*%x
xty<-xt%*%y
xtxinverse<-solve(xtx)
a<-xtxinverse%*%xty
a

fit<-lm(y1~x1)
summary(fit)
plot(fit)

fit<-lm(y1~x2)
summary(fit)
plot(fit)

fit<-lm(y1~x3)
summary(fit)
plot(fit)

fit<-lm(y1~x1+x2)
summary(fit)
plot(fit)

fit<-lm(y1~x1+x3)
summary(fit)
plot(fit)






