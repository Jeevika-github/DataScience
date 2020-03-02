x<-read.csv('realdata.csv')
#x1<-x[,1]
x2<-x[,2]
x3<-x[,3]
x4<-x[,4]
x5<-x[,5]
x6<-x[,6]
x7<-x[,7]
x8<-x[,8]
x9<-x[,9]
x10<-x[,10]
x11<-x[,11]
y<-x[,12]

fit<-glm(y~x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,family=binomial)
summary(fit)
plot(fit)
y=(1.571e+00)+(2.062e-01)x2-(8.279e-02)x3-(9.640e-04)x4-(2.399e-02)x5
+(5.604e-02)x6-(2.000e-06)x7-(5.148e-06)x8+(1.002e-04)x9+(1.205e-03)x10+(9.089e-02)x11




