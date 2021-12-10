#. Simulate the process with given PMF ( particularly Binomial, Geometric and Poisson)
##expectation - m^n
#variance -m^(n-1)((m^n)-1)((sigma)^2)/m-1
#Poisson
#no.of generation: n=10- simulation
#lamda - rate of producing offspring in a generation
#case1: lambda=5
Z<-1
Z[1] = 1
lambda_1 = 5
N = 11
set.seed(45)
Z[2] = rpois(Z[1],lambda_1)
for (i in 3:N)
{
  if(Z[i-1]==0) {Z[i]=0} else 
  {
    x=rpois(Z[i-1], lambda_1) 
    Z[i] = sum(x)
  }
}
Z

# expected size of each generation
plot(Z, type = "l",ylim=c(0,14648440))
expectation<-(lambda_1)^10
variance<-(((lambda_1)^9)*(((lambda_1)^10)-1)*(lambda_1))/(lambda_1-1)
ul<-expectation+sqrt(variance)
ll<-expectation-sqrt(variance)
abline(h=ll,col="red",lty=2)
abline(h=ul,col="red",lty=2)
expectation
variance
ul
ll

#changing rate
#case2 : lamda=7
Z<-1
Z[1] = 1
lambda_1 =7
N = 11
set.seed(45)
Z[2] = rpois(Z[1],lambda_1)
for (i in 3:N)
{
  if(Z[i-1]==0) {Z[i]=0} else 
  {
    x=rpois(Z[i-1], lambda_1) 
    Z[i] = sum(x)
  }
}
Z

# expected size of each generation
plot(Z, type = "l",ylim=c(0,471954880))
expectation<-(lambda_1)^10
variance<-(((lambda_1)^9)*(((lambda_1)^10)-1)*(lambda_1))/(lambda_1-1)
ul<-expectation+sqrt(variance)
ll<-expectation-sqrt(variance)
abline(h=ll,col="red",lty=2)
abline(h=ul,col="red",lty=2)
expectation
variance
ul
ll

#Binomial
#p=1/4 ylim=c(0,95210)
#p=1/2 ylim=c(0,80000000)
#n-no.of offsprings in a generation p-probability of offsprings
x<-1
x[1] = 1
n = 12# no.of springs
p=1/4
q=1-p
N = 11# 10 generation
x[1] = 1
set.seed(45)
x[2] = rbinom(x[1],n,p)

for (i in 3:N)
{
  if(x[i-1]==0) {x[i]=0} else 
  {
    y=rbinom(x[i-1],n,p ) 
    x[i] = sum(y)
  }
}
x
expectation1<-(n*p)^10
variance1<-(((n*p)^9)*(((n*p)^10)-1)*(n*p*q))/((n*p)-1)
ul1<-expectation1+(sqrt(variance1))
ll1<-expectation1-(sqrt(variance1))
plot(x, type = "l",ylim=c(0,95210))
abline(h=ll1,col="red",lty=2)
abline(h=ul1,col="red",lty=2)
expectation1
variance1
ul1
ll1

#p=1/2
x<-1
x[1] = 1
n = 12# no.of springs
p=1/2
q=1-p
N = 11# 10 generation
x[1] = 1
set.seed(45)
x[2] = rbinom(x[1],n,p)

for (i in 3:N)
{
  if(x[i-1]==0) {x[i]=0} else 
  {
    y=rbinom(x[i-1],n,p ) 
    x[i] = sum(y)
  }
}
x
expectation1<-(n*p)^10
variance1<-(((n*p)^9)*(((n*p)^10)-1)*(n*p*q))/((n*p)-1)
ul1<-expectation1+(sqrt(variance1))
ll1<-expectation1-(sqrt(variance1))
plot(x, type = "l",ylim=c(0,80000000))
abline(h=ll1,col="red",lty=2)
abline(h=ul1,col="red",lty=2)
expectation1
variance1
ul1
ll1

#Geometric
#as p value decreases the rgeom values are different
#p=1/2
#p=1/12
N<-11
k<-1
p<-1/8
k[1]=1
set.seed(45)
k[2]=rgeom(k[1],prob=p)

for (i in 3:N){
  if(k[i-1]==0){k[i]=0}else{
    e<-rgeom(k[i-1],prob=p)
    k[i]=sum(e)
  }
}
k
#mean (1-p)/p
q<-(1-p)/p
expectation2<-((1-p)/p)^10
#variance (1-p)/(p^2)
r<-(1-p)/(p^2)
variance2<-(q^9)*((q^10)-1)*(r)/(q-1)
ul2<-expectation2+sqrt(variance2)
ll2<-expectation2-sqrt(variance2)
plot(k, type = "l",ylim=c(ll2,ul2))
abline(h=ll2,col="red",lty=2)
abline(h=ul2,col="red",lty=2)
expectation2
variance2
ul2
ll2
#p=1/2
N<-11
k<-1
p<-1/2
k[1]=1
set.seed(45)
k[2]=rgeom(k[1],prob=p)

for (i in 3:N){
  if(k[i-1]==0){k[i]=0}else{
    e<-rgeom(k[i-1],prob=p)
    k[i]=sum(e)
  }
}
k
#mean (1-p)/p
q<-(1-p)/p
expectation2<-((1-p)/p)^10
#variance (1-p)/(p^2)
r<-(1-p)/(p^2)
variance2<-(q^9)*((q^10)-1)*(r)/(q-1)
ul2<-expectation2+sqrt(variance2)
ll2<-expectation2-sqrt(variance2)
plot(k, type = "l",ylim=c(-43699080,608649580))
abline(h=ll2,col="red",lty=2)
abline(h=ul2,col="red",lty=2)
expectation2
variance2
ul2
ll2
#p=1/10
N<-11
k<-1
p<-1/10
k[1]=1
set.seed(45)
k[2]=rgeom(k[1],prob=p)

for (i in 3:N){
  if(k[i-1]==0){k[i]=0}else{
    e<-rgeom(k[i-1],prob=p)
    k[i]=sum(e)
  }
}
k
#mean (1-p)/p
q<-(1-p)/p
expectation2<-((1-p)/p)^10
#variance (1-p)/(p^2)
r<-(1-p)/(p^2)
variance2<-(q^9)*((q^10)-1)*(r)/(q-1)
ul2<-expectation2+sqrt(variance2)
ll2<-expectation2-sqrt(variance2)
plot(k, type = "l",ylim=c(ll2,ul2))
abline(h=ll2,col="red",lty=2)
abline(h=ul2,col="red",lty=2)
expectation2
variance2
ul2
ll2



# s v/s G(s)

#example 1
#po=0.2,p1=0.4,p3=0.2,p4=0.2
g=function(s){
  return(0.2+0.4*s+0.2*(s^3)+0.2*(s^4))
}
f=function(s){return(s)}
s=seq(0,1,by=0.01)
plot(s,g(s),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s,f(s),ylim=c(0,1),col="blue",type="l")

locator()
abline(v=0.3467463,col="green")
abline(h=0.3528926,col="green")
points(0.3467463,0.3528926,pch=16)

#example 2
#p0=0.25,p1=0.25,p2=0,p3=0.25,p4=0.2,p5=0.05
g1=function(s1){
  return(0.25+0.25*s1+0.25*(s1^3)+0.2*(s1^4)+0.05*(s1^5))
}
f1=function(s1){return(s1)}
s1=seq(0,1,by=0.01)
plot(s1,g1(s1),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s1,f1(s1),ylim=c(0,1),col="blue",type="l")
#abline(h=s, col="blue")
locator()
abline(v=0.3432394,col="green")
abline(h=0.3487603,col="green")
points(0.3432394,0.3487603,pch=16)

#example 3
#p0=0.25,p1=0.75
g2=function(s2){
  return(0.25+0.75*s2)
}
f2=function(s1){return(s2)}
s2=seq(0,1,by=0.01)
plot(s2,g2(s2),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s2,f2(s2),ylim=c(0,1),col="blue",type="l")
