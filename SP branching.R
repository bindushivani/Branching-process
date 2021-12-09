#. Simulate the process with given PMF ( particularly Binomial, Geometric and Poisson)
#Poisson
#lamda=5 
#no.of offsprings: k=30
#no.of generation: n=10- simulation
#initialize list of population size at generation n
Z<-1

#pop. size 1 at generation 0
Z[1] = 1

#poisson r.v.
#each member generates poisson(lambda) offspring
#sum offspring
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

plot(Z, type = "l",ylim=c(0,14648440))

# expected size of each generation
#expectation - m^n
#variance -m^(n-1)((m^n)-1)((sigma)^2)/m-1
expectation<-5^10
variance<-((5^9)*((5^10)-1)*(5))/4
ul<-expectation+sqrt(variance)
ll<-expectation-sqrt(variance)
abline(h=ll,col="red",lty=2)
abline(h=ul,col="red",lty=2)

#Binomial
x<-1
x[1] = 1
n = 12# no.of springs
p=1/4
N = 11# 10 generation
x[1] = 1
set.seed(45)
x[2] = rbinom(x[1],12,1/4)

for (i in 3:N)
{
  if(x[i-1]==0) {x[i]=0} else 
  {
    y=rbinom(x[i-1],12,1/4 ) 
    x[i] = sum(y)
  }
}
x

expectation1<-(n*p)^10
variance1<-((3^9)*((3^10)-1)*(2.25))/2

ul1<-expectation1+(sqrt(variance1))
ll1<-expectation1-(sqrt(variance1))
plot(x, type = "l",ylim=c(0,95210))
abline(h=ll1,col="red",lty=2)
abline(h=ul1,col="red",lty=2)

#Geometric
#as p value decreases the rgeom values are different
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
expectation2
#variance (1-p)/(p^2)
#variance -m^(n-1)((m^n)-1)((sigma)^2)/m-1
r<-(1-p)/(p^2)
variance2<-(q^9)*((q^10)-1)*(r)/(q-1)
#
ul2<-expectation2+sqrt(variance2)
ul2
ll2<-expectation2-sqrt(variance2)
ll2
plot(k, type = "l",ylim=c(-43699080,608649580))
abline(h=ll2,col="red",lty=2)
abline(h=ul2,col="red",lty=2)

# 3rd subdivision

##po=0.2,p1=0.4,p3=0.2,p4=0.2
g=function(s){
  return(0.2+0.4*s+0.2*(s^2)+0.2*(s^3))
}
f=function(s){return(s)}
s=seq(0,1,by=0.01)
plot(s,g(s),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s,f(s),ylim=c(0,1),col="blue",type="l")
#abline(h=s, col="blue")
locator()
abline(v=0.4119109,col="green")
abline(h=0.4190083,col="green")
points(0.4119109,0.4190083,pch=16)

#p0=0.25,p1=0.25,p2=0,p3=0.25,p4=0.2,p5=0.05
g1=function(s1){
  return(0.25+0.25*s1+0.25*(s1^3)+0.05*(s1^5))
}
f1=function(s1){return(s1)}
s1=seq(0,1,by=0.01)
plot(s1,g1(s1),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s1,f1(s1),ylim=c(0,1),col="blue",type="l")
#abline(h=s, col="blue")
locator()
abline(v=0.3454946,col="green")
abline(h=0.3487603,col="green")
points(0.3454946,0.3487603,pch=16)

#p0=0.25,p1=0.75
g2=function(s2){
  return(0.25+0.75*s2)
}
f2=function(s1){return(s2)}
s2=seq(0,1,by=0.01)
plot(s2,g2(s2),ylim=c(0,1),col="red",type="l")
par(new=T)
plot(s2,f2(s2),ylim=c(0,1),col="blue",type="l")
