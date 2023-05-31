m=matrix(c(6000,6200,6300,6350,6400,6600,6800,80.2,47.1,35.9,31.3,27.7,16.6,11.4),ncol=2)
colnames(m)=c("K","Vmar")

BS=function(S,K,T,r,sigma)
{
  d1=(log(S/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2=d1-sigma*sqrt(T)
  
  phid1=pnorm(d1,mean=0,sd=1,log=FALSE)
  phid2=pnorm(d2,mean=0,sd=1,log=FALSE)
  
  vcall=S*phid1-exp(-r*T)*K*phid2
  
  return(vcall)
}

vega=function(S,K,T,r,sigma)
{
 d1=(log(S/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
 phid1=dnorm(d1,mean=0,sd=1,log=FALSE)
 
 return(S*sqrt(T)*phid1)
}

newton=function(sigma0,tolx,tolf,maxiter,S,K,T,r,Vmar)
{
  f0=Vmar-BS(S,K,T,r,sigma0)
  fp0=-vega(S,K,T,r,sigma0)
  sigma1=sigma0-f0/fp0
 
  f1=Vmar-BS(S,K,T,r,sigma1)
  
  k=1
  while(abs(f1)>tolf && abs(sigma1-sigma0)>tolx && k<maxiter)
  {
    fp1=-vega(S,K,T,r,sigma1)
    sigma2=sigma1-f1/fp1
   
    sigma0=sigma1
    sigma1=sigma2
        
    f1=Vmar-BS(S,K,T,r,sigma1)
        
    k=k+1
  }
  return(sigma1)
}

sigmavec=c()
for(i in 1:7) sigmavec[i]=newton(0.5,10^(-6),10^(-6),25,5290.36,m[i,1],0.211,0.0328,m[i,2])

m=cbind(m,sigmavec)
colnames(m)=c("K","Vmar","volatilidad")

plot(m[,"K"],m[,"volatilidad"])