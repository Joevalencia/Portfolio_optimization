#Valoracion de put europea con reduccion de varianza
K=10 #Strike
S0=5 #Spot
r=0.06 #Interes libre de riesgo
sig=0.3 #Volatilidad
T=1 #Vencimiento
n=10^6 #Tananyo muestra Monte Carlo

valoracionMC=function(K,S0,r,sig,T,n)
{
  Z=rnorm(n)
  S=S0*exp((r-0.5*sig^2)*T+sig*sqrt(T)*Z)
  payoff=exp(-r*T)*pmax(K-S,0)
  vputMC=mean(payoff)
  #IC 95%
  ICl=vputMC-1.96*sd(payoff)/sqrt(n)
  ICr=vputMC+1.96*sd(payoff)/sqrt(n)
  return(c(vputMC,ICl,ICr))
}

valoracionMCVA=function(K,S0,r,sig,T,n)
{
  Z=rnorm(n)
  S=S0*exp((r-0.5*sig^2)*T+sig*sqrt(T)*Z)
  SVA=S0*exp((r-0.5*sig^2)*T+sig*sqrt(T)*(-Z))
  payoff=exp(-r*T)*pmax(K-S,0)
  payoffVminus=exp(-r*T)*pmax(K-SVA,0)
  payoffVA=0.5*(payoff+payoffVminus)
  
  vputMCVA=mean(payoffVA)
  #IC 95%
  ICl=vputMCVA-1.96*sd(payoffVA)/sqrt(n)
  ICr=vputMCVA+1.96*sd(payoffVA)/sqrt(n)
  covar=cov(payoff,payoffVminus)
  return(c(vputMCVA,ICl,ICr,covar))
}

valoracionBS=function(K,S0,r,sig,T)
{
  d1=(log(S0/K)+(r+0.5*sig^2)*T)/(sig*sqrt(T))
  d2=d1-sig*sqrt(T)
  vput=exp(-r*T)*K*pnorm(-d2)-S0*pnorm(-d1)
  return(vput)
}

#Valor exacto put
vBS=valoracionBS(K,S0,r,sig,T)

#Valor put MC
tmp=valoracionMC(K,S0,r,sig,T,n)
vMC=tmp[[1]]
ICl=tmp[[2]]
ICr=tmp[[3]]

#Valor put MC con VA
tmpVA=valoracionMCVA(K,S0,r,sig,T,n)
vMCVA=tmpVA[[1]]
IClVA=tmpVA[[2]]
ICrVA=tmpVA[[3]]
covar=tmpVA[[4]]

#Error
print("Error MC")
abs(vMC-vBS)
print("Error MC VA")
abs(vMCVA-vBS)
print("Error teorico")
1/sqrt(n)





