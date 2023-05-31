#Valoracion de call europea
K=10 #Strike
S0=11 #Spot
r=0.06 #Interes libre de riesgo
sig=0.3 #Volatilidad
T=1 #Vencimiento
n=10^6 #Tananyo muestra Monte Carlo

valoracionMC=function(K,S0,r,sig,T,n)
{
  Z=rnorm(n)
  S=S0*exp((r-0.5*sig^2)*T+sig*sqrt(T)*Z)
  payoff=exp(-r*T)*pmax(S-K,0)
  vcallMC=mean(payoff)
  #IC 95%
  ICl=vcallMC-1.96*sd(payoff)/sqrt(n)
  ICr=vcallMC+1.96*sd(payoff)/sqrt(n)
  return(c(vcallMC,ICl,ICr))
}

valoracionBS=function(K,S0,r,sig,T)
{
  d1=(log(S0/K)+(r+0.5*sig^2)*T)/(sig*sqrt(T))
  d2=d1-sig*sqrt(T)
  vcall=S0*pnorm(d1)-exp(-r*T)*K*pnorm(d2)
  return(vcall)
}

#Valor call
tmp=valoracionMC(K,S0,r,sig,T,n)
vMC=tmp[[1]]
ICl=tmp[[2]]
ICr=tmp[[3]]
vBS=valoracionBS(K,S0,r,sig,T)

#Error
print("Error MC")
abs(vMC-vBS)
print("Error teorico")
1/sqrt(n)





