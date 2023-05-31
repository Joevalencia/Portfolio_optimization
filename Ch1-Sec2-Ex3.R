#Valoracion de call asiatica
K=10 #Strike
S0=11 #Spot
r=0.06 #Interes libre de riesgo
sig=0.3 #Volatilidad
T=1 #Vencimiento
n=10^6 #Tamano muestra Monte Carlo
m=25 #Fechas de monitorizacion
Dt=T/m #Paso de tiempo

valoracionMC=function(K,S0,r,sig,T,n,m)
{
  Sold=rep(S0,n) 
  print(Sold)
  Save=(1/(m+1))*Sold
  print(Save)
  for(i in 1:m)
  {
    Z=rnorm(n)
    S=Sold*exp((r-0.5*sig^2)*Dt+sig*sqrt(Dt)*Z)
    
    Sold=S
    #print(Sold)
    Save=Save+(1/(m+1))*Sold
    #print(Save)
  }
  payoff=exp(-r*T)*pmax(Save-K,0)
  vcallMC=mean(payoff)
  #IC 95%
  ICl=vcallMC-1.96*sd(payoff)/sqrt(n)
  ICr=vcallMC+1.96*sd(payoff)/sqrt(n)
  return(c(vcallMC,ICl,ICr))
}

#Valor call asiatica
tmp=valoracionMC(K,S0,r,sig,T,n,m)
vMC=tmp[[1]]
ICl=tmp[[2]]
ICr=tmp[[3]]






