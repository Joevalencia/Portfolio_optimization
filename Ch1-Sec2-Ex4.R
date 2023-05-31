S1=5
S2=5
C=1
K1=5
K2=5
C=1
T=1
sig1=0.2
sig2=0.3
rho=0.3
r=0.1
n=10^5

Z1=rnorm(n)
Z2=rnorm(n)
S1new=S1*exp((r-0.5*sig1^2)*T+sig1*sqrt(T)*Z1)
S2new=S2*exp((r-0.5*sig2^2)*T+sig2*(rho*sqrt(T)*Z1+sqrt(1-rho^2)*sqrt(T)*Z2))
             
payoff=exp(-r*T)*ifelse(S1new<K1 & S2new<K2,C,0)
v=mean(payoff)
