N=10^8
z=runif(N)
f=5*z^4
m=mean(f)
error=abs(1-m)
errort=1/sqrt(N)
