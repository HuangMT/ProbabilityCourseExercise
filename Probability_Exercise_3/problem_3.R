lambda=1

n=20;
p=lambda/n;
Bin=numeric(n+1);
Poisson=numeric(n+1);
for (k in 0:n){
  Bin[k+1]=choose(n,k)*p^k*(1-p)^(n-k);
  Poisson[k+1]=lambda^k*exp(-lambda)/factorial(k);
}
plot(0:n,Bin,col="blue", pch="o", xlab = "k", ylab = "Probability",xlim=c(0,10));
lines(0:n,Bin,col="blue", lty=1,);
points(0:n,Poisson,col="red", pch="*");
lines(0:n,Poisson,col="red",lty=2)
legend(0.65*n,0.35,legend=c("Bin(n,p)","Poisson(np)"),col=c("blue","red"),
       pch=c("o","*"),lty=c(1,2))

