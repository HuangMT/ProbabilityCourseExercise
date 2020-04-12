n=6;

PW=numeric(n+1);
for (k in 0:n){
  for (i in 0:(n-k)){
    print(n-k)
    print(i)
    PW[k+1]=PW[k+1]+(-1)^i / factorial(i);
  }
  PW[k+1]=PW[k+1]/factorial(k);
}

lambda=1
Poisson=numeric(n+1);
for (k in 0:n){
  Poisson[k+1]=lambda^k*exp(-lambda)/factorial(k);
}


plot(0:n,PW,col="blue", pch="o", xlab = "k", ylab = "Probability");
lines(0:n,PW,col="blue", lty=1,);
points(0:n,Poisson,col="red", pch="*");
lines(0:n,Poisson,col="red",lty=2)
legend(0.65*n,0.35,legend=c("P(W=k)","Poisson(1)"),col=c("blue","red"),
       pch=c("o","*"),lty=c(1,2))