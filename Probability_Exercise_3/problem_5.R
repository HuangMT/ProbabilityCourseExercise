n=30;
m=n*(n-1);


PW=numeric(n*(n-1)/2+1);
N_sim <- 10000
for (n_sim in 1:N_sim){
  W=0;
  y=sample(1:m, n, repl=T);
  for (i in 2:n){
    for (j in 1:(i-1)){
      if (y[i]==y[j]){
        W=W+1;
      }
    }
  }
  PW[W+1]=PW[W+1]+1;
}
PW=PW/N_sim;

n_max=n*(n-1)/2;
lambda=n*(n-1)/2/m;
Poisson=numeric(n_max+1);
for (k in 0:n_max){
  Poisson[k+1]=lambda^k*exp(-lambda)/factorial(k);
}
plot(0:n_max,PW,col="blue", pch="o", xlab = "k", ylab = "Probability",xlim=c(0,10));
lines(0:n_max,PW,col="blue", lty=1,);
points(0:n_max,Poisson,col="red", pch="*");
lines(0:n_max,Poisson,col="red",lty=2)
legend(0.65*n_max,0.55,legend=c("P(W=k)","Poisson(1)"),col=c("blue","red"),
       pch=c("o","*"),lty=c(1,2))