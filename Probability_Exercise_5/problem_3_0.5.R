N_sim = 100000;
lambda=0.5;
T_max=10;
Z_max=100;

p_Z=matrix(data=0, nrow=T_max+1,ncol=Z_max+2); 
for (n in 1:N_sim){
  Z=numeric(T_max+1);
  Z[1]=1;
  p_Z[1,1+1]=p_Z[1,1+1]+1;
  for (k in 1:T_max){
    Z[k+1]=sum(rpois(Z[k],lambda));
    if(Z[k+1]<=Z_max){
      p_Z[k+1,Z[k+1]+1]=p_Z[k+1,Z[k+1]+1]+1;
    }else{
      p_Z[k+1,Z_max+1]=p_Z[k+1,Z_max+1]+1;
    }
  }
  
}
p_Z=p_Z/N_sim;
E_Z=numeric(T_max+1);
E_Z[1]=1;
for (k in 2:(T_max+1)){
  E_Z[k]=sum((0:Z_max)*p_Z[k,1:(Z_max+1)]);
}
Time=0:T_max;
plot(Time,E_Z,ylim=c(0,1),xlab = "Time",ylab = "E[Z]")
lines(Time,E_Z,col="blue")

K=1:10
plot(K,p_Z[10,2:11], pch="o",xlab = "k", ylab = "P(Z=k)");
lines(K,p_Z[10,2:11], col="blue")
p_Z[10,1]