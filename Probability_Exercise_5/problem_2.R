
p=1/3;
theta=1/4;
N_sim = 10000;

T_max=10;
p_T=numeric(T_max+2);    #仿真结果
for (n in 1:N_sim){
  N=rgeom(1,1-theta);
  X=rbinom(N,1,p);
  T=sum(X);
  if(T<=T_max){
    p_T[T+1]=p_T[T+1]+1;
  }else{
    p_T[T_max+2]=p_T[T_max+2]+1;
  }
}
p_T=p_T/N_sim;
p_T=p_T[1:(T_max+1)];
T_list=0:T_max;
plot(T_list,p_T,xlab = "k", ylab = "P(T=k)");
#plot(p_T)

p_T_g=numeric(T_max+1);    #理论结果
q=(1-theta)/(1-theta+theta*p)
for (k in 1:(T_max+1)){
  p_T_g[k]=q*(1-q)^(k-1);
}
lines(T_list,p_T_g, col="blue")