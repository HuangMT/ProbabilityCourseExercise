N_sim = 10000;
T_max=20;
p_T=numeric(T_max+1); 
for (n in 1:N_sim){
  S=0;
  T=0;
  while (S!=1 && T<=T_max) {
    X=2*rbinom(1,1,0.5)-1;
    S=S+X;
    T=T+1;
  }
  if(T<=T_max){
    p_T[T]=p_T[T]+1;
  }else{
    p_T[T_max+1]=p_T[T_max+1]+1;
  }
}
p_T=p_T/N_sim;
p_T=p_T[1:T_max];
T_list=1:T_max;
plot(T_list,p_T,xlab = "k", ylab = "P(T=k)");
lines(T_list,p_T, col="blue")