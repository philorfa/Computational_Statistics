set.seed(500)
n<-10000
poly<-rpois(n,lambda = rgamma(n,shape=2,rate=5))

a_old<-1
b_old<-1

reps_em<-1

repeat{
  C<-vector()
  D<-vector()
  
  for(i in 1:length(poly)){
    C_mid<-(poly[i]+a_old)/(1+b_old)
    D_mid<-digamma(poly[i]+a_old)-log(1+b_old)
    
    C<-append(C,C_mid)
    D<-append(D,D_mid)
    
  }
  
  b_new<-a_old*n/(sum(C))
  reps_nr<-1
  repeat{
    
    ar<-sum(D)+n*log(a_old)-n*log(sum(C)/n)-n*digamma(a_old)
    par<-(n/a_old)- n*trigamma(a_old)
    a_new<-a_old-(ar/par)
    
    
    if((a_new-a_old)^2<=10^(-10)){
      break
    }
    a_old<-a_new
  }
  
  
  
  if((a_new-a_old)^2+(b_new-b_old)^2<=10^(-10)){
    break
  }
  if(reps_em%%10==0){
  print(paste("Criterion:",(a_new-a_old)^2+(b_new-b_old)^2))
  print(paste("a:",a_new))
  print(paste("b:",b_new))
  print(paste("EM REP COUNTER:",reps_em))
  }
  a_old<-a_new
  b_old<-b_new
  reps_em<-reps_em+1
}
print(paste(a_new))
print(paste(b_new))
print(reps_em)

print(paste("Criterion:",(a_new-a_old)^2+(b_new-b_old)^2))
