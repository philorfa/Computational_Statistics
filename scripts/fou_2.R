rm(list = ls())

# ERWTHMA 1

# cdf equals to pdf F(x)=e^x/(e^3-1)
# inverse ln((1+e^3)x)

inverse <-function(){
  
  U<-runif(1000,0,1)
  samples<-log((exp(3)-1)*U +1)
  
  hist(samples,
       prob = TRUE)
  curve(exp(x)/(exp(3)-1),add=TRUE,col = "red")
  legend("topleft", 
         legend = c("f(x)", "Inverse Method"), 
         col = c('red', 
                'grey'), 
         pch = c(17,19), 
         bty = "n", 
         text.col = "black", 
         horiz = F , 
         inset = c(0.1, 0.1))
}

inverse()


# ERWTHMA 2

phi_x<-function(x){
  return (exp(x)/(exp(3)-1))
}

M_uni<-3*exp(3)/(exp(3)-1)
print(M_uni)

rejection_uni<-function(){
  acc<-0
  reject<-0
  samples<-vector()
  while(acc<1000){
    y<-runif(1,0,3)
    u<-runif(1)
    paron_uni<-M_uni/3
    if((phi_x(y)/paron_uni)>=u){
      acc=acc+1
      samples<-append(samples,y)
    }
    else{
      reject<-reject+1
    }
  }
  print(reject)
  hist(samples,prob = TRUE)
  curve(exp(x)/(exp(3)-1),add=TRUE,col = "red")
  legend("topleft", 
         legend = c("f(x)", "Rejection Method"), 
         col = c('red', 
                 'grey'), 
         pch = c(17,19), 
         bty = "n", 
         text.col = "black", 
         horiz = F , 
         inset = c(0.1, 0.1))
  
}

rejection_uni()

#ERWTHMA 3


set.seed(4)
inverse_3 <-function(){
  
  U<-runif(100,0,1)
  samples<-log((exp(3)-1)*U +1)
  
  return (samples)
}

sim<-inverse_3()


epanechnikov<-function(x){
  
  if(abs(x)<1){
    ret<-3*(1-x^2)/4
  }
  else{
    ret<-0
  }
  return (ret)
}

leave_one_out_f<-function(h,curr,i){
  N<-99
  pol<-1/(99*h)
  vect<-sim[-i]
  ep<-0
  for(j in 1:99){
    val<-(curr-vect[j])/h
    epan<-epanechnikov(val)
    ep<-ep+epan
  }
  return (ep*pol)
}

calculate_likel<-function(h)
{
  temp<-1
  for (j in 1:100){
    
      
      temp<-temp*leave_one_out_f(h,sim[j],j)
    
  }
  
  return(temp)
}


max_likelihood<-function(){
  h<-seq(0.001, 0.5, by = 0.001)
  max<-0
  h_opt<-0
  for(i in 1:length(h)){
    print(i)
    res<-calculate_likel(h[i])
    if (res>max){
      max<-res
      h_opt<-h[i]
    }
  }
  
  return (h_opt)
  
}

h_final<-max_likelihood()


f_hat<-function(x){
  par<-1/(100*h_final)
  sum<-0
  for(i in 1:100){
    val<-(x-sim[i])/h_final
    ep<-epanechnikov(val)
    sum<-sum+ep
  }
  return (par*sum)
}

x_val<-seq(0,3,0.001)

points<-vector()

for(i in 1:length(x_val)){
  points<-append(points,f_hat(x_val[i]))
}
plot(x_val,points,cex=0.1)
curve(exp(x)/(exp(3)-1),add=TRUE,col = "red")
legend("topleft", 
       legend = c("f(x)", "KDE"), 
       col = c('red', 
               'black'), 
       pch = c(17,19), 
       bty = "n", 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### ERWTHMA 4

set.seed(10)
inverse_4 <-function(){
  
  U<-runif(10,0,1)
  samples<-log((exp(3)-1)*U +1)
  
  return (samples)
}

simul<-inverse_4()
m<-mean(simul)
T<-abs(m-2)
simul_m<-simul-m+2


bootstrap_4_p <- function(){
  b<-1000
  boot_p<-vector()
  for(i in 1:b){
    sel<-sample(simul_m,10,replace=TRUE)
    med<-abs(mean(sel)-2)
    boot_p<-append(boot_p,med)
  }
  
  ar<-sum(boot_p>T)+1
  par<-b+1
  return (ar/par)
 
}

print(bootstrap_4_p())

bootstrap_4_95 <- function(){
  b<-1000
  boot_95<-vector()
  for(i in 1:b){
    sel<-sample(simul,10,replace=TRUE)
    boot_95<-append(boot_95,mean(sel))
  }
  
  ci=sort(boot_95)[25:975]
  hist(ci)
  print(paste("[",ci[1],",",ci[951],"]"))
  if((ci[1]<=2)&&(ci[950]>=2)){
    print("H0 NOT REJECTED")
  }
  else{
    print("HO REJECTED")
  }
  
}

bootstrap_4_95()
