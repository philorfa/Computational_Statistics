rm(list = ls())

# ERWTHMA 1

mc_integr <-function(n){
  
  for(a in 0:4){
  
  x<-rnorm(n,0,1)
  y<-(x+a)^2
  ret<-sum(y)/n
  
  print((paste("N = ", n," a =",a,"Result = ",ret, "Real Value=",1+a^2)))
  }
}

mc_integr(100)
mc_integr(1000)


# ERWTHMA 3


import_samp <-function(n){
  
  for(a in 0:4){
    
    x<-rnorm(n,a,1)
    y<-((x+a)^2)*exp(0.5*(a^2-2*a*x))
    ret<-sum(y)/n
    print((paste("N = ", n," a =",a,"Result = ",ret, "Real Value=",1+a^2)))
  }
}
import_samp(100)
import_samp(1000)

comp <-function(n){
  for(a in 0:4){
    sdmc<- sqrt((4*a^{2}+2)/n)
    sdimp<-sqrt((3*exp(a^2) - 1 - 2*(a^2) - a^4)/n)
    print((paste("N = ", n," a =",a,"MC = ",sdmc, "IM=",sdimp)))
  }
}

comp(100)
comp(1000)


# ERWTHMA 4

bootstrap <- function(b){
  boot<-vector()
  
  x<-rnorm(1000,0,1)
  
  for(i in 1:b){
    sel<-sample(x,1000,replace=TRUE)
    y<-(sel+4)^2
    ret<-sum(y)/1000
    boot<-append(boot,ret)
  }
  mean_boot<-sum(boot)/b
  se_boot<-sqrt((1/(b-1))*(sum((boot-mean_boot)^2)))
  theor_var<- (4*4^{2}+2)/1000
  theor_sd <-sqrt(theor_var)
  theor_se <- theor_sd/sqrt(1000)
  print(paste("Bootstrap SE = ", se_boot , "Theoretical SD",theor_sd))
}

bootstrap(50)
bootstrap(100)
bootstrap(200)
bootstrap(500)


# https://www.stat.cmu.edu/~ryantibs/advmethods/notes/bootstrap.pdf