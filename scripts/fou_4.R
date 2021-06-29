### slides from my Course: http://www.math.ntua.gr/~fouskakis/Programming_R/progr_slides.html 
### Useful for plotting: https://www.r-graph-gallery.com/ggplot2-package.html
### Google is your Friend


### Go to Tools -> Intall Packages and install all of them
### e.g. data.table,dplyr,ggplot2 etc

#library(VIF)
#library(BAS)
#library(data.table)
#library(dplyr)
#library(ggplot2)
#library(car)
#library(corrplot)
#library(ggpubr)
#library(readxl)

### Clean your Global Environment

rm(list = ls())


##### 4

### SET MODEL

set.seed(42)

X_10<-matrix(rnorm(10*50,0,1),ncol=10)


X_5 <- array(NA,dim = c(50,5))


for(j in 1:5){
  
  for(i in 1:50){
    
    mu <- (0.2*X_10[i,1]) + (0.4*X_10[i,2]) + (0.6*X_10[i,3]) + (0.8*X_10[i,4]) + (1.1*X_10[i,5])
    
    sim<-rnorm(1,mu,1)
    
    X_5[i,j] <- sim
    }
}

X <- cbind(X_10,X_5)
colnames(X) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")



Y<-array(NA,dim = c(50,1))

for(i in 1:50){
  mu<-(4+2*X[i,'X1']-X[i,'X5']+2.5*X[i,'X7']+1.5*X[i,'X11']+0.5*X[i,'X13'])
  Y[i]<- rnorm(1,mu,1.5) 
}

colnames(Y) <- c("Y")

X_inp <- as.data.frame(X)
Y_inp <- as.data.frame(Y)

linearMod <- lm(Y_inp[,"Y"] ~ ., data=X_inp)

###

### A ERWTHMA

models=data.frame(matrix(ncol = 15, nrow = ((2^15))))
as.numeric(intToBits(2^15))


for (i in 0:(2^15-1)){
  a=as.numeric(intToBits(i))
  for (j in 1:15){
    models[i, j]<-a[j]
  }
  print(i)
}


exhaustive_search <- function (X,Y){
  BIC <-rep (NA , nrow (models))
  reg0 <-lm(Y[,'Y']~1, data =X) 
  BIC [1] <-BIC ( reg0 )
  for (i in 1: (2^15-1)){
    data <-X[ which (models[i ,] %in% 1) ]
    mod <-lm(Y[,'Y'] ~ ., data = data )
    BIC[i] <- BIC(mod)
    print(i)
  }
  models <-data.frame(models,BIC)
  return(models[order(models$BIC) ,])
}


outcome<-exhaustive_search(X_inp,Y_inp)
head(outcome)
check<-lm(Y[,'Y']~., data =X_inp[,c("X1",'X5','X7','X11','X13')])
BIC(check)


### B ERWTHMA - 1 standard error rule

library(glmnet)
lasso <- glmnet(X_inp, Y_inp[,'Y'])
plot(lasso, label=T)
plot(lasso,xvar='lambda', label=T)

X_inp1<-as.matrix(X_inp)
lasso1 <- cv.glmnet(X_inp1, Y_inp[,'Y'])
plot(lasso1)

lasso1$lambda.min
log(lasso1$lambda.min)
lasso1$lambda.1se
log(lasso1$lambda.1se)
plot(lasso1)

lasso1

reg <-lm(Y_inp[,'Y']~., data=X_inp)

blasso <- coef(lasso1, s="lambda.min")
blasso

# coefficients for this lambda standardized data
zblasso <- blasso[-1] * apply(X_inp, 2, sd)
#ols coefficients for standardized data
zbols <- coef(reg)[-1] * apply(X_inp, 2, sd)
#s that minimizes
s <- sum(abs(zblasso)) / sum(abs(zbols))
print(s)


blasso_1se <- coef(lasso1)
zblasso_1se <- blasso_1se[-1] * apply(X_inp, 2, sd)
zbols_1se <- coef(reg)[-1] * apply(X_inp, 2, sd)
s_1se <- sum(abs(zblasso_1se)) / sum(abs(zbols_1se))
print(s_1se)

blasso_1se


apply(X_inp, 2, sd)
blasso_1se[-1]
apply(X_inp, 2, sd)

s_1se <- sum(abs(blasso_1se[-1])) / sum(abs(coef(reg)[-1]))
print(s_1se)
zblasso_1se

coef(reg)
