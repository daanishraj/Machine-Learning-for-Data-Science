
setwd("/Users/DaanishRaj/Daanish_ALL/Aug 19 2014/Columbia 2014-16/Spring 2017/ML for Data Science/HW/HW 1/hw1-data")

options(scipen = 999)
library(dplyr)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
library(boot)

rm(list=ls())

X<-read.csv("X_train.csv")
df<-as.data.frame(X)
Xmatrix<-data.matrix(df)

y<-read.csv("y_train.csv")
yvector<-data.matrix(y)
##X<-matrix(X)

###svd

s<-svd(Xmatrix)
S<-diag(s$d)
V<-s$v
U<-s$u

###we want to generate wRR= VMVtwLS

##generate wLS= (XtX)^-1 XtX
wLS<-(solve(t(Xmatrix)%*%Xmatrix))%*%(t(Xmatrix)%*%yvector)
I<-diag(ncol(Xmatrix))
S2<-S%*%S

dflambda<-rep(0, 5001)
wRRmat<-matrix(nrow=5001,ncol=7)


for (i in 0:5000){
  lambda<-i
  M<-solve(lambda*solve(S2) + I)
  wRR<-V%*%M%*%t(V)%*%wLS
  dflambda[i]<-sum(diag(M))
  wRRmat[i+1,]<-t(wRR)
}


dflambdaMat_1<-c(0,0)
dflambdaMat_2<-c(0,0)

dflambdaMat_1<-cbind(dflambda)

dflambdaMat_2<-cbind(wRRmat[,1])
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,2])  
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,3])  
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,4])  
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,5])  
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,6])
dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,7])  


''' for some reason, this simple for loop doesnt giver the right results - how
does one construct a new matrix using a for loop
for (k in 1:6){
  dflambdaMat_2<-cbind(dflambdaMat_2, wRRmat[,k])  
}
'''

matplot(dflambdaMat_1,dflambdaMat_2, type = "l", pch=1, col = 1:7, xlab="dflambda",
        ylab='', main="Comparing Predictors") #plot
legend("bottomleft", legend = 1:7, col=1:7, pch=1) 



'''
###plot the 

plot(dflambda, wRRmat[,1], type='l', xlim=range(0,7), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,2], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,3], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,4], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,5], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,6], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')
plot(dflambda, wRRmat[,7], type='l', xlim=range(0,7), ylim=range(-2,2), xlab= 'df(lambda)', ylab='')

plot(dflambda, wRRmat[,1], type='l', xlim=range(0,7), ylim=range(-2,5), xlab= 'df(lambda)', ylab='')
# par(new=TRUE)


for(j in 2:7){
  # par(new=TRUE)
  lines(dflambda, wRRmat[,j], type='l', xlab= 'df(lambda)', ylab='', axes=FALSE)
}




for(j in 2:7){
  par(new=TRUE)
  plot(dflambda, wRRmat[,j], axes = FALSE)
}

'''


####(c)
X_test<-read.csv("X_test.csv")
df_test<-as.data.frame(X_test)
X_test_matrix<-data.matrix(df_test)

y_test<-read.csv("y_test.csv")
y_test_vector<-data.matrix(y_test)


##y_pred<-rep(0,42)

RMSE<-rep(0,51)

for (i in 1:51){
  error<-0
  weights<-as.matrix(wRRmat[i,])
    for (j in 1:42){
    obs<-as.matrix(X_test_matrix[j,])
    ##y_pred<-wRRmat[i,]%*% (X_test_matrix[j,])

    y_pred<-t(weights)%*%obs
    error<-error + (y_test[j,]-y_pred)^2
  }
  RMSE[i]<-(error/42)^0.5
}

lambda<-c(0:50)

plot(lambda, RMSE, type='l')


####Part 2

###generating different data matrices for different values of p
'''
construct<-function(X,p)
{
  data<-c()
  for (i in 1:6)
  {
    for (j in 1:p)
    {
      data<-cbind(data,(X[,i])**p)
    }
  }
  data<-cbind(data,X[,7])
  return(data)
}
'''


'''brute force construction - functions not working :(
'''



########Part 2 d)

testErrorMat<-matrix(nrow=501, ncol=3)

########when p=1


Xmatrix_1<-Xmatrix


s<-svd(Xmatrix_1)
S<-diag(s$d)
V<-s$v
U<-s$u

###we want to generate wRR= VMVtwLS

##generate wLS= (XtX)^-1 XtX
wLS<-(solve(t(Xmatrix_1)%*%Xmatrix_1))%*%(t(Xmatrix_1)%*%yvector)
I<-diag(ncol(Xmatrix_1))
S2<-S%*%S

#dflambda<-rep(0, 5001)
wRRmat_1<-matrix(nrow=501,ncol=7)


for (i in 0:500){
  lambda<-i
  M<-solve(lambda*solve(S2) + I)
  wRR<-V%*%M%*%t(V)%*%wLS
  #dflambda[i]<-sum(diag(M))
  wRRmat_1[i+1,]<-t(wRR)
}

##X_testmatrix_2<-construct(X_test_matrix, 2)
X_test_matrix_1<-X_test_matrix


for (i in 1:501){
  error<-0
  weights<-as.matrix(wRRmat_1[i,])
  for (j in 1:42){
    obs<-as.matrix(X_test_matrix_1[j,])
    ##y_pred<-wRRmat[i,]%*% (X_test_matrix[j,])
    
    y_pred<-t(weights)%*%obs
    error<-error + (y_test[j,]-y_pred)^2
  }
  testErrorMat[i, 1]<-(error/42)^0.5
}




#########when p=2

Xmatrix_2<-c()
Xmatrix_2<-cbind(Xmatrix_2,Xmatrix[,1]**2)
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,2]**2)
Xmatrix_2<-cbind(Xmatrix_2,Xmatrix[,3]**2)
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,4]**2)
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,5]**2)
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,6]**2)
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,1])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,2])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,3])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,4])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,5])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,6])
Xmatrix_2<-cbind(Xmatrix_2, Xmatrix[,7])



'''
construct<-function(X,p)
{
  data<-cbind(data,X,X**2)
}  
'''
  
###Xmatrix_2<-construct(Xmatrix, 2)
##Xmatrix_3<-construct(Xmatrix, 3)



###when p=2

s<-svd(Xmatrix_2)
S<-diag(s$d)
V<-s$v
U<-s$u

###we want to generate wRR= VMVtwLS

##generate wLS= (XtX)^-1 XtX
wLS<-(solve(t(Xmatrix_2)%*%Xmatrix_2))%*%(t(Xmatrix_2)%*%yvector)
I<-diag(ncol(Xmatrix_2))
S2<-S%*%S

#dflambda<-rep(0, 5001)
wRRmat_2<-matrix(nrow=501,ncol=13)


for (i in 0:500){
  lambda<-i
  M<-solve(lambda*solve(S2) + I)
  wRR<-V%*%M%*%t(V)%*%wLS
  #dflambda[i]<-sum(diag(M))
  wRRmat_2[i+1,]<-t(wRR)
}

##X_testmatrix_2<-construct(X_test_matrix, 2)
X_test_matrix_2<-c()
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,1]**2)
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,2]**2)
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,3]**2)
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,4]**2)
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,5]**2)
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,6]**2)

X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,1])
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,2])
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,3])
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,4])
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,5])
X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,6])

X_test_matrix_2<-cbind(X_test_matrix_2,X_test_matrix[,7])


#RMSE<-rep(0,51)

for (i in 1:501){
  error<-0
  weights<-as.matrix(wRRmat_2[i,])
  for (j in 1:42){
    obs<-as.matrix(X_test_matrix_2[j,])
    ##y_pred<-wRRmat[i,]%*% (X_test_matrix[j,])
    
    y_pred<-t(weights)%*%obs
    error<-error + (y_test[j,]-y_pred)^2
  }
  testErrorMat[i, 2]<-(error/42)^0.5
}


##### for p=3

Xmatrix_3<-c()
Xmatrix_3<-cbind(Xmatrix_3,Xmatrix[,1]**2)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,2]**2)
Xmatrix_3<-cbind(Xmatrix_3,Xmatrix[,3]**2)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,4]**2)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,5]**2)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,6]**2)

Xmatrix_3<-cbind(Xmatrix_3,Xmatrix[,1]**3)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,2]**3)
Xmatrix_3<-cbind(Xmatrix_3,Xmatrix[,3]**3)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,4]**3)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,5]**3)
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,6]**3)


Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,1])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,2])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,3])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,4])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,5])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,6])
Xmatrix_3<-cbind(Xmatrix_3, Xmatrix[,7])


s<-svd(Xmatrix_3)
S<-diag(s$d)
V<-s$v
U<-s$u

###we want to generate wRR= VMVtwLS

##generate wLS= (XtX)^-1 XtX
wLS<-(solve(t(Xmatrix_3)%*%Xmatrix_3))%*%(t(Xmatrix_3)%*%yvector)
I<-diag(ncol(Xmatrix_3))
S2<-S%*%S

#dflambda<-rep(0, 5001)
wRRmat_3<-matrix(nrow=501,ncol=19)


for (i in 0:500){
  lambda<-i
  M<-solve(lambda*solve(S2) + I)
  wRR<-V%*%M%*%t(V)%*%wLS
  #dflambda[i]<-sum(diag(M))
  wRRmat_3[i+1,]<-t(wRR)
}

##X_testmatrix_2<-construct(X_test_matrix, 2)
X_test_matrix_3<-c()
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,1]**2)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,2]**2)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,3]**2)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,4]**2)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,5]**2)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,6]**2)

X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,1]**3)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,2]**3)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,3]**3)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,4]**3)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,5]**3)
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,6]**3)


X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,1])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,2])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,3])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,4])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,5])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,6])
X_test_matrix_3<-cbind(X_test_matrix_3,X_test_matrix[,7])


#RMSE<-rep(0,51)

for (i in 1:501){
  error<-0
  weights<-as.matrix(wRRmat_3[i,])
  for (j in 1:42){
    obs<-as.matrix(X_test_matrix_3[j,])
    ##y_pred<-wRRmat[i,]%*% (X_test_matrix[j,])
    
    y_pred<-t(weights)%*%obs
    error<-error + (y_test[j,]-y_pred)^2
  }
  testErrorMat[i, 3]<-(error/42)^0.5
}




lambda<-c(0:500)



##plot(lambda, tesErrorMat[,1], type='l', xlim=range(0,7), ylim=range(-2,5), xlab= 'df(lambda)', ylab='')
# par(new=TRUE)

plot(lambda, testErrorMat[,1], type='l', xlab= 'lambda', ylim=range(0,3.5), ylab='RMSE')


for(j in 2:3){
  # par(new=TRUE)
  lines(lambda, testErrorMat[,j], type='l', axes=FALSE)
}



'''
dat <- matrix(runif(40,1,20),ncol=4) # make data
matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) 
'''

matplot(testErrorMat,pch=3, col = 1:3, type="l", lwd=4, lty=4, xlab='lambda', ylab='RMSE',
        main="Degree of p & lambda") #plot
legend("topleft", legend = 1:3, col=1:3, pch=6) 

####finding optimal value of lambda

which.min(testErrorMat[,2])








