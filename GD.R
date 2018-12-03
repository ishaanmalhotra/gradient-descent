#Task 1
#Calculation of sigmoid
sigmoid <- function(x){
  s <- 1 + exp(-x)
  return(1/s)
}

#Calculation of derivative of sigmoid
ds <-function(x){
  return (sigmoid(x) * (1-sigmoid(x)))
}



#Step 1 Compute xi
calc_xi <-function(beta,dataset){
  var = c()
  m = dim(dataset)[1]
  for (i in 1:m) {
   var[i] = beta[1] + (beta[2] * dataset[i,1]) + (beta[3] * dataset[i,2]) + (beta[4] * dataset[i,3]) + (beta[5] * dataset[i,4]) + (beta[6] * dataset[i,5]) + (beta[7] * dataset[i,6])
  }
return(var)
}

#Compute Pi
calc_sigmoid <- function(x){
  p = c()
  for (i in 1:length(x)){
    p[i] = sigmoid(x[i])
  }
  return (p)
}

#Step 2 Compute coefficients d
calc_coeff <-function(p,y,x){
  d = c()
  for (i in 1:length(x)){
  d[i] = 2 * (y[i]- p[i]) * ds(x[i])
  }
  return(d)
}


#Function to compute MSE
calc_mse <- function(p,y){
  res = c()
  for ( i in 1:length(p)){
    res[i] = (y[i] - p[i])^2
  }
  return(sum(res)/length(p))
}

#Step 3 Update the weights
calc_grad <- function(d,dataset,beta,n){
  grad = c()
  temp = 0
  s = sum(d)
  for (p in 1:length(d)){
    
    temp = temp + (n/length(d)) * s
  }
  grad[1] = temp
  for (i in 2:7){
    res = c()
    val = beta[i]
    for (j in 1:length(d)){
      res[j] =(d[j] * dataset[j,i])
    }
    for (q in 1:length(d)){
    val =val + (n/length(d)) * sum(res)
    }
    grad[i] = val
  }
  return(grad)
}

#Function to calculate prediction for the test set using values retrieved from the training set.
predict<- function(beta,dataset){
  x<-calc_xi(beta,dataset)
  p<- calc_sigmoid(x)
  return(p)
}


#Task 2
library(ISLR)
Auto <- read.table("Auto.data", header=T,na.strings="?")
attach(Auto)
fix(Auto)
origin1= as.numeric(origin<2)
origin3=as.numeric(origin>2)
origin= as.numeric(origin==2)
Auto<-data.frame(year,origin1,origin3,origin,horsepower,weight)
Auto<- scale(Auto)
High<-ifelse(mpg>=23,1,0)
#Creating dummy variables for origin

Auto<-data.frame(Auto,High)
#Normalizing the dataset 

Auto<- na.omit(Auto)


#Task 3 
smp_size <- floor(0.50 * nrow(Auto))
#Select Seed
set.seed(804)
train_ind <- sample(seq_len(nrow(Auto)), size = smp_size)
train <- Auto[train_ind, ]
test <- Auto[-train_ind, ]


#Task 4
beta = 1.4 * runif(7) - 0.7
x <- calc_xi(beta,train)
p <- calc_sigmoid(x)
d<-calc_coeff(p,train[,7],x)
mse <- calc_mse(p,train[,7])
updated_weights <- calc_grad(d,train,beta,0.01)
train_mse_pred <- predict(updated_weights,train)
train_mse <- calc_mse(train_mse_pred,train[,7])
print(train_mse)
pred<-predict(updated_weights,test)
mse_pred <- calc_mse(pred,test[,7])
print(mse_pred)

#Changing value of learning rate
updated_weights <- calc_grad(d,train,beta,0.1)
train_mse_pred<- predict(updated_weights,train)
train_mse <- calc_mse(train_mse_pred,train[,7])
print(train_mse)
pred<-predict(updated_weights,test)
mse_pred <- calc_mse(pred,test[,7])
print(mse_pred)

#Changing value of learning rate
updated_weights <- calc_grad(d,train,beta,0.001)
train_mse_pred<- predict(updated_weights,train)
train_mse <- calc_mse(train_mse_pred,train[,7])
print(train_mse)
pred<-predict(updated_weights,test)
mse_pred <- calc_mse(pred,test[,7])
print(mse_pred)

#Task 6
mse_res  = c()
for (i in 1:100){
  beta = (1.4 * runif(7)) - 0.7
  x <- calc_xi(beta,train)
  p <- calc_sigmoid(x)
  d<-calc_coeff(p,train[,7],x)
  updated_weights <- calc_grad(d,train,beta,0.01)
  pred<-predict(updated_weights,test)
  mse <- calc_mse(pred,test[,7])
  mse_res[i] = mse
  
}

boxplot(mse_res)



#Task 7
rules= c()
for (i in 1:4){
  beta = 1.4 * runif(7) - 0.7
  x <- calc_xi(beta,train)
  p <- calc_sigmoid(x)
  d<-calc_coeff(p,train[,7],x)
  updated_weights <- calc_grad(d,train,beta,0.01)
  pred<-predict(updated_weights,test)
  mse_pred <- calc_mse(pred,test[,7])
  rules[i] = mse_pred
}
print(min(rules))

