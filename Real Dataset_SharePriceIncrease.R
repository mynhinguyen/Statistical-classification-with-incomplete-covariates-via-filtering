print(Sys.time())

setwd("D:/ASUS 20170615/My-Nhi Nguyen/Documents/MS of Applied Maths/THESIS/Dataset/SharePriceIncrease")

library(MASS)
df <- read.csv(file="SharePriceIncrease.csv", header=TRUE, sep=",")



# No of Missing Patterns : 3
# s1 = [1:60]
# s2 = [1:45]
# s2 = [1:30]
t <- c(60:1)
t1 <- c(1:60)
t2 <- c(1:45)
t3 <- c(1:30)


# The Classifier: 1 or 2 or 3
# fully observable data set: classifier = 1
# the proposed classifier: classifier = 2
# the complete case: classifier = 3
classifier <- 2

#Missing Mechanism: 
# NMAR or MAR : 1
# MCAR : 2
#missMech <- 2

#Probability of misssingness in the case of MCAR
#MCARp <- 0.2

# Do testTime number of Monte-carlo runs for the entired process
testTime <- 100

#Do B different random partitions of the data
B <- 20

# set the sample size of an original data
n <- nrow(df)

# set the sample size of independent testing sample
#testN <- nrow(IncomeToNeed) 

#set the maximum dimension dn = (c0*log(n))^(1-lamda) 
#for any constant c0 >0 and any 0 < lamda < 1
# If using Fourier basis, dn should be an odd number
dn <- 8

#Grid of smoothing parameter for Kernel classifier
#lengthH <- 6
#H <- seq(from = 0, to = 1, length = lengthH)[c(-1,-lengthH)]
H <- c(0.05, 0.07, 0.085)

# set the parameters a,b,c
#a <- 0
#b <- 0.95
#c <- 0.13

#a3 <- 0
#b3 <- 0.2
#c3 <- 1

# Set up an vector to store the error of each entire Monte-Carlo run
testError <- NULL

# Set up vectors to store the bestD and bestH used in each run
d_hat <- NULL
h_hat <- NULL


  
  #Y=0,1 taken from the last column of the dataset
  
  y <- df$Y
  #delta_full <- rep(1,n)
  delta <- df$delta_MCAR
  
  # if Y=0, ai ~ Unif[0,5], bi ~ Unif[0,1]
  # if Y=1, ai ~ Normal[5,4], bi ~ Normal[1, 0.25]
  #ai <- y*rnorm(n, 5, 2)+(1-y)*runif(n, 0, 5)
  #bi <- y*rnorm(n, 1, 0.5)+(1-y)*runif(n, 0, 1)
  
  #to double check the distribution of ai and bi
  #x11()
  #par(mfrow=c(2,2))
  #hist(ai[y==1], freq = FALSE)
  #curve(dnorm(x,mean=5,sd=2),add=TRUE, col='red')
  #hist(ai[y==0],freq = FALSE)
  #curve(dunif(x,min=0,max=5),add=TRUE, col='red')
  #hist(bi[y==1],freq = FALSE)
  #curve(dnorm(x,mean=1,sd=0.5),add=TRUE, col='red')
  #hist(bi[y==0],freq = FALSE)
  #curve(dunif(x,min=0,max=1),add=TRUE, col='red')
  #dev.off()
  
  #function of sampled curves
  #X <- function(t){ai*(t-0.5)^2+bi} 
  
  #generate the Missing Probability Mechanism only if
  # the classifier is not for fully observable data set
  # and in the case of NMAR or MAR
  #if(classifier != 1){
    
  #  if(missMech==1){#for the case of NMAR or MAR
  #    missingPi <- NULL
      #the missing probability P(delta = 1|Y=y, X(t)=x(t))
      # = exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      # / 1+ exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      
      
      
      # Missing Probability Mechanism for s2 = [0,0.3]U[0.5,1]
      
      
  #    for (i in 1:n){
  #     missingPi[i] <- exp(a*(1-y[i])
  #                          +b*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.3)$value
  #                              +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.5,upper=1)$value)
  #                          +c*integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.3,upper=0.5)$value)/
  #        (1+exp(a*(1-y[i])
  #               +b*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.3)$value
  #                   +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.5,upper=1)$value)
  #               +c*integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.3,upper=0.5)$value))
        
  #    }
      
      # Missing Probability Mechanism for s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
      
  #   if(missPattNo ==2){
  #      missingPi3 <- NULL
        
  #      for (i in 1:n){
  #        missingPi3[i] <- exp(a3*(1-y[i])
  #                             +b3*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.1)$value
  #                                  +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.2,upper=0.45)$value
  #                                  +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.6,upper=0.85)$value
  #                                  +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.9,upper=1)$value
  #                             )
  #                             +c3*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.1,upper=0.2)$value
  #                                  +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.45,upper=0.6)$value
  #                                  +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.85,upper=0.9)$value
  #                             )
  #        )/
  #          (1+exp(a3*(1-y[i])
  #                 +b3*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.1)$value
  #                      +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.2,upper=0.45)$value
  #                      +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.6,upper=0.85)$value
  #                      +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.9,upper=1)$value
  #                 )
  #                 +c3*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.1,upper=0.2)$value
  #                      +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.45,upper=0.6)$value
  #                      +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.85,upper=0.9)$value
  #                 )
  #          )
  #         )
          
  #      }
        
  #   }    
      
      
  #  }
    
  #}
  
  
  
  
  
  
  
  #generate a vector of missing pattern delta only if the Classifier is not fully observable data set
    
  
  
  
  #if (classifier != 1){
    # if there is only 1 missing pattern
  #  if(missPattNo == 1){
  #    if(missMech==1){#NMAR or MAR
   #     for (i in 1:n){
   #       delta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(missingPi[i],1-missingPi[i]))
   #     } 
        
    #  }else if(missMech==2){#MCAR
    #    for (i in 1:n){
    #      delta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
     #   } 
     # }
      
      # if there are 2 missing patterns  
   # }else if(missPattNo ==2){
    #  if(missMech==1){#NMAR or MAR
        
     #   for (i in 1:n){
          # pick the missing pattern probability mechanism with Benoulli flip coin 
     #     pattern <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
          
      #    if(pattern==2){
      #      delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi[i],1-missingPi[i]))
            
       #   }else if(pattern==3){
       #     delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi3[i],1-missingPi3[i]))
       #   }
       # }
        
        
        
     # }else if(missMech==2){#MCAR
     #   for (i in 1:n){
          # if if 0 the data is fully observable, 1 the data is missing 
     #     missing <- sample(c(0,1), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
          
     #     if(missing==0){
     #       delta[i] <- 1
     #     }else if(missing==1){
     #       delta[i] <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
      #    }
          
          
     #   }
     # }
   # }
 # }
  
  
  
  #create the data frame of the sample. Each observation includes:
  #function X (represented by ai and bi), Y, delta
  #dataL2 <- data.frame(ai,bi,y,delta)
  
  #Plot some of the curves in class Y=0
  #s1 <- seq (0,1, length=1000)
  #s2_1 <- seq (0,0.3, length=500)
  #s2_2 <- seq(0.5,1, length=500)
  #x11()
  #plot(s1,s1, type="n",main = "Class 0", xlab = "t", ylab= "X(t)", 
  #    xlim=c(0,1), ylim=c(0,3))
  
  #for (i in 1:30 ){
  #  if (y[i]==0 & delta[i]==1){
  #    lines(s1, ai[i]*(s1-0.5)^2+bi[i])
  #  }
  #}
  #for (i in 1:30 ){
  #  if (y[i]==0 & delta[i]==2){
  #    lines(s2_1, ai[i]*(s2_1-0.5)^2+bi[i])
  #    lines(s2_2, ai[i]*(s2_2-0.5)^2+bi[i])
  #  }
  #}
  
  #Plot some of the curves in class Y=1
  #x11()
  #plot(s1,s1, type="n",main = "Class 1", xlab = "t", ylab= "X(t)", 
  #     xlim=c(0,1), ylim=c(0,3))
  
  #for (i in 1:30 ){
  #  if (y[i]==1 & delta[i]==1){
  #    lines(s1, ai[i]*(s1-0.5)^2+bi[i])
  #  }
  #}
  #for (i in 1:30 ){
  #  if (y[i]==1 & delta[i]==2){
  #    lines(s2_1, ai[i]*(s2_1-0.5)^2+bi[i])
  #    lines(s2_2, ai[i]*(s2_2-0.5)^2+bi[i])
  #  }
  #}
  
  
  
  #create trigonometric basis psi1(t) = 1, psi(2k)(t)= sqrt(2)*cos(2*pi*k*t)
  # psi(2k+1)(t)= sqrt(2)*sin(2*pi*k*t)
  
  
  # No of Missing Patterns : 1
  # s1 = from [1969,2013]
  # s2 = from [1998,2013]
  #t <- c(1:30, seq(from=31,to=45,by=2))
  #t1 <- c(1:38)
  #t2 <- c(30:38)

  
  #create a psi matrix
  psi <- matrix(, nrow = length(t), ncol=dn)
  
  for (j in 1:dn){
    
    if (j==1){
      psi[,j] <- sapply(t,function(t) 1)
    }else if (j%%2==0){
      psi[,j] <- sapply(t,function(t) sqrt(2)*cos(2*pi*(j%/%2)*t))
    }else if (j%%2==1){
      psi[,j] <- sapply(t,function(t) sqrt(2)*sin(2*pi*(j%/%2)*t))
    }
    
  }
  
  
  #generate the coefficient matrix in l2 with dn dimensions
  #Using Least Square method to estimate the coefficient matrix
  Xicoef <- matrix(, nrow = n, ncol=dn)
  
  #generate the Xicoef if the classifier for fully observable data set
  #if(classifier==1){
  #  for (i in 1:n){
      
  #      Xicoef[i,] <- ginv(t(psi)%*%psi)%*% t(psi)%*%t(ECGRaw[i,t1])
        
  #  }
    #generate the Xicoef if the classifier not for fully observable data set  
  #}else if (classifier==2){
    for (i in 1:n){
      if (delta[i]==1){#s1 = t1
        
        Xicoef[i,] <- ginv(t(psi[t1,])%*%psi[t1,])%*% t(psi[t1,])%*%t(df[i,t1])
        
      }else if(delta[i]==2){#s2 = t2
        
        Xicoef[i,] <- ginv(t(psi[t2,])%*%psi[t2,])%*% t(psi[t2,])%*%t(df[i,t2])
      }else if(delta[i]==3){#s3 = t3
        
        Xicoef[i,] <- ginv(t(psi[t3,])%*%psi[t3,])%*% t(psi[t3,])%*%t(df[i,t3])
        
      }#else if(delta[i]==3){#s3 = t3
        
       # Xicoef[i,] <- ginv(t(phi[t3,])%*%phi[t3,])%*% t(phi[t3,])%*%t(ECGRaw[i,t3])
        
      #}
    }        
    
  #}else if (classifier==3){# in the Complete Case only need to generate for delta=1
  #  for (i in 1:n){
  #    if (delta[i]==1){
        
  #      Xicoef[i,] <- ginv(t(phi)%*%phi)%*% t(phi)%*%t(ECGRaw[i,t1])
        
  #    }
  #  }
    
  # }
  

  
  #create a data frame in l2 containing Coefficient matrix, y and delta
  #if(classifier !=1){
    datal2 <- data.frame(Xicoef,y,delta)
  #}else{
  #  datal2 <- data.frame(Xicoef,y)
  #}
  
  
  
  #euc.dist <- function(x1, x2 , h) {sqrt(sum(((x1 - x2)) ^ 2))/h}
  
  
  #Carry out Gaussian Kernel classification
  
  #Set up the dn by |H| by B error matrix to record the error of each (d,h) combination
  #for B different random partitions of the data. It takes an initial value = 1 for each cell
  
  error <- array(1,c(dn,length(H),B))
  
  #Randomly split the data for B times and record the error for each (d,h) in each split
  for (j in 1:B)
  {
    cat("spliting data time = ",j," ")
    print(Sys.time())
    #Split the dataset into a train set, and a test set. Use a 70/30 split
    shuffled <- datal2[sample(n),]
    train <- shuffled[1:round(0.7 * n),]
    test <- shuffled[(round(0.7 * n) + 1):n,]
    
    #create a vector in test dataframe to store the predicted value
    
    
    #train1 <- train[train$delta==1,]
    #train2 <- train[train$delta==2,]
    
    for (d in 2:dn){ #only run for d>=1
      for (h in H){
        
        #cat("d = ",d," and h = ",h," ")
        #print(Sys.time())
        
        test$predictedY <- NULL
        
        if(classifier == 2){
          for (i in 1:nrow(test)){
            
            if(test$delta[i]==1){
              subtrain <- train[train$delta==1,]
            }else if (test$delta[i]==2){
              subtrain <- train[train$delta==2,]
            }else if (test$delta[i]==3){
             subtrain <- train[train$delta==3,]
            }
            
            #calculate phi and perform classification
            phi <- sum((2*subtrain$y-1)*
                         t(apply(subtrain[,1:d], MARGIN =1, 
                                 function(x) exp(-sum((x - test[i,1:d]) ^ 2)/(h^2)) )))
            
            if(phi>0){test$predictedY[i] <- 1} 
            else{test$predictedY[i] <- 0}
            
            # if the Complete Case Analysis  
          }
        }else if (classifier == 3){
          for (i in 1:nrow(test)){
            if(test$delta[i]==1){
              subtrain <- train[train$delta==1,]
              
              #calculate phi and perform classification
              phi <- sum((2*subtrain$y-1)*
                           t(apply(subtrain[,1:d], MARGIN =1, 
                                   function(x) exp(-sum((x - test[i,1:d]) ^ 2)/(h^2)) )))
              
              if(phi>0){test$predictedY[i] <- 1} 
              else{test$predictedY[i] <- 0}
              
              # an observation that was only partially observed was assigned to class 0 or 1
              # based on a flip of a fair Bernoulli coin 
            }else {
              test$predictedY[i]<- sample(c(0,1), size=1, replace=TRUE, prob=c(0.5,0.5))
            }
          }
        }
        
       
        
        #record the error
        error[d,which(H==h),j]=length(which(test$y!=test$predictedY))/nrow(test)
      }
    }
    
  }
  #Calculate the average error for each (d,h)
  errorAvg <- apply(error, c(1,2), mean)
  
  #get the best h and d which minimize the average error. If ties happens, select the first one
  bestD <- arrayInd(which.min(errorAvg), dim(errorAvg))[1,1]
  bestH <- H[arrayInd(which.min(errorAvg), dim(errorAvg))[1,2]]
  
  cat("bestD = ",bestD,"\n")
  cat("bestH = ",bestH,"\n")
  
  #append the new bestD and bestH to d_hat and h_hat to keep track
  
  d_hat <- c(d_hat,bestD)
  h_hat <- c(h_hat,bestH)
  
  
  
  print(Sys.time())
  
  #using the best (d,h) above to perform classification on entire dataset

for(k in 1:testTime){
    
    print(Sys.time())
    cat("testing time = ",k,"\n")  
  
  
  
  #perform the Kernel Classification using (bestD,bestH) and the original data set to train
    shuffled <- datal2[sample(n),]
    train <- shuffled[1:round(0.5 * n),]
    test <- shuffled[(round(0.5 * n) + 1):n,]
    
  
  #create a vector in test dataframe to store the predicted value
  test$predictedY <- NULL
  
  #train1 <- train[train$delta==1,]
  #train2 <- train[train$delta==2,]
  
  
  
  if(classifier == 2){
    for (i in 1:nrow(test)){
      
      if(test$delta[i]==1){
        subtrain <- train[train$delta==1,]
      }else if (test$delta[i]==2){
        subtrain <- train[train$delta==2,]
      }else if (test$delta[i]==3){
      subtrain <- train[train$delta==3,]
      }
      
      #calculate phi and perform classification
      phi <- sum((2*subtrain$y-1)*
                   t(apply(subtrain[,1:bestD], MARGIN =1, 
                           function(x) exp(-sum((x - test[i,1:bestD]) ^ 2)/(bestH^2)) )))
      
      if(phi>0){test$predictedY[i] <- 1} 
      else{test$predictedY[i] <- 0}
      
      # if the Complete Case Analysis  
    }
  }else if (classifier == 3){
    for (i in 1:nrow(test)){
      if(test$delta[i]==1){
        subtrain <- train[train$delta==1,]
        
        #calculate phi and perform classification
        phi <- sum((2*subtrain$y-1)*
                     t(apply(subtrain[,1:bestD], MARGIN =1, 
                             function(x) exp(-sum((x - test[i,1:bestD]) ^ 2)/(bestH^2)) )))
        
        if(phi>0){test$predictedY[i] <- 1} 
        else{test$predictedY[i] <- 0}
        
        # an observation that was only partially observed was assigned to class 0 or 1
        # based on a flip of a fair Bernoulli coin 
      }else {
        test$predictedY[i]<- sample(c(0,1), size=1, replace=TRUE, prob=c(0.5,0.5))
      }
    }
  }
  
  
  
  #record the error
  testError=c(testError,length(which(test$y!=test$predictedY))/nrow(test))
  cat("test Error of this run = ", length(which(test$y!=test$predictedY))/nrow(test),"\n")
  
  
  
  
  
  
  
  
}

#Calculate the mean and sd of the error rates
testErrorAvg <- mean(testError)
testErrorSd <- sd(testError)

#combine all the bestD and bestH in all the runs
bestPair <- cbind(d_hat,h_hat)

#print all the parameters and result
cat("SharePriceIncrease","\n")
#cat("No of Missing Patterns = ",missPattNo,"\n")
#cat("Remark: No of Missing Patterns is not applicable in the case of fully observable data set","\n")
cat("The classifier : ")
if (classifier==1){
  print("fully observable data set")
}else if(classifier==2){
  print("the proposed classifier")
}else if(classifier==3){
  print("the complete case")
}
#cat("Missing Mechanism : ")
#if (classifier==1){
#  print("NA")
#}else {
#  if(missMech ==1){
#    print("NMAR or MAR")  
#  }else if(missMech ==2){
#    print("MCAR") 
#    cat("  Missing percentage = ", MCARp,"\n")
#  }
#}
cat("sample size n = ",n,"\n")
cat("dn = ",dn,"\n")
cat("H ",H,"\n")
#cat("independent testing sample size N = ",testN,"\n")
cat("# of entire Monte-Carlo runs = ",testTime,"\n")
cat("# of data splitting = ",B,"\n")
#cat("a = ",a,"\n")
#cat("b = ",b,"\n")
#cat("c = ",c,"\n")
#cat("a3 = ",a3,"\n")
#cat("b3 = ",b3,"\n")
#cat("c3 = ",c3,"\n")
#cat("Remark: a,b,c,a3,b3,c3 are not applicable in the case of MCAR","\n")
cat("testErrorAvg = ", testErrorAvg,"\n")
cat("testErrorSd = ", testErrorSd,"\n")
cat("all the pairs of best(d,h): ","\n")
print(bestPair)
cat("testError = ", testError,"\n")

print(Sys.time())


# No of Missing Patterns : 3
# s1 = [012345-7-10-26]
# s2 = [01235-10-26]
# s2 = ['0-5-26]
#Set the number of curves want to graph
cNo <- 5
#Set the curve width
w <-2
cex <- 4
margin <- c(4,4,2,1)
#Plot some of the curves
x11()
#par(mar=c(5, 4, 4, 10), xpd=TRUE)

#m <- matrix(c(1,2,3,4,5,6),nrow = 6,ncol = 1,byrow = TRUE)

#layout(mat = m,
       #widths = c(0.8,0.8), 
#       heights=c(16,16,4)
#       )
tiff("SharePriceIncrease_plots.tiff", width = 7, height = 9, units = 'in', res = 300)
layout(matrix(c(1,1,2,2,3,4,5,5), 4, 2, byrow = TRUE), heights=c(25,25,22,17))

par(mar = margin)
plot(t,t, type="n",main = "Original Curves in Class Y=0", xlab = "t (days)", 
     ylab="% change of the close price", 
     xlim=c(0,61.5), ylim=c(-11,11),
     xaxt="n", cex=cex)
axis(1, at = t, las=1)
#dev.off()

c1 <- 0
c2 <- 0
c3 <- 0


for (i in 1:n ){
  if (y[i]==0){
    if(delta[i]==1&c1<cNo){
      lines(t[t1], df[i,t1], lwd =w, type = 'o')
      c1=c1+1
    }else if (delta[i]==2&c2<cNo){
      lines(t[t2], df[i,t2],col ="red", lwd =w, type = 'o')
      c2=c2+1
    }else if (delta[i]==3&c3<cNo){
      lines(t[t3], df[i,t3],col ="blue", lwd =w, type = 'o')
      c3=c3+1
    }
    
  }
  if(c1==cNo&c2==cNo&c3==cNo){
    break
  }
}


par(mar = margin)
plot(t,t, type="n",main = "Original Curves in Class Y=1", xlab = "t (days)", 
     ylab="% change of the close price", 
     xlim=c(0,61.5), ylim=c(-11,11),
     xaxt="n", cex=cex)
axis(1, at = t, las=1)
#dev.off()

c1 <- 0
c2 <- 0
c3 <- 0


for (i in 1:n ){
  if (y[i]==1){
    if(delta[i]==1&c1<cNo){
      lines(t[t1], df[i,t1], lwd =w, type = 'o')
      c1=c1+1
    }else if (delta[i]==2&c2<cNo){
      lines(t[t2], df[i,t2],col ="red", lwd =w, type = 'o')
      c2=c2+1
    }else if (delta[i]==3&c3<cNo){
      lines(t[t3], df[i,t3],col ="blue", lwd =w, type = 'o')
      c3=c3+1
    }
    
  }
  if(c1==cNo&c2==cNo&c3==cNo){
    break
  }
}







#Plot some of the Coefficient vectors
par(mar = margin)
plot(1:dn,1:dn, type="n",main = "Projected Curves in Class Y=0",
     xlab="Dimension d", ylab=expression('X'[i]), ylim=c(-0.17,0.2), cex=cex)
#dev.off()

c1 <- 0
c2 <- 0
c3 <- 0


for (i in 1:n ){
  if (y[i]==0){
    if(delta[i]==1&c1<cNo){
      lines(1:dn, Xicoef[i,], lwd =w)
      c1=c1+1
    }else if (delta[i]==2&c2<cNo){
      lines(1:dn, Xicoef[i,],col ="red", lwd =w)
      c2=c2+1
    }else if (delta[i]==3&c3<cNo){
      lines(1:dn, Xicoef[i,],col ="blue", lwd =w)
      c3=c3+1
    }
  }
  if(c1==cNo&c2==cNo&c3==cNo){
    break
  }
}

par(mar = margin)
plot(1:dn,1:dn, type="n",main = "Projected Curves in Class Y=1",
     xlab="Dimension d", ylab=expression('X'[i]), ylim=c(-0.17,0.2), cex=cex)
#dev.off()

c1 <- 0
c2 <- 0
c3 <- 0


for (i in 1:n ){
  if (y[i]==1){
    if(delta[i]==1&c1<cNo){
      lines(1:dn, Xicoef[i,], lwd =w)
      c1=c1+1
    }else if (delta[i]==2&c2<cNo){
      lines(1:dn, Xicoef[i,],col ="red", lwd =w)
      c2=c2+1
    }else if (delta[i]==3&c3<cNo){
      lines(1:dn, Xicoef[i,],col ="blue", lwd =w)
      c3=c3+1
    }
  }
  if(c1==cNo&c2==cNo&c3==cNo){
    break
  }
}


#par(mar = c(4,4,1,1))
#plot(1, type = "n", axes=FALSE, xlab="", ylab="")



par(mar = c(1,4,1,1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

#legend("topleft", legend=c(expression(paste("Full Curve ", delta,"=1")),
#                        expression(paste("Missing Pattern ", delta,"=2")),
#                        expression(paste("Missing Pattern ", delta,"=3")),
#                        expression(paste("Missing Pattern ", delta,"=4")),
#                        expression(paste("Missing Pattern ", delta,"=5"))
#                        ), col=c("black","red", "blue", "green","gold"), cex=0.8,
#       title="Missing types", text.font=4, bg='lightblue')

legend("top", legend=c(expression(paste("Full Curve ", delta,"=1")),
                       expression(paste("Missing Pattern ", delta,"=2")),
                       expression(paste("Missing Pattern ", delta,"=3"))
),
col=c("black","red", "blue"), lty=1, cex=1,
title="Missing Pattern types", text.font=4, bty = "n")
dev.off()

#plot distribution of missing patterns
x11()
counts <- table(y, delta)
par(mar = c(5,4,4,8))
barplot(counts, main="Class Distribution by Missing Pattern in MCAR case",
        xlab="Missing Pattern", col=c("antiquewhite1","coral1"), ylim=c(0,900),
        legend.text=TRUE,
        args.legend=list(x = 'top',legend = c(1,0),
                         title="Class Y",xpd = TRUE, horiz = TRUE)
)


