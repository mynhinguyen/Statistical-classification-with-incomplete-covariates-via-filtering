print(Sys.time())



# No of Missing Patterns (1-4)
# s1 = [0,1]
# s2 = [0,0.3]U[0.5,1]
# s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
# s4 = [0.25,0.5]U[0.65,1]
# s5 = [0,0.2]U[0.3,0.55]U[0.75,0.9]
missPattNo <- 4

# The Classifier: 1 or 2 or 3
# fully observable data set: classifier = 1
# the proposed classifier: classifier = 2
# the complete case: classifier = 3
classifier <- 2

#Missing Mechanism: 
# NMAR or MAR : 1
# MCAR : 2
missMech <- 2

#Probability of misssingness in the case of MCAR
MCARp <- 0.3

# Do testTime number of Monte-carlo runs for the entired process
testTime <- 2

#Do B different random partitions of the data
B <- 2

# set the sample size of an original data
n <- 100

# set the sample size of independent testing sample
testN <- 1000

#set the maximum dimension dn = (c0*log(n))^(1-lamda) 
#for any constant c0 >0 and any 0 < lamda < 1
dn <- 11

#Grid of smoothing parameter for Kernel classifier
#lengthH <- 6
#H <- seq(from = 0, to = 1, length = lengthH)[c(-1,-lengthH)]
H <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

# set the parameters a,b,c
a2 <- 0
b2 <- 0.15
c2 <- 1.2

a3 <- 0
b3 <- 0.2
c3 <- 1

a4 <- 0
b4 <- 0.2
c4 <- 1

a5 <- 0
b5 <- 0.2
c5 <- 1

# Set up an vector to store the error of each entire Monte-Carlo run
testError <- NULL

# Set up vectors to store the bestD and bestH used in each run
d_hat <- NULL
h_hat <- NULL

for(k in 1:testTime){
  
  print(Sys.time())
  cat("testing time = ",k,"\n")
  
  #choose P(Y=1)=P(Y=0)=0.5 and generate vector y
  p <- 0.5
  y <- sample(c(0,1), size = n, replace=TRUE, prob = c(p,1-p))
  
  
  # if Y=0, ai ~ Unif[0,5], bi ~ Unif[0,1]
  # if Y=1, ai ~ Normal[5,4], bi ~ Normal[1, 0.25]
  ai <- y*rnorm(n, 5, 2)+(1-y)*runif(n, 0, 5)
  bi <- y*rnorm(n, 1, 0.5)+(1-y)*runif(n, 0, 1)
  
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
  if(classifier != 1){
    
    if(missMech==1){#for the case of NMAR or MAR
      missingPi2 <- NULL
      #the missing probability P(delta = 1|Y=y, X(t)=x(t))
      # = exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      # / 1+ exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      
      
      
      # Missing Probability Mechanism for s2 = [0,0.3]U[0.5,1]
      
      
      for (i in 1:n){
        missingPi2[i] <- exp(a2*(1-y[i])
                            +b2*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.3)$value
                               +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.5,upper=1)$value)
                            +c2*integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.3,upper=0.5)$value)/
          (1+exp(a2*(1-y[i])
                 +b2*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.3)$value
                    +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.5,upper=1)$value)
                 +c2*integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.3,upper=0.5)$value))
        
      }
      
      # Missing Probability Mechanism for s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
      if(missPattNo >=2){
        missingPi3 <- NULL
        
        for (i in 1:n){
          missingPi3[i] <- exp(a3*(1-y[i])
                               +b3*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.1)$value
                                   +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.2,upper=0.45)$value
                                   +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.6,upper=0.85)$value
                                   +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.9,upper=1)$value
                                   )
                               +c3*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.1,upper=0.2)$value
                                   +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.45,upper=0.6)$value
                                   +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.85,upper=0.9)$value
                                   )
                             )/
            (1+exp(a3*(1-y[i])
                   +b3*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.1)$value
                       +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.2,upper=0.45)$value
                       +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.6,upper=0.85)$value
                       +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.9,upper=1)$value
                       )
                   +c3*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.1,upper=0.2)$value
                       +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.45,upper=0.6)$value
                       +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.85,upper=0.9)$value
                       )
                  )
            )
          
        }
        
      }    
      #Missing Probability Mechanism for s4 = [0.25,0.5]U[0.65,1]
      if(missPattNo >=3){
        missingPi4 <- NULL
        
        for (i in 1:n){
          missingPi4[i] <- exp(a4*(1-y[i])
                               +b4*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0.25,upper=0.5)$value
                                    +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.65,upper=1)$value
                               )
                               +c4*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0,upper=0.25)$value
                                    +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.5,upper=0.65)$value
                               )
          )/
            (1+exp(a4*(1-y[i])
                   +b4*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0.25,upper=0.5)$value
                        +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.65,upper=1)$value
                   )
                   +c4*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0,upper=0.25)$value
                        +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.5,upper=0.65)$value
                   )
            )
            )
          
        }
        
      }        
   
      #Missing Probability Mechanism for s5 = [0,0.2]U[0.3,0.55]U[0.75,0.9]
      if(missPattNo >=4){
        missingPi5 <- NULL
        
        for (i in 1:n){
          missingPi5[i] <- exp(a5*(1-y[i])
                               +b5*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.2)$value
                                    +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0.3,upper=0.55)$value
                                    +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.75,upper=0.9)$value
                               )
                               +c5*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.2,upper=0.3)$value
                                    +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.55,upper=0.75)$value
                                    +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.9,upper=1)$value
                               )
          )/
            (1+exp(a5*(1-y[i])
                   +b5*(integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0,upper=0.2)$value
                        +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower= 0.3,upper=0.55)$value
                        +integrate(function(t) ai[i]*(t-0.5)^2+bi[i],lower=0.75,upper=0.9)$value
                   )
                   +c5*(integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.2,upper=0.3)$value
                        +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.55,upper=0.75)$value
                        +integrate(function(t) t*(ai[i]*(t-0.5)^2+bi[i]),lower=0.9,upper=1)$value
                   )
            )
            )
          
        }
        
      }  
      
      
      
      
       }
    
  }
  
  
  
  
  
  
  
  #generate a vector of missing pattern delta only if the Classifier is not fully observable data set
  delta <- NULL  
  
  
  
  if (classifier != 1){
    # if there is only 1 missing pattern
    if(missPattNo == 1){
      if(missMech==1){#NMAR or MAR
        for (i in 1:n){
          delta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(missingPi[i],1-missingPi[i]))
        } 
        
      }else if(missMech==2){#MCAR
        for (i in 1:n){
          delta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
        } 
      }
      
    # if there are 2 missing patterns  
    }else if(missPattNo ==2){
      if(missMech==1){#NMAR or MAR
        
        for (i in 1:n){
          # pick the missing pattern probability mechanism with Benoulli flip coin 
          pattern <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
          
          if(pattern==2){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi2[i],1-missingPi2[i]))
            
          }else if(pattern==3){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi3[i],1-missingPi3[i]))
          }
        }
        
        
        
      }else if(missMech==2){#MCAR
        for (i in 1:n){
          # if if 0 the data is fully observable, 1 the data is missing 
          missing <- sample(c(0,1), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
          
          if(missing==0){
            delta[i] <- 1
          }else if(missing==1){
            delta[i] <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
          }
          
          
        }
      }
    }else if(missPattNo ==4){
      if(missMech==1){#NMAR or MAR
        
        for (i in 1:n){
          # pick the missing pattern probability mechanism with Benoulli flip coin 
          pattern <- sample(c(2,3,4,5), size = 1, replace=TRUE, prob = c(0.25,0.25,0.25,0.25))
          
          if(pattern==2){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi2[i],1-missingPi2[i]))
            
          }else if(pattern==3){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi3[i],1-missingPi3[i]))
          }else if(pattern==4){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi4[i],1-missingPi4[i]))
          }else if(pattern==5){
            delta[i] <- sample(c(1,pattern), size = 1, replace=TRUE, prob = c(missingPi5[i],1-missingPi5[i]))
          }
          
        }
        
        
        
      }else if(missMech==2){#MCAR
        for (i in 1:n){
          # if if 0 the data is fully observable, 1 the data is missing 
          missing <- sample(c(0,1), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
          
          if(missing==0){
            delta[i] <- 1
          }else if(missing==1){
            delta[i] <- sample(c(2,3,4,5), size = 1, replace=TRUE, prob = c(0.25,0.25,0.25,0.25))
          }
          
          
        }
      }
    }
 
    
    
    
    
    
     }
  
   
  
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
  
  #generate the coefficient matrix in l2 with dn dimensions
  Xicoef <- matrix(, nrow = n, ncol=dn)
  
  #generate the Xicoef if the classifier for fully observable data set
  if(classifier==1){
    for (i in 1:n){
      for (j in 1:dn){
        
          if (j==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=1)$value
          }else if (j%%2==0){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }else if (j%%2==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }
        
      }
    }
  #generate the Xicoef if the classifier not for fully observable data set  
  }else if (classifier==2){
    for (i in 1:n){
      if (delta[i]==1){
        for (j in 1:dn){
        
        #s1 = [0,1]
          if (j==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=1)$value
          }else if (j%%2==0){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }else if (j%%2==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }
        }
        }else if(delta[i]==2){#s2 = [0,0.3]U[0.5,1]
          for (j in 1:dn){
          if (j==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=0.3)$value 
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.5,upper=1)$value
            
          }else if (j%%2==0){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.3)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.5,upper=1)$value
          }else if (j%%2==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.3)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.5,upper=1)$value
          }
          }
        }else if(delta[i]==3){#s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
          for (j in 1:dn){
          if (j==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=0.1)$value 
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.2,upper=0.45)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.6,upper=0.85)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.9,upper=1)$value
          }else if (j%%2==0){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.1)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.2,upper=0.45)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.6,upper=0.85)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.9,upper=1)$value
          }else if (j%%2==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.1)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.2,upper=0.45)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.6,upper=0.85)$value
                          +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.9,upper=1)$value
          }
        }
        
        }else if(delta[i]==4){#s4 = [0.25,0.5]U[0.65,1]
          for (j in 1:dn){
            if (j==1){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.25,upper=0.5)$value 
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.65,upper=1)$value
          
            }else if (j%%2==0){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.25,upper=0.5)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.65,upper=1)$value
            
            }else if (j%%2==1){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.25,upper=0.5)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.65,upper=1)$value
              
            }
          }
          
        }else if(delta[i]==5){#s5 = [0,0.2]U[0.3,0.55]U[0.75,0.9]
          for (j in 1:dn){
            if (j==1){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=0.2)$value 
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.3,upper=0.55)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0.75,upper=0.9)$value
              
            }else if (j%%2==0){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.2)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.3,upper=0.55)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.75,upper=0.9)$value

            }else if (j%%2==1){
              Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.2)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.3,upper=0.55)$value
              +integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.75,upper=0.9)$value

            }
          }
          
        }
      }        
    
  }else if (classifier==3){# in the Complete Case only need to generate for delta=1
    for (i in 1:n){
      if (delta[i]==1){
      for (j in 1:dn){
        
        #s1 = [0,1]
          if (j==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*1,lower=0,upper=1)$value
          }else if (j%%2==0){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }else if (j%%2==1){
            Xicoef[i,j] <- integrate(function(t) (ai[i]*(t-0.5)^2+bi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }
          
        }
        
      }
    }
    
  }
  
  
  
  #create a data frame in l2 containing Coefficient matrix, y and delta
  if(classifier !=1){
    datal2 <- data.frame(Xicoef,y,delta)
  }else{
    datal2 <- data.frame(Xicoef,y)
  }
  
  
  
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
    train <- shuffled[1:round(0.65 * n),]
    test <- shuffled[(round(0.65 * n) + 1):n,]
    
    #create a vector in test dataframe to store the predicted value
    
    
    #train1 <- train[train$delta==1,]
    #train2 <- train[train$delta==2,]
    
    for (d in 2:dn){ #only run for d>=2
      for (h in H){
        
        #cat("d = ",d," and h = ",h," ")
        #print(Sys.time())
        
        test$predictedY <- NULL
        for (i in 1:nrow(test)){
          
          # if the fully observable data set
          if (classifier == 1){
            
            #calculate phi and perform classification
            phi <- sum((2*train$y-1)*
                         t(apply(train[,1:d], MARGIN =1, 
                                 function(x) exp(-sum((x - test[i,1:d]) ^ 2)/(h^2)) )))
            
            if(phi>0){test$predictedY[i] <- 1} 
            else{test$predictedY[i] <- 0}
            
          # if the proposed classifier 
          }else if (classifier == 2) {
            #subset the training set to contain only observations having same delta with the observation to be classified
            if(test$delta[i]==1){
              subtrain <- train[train$delta==1,]
            }else if (test$delta[i]==2){
              subtrain <- train[train$delta==2,]
            }else if (test$delta[i]==3){
              subtrain <- train[train$delta==3,]
            }else if (test$delta[i]==4){
              subtrain <- train[train$delta==4,]
            }else if (test$delta[i]==5){
              subtrain <- train[train$delta==5,]
            }
            
            #calculate phi and perform classification
            phi <- sum((2*subtrain$y-1)*
                         t(apply(subtrain[,1:d], MARGIN =1, 
                                 function(x) exp(-sum((x - test[i,1:d]) ^ 2)/(h^2)) )))
            
            if(phi>0){test$predictedY[i] <- 1} 
            else{test$predictedY[i] <- 0}
            
          # if the Complete Case Analysis  
          }else if (classifier == 3){
            #if the observation is fully observable
            #subset the training set to contain only observations having same delta with the observation to be classified
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
  
  #using the best (d,h) above to perform classification on an independent testing sample of size testN

  #choose P(Y=1)=P(Y=0)=0.5 and generate vector y
  #p <- 0.5
  testY <- sample(c(0,1), size = testN, replace=TRUE, prob = c(p,1-p))
  
  
  # if Y=0, ai ~ Unif[0,5], bi ~ Unif[0,1]
  # if Y=1, ai ~ Normal[5,4], bi ~ Normal[1, 0.25]
  testai <- testY*rnorm(testN, 5, 2)+(1-testY)*runif(testN, 0, 5)
  testbi <- testY*rnorm(testN, 1, 0.5)+(1-testY)*runif(testN, 0, 1)
  
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
  if(classifier != 1){
    
    if(missMech==1){#for the case of NMAR or MAR
      testMissingPi2 <- NULL
      #the missing probability P(delta = 1|Y=y, X(t)=x(t))
      # = exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      # / 1+ exp(a*(1-y)+b*integrate(X(t),s)+c*intergrate(t*X(t), [0,1]\s))
      
      
      
      # Missing Probability Mechanism for s2 = [0,0.3]U[0.5,1]
      
      
      for (i in 1:testN){
        testMissingPi2[i] <- exp(a2*(1-testY[i])
                               +b2*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0,upper=0.3)$value
                                  +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.5,upper=1)$value)
                               +c2*integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.3,upper=0.5)$value)/
          (1+exp(a2*(1-testY[i])
                +b2*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0,upper=0.3)$value
                   +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.5,upper=1)$value)
                +c2*integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.3,upper=0.5)$value))
        
      }
      
      # Missing Probability Mechanism for s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
      
      if(missPattNo >=2){
        testMissingPi3 <- NULL
        
        for (i in 1:testN){
          testMissingPi3[i] <- exp(a3*(1-testY[i])
                               +b3*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0,upper=0.1)$value
                                   +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.2,upper=0.45)$value
                                   +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.6,upper=0.85)$value
                                   +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.9,upper=1)$value
                                   )
                               +c3*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.1,upper=0.2)$value
                                   +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.45,upper=0.6)$value
                                   +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.85,upper=0.9)$value
                                   )
                                 )/
            (1+exp(a3*(1-testY[i])
                   +b3*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0,upper=0.1)$value
                       +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.2,upper=0.45)$value
                       +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.6,upper=0.85)$value
                       +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.9,upper=1)$value
                       )
                   +c3*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.1,upper=0.2)$value
                       +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.45,upper=0.6)$value
                       +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.85,upper=0.9)$value
                       )
                 )
            )
          
        }
        
      }    

      #Missing Probability Mechanism for s4 = [0.25,0.5]U[0.65,1]
      if(missPattNo >=3){
        testMissingPi4 <- NULL
        
        for (i in 1:testN){
          testMissingPi4[i] <- exp(a4*(1-testY[i])
                               +b4*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0.25,upper=0.5)$value
                                    +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.65,upper=1)$value
                               )
                               +c4*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0,upper=0.25)$value
                                    +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.5,upper=0.65)$value
                               )
          )/
            (1+exp(a4*(1-testY[i])
                   +b4*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0.25,upper=0.5)$value
                        +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.65,upper=1)$value
                   )
                   +c4*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0,upper=0.25)$value
                        +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.5,upper=0.65)$value
                   )
            )
            )
          
        }
        
      }        
      
      #Missing Probability Mechanism for s5 = [0,0.2]U[0.3,0.55]U[0.75,0.9]
      if(missPattNo >=4){
        testMissingPi5 <- NULL
        
        for (i in 1:testN){
          testMissingPi5[i] <- exp(a5*(1-testY[i])
                               +b5*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0,upper=0.2)$value
                                    +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0.3,upper=0.55)$value
                                    +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.75,upper=0.9)$value
                               )
                               +c5*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.2,upper=0.3)$value
                                    +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.55,upper=0.75)$value
                                    +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.9,upper=1)$value
                               )
          )/
            (1+exp(a5*(1-testY[i])
                   +b5*(integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0,upper=0.2)$value
                        +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower= 0.3,upper=0.55)$value
                        +integrate(function(t) testai[i]*(t-0.5)^2+testbi[i],lower=0.75,upper=0.9)$value
                   )
                   +c5*(integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.2,upper=0.3)$value
                        +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.55,upper=0.75)$value
                        +integrate(function(t) t*(testai[i]*(t-0.5)^2+testbi[i]),lower=0.9,upper=1)$value
                   )
            )
            )
          
        }
        
      }  
      
      
            
      
    }
    
  }
  
  
  
  
  
  
  
  #generate a vector of missing pattern delta only if the Classifier is not fully observable data set
  testDelta <- NULL  
  
  
  
  if (classifier != 1){
    # if there is only 1 missing pattern
    if(missPattNo == 1){
      if(missMech==1){#NMAR or MAR
        for (i in 1:testN){
          testDelta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(testMissingPi2[i],1-testMissingPi2[i]))
        } 
        
      }else if(missMech==2){#MCAR
        for (i in 1:testN){
          testDelta[i] <- sample(c(1,2), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
        } 
      }
      
      # if there are 2 missing patterns  
    }else if(missPattNo ==2){
      if(missMech==1){#NMAR or MAR
        
        for (i in 1:testN){
          # pick the missing pattern probability mechanism with Benoulli flip coin 
          testPattern <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
          
          if(testPattern==2){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi2[i],1-testMissingPi2[i]))
            
          }else if(testPattern==3){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi3[i],1-testMissingPi3[i]))
          }
        }
        
        
        
      }else if(missMech==2){#MCAR
        for (i in 1:testN){
          # if if 0 the data is fully observable, 1 the data is missing 
          testMissing <- sample(c(0,1), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
          
          if(testMissing==0){
            testDelta[i] <- 1
          }else if(testMissing==1){
            testDelta[i] <- sample(c(2,3), size = 1, replace=TRUE, prob = c(0.5,0.5))
          }
          
          
        }
      }
    }else if(missPattNo ==4){
      if(missMech==1){#NMAR or MAR
        
        for (i in 1:testN){
          # pick the missing pattern probability mechanism with Benoulli flip coin 
          testPattern <- sample(c(2,3,4,5), size = 1, replace=TRUE, prob = c(0.25,0.25,0.25,0.25))
          
          if(testPattern==2){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi2[i],1-testMissingPi2[i]))
            
          }else if(testPattern==3){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi3[i],1-testMissingPi3[i]))
          }else if(testPattern==4){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi4[i],1-testMissingPi4[i]))
          }else if(testPattern==5){
            testDelta[i] <- sample(c(1,testPattern), size = 1, replace=TRUE, prob = c(testMissingPi5[i],1-testMissingPi5[i]))
          }
          
        }
        
        
        
      }else if(missMech==2){#MCAR
        for (i in 1:testN){
          # if if 0 the data is fully observable, 1 the data is missing 
          testMissing <- sample(c(0,1), size = 1, replace=TRUE, prob = c(1-MCARp,MCARp))
          
          if(testMissing==0){
            testDelta[i] <- 1
          }else if(testMissing==1){
            testDelta[i] <- sample(c(2,3,4,5), size = 1, replace=TRUE, prob = c(0.25,0.25,0.25,0.25))
          }
          
          
        }
      }
    }
    
    
    
  }
  
  
  
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
  
  #generate the coefficient matrix in l2 with dn dimensions
  testXicoef <- matrix(, nrow = testN, ncol=bestD)
  
  #generate the Xicoef if the classifier for fully observable data set
  if(classifier==1){
    for (i in 1:testN){
      for (j in 1:bestD){
        
        if (j==1){
          testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=1)$value
        }else if (j%%2==0){
          testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
        }else if (j%%2==1){
          testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
        }
        
      }
    }
    #generate the Xicoef if the classifier not for fully observable data set  
  }else if (classifier==2){
    for (i in 1:testN){
     if (testDelta[i]==1){
      for (j in 1:bestD){
        #s1 = [0,1]
          if (j==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=1)$value
          }else if (j%%2==0){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }else if (j%%2==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }
        } 
        }else if(testDelta[i]==2){#s2 = [0,0.3]U[0.5,1]
          for (j in 1:bestD){
          if (j==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=0.3)$value 
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.5,upper=1)$value
            
          }else if (j%%2==0){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.3)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.5,upper=1)$value
          }else if (j%%2==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.3)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.5,upper=1)$value
          }
          }
        }else if(testDelta[i]==3){#s3 = [0,0.1]U[0.2,0.45]U[0.6,0.85]U[0.9,1]
          for (j in 1:bestD){
          if (j==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=0.1)$value 
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.2,upper=0.45)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.6,upper=0.85)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.9,upper=1)$value
          }else if (j%%2==0){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.1)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.2,upper=0.45)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.6,upper=0.85)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.9,upper=1)$value
          }else if (j%%2==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.1)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.2,upper=0.45)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.6,upper=0.85)$value
                              +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.9,upper=1)$value
          }
        }
        }else if(testDelta[i]==4){#s4 = [0.25,0.5]U[0.65,1]
          for (j in 1:bestD){
            if (j==1){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.25,upper=0.5)$value 
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.65,upper=1)$value
              
            }else if (j%%2==0){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.25,upper=0.5)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.65,upper=1)$value
              
            }else if (j%%2==1){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.25,upper=0.5)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.65,upper=1)$value
              
            }
          }
          
        }else if(testDelta[i]==5){#s5 = [0,0.2]U[0.3,0.55]U[0.75,0.9]
          for (j in 1:bestD){
            if (j==1){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=0.2)$value 
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.3,upper=0.55)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0.75,upper=0.9)$value
              
            }else if (j%%2==0){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=0.2)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.3,upper=0.55)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0.75,upper=0.9)$value
              
            }else if (j%%2==1){
              testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=0.2)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.3,upper=0.55)$value
                                +integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0.75,upper=0.9)$value
              
            }
          }
          
        }
    
        }
    
  }else if (classifier==3){#the complete case, only need if delta =1
    for (i in 1:testN){
      if (testDelta[i]==1){
      for (j in 1:bestD){
        #s1 = [0,1]
          if (j==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*1,lower=0,upper=1)$value
          }else if (j%%2==0){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*cos(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }else if (j%%2==1){
            testXicoef[i,j] <- integrate(function(t) (testai[i]*(t-0.5)^2+testbi[i])*sqrt(2)*sin(2*pi*(j%/%2)*t),lower=0,upper=1)$value
          }
          
        }
        
      }
    }
    
  }
  
  
  
  #create a data frame in l2 containing Coefficient matrix, y and delta
  if(classifier !=1){
    testDatal2 <- data.frame(testXicoef,testY,testDelta)
  }else{
    testDatal2 <- data.frame(testXicoef,testY)
  }
  
  

  #perform the Kernel Classification using (bestD,bestH) and the original data set to train

    train <- datal2
    test <- testDatal2
    
    #create a vector in test dataframe to store the predicted value
    test$predictedY <- NULL
    
    #train1 <- train[train$delta==1,]
    #train2 <- train[train$delta==2,]
    

        
        for (i in 1:nrow(test)){
          
          # if the fully observable data set
          if (classifier == 1){
            
            #calculate phi and perform classification
            phi <- sum((2*train$y-1)*
                         t(apply(train[,1:bestD], MARGIN =1, 
                                 function(x) exp(-sum((x - test[i,1:bestD]) ^ 2)/(bestH^2)) )))
            
            if(phi>0){test$predictedY[i] <- 1} 
            else{test$predictedY[i] <- 0}
            
            # if the proposed classifier 
          }else if (classifier == 2) {
            #subset the training set to contain only observations having same delta with the observation to be classified
            if(test$testDelta[i]==1){
              subtrain <- train[train$delta==1,]
            }else if (test$testDelta[i]==2){
              subtrain <- train[train$delta==2,]
            }else if (test$testDelta[i]==3){
              subtrain <- train[train$delta==3,]
            }else if (test$testDelta[i]==4){
              subtrain <- train[train$delta==4,]
            }else if (test$testDelta[i]==5){
              subtrain <- train[train$delta==5,]
            }
            
            #calculate phi and perform classification
            phi <- sum((2*subtrain$y-1)*
                         t(apply(subtrain[,1:bestD], MARGIN =1, 
                                 function(x) exp(-sum((x - test[i,1:bestD]) ^ 2)/(bestH^2)) )))
            
            if(phi>0){test$predictedY[i] <- 1} 
            else{test$predictedY[i] <- 0}
            
            # if the Complete Case Analysis  
          }else if (classifier == 3){
            #if the observation is fully observable
            #subset the training set to contain only observations having same delta with the observation to be classified
            if(test$testDelta[i]==1){
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
    testError=c(testError,length(which(test$testY!=test$predictedY))/nrow(test))
    cat("test Error of this run = ", length(which(test$testY!=test$predictedY))/nrow(test),"\n")

    
  

  

    

}

#Calculate the mean and sd of the error rates
testErrorAvg <- mean(testError)
testErrorSd <- sd(testError)

#combine all the bestD and bestH in all the runs
bestPair <- cbind(d_hat,h_hat)

print(Sys.time())
#print all the parameters and result
cat("Covariate function : ai(t-0.5)^2+bi","\n")
cat("No of Missing Patterns = ",missPattNo,"\n")
cat("Remark: No of Missing Patterns is not applicable in the case of fully observable data set","\n")
cat("The classifier : ")
    if (classifier==1){
      print("fully observable data set")
    }else if(classifier==2){
      print("the proposed classifier")
    }else if(classifier==3){
      print("the complete case")
    }
cat("Missing Mechanism : ")
    if (classifier==1){
      print("NA")
    }else {
      if(missMech ==1){
        print("NMAR or MAR")  
      }else if(missMech ==2){
        print("MCAR") 
        cat("  Missing percentage = ", MCARp,"\n")
      }
    }
cat("sample size n = ",n,"\n")
cat("dn = ",dn,"\n")
cat("H = ",H,"\n")
cat("independent testing sample size N = ",testN,"\n")
cat("# of entire Monte-Carlo runs = ",testTime,"\n")
cat("# of data splitting = ",B,"\n")
cat("a2 = ",a2," ")
cat("b2 = ",b2," ")
cat("c2 = ",c2,"  ,")
cat("a3 = ",a3," ")
cat("b3 = ",b3," ")
cat("c3 = ",c3,"  ,")
cat("a4 = ",a4," ")
cat("b4 = ",b4," ")
cat("c4 = ",c4,"  ,")
cat("a5 = ",a5," ")
cat("b5 = ",b5," ")
cat("c5 = ",c5,"\n ")
cat("Remark: a,b,c,a3,b3,c3 are not applicable in the case of MCAR","\n")
cat("testErrorAvg = ", testErrorAvg,"\n")
cat("testErrorSd = ", testErrorSd,"\n")
cat("all the pairs of best(d,h): ","\n")
print(bestPair)
cat("testError= ", testError,"\n")

print(Sys.time())