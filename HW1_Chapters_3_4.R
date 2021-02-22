# Install packages 

install.packages("tidyverse")
install.packages("matlib")
install.packages("dplot")
install.packages("ellipse")
library(matlib)
library(dplot)
library(ellipse)

# Nate Islip 
# 1/20/21 
# Homework #1 MATH 585


#---------------------------------------- Chapter 3 ----------------------------------------
# 3.10 
# Part (A) 

A <- matrix(c(3, 6, 4, 7, 5, 1, 4, 2, 0, 3, 0, 6, 2, 3, 4), ncol = 3) # Generate the matrix A 
x <- matrix(c(5,5,5,5,5,2,2,2,2,2,3,3,3,3,3), ncol = 3) # mean matrix [5 2 3]
C = A - x # Corrected mean matrix

# Check for linear dependence where a1v1 + a2v2 + ... + akvk = 0 

C # COnsole output 

# From the Corrected matrix, we can see there is a linear combination x1 + x2 = x3, a = [1,1,-1]
# Part (b) 

S <- cov(A) # Output Sample Variance matrix 

# By hand 

n <- nrow(A) # n = number of rows in A (Original matrix)
SM <- t(C) %*% C / (n-1) # Note, multiplying matrices in R %*%

# testing for generalized variance 

DS <- det(SM) # Determinant is 0 
#------------------------------------Problem 4.2 c) ------------------------------------

install.packages("ellipse")
install.packages("MASS")
install.packages("mvtnorm")

library(ellipse)
library(MASS)
library(mvtnorm)

set.seed(123)

mu <- c(0,2) # mu vector
Sigma <- matrix(c(2,sqrt(2)/2,sqrt(2)/2,1), nrow=2, ncol=2) # Sigma matrix 
X <- mvrnorm(n=10000,mu=mu, Sigma=Sigma) # produces a scatter pf n points to surround ellipse
lambda <- eigen(Sigma)$values # Producing eigenvectors, and values for axis
Gamma <- eigen(Sigma)$vectors #
elps <- t(t(ellipse(Sigma, level=0.5, npoints=1000))+mu) # produces the proper ellipse
chi <- qchisq(0.5,df=2) # chi-squared distribution 
c <- sqrt(chi)
factor <- c*sqrt(lambda)
plot(X[,1],X[,2], main = "Plot of Ellipse") # plots points 
lines(elps, col = "blue") # Colorsin the lines of the ellipse
points(mu[1], mu[2], col = "red") # highlights the point of mu_1 and mu_2
segments(mu[1],mu[2],factor[1]*Gamma[1,1],factor[1]*Gamma[2,1]+mu[2])
segments(mu[1],mu[2],factor[2]*Gamma[1,2],factor[2]*Gamma[2,2]+mu[2])

#--------------------------------- Problem 4.4---------------------------------------
#4.4

Sigma = matrix(c(1,1,1,1,3,2,1,2,2), ncol = 3) # Population variance covariance 
mu = matrix(c(2,-3,1), ncol = 1) # vector population sample means 
alpha = matrix(c(3,-2,1)) # linear combination 
alphaT = t(alpha)

mu1 = alphaT %*% mu # Answer = 13 
sigma_11 = alphaT %*% Sigma %*% alpha # Answer = 9 


#---------------------Problem 4.40 ---------------------------------------------------
#4.40

# Scatter Plot 

df <- data.frame(National_Parks_1_27_data) # Scatter for National parks data 
plot(x = df$Size.acres, y = df$Visitors,
       xlab = "Size (Acres)",
       ylab = "Visitors(millions)",
       title("National Parks data"))

# Q-Q Plots 

qqnorm(df$Size.acres, pch = 1, frame = FALSE) # Q-Q Plot for Acreage 
qqline(df$Size.acres, col = "steelblue", lwd = 2)

qqnorm(df$Visitors, pch = 1, frame = FALSE) # Q-Q Plot for Visitors 
qqline(df$Visitors, col = "steelblue", lwd = 2) 

SqDist2 <- mahalanobis(df, colMeans(df), cov(df))


# Constructing values for Q-Q plot 

n <- nrow(df) # 15 rows in our data set 

storage <- numeric(5) # empty vector to store new values 

#-------------------Problem 4.26-----------------------------------------------------

# Part A: 

df2 <- X4_26

# Data set was imported via excel, Global environment
# Calculate the mean for each of the variables in data se

Mx1 <- mean(df2$x1) # 5.20 
meanvector <- rep(Mx1, n) # fills empty vector with repeated mean values
Mx2 <- mean(df2$x2) #12.481
meanvector2 <- rep(Mx2, n) 

#----------------------------------------------------------

x1 <- matrix(c(df2$x1), ncol = 1) # vectors of x1 and x2 
x2 <- matrix(c(df2$x2), ncol = 1)
x1x2 <- cbind(x1, x2) # 10 rows and 2 columns

SqDist <- mahalanobis(df2, colMeans(df2), cov(df2)) # Here are the distances (4-32)


#---------------------------------------------------------
# Part B 

MX <- matrix(c(Mx1, Mx2), ncol = 1) # VEctor Means
df2COV <- cov(df2) # see console SAMPLE COVARIANCE 
INVCOV <- inv(df2COV) # see console
n = nrow(df2)
empty <- rep(0, n)


for (i in 1:n){
    vector <- matrix(c(x1x2[i,1], x1x2[i,2]), ncol = 1) # issue 
    empty[i] <- t((vector - MX)) %*% INVCOV %*% (vector - MX)
    vector <- matrix(0, 2, 1)
}

vector <- matrix(c(x1x2[1,1], x1x2[1,2]), ncol = 1)

# Part C: 
# Chi Squared Plot 

# Computing the values for each quantile 

n = 10 

empty <- matrix(0, n, 1)
for (j in 1:n){
    empty[j] <- (j - (0.5))/10
}

Quantiles <- qchisq(empty, df = 2)

plot(empty, Quantiles, main = expression(paste(" Constructing ", chi^2, " Plot")), 
     xlab = expression(paste(q[j], " quantiles")), ylab = expression(paste(d[j], " Distances") 
         ))
                
#--------------------------------------------------------








