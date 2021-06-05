rm(list=ls())
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
par(mfrow=c(1,1))

####################################Packages##################################
library(readr)
library(car)
library(reshape2)
library(MASS)
library(vars)
library(stats)
library(readxl)
library(tseries)
library(xtable)
library(plyr)
library(verification)
library(corrplot)
library(Hmisc)
library(grid)
library(gridBase)
library(gridExtra)
library(gtable)
#library(MTS)
#################################Set-up#######################################

#SET WORKSPACE HERE
#setwd("C:/Users/Christian_2/Dropbox/Bachelor/R_code")
#setwd("C:/Users/Christian/Dropbox/Bachelor/R_code")
#setwd("C:/Users/Fanny/Dropbox/DTU/6.Var_2018-Sjatte_semestern/Bachelor/R_code")

#Use VARextension to have fixed threshold
source("VARextension.R")
#Use VARextension2 to have dynamic threshold
#source("functions/VARextension2.R")

##############################INITIALIZE######################################

include.predictor <- F
choose.dataset <- 5
p <- 1 #choose lag

#color scheme
col1<-c("orange",2:5, "antiquewhite4", "darkcyan", "deeppink", 
        "rosybrown4", "seagreen1", "dodgerblue", "skyblue2", 
        "brown4", "chartreuse", "burlywood4", "coral2", 
        "aquamarine2", "darkgoldenrod", "darkmagenta", "darkolivegreen", 
        "darkorchid1", "darkorchid4", "hotpink", "aquamarine4", "deeppink4", 
        "dodgerblue4", "firebrick4", "hotpink4", "lightslateblue", 
        "maroon", "maroon4", "mediumpurple", "mediumseagreen", 
        "navyblue", "olivedrab1", "orangered", "palegreen4", 
        "palevioletred",  "royalblue3", "salmon", "tan3", "tan4", 
        "violet", "yellow4", "wheat4", "slategray4", 
        "brown1", "tan1")

####PREPARE INDUSTRY DATA####
if(choose.dataset == 5){
  dat <- read.csv("Data/5_Industry_Portfolios.CSV", header=T, skip=1,
                  nrows=1099, sep=";", colClasses = c("factor",rep("numeric",5)))
}else if(choose.dataset== 10){
  dat <- read.csv("Data/10_Industry_Portfolios.CSV", header=T, skip=1,
                  nrows=1099, sep=";", colClasses = c("factor",rep("numeric",10)))
}else if(choose.dataset==17){
  dat <- read.csv("Data/17_Industry_Portfolios.CSV", header=T, skip=1,
                  nrows=1099, sep=";", colClasses = c("factor",rep("numeric",17)))
}else if(choose.dataset==48){
  dat <- read.csv("Data/48_Industry_Portfolios.CSV", header=T, skip=1,
                  nrows=1099, sep=";", colClasses = c("factor",rep("numeric",48)))
}else{
  print("Data set doesn't exist")
}

names(dat)[1]<-c("Date")
dat$Date<-as.Date(paste(dat$Date,"01",sep=""),format="%Y%m%d")
dat.n <- length(dat[,1])

#Check for and removing missing data
sum(is.na(dat))
dat.miss<-which(dat[2:ncol(dat)] <= -99.99, arr.ind = T)
if(length(dat.miss)>=1){
  dat.miss[,2] <- dat.miss[,2]+1
  dat[dat.miss] <- NA
  dat<-dat[rowSums(is.na(dat)) == 0, ]
}

#Prepare some information
N<-nrow(dat)
N.col <- ncol(dat)
n.endo <- N.col-1
names.endo <- names(dat[2:N.col])
dat.names <- names(dat[2:N.col])

#Shortly look at data before removing data
plot(dat$Date[1:N], dat[1:N,2], type="l", 
     xaxt="n", xlab="Time", ylab="Return", 
     main="Monthly Return by Industries", col=col1[2-1], 
     ylim=c(min(dat[,2:N.col], na.rm=T)+1, max(dat[,2:N.col], na.rm=T)+1))
axis(1, dat$Date[c(seq(1,N,length.out=12))], 
     format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"), 
     cex.axis = 1)
for(i in c(3:N.col)){
  lines(dat$Date[1:N], dat[1:N,i], col=col1[i-1])
  abline(mean(dat[1:N,i], na.rm=T),0, col="black")
}
legend("topright",
       legend=dat.names,
       col=col1,lty=1)
summary(dat)

########################PREPARE PREDICTOR DATA################################
PredData16 <- NULL
if(include.predictor){
  PredData16 <- read_excel("Data/PredictorData2016.xlsx", 
                           col_types = c("numeric", rep("numeric",17)))
  
  names(PredData16)[1] <- c("Date")
  PredData16$Date <- as.factor(PredData16$Date)
  PredData16$Date <- as.Date(paste(PredData16$Date,"01",sep=""),format="%Y%m%d")
  #remove years not used 
  PredData16 <- PredData16[829:nrow(PredData16), ]
  n.data16 <-nrow(PredData16)
  
  
  #Extract data
  #Remove last NA observation
  PredData16<-PredData16[-nrow(PredData16),]
  DatePred <- PredData16$Date
  PredData16 <- PredData16[, c(2,3,12)]
  sum(is.na(PredData16))
  PredData16<- as.data.frame(list(Index=PredData16$Index, D12=PredData16$D12, Infl =PredData16$infl))
  
  PredData16$D12<-log(PredData16$D12)-log(PredData16$Index)
  laggedIndex <-Lag(PredData16$Index, -1)
  laggedIndex <- laggedIndex[-length(laggedIndex)]
  return1 <- diff(PredData16$Index)/laggedIndex*100
  
  DatePred <- DatePred[-1]
  PredData16<-PredData16[-1,]
  
  PredData16$Index <- return1
  names(PredData16) <- c("Return","D/Y","infl")
  
  
  par(mfrow=c(3,1))
  plot(PredData16[,1], xaxt='n', main="Market Returns", ylab="Return", xlab="Date" )
  axis(1, c(seq(1,N,length.out=12)), 
       format(DatePred[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  plot(PredData16[,2], xaxt='n', main="Dividend Yield", ylab="Yield", xlab="Date")
  axis(1, c(seq(1,N,length.out=12)), 
       format(DatePred[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  plot(PredData16[,3], main="Monthly Inflation", ylab="Inflation", xlab="Date",xaxt='n')
  axis(1, c(seq(1,N,length.out=12)), 
       format(DatePred[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  
  
  ###DIFFERENCING
  diff.lag=1
  par(mfrow=c(1,1))
  temp1<-diff(PredData16[,2], lag=diff.lag)
  plot(temp1, xaxt='n', main="Dividend Yield", ylab="Yield", xlab="Date")
  axis(1, c(seq(1,N,length.out=12)), 
       format(DatePred[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  temp2<-PredData16$Return[-1]
  temp3<-PredData16$infl[-1]
  DatePred <- DatePred[-1]
  PredData16 <- NULL
  PredData16<-as.data.frame(list("Return"=temp2, "D/Y"=temp1, "Infl" =temp3))
  
  ###
  
  dat<-dat[(163+diff.lag):(dat.n-13-1),]
  dat<-cbind(dat, PredData16)
  
}else{
  
  if(length(dat.miss) <= 0){
    dat <- dat[163:dat.n, ]  
  }
}

n.exo <- ncol(dat)-1
N<-nrow(dat)
N.col <- ncol(dat)
dat.names <- names(dat[2:N.col])

####LOOK AT Correlation####
cor(dat[2:N.col])
corrplot(cor(dat[2:(n.endo+1)]), method="color")
mtext(paste("Correlation between each of the", choose.dataset,"industries"))

####LOOK AT NORMALITY####
par(mfrow=c(2,3))
for(i in 1:n.endo){
  qqPlot(dat[,i+1], main=dat.names[i], ylab="Observation")
}

################################### Plots  ###################################

#All attributes in one plot
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
plot(dat$Date[1:N], dat[1:N,2], type="l", 
     xaxt="n", xlab="Time", ylab="Return", 
     main=paste("Monthly Return by",choose.dataset, "Industries"), col=col1[2-1], 
     ylim=c(min(dat[,2:(n.endo+1)], na.rm=T)+1, max(dat[,2:(n.endo+1)], na.rm=T)+2))
axis(1, dat$Date[c(seq(1,N,length.out=12))], 
     format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"), 
     cex.axis = 1)
for(i in c(3:(n.endo+1))){
  lines(dat$Date[1:N], dat[1:N,i], col=col1[i-1])
  abline(mean(dat[1:N,i], na.rm=T),0, col="black")
}
par(mgp=c(2,0.8,0), mar=c(1,1,1,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)

####MAKE LEGEND FOR PLOT####
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft", c(paste(names.endo)), 
       pt.bg=col1[1:n.endo], ncol=round(choose.dataset/5), 
       pch=c(rep(22,n.endo)), lty=c(rep(NA,n.endo), 1))


par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
#### Plot each attribute alone ####
for(i in c(2:(n.endo+1))){
  plot(dat$Date[1:N], dat[1:N,i], type="l", 
       xaxt="n", xlab="Time", ylab="Return", 
       main=paste("Monthly Return of", dat.names[i-1]), col=col1[i-1])
  axis(1, dat$Date[c(seq(1,N,length.out=12))], 
       format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  abline(mean(dat[1:N,i],na.rm=T),0, col="red")
}

#### Boxplot ####
par(mfrow=c(1,1))
par(mgp=c(2,0.8,0), mar=c(1,3,1,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
boxplot(dat[2:(n.endo+1)], col=col1, names=F)

par(mgp=c(2,0.8,0), mar=c(1,1,1,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
#Make legend, insets manually to place correctly
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft", c(paste(names.endo)), 
       pt.bg=col1[1:n.endo], ncol=round(choose.dataset/5), 
       pch=c(rep(22,n.endo)), lty=c(rep(NA,n.endo), 1))
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)

##############################ACF PACF CCF########################################
par(mfrow=c(1,1), mar=c(3.5,3,2,1), mgp=c(2,0.8,0))
lag.cf <- 40
plot(acf(dat[,2],plot=FALSE, lag.max=lag.cf,na.action=na.pass),col=col1[1],
     type="l", max.mfrow=1, ylim=c(-0.15,1), main=" ")
title("ACF of Each Industry", line=0.5)
points(0:lag.cf,acf(dat[,2],plot=FALSE, 
                    lag.max=lag.cf,na.action=na.pass)$acf, col=col1[2-1], pch=1, lwd=2)
for(i in 3:(n.endo+1)){
  lines(0:lag.cf,acf(dat[,i],plot=FALSE, 
                     lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i-1])
  points(0:lag.cf,acf(dat[,i],plot=FALSE, 
                      lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i-1], 
         pch=1, lwd=2)
}
legend("topright", legend=names.endo, col=col1, 
       pch=1, ncol=choose.dataset/5, lty=1)

#### PACF ####
par(mfrow=c(1,1), mar=c(3.5,3,2,1), mgp=c(2,0.8,0))
plot(pacf(dat[,2],plot=FALSE, lag.max=lag.cf,na.action=na.pass), type="l", 
     max.mfrow=1, 
     ylim=c(-0.2,0.5), col=col1[2-1], 
     main=" ")
title("PACF of Each Industry", line=0.5)
points(1:lag.cf,pacf(dat[,2],plot=FALSE, 
                     lag.max=lag.cf,na.action=na.pass)$acf, col=col1[2-1], pch=1, lwd=2)
for(i in 3:(n.endo+1)){
  lines(1:lag.cf,pacf(dat[,i],plot=FALSE, 
                      lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i-1])
  points(1:lag.cf,pacf(dat[,i],plot=FALSE, 
                       lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i-1], pch=1, lwd=2)
}
legend("topright", legend=names.endo,
       col=col1,lty=1, pch=1, ncol=choose.dataset/5)

#### CCF ####
par(ps=11)
name.temp <- matrix(NA, ncol=2, nrow=choose(n.endo,2))
col.temp <- matrix(0, ncol=2, nrow=choose(n.endo,2))
k=0;
par(mfrow=c(1,1), mar=c(3.5,3,2,1), mgp=c(2,0.8,0))
plot(ccf(dat[2],dat[3],plot=FALSE, lag.max=lag.cf,na.action=na.pass), type="l", max.mfrow=1, 
     ylim=c(-0.2, 0.9), 
     main=" ",col=col1[2-1])
title("CCF of Each Industry", line=0.5)
points(-lag.cf:lag.cf,ccf(dat[2],dat[3],plot=FALSE, lag.max=lag.cf,na.action=na.pass)$acf,col=col1[3-1], pch=1, lwd=2)
for(i in c(2:(n.endo+1))){
  for(j in c(2:(n.endo+1))){
    if(j != i & j>i){
      k=k+1;
      lines(-lag.cf:lag.cf,ccf(dat[i],dat[j],plot=FALSE, 
                               lag.max=lag.cf,na.action=na.pass)$acf,col=col1[i-1])
      points(-lag.cf:lag.cf,ccf(dat[i],dat[j],plot=FALSE, 
                                lag.max=lag.cf,na.action=na.pass)$acf,col=col1[j-1], pch=1, lwd=2)
      col.temp[k, 1] <- i-1
      col.temp[k, 2] <- j-1
      name.temp[k, 1] <- names.endo[i-1]
      name.temp[k, 2] <- names.endo[j-1]
    }
  }
}

#Use col.temp for color of each combination, need to make vector of data names.

if(choose.dataset != 48){
  legend("topleft", legend=dat.names, 
         pt.bg=col1, ncol=choose.dataset/5, 
         pch=c(rep(22,n.endo)), lty=c(rep(NA,n.endo), 1))
}else{
  # Legends for 48 industries
  legend("topleft", legend=dat.names[1:24], 
         pt.bg=col1[1:24], ncol=3, 
         pch=c(rep(22,n.endo)), lty=c(rep(NA,n.endo), 1))
  legend("topright", legend=dat.names[25:48], 
         pt.bg=col1[25:48], ncol=3, 
         pch=c(rep(22,n.endo)), lty=c(rep(NA,n.endo), 1))
}

#############################CHECK MODEL ORDER#############################
# Using VARselect and ACF, PACF and CCF to choose model and order.
var.test1 <- VARselect(dat[,2:(n.endo+1)], lag.max=5, type="both")

#Const is best (lowest AIC) with order 1, var.test3.

#Fitting model
var.fit1 <- vars:::VAR(dat[,2:(n.endo+1)], p=1, type="both")
mod <-restrict2(var.fit1, method="ser", 
                thresh=2, m=NULL)
summary(var.fit1)

par(mfrow=c(5,1))
par(ps=18)
par(mgp=c(2,0.8,0), mar=c(3.5,4,3,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
for(i in 1:n.endo){
  plot(residuals(var.fit1$varresult[[i]]), ylab="Residual", xlab="Time", xaxt='n', main=dat.names[i], type='l',col=col1[i])
  abline(mean(residuals(var.fit1$varresult[[i]])),0, col=1)
  axis(1, c(seq(1,N,length.out=12)), 
       format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"))
}

par(mfrow=c(2,3))
for(i in 1:n.endo){
  qqPlot(residuals(var.fit1$varresult[[i]]), ylab="Residual", main=dat.names[i])
  
}

par(mfrow=c(2,3))
par(ps=18)
par(mgp=c(2,0.8,0), mar=c(3.5,4,3,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
for(i in 1:n.endo){
  acf(residuals(var.fit1$varresult[[i]]), lag=30, main="")
  mtext(paste("ACF of residuals for",dat.names[i]) )
}

par(mfrow=c(2,3))
for(i in 1:n.endo){
  pacf(residuals(var.fit1$varresult[[i]]), lag=100, main="")
  mtext(paste("PACF of residuals for",dat.names[i]))
}


################################# FUNCTIONS ##################################

#### Scale data so it lies between x and y
norm.scale<- function(dat, x, y){
  #Normalize to [0, 1]:
  m = min(dat);
  range = max(dat) - m;
  dat = (dat - m)/range;
  #scale to [x,y]:
  range2 = y - x;
  normalized = (dat*range2) + x;
  return(normalized)
}

# Function that makes model defined as attributes names to matrix 
#   with ones and zeros and chosen type.
# Input:  String with attribute names.
# Output: chosenVariables: Matrix with zeros and ones representing if a attribute is used or not.
#         chosenType: What type to use; "const", "trend", "both" or "none".
# resmat      <-  Restrict matrix
make.resmat <- function(attribute.names){
  resmat <- matrix(data=0, nrow=n.endo, ncol=p*n.exo+2)
  count <- 1
  for (model in attribute.names) {
    temp <- strsplit(model, " ")[[1]]
    ind.names <- match(temp, strDat)
    resmat[count, ind.names] <- 1
    count <- count+1
  }
  return(resmat)
}

# Function for variance of predictions and model
# x is a VarEst type
# n.ahead is the number of predictions ahead.
sig <- function(x, n.ahead){
  #degrees of freedom for each par
  n.par <- sapply(x$varresult, function(x) summary(x)$df[2])
  #cross product of residuals divided by df
  sigma.u <- crossprod(resid(x))/n.par
  Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead)) #Preallocate
  Sigma.yh[, , 1] <- sigma.u #save sigma.u in sigma.yh and remove names
  Phi <- Phi(x, nstep = n.ahead) #find phi
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, 
                                                        , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}

###LJUNG BOX TEST
ljung.test<-function(x, nlag, modnr){
  dat <- x
  pval <- sapply(1:nlag, function(i) Box.test(
    dat, i, type = "Ljung-Box")$p.value)
  plot(1L:nlag, pval, xlab = "lag", 
       ylab = "p value", ylim = c(0,1), 
       main = paste("Ljung-Box statistic:",modnr))
  abline(h = 0.05, lty = 2, col = "blue")
}
##############################Prepare strDat##################################
# StrDat contains variable names, it is made so it can also handle p>1

strDat <- c()
k<-0
for(i in 1:p){
  for(j in 1:n.endo){
    k<-k+1
    strDat[k] <- paste(dat.names[j], i,sep="")
  }
}
k<-k+1
strDat[k] <- "Const"
k<-k+1
strDat[k] <- "Trend"

if(include.predictor){
  for(j in (n.endo+1):(n.exo)){
    k<-k+1
    strDat[k] <- paste(dat.names[j], 1,sep="")
  }
}


############################# SLIDING MODEL ##################################

############################# INITIALIZE #####################################
# init.block  <-  Initialize block length
# n.ahead     <-  Prediction ahead
# p           <-  Order of model
# threshold   <-  Threshold of significant parameter
# res.x       <-  List of residuals 
# est.x       <-  List of estimates
# estNorm.mat <-  Normalized estimates
# strDat      <-  Save names of each attribute with lag.
# dim.mat     <-  Dimensions of residual matrix

init.block <- 9*12
n.ahead <- 1 
if(init.block>=N){
  print("Warning, block is too large")
}
n.models <- N - init.block
threshold <- 1.96
res.list <- vector(mode="list", n.models)
est.list <- vector(mode="list", n.models)
norm.test<- matrix(0, nrow=n.models, 2)
res.mat <- matrix(0, nrow=n.models, p*n.exo+2)
est.mat <- matrix(0, nrow=n.models, p*n.exo+2)
estNorm.mat <- matrix(0, nrow=n.models, p*n.exo+2)
end <- init.block
start <- 1
res.x <- vector(mode="list", n.endo)
est.x <- vector(mode="list", n.endo)

###########################RUN SLIDING MODEL##################################
while (end < N) {
  if(include.predictor){
    mod <-VAR(dat[(start:end),2:(n.endo+1)], 
              exogen=dat[start:end,(n.endo+2):(n.exo+1)], p=p, type="both")
  }else{
    mod <-vars::VAR(dat[(start:end),2:(n.endo+1)], p=p, type="both")
  }
  
  mod.Red <-restrict2(mod, method="ser", 
                      thresh=threshold, m=NULL)
  
  for(variable in 1:n.endo){
    tvals <- abs(coef(summary(mod.Red$varresult[[variable]]))[,3])
    est <- Bcoef(mod.Red)
    index <- which(mod.Red$restrictions[variable,]==1)
    if(min(tvals) <= threshold){
      mod.Red$restrictions[variable, index] <- 0
      est[variable, index]<-0
      index <- 0
    }
  }
  est.list[[start]] <- est
  res.list[[start]] <- mod.Red$restrictions
  
  ############### RUN THIS PART TO LOOK AT P VALUES JB TEST##################
  #norm.test[start,1]<-as.numeric(normality.test(mod, multivariate.only = TRUE)$jb.mul$JB$p.value)
  #mod2 <-VAR(dat[(start:end),2:(n.endo+1)], p=p, type="both")
  #mod.Red2 <-restrict2(mod2, method="ser", thresh=threshold, m=NULL)
  #norm.test[start,2]<-as.numeric(normality.test(mod2, multivariate.only = TRUE)$jb.mul$JB$p.value)
  ############################################################################
  
  start <- start+n.ahead
  end <- end+n.ahead
}

########## LOOK AT MOST SIGNIFICANT VARIABLES #############
temp <- Reduce('+', res.list) #/rowSums(Reduce('+', res.list))*100, digits=1)
xtable(temp)


###################################PLOT SLIDING MODEL#########################
## Scales and plots the model.
n.figures=5
par(mfrow=c(n.figures,1))
par(mgp=c(2,0.8,0), mar=c(0,3,1.1,7.1), oma=c(3,0,1,1) ,las=0, pty="m", xpd=F, ps=10)
for(variable in 1:n.endo){
  for(i in 1:length(res.list)){
    res.mat[i, ] <- res.list[[i]][variable,]
    est.mat[i, ] <- est.list[[i]][variable,]
    
  }
  
  #Remove 0 values
  est.mat[which(est.mat == 0)] <- NA
  res.mat[which(res.mat == 0)] <- NA
  
  #Dimensions of matrix and min and max value of the estimates
  dim.mat<-dim(res.mat)
  min.val <- 0
  max.val <- 0
  for(i in 1:dim.mat[2]){
    min.val[i] <- min(est.mat[,i], na.rm=T)
    max.val[i] <- max(est.mat[,i], na.rm=T)
  }
  
  #Normalize
  for(i in 1:dim.mat[2]){
    estNorm.mat[,i] <- (est.mat[,i]-min(est.mat[,i], na.rm=T))/diff(range(est.mat[,i],na.rm=T))
    estNorm.mat[,i] <- 2*estNorm.mat[,i] - 1 + (i-1)*2
  }
  
  #Multiply with 2, since values range -1 to 1
  image(1:dim.mat[1], 2*0:(dim.mat[2]-1), res.mat, col=c(0,0), axes=F, ylab="")
  grid(60,30)
  image(1:dim.mat[1], 2*0:(dim.mat[2]-1), res.mat, col=c(col1[variable]), 
        axes=F, ylab=strDat[variable], xlab="", add=T)
  
  for(i in 1:dim.mat[2]){
    lines(1:dim.mat[1], estNorm.mat[,i])
  }
  
  #Y-axis label, indicate exogeneous variable with index
  axis(2, seq(0,2*(dim.mat[2]-1), length.out=length(strDat)), 1:length(strDat), cex.axis=1)
  mtext(dat.names[variable], side=2, line=1.9)
  #Right axis, shows range of variables
  axis(4, seq(0,2*(dim.mat[2]-1), length.out=length(strDat)),
       paste("[",round(min.val, digits=2),",",
             round(max.val, digits=2) ,"]"), cex.axis=1,  las=1)
  
  if(variable %% n.figures == 0){
    mtext( "Significant Variables of Sliding Models", outer = TRUE, line=-1 )
  }
  if(variable %% n.figures == 0){
    #Plot overall title for x-axis
    #title(xlab="Year", line=1, cex.lab=1.2)
    #Overall plot title
    mtext( "Value of Coefficients", outer = T, side=4, line=-0.4)
  }
  if(variable %% n.figures == 0){
    axis(1, seq(1,dim.mat[1],length.out=15), 
         format(dat$Date[c(seq(init.block,
                               init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
         cex.axis = 1)
    mtext( "Year", outer = T, side=1, line=2.1)
  }
  
  res.x[[variable]] <- res.mat
  est.x[[variable]] <- est.mat
}

# Make legend
par(mgp=c(2,0.8,0), mar=c(0,0,0,0),oma=c(1,1,1,1),las=0, pty="m", xpd=F)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft", c(paste(1:length(strDat),"=", strDat), "Coefficient"), 
       pt.bg=c(col1[1:(p*n.endo)],rep(NA,n.exo-n.endo), NA ,NA), ncol=4, 
       pch=c(rep(22,length(strDat)), NA), lty=c(rep(NA,length(strDat)), 1))

#### LOOK AT P VALUE. ####
#par(mfrow=c(1,1))
#par(mgp=c(2,0.8,0), mar=c(3,3,2,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
#plot(log(norm.test[,1]), ylab="Log of P-value", xlab="Date", 
#     main="P-value of JB Test", xaxt="n")
#points(log(norm.test[,2]), col=2)
#axis(1, seq(1,dim.mat[1],length.out=15), 
#     format(dat$Date[c(seq(init.block+1,
#                             init.block+dim.mat[1],length.out=14))],"%Y-%m"), 
#     cex.axis = 1)
#legend("bottomleft", legend=c("With Predictor Data", "Without Predictor Data"), text.col=c(1,2))

################################TABULATOR#####################################
# Tabulator sorts all models and look at unique combinations of each model
# Tab         <-  Tabulator matrix
# Tab.list    <-  list of tabulator matrix
# combos.list <-  List of combinations
# combo       <-  combinations of a given model
# name.combos <-  Combinations of names

tab.list <- vector(mode="list", n.endo)
Tab <- matrix(data=0,nrow=dim.mat[2],ncol=dim.mat[2])
combos.list <- vector(mode="list", n.endo)
for(variable in 1:n.endo){
  for (k in 1:dim.mat[1]){
    sum1 <- sum(res.x[[variable]][k,], na.rm=T)
    if (sum1 > 1){
      for (i in 1:sum1){
        indr <- unname(which(res.x[[variable]][k,]==1)[i])
        for (j in i:sum1){
          indc <- unname(which(res.x[[variable]][k,]==1)[j])
          Tab[indr,indc] = Tab[indr,indc]+1
          if (indc!=indr){
            Tab[indc,indr] = Tab[indc,indr]+1
          }
        }
      }
    } else if (sum1 == 1){
      ind <- unname(which(res.mat[k,]==1))
      Tab[ind,ind] <- Tab[ind,ind]+1
    }  
  }
  rownames(Tab) <-strDat
  colnames(Tab) <-strDat
  tab.list[variable]<-list(Tab)
  
  
  #Unique count
  unique.models <- unique(res.x[[variable]])
  combo <- matrix(data=0,nrow=nrow(unique.models),ncol=1)
  for(i in 1:nrow(res.x[[variable]])){
    match.idx <- which(apply(unique.models, 1, identical, 
                             res.x[[variable]][i, 1:(n.exo*p+2)]))
    combo[match.idx] <- combo[match.idx] + 1
  }
  
  #Attributes used in each model
  name.combos <- matrix(data="", nrow=nrow(unique.models), ncol=1)
  for(i in 1:nrow(unique.models)){
    name.combos[i] <- paste(strDat[which(unique.models[i,]==1)], collapse= " ")
  }
  rownames(combo) <- name.combos
  combo <- apply(apply(combo,2,sort),2,rev)
  combos.list[variable] <- list(apply(apply(combo,2, sort),2,rev))
}

###################DECIDES HOW MANY MODELS TO LOOK AT#########################
#Threshold for how many models to use
# models.thresh    <- threshold for how many times a model minimum has to be used.
# nb.models        <- number of models used to check skill scores and so on.

models.threshold <- round(n.models*0.05)
temp <- c()
for(i in 1:n.endo){
  temp[i] <- sum(combos.list[[i]] >= models.threshold)
}
nb.models <- max(temp)
if(nb.models < 6){
  nb.models <- 6
}
print(paste("Number of models to look at:", nb.models))


########Choose the nb.models that appears significant most of the times#######
#combined top models of sliding model 
# nb.models       <-  Choose number of models to look at
# unique.tot      <-  Total unique models
# combos.tot      <-  Total occurence of different models
# combined.models <-  Sorted combos tot.
# combined.best   <-  Top nb.models chosen from combined.models

temp <- do.call("rbind", combos.list)
unique.tot <- unique(rownames(temp))

#Remove models without any variables
if(sum(match(unique.tot, ""), na.rm = TRUE)==1){
  unique.tot <- unique.tot[-which(match(unique.tot, "")==1)]
}

#Find most used models throughout all variables
combined.models <- matrix(data=0,nrow=length(unique.tot),ncol=1)
for(i in 1:nrow(temp)){
  match.idx <- which(match(unique.tot, rownames(temp)[i])==1)
  combined.models[match.idx] <- combined.models[match.idx] + temp[i]
}
rownames(combined.models) <- unique.tot
combined.models <- apply(apply(combined.models,2,sort),2,rev)

combined.best <- matrix(data=0, nrow=nb.models, ncol=n.endo)
for (i in 1:nb.models) {
  combined.best[i,] <- rep(rownames(combined.models)[i],n.endo)
}
colnames(combined.best) <- names.endo
combined.best

####################Choose best model for each variable#######################
#top models for each variable of sliding model
# specific.best      <-  Best attribute for exogeneous variables

specific.best <- matrix(data=0, nrow=nb.models, ncol=n.endo)
for (i in 1:n.endo){
  temp <- rownames(combos.list[[i]])
  if(sum(match(temp, ""), na.rm = TRUE)==1){
    temp <- temp[-which(match(temp, "")==1)]
  }
  specific.best[,i] <- temp[1:nb.models]
}
colnames(specific.best) <- names.endo

specific.best

#######################PREDICT WITH CHOSEN MODELS#############################
## What model to use: Top for each attributes or combined top.
# chosenModel       <-  Choose between specific.best or combined.best
# pred.residuals    <-  save residuals in list for each chosen model
# pred.list.sd      <-  Save standard deviations in list
# pred.list         <-  Save predictions in list
# pred.cov          <-  Save covariance matrix
par(mfrow=c(5,1))
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)

chosenModel <- specific.best 

block2 <- init.block # Choose desired initial block length in months.
n.ahead<-1      # steps ahead for predictions.
pred.residuals <- vector(mode="list", nb.models)
pred.list <- vector(mode="list", nb.models)
count<-0;
pred.list.sd <- vector(mode="list", nb.models)

#Make predictions and standardize predictions for skill scores.
for(i in 1:nb.models){
  print(chosenModel[i,]) #Print the chosen model
  #count <- count+1
  resmat <- make.resmat(unname(chosenModel[i,])) #make restrict matrix
  end <- block2
  start <- 1
  pred    <- matrix(0, ncol=n.endo, nrow = n.models)
  pred.CI <- matrix(0, ncol=n.endo, nrow = n.models)
  pred.sd <- matrix(0, ncol=n.endo, nrow = n.models)
  
  while (end < N) {
    if(include.predictor){
      var.fit <-VAR(dat[(start:end),2:(n.endo+1)], 
                    exogen=dat[start:end,7:9], p=p, type="both")
    }else{
      var.fit <-VAR(dat[(start:end),2:(n.endo+1)], p=p, type="both")
    }
    var.fitRed <-restrict2(var.fit, method = "manual", 
                           resmat = resmat)
    
    # Forecasting for one attribute
    if(include.predictor){
      var.pred <- predict(var.fitRed, n.ahead = n.ahead, ci = 0.95, 
                          dumvar=matrix(as.numeric(dat[end,7:9]),
                                        nrow=n.ahead, ncol= n.exo-n.endo))
    }else{
      var.pred <- predict(var.fitRed, n.ahead = n.ahead, ci = 0.95, 
                          nrow=n.ahead, ncol= n.exo-n.endo)
    }
    
    pred.cov <- sig(var.fitRed, n.ahead=n.ahead)
    
    #SAVE PREDS
    for(j in 1:n.endo){
      pred[start,j] <- var.pred$fcst[[j]][1]
      pred.CI[start,j] <- var.pred$fcst[[j]][4]
      pred.sd[start,j] <- sqrt(diag(pred.cov[, , 1]))[j]
    }
    
    
    start <- start+n.ahead
    end <- end+n.ahead
  }
  pred.list.sd[[i]]<-pred.sd
  pred.list[[i]] <- pred
  
  #Plot the predictions with intervals
  for(j in 1:n.endo){
    plot(dat$Date[1:N], dat[1:N,(j+1)], type='l', 
         ylab = " Return", xlab="Date", 
         main=paste("Endogenous:", dat.names[j], ", Exogenous: ",chosenModel[i,j]))
    lines(dat$Date[(block2+1):N], pred[,j], col=2, lwd=1)
    lines(dat$Date[(block2+1):N], (pred[,j]-pred.CI[,j]), col=4, lwd=1)
    lines(dat$Date[(block2+1):N], (pred[,j]+pred.CI[,j]), col=4, lwd=1)
    
  }
  pred.residuals[[i]] <- dat[(block2+1):N,2:(n.endo+1)]-pred
}

#Make a legend:
par(mgp=c(2,0.8,0), mar=c(1,1,1,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft", legend=c("Observed","Prediction","95% Prediction Interval"),
       col=c(1,2,4),lty=c(1,1,1), lwd=c(1,2,2), ncol=3)

###################Look into variance of predictions##########################
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
par(mfrow=c(1,1))
for(i in 1:nb.models){
  plot(pred.list.sd[[i]][,1]^2, ylab="Variance of Prediction", xlab="Time", type='l', 
       main=paste("Model", i, "- Variance of predictions for each endogenous variable"), 
       col=col1[1], ylim=c(0,80), axes=F,xaxt="n")
  box(which = "plot", lty = "solid")
  for(j in 2:n.endo){
    lines(pred.list.sd[[i]][,j]^2, ylab="Variance of Prediction", 
          xlab="Date", type='l', col=col1[j])
  }
  axis(1, seq(1,dim.mat[1],length.out=15), 
       format(dat$Date[c(seq(init.block+1,
                             init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
       cex.axis = 1)
  #y-axes
  axis(2, seq(0,80, length.out=5), seq(0,80, length.out=5), cex.axis=1)
  legend("topleft", names.endo, col=col1[1:n.endo], lty=c(rep(1, n.endo)), ncol=5)
}

###############################CALCULATE MSE##################################

precision <- vector(mode='list', nb.models)
for(i in 1:nb.models){
  precision[[i]] <- pred.residuals[[i]]^2
}

#Reference MSE
ref <- matrix(0, nrow=n.models-1, ncol=n.endo)
for(i in 1:n.endo){
  ref[,i]<-(diff(dat[(block2+1):N,i+1], 1))^2
}

mse <- matrix(0, ncol=n.endo, nrow=nb.models+1)

# Plot MSE
par(mfrow=c(1,1))
par(mgp=c(2,0.8,0), mar=c(3.2,3,2,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
for(i in 1:nb.models){
  plot(precision[[i]][,1], type='l', lwd=1.2, ylab="MSE", xlab="Date", 
       main=paste("Model", i, "- MSE"), 
       axes=F, xaxt="n", ylim=c(0,round_any(max(precision[[i]]),100,f=ceiling)))
  box(which = "plot", lty = "solid")
  for(j in 1:n.endo){
    
    lines(precision[[i]][,j],col=col1[j])
    mse[i,j]<-mean(precision[[i]][,j])
  }
  #x-axis
  axis(1, seq(1,dim.mat[1],length.out=15), 
       format(dat$Date[c(seq(init.block+1,
                             init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
       cex.axis = 1)
  #y-axis
  axis(2, seq(0,1200, length.out=7), seq(0,1200, length.out=7), cex.axis=1)
  legend("topleft", legend=names.endo, col=col1[1:n.endo], lty=c(rep(1, n.endo)), ncol=1)
}

for(i in 1:n.endo){
  mse[nb.models+1,i] <- mean(ref[,i])
}

colnames(mse) <- names.endo
mse

########################## SORT MODELS INTO ORDER ############################
# Find order of attributes for each model
d<-dim(precision[[1]])

temp.list <- vector(mode='list', nb.models)
temp.mat <- matrix(0, ncol=nb.models, nrow=d[1])
order.list.mse <- vector(mode='list', n.endo)

for(i in 1:n.endo){
  temp<-rapply(precision, classes = 'matrix', how = 'list', f = function(x) x[, i, drop = FALSE])
  for(j in 1:nb.models){
    temp.mat[,j] <- temp[[j]]
  }
  temp.list[[i]] <- temp.mat
}
temp.mat <- 0

temp <- matrix(0, ncol=nb.models, nrow=d[1])
for(i in 1:n.endo){
  for(k in 1:d[1]){
    temp[k,]<-rank(temp.list[[i]][k,])
  }
  order.list.mse[[i]] <- temp
}

order.list.mse

### EXPLANATION:
#Each list contains a matrix in order.list, the matrix in order list is for each attribute
#from the first to the last attribute, ex. Cnsmr -> Manuf -> Hitec and so on
# Each column is a given model for that specific attribute, so
# order.list[[1]][,1], would be cnsmr ([[1]]) and [,1] would be the first model for cnsmr in
# "chosenmodel", in this case it would be "hlth, const" in matplot 1 is best and 5 is worst.

########################## Make the rank plots ###############################
par(mgp=c(2,0.8,0), mar=c(3.2,3,2,4),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
mean.order.mse <- matrix(0, ncol=n.endo, nrow=nb.models)
mean.se.mse <- matrix(0, ncol=n.endo, nrow=nb.models)
for(i in 1:n.endo){
  par(mfrow=c(4,2))
  for(j in 1:nb.models){
    matplot(order.list.mse[[i]][1:d[1],j], pch=1, cex = 0.7, col= col1[i],
            main=paste("Model", j, "- Rank MSE"),
            ylab="Rank", xlab="Date", axes=F)
    #main=paste("Endogenous:", dat.names[i], ", Exogenous: ",chosenModel[j,i])
    box(which = "plot", lty = "solid")
    temp <- c(1:nb.models, (nb.models+1)/2)
    axis(2, at = temp)
    sum1<-c()
    for(k in 1:nb.models){
      sum1[k] <- sum(order.list.mse[[i]][1:d[1],j]==k)
    }
    #x-axis
    axis(1, seq(1,dim.mat[1],length.out=15), 
         format(dat$Date[c(seq(init.block+1,
                               init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
         cex.axis = 1)
    #right-axis
    axis(4, at = 1:nb.models, labels=sum1, las=1)
    mtext("Rank Totals", side=4, line=2.3, cex=0.7)
    temp <- data.frame(y=order.list.mse[[i]][1:d[1],j], x=1:d[1])
    lo <- loess(y~x, data=temp, span = 0.1)
    lines(predict(lo))
    abline(h=mean(order.list.mse[[i]][1:d[1],j]), lty=2)
    mean.order.mse[j,i] <- mean(order.list.mse[[i]][1:d[1],j])
    mean.se.mse[j,i]<-mean(predict(lo,se=T)$se) 
  }
}

###############################LOOK INTO CRPS##################################
# skill.score   <-  Value of error for skill score

CRPS <- matrix(0, ncol=n.endo, nrow= nb.models)
crps.list <- vector(mode='list', nb.models)

#Method 1
for(i in 1:nb.models){ #for each model
  crps <- matrix(0, ncol=n.endo, nrow=nrow(pred))
  for(j in 1:n.endo){ #for each attribute
    mu <- pred.list[[i]][,j]
    sigma <- pred.list.sd[[i]][,j]
    obs<-dat[(block2+1):N,(1+j)]
    z <- (obs - mu)/sigma
    crps[,j] <- sigma * (z * (2 * pnorm(z, 0, 1) - 1) + 
                           2 * dnorm(z, 0, 1) - 1/sqrt(pi))
    CRPS[i,j]<-mean(crps[,j])
  }
  crps.list[[i]] <- crps
  
}
colnames(CRPS) <- names.endo

###############################PLOT CRPS##################################
par(mfrow=c(2,1))
par(mgp=c(2,0.8,0), mar=c(3.2,3,2,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
for(i in 2:nb.models){
  plot(crps.list[[i]][,1], type='l', lwd=1.2, ylab="CRPS", xlab="Date", 
       main=paste("Model", i, "- CRPS"), 
       axes=F, xaxt="n", ylim=c(0,round_any(max(crps.list[[i]]),1,f=ceiling)))
  box(which = "plot", lty = "solid")
  for(j in 1:n.endo){
    
    lines(crps.list[[i]][,j],col=col1[j])
  }
  #x-axis
  axis(1, seq(1,dim.mat[1],length.out=15), 
       format(dat$Date[c(seq(init.block+1,
                             init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
       cex.axis = 1)
  #y-axis
  axis(2, seq(0,40, length.out=5), seq(0,40, length.out=5), cex.axis=1)
  legend("topleft", legend=names.endo, col=col1[1:n.endo], lty=c(rep(1, n.endo)), ncol=2)
}

#################### FIND ORDER OF MODELS FOR CRPS ###########################
#find order of attributes for each model, do the same with MSE
d<-dim(crps)

temp.list <- vector(mode='list', nb.models)
temp.mat <- matrix(0, ncol=nb.models, nrow=d[1])
order.list <- vector(mode='list', n.endo)

for(i in 1:n.endo){
  temp<-rapply(crps.list, classes = 'matrix', how = 'list', 
               f = function(x) x[, i, drop = FALSE])
  for(j in 1:nb.models){
    temp.mat[,j] <- temp[[j]]
  }
  temp.list[[i]] <- temp.mat
}
temp.mat <- 0

temp <- matrix(0, ncol=nb.models, nrow=d[1])
for(i in 1:n.endo){
  for(k in 1:d[1]){
    temp[k,]<-rank(temp.list[[i]][k,])
  }
  order.list[[i]] <- temp
}

########################## Make the rank plots ###############################
par(mgp=c(2,0.8,0), mar=c(3.2,3,2,4),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
mean.order.crps <- matrix(0, ncol=n.endo, nrow=nb.models)
mean.se.crps <- matrix(0, ncol=n.endo, nrow=nb.models)
for(i in 1:n.endo){
  par(mfrow=c(4,2))
  for(j in 1:nb.models){
    matplot(order.list[[i]][1:d[1],j], pch=1, cex = 0.7, col= col1[i],
            main=paste("Model", j, "- Rank CRPS"),
            ylab="Rank", xlab="Date", axes=F)
    box(which = "plot", lty = "solid")
    #main=paste("Endogenous:", dat.names[i], ", Exogenous: ",chosenModel[j,i])
    temp <- c(1:nb.models, (nb.models+1)/2)
    axis(2, at = temp)
    sum1<-c()
    for(k in 1:nb.models){
      sum1[k] <- sum(order.list[[i]][1:d[1],j]==k)
    }
    axis(4, at = 1:nb.models, labels=sum1, las=1)
    #x-axis
    axis(1, seq(1,dim.mat[1],length.out=15), 
         format(dat$Date[c(seq(init.block+1,
                               init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
         cex.axis = 1)
    mtext("Rank Totals", side=4, line=2.3, cex=0.7)
    temp <- data.frame(y=order.list[[i]][1:d[1],j], x=1:d[1])
    lo <- loess(y~x, data=temp, span = 0.1)
    lines(predict(lo))
    abline(h=mean(order.list[[i]][1:d[1],j]), lty=2)
    mean.order.crps[j,i] <- mean(order.list[[i]][1:d[1],j])
    mean.se.crps[j,i]<-mean(predict(lo,se=T)$se)
  }
}

######################## CHOOSE THE TOP 3 MODELS #############################
#This will be based on several things.

top3 <- 3
rank.crps <- matrix(0, ncol=n.endo, nrow=top3)
rank.mse <- matrix(0, ncol=n.endo, nrow=top3)
rank.order.mse <- matrix(0, ncol=n.endo, nrow=top3)
rank.order.crps <- matrix(0, ncol=n.endo, nrow=top3)
rank.se.mse <- matrix(0, ncol=n.endo, nrow=top3)
rank.se.crps <- matrix(0, ncol=n.endo, nrow=top3)

top3.ranks.list <- vector(mode='list', n.endo)
for(i in 1:n.endo){
  temp <- rank(CRPS[,i], ties.method = "first")
  temp2 <- rank(mse[,i], ties.method = "first")
  temp3 <- rank(mean.order.mse[,i], ties.method = "first")
  temp4 <- rank(mean.order.crps[,i], ties.method = "first")
  temp5 <- rank(mean.se.mse[,i], ties.method = "first")
  temp6 <- rank(mean.se.crps[,i], ties.method = "first")
  for(j in 1:top3){
    rank.crps[j,i] <- match(j, temp)
    rank.mse[j,i] <- match(j, temp2)
    rank.order.mse[j,i] <- match(j, temp3)
    rank.order.crps[j,i] <- match(j, temp4)
    rank.se.mse[j,i] <- match(j, temp5)
    rank.se.crps[j,i] <- match(j, temp6)
  }
  top3.ranks.list[[i]] <- cbind( rank.mse[,i], rank.crps[,i], rank.order.mse[,i], 
                                 rank.order.crps[,i], rank.se.mse[,i], rank.se.crps[,i])
  colnames(top3.ranks.list[[i]]) <- c("MSE", "Mean CRPS", "Mean Rank MSE", 
                                      "Mean Rank CRPS", "SE Loess MSE", "SE Loess CRPS")
}

top3.ranks.list

##################### Automatically choose best model ########################

top3 <- 3
Best.Models.Index <- matrix(0, ncol=n.endo, nrow=top3)

for(i in 1:n.endo){
  models <- as.vector(top3.ranks.list[[i]])
  temp <-(as.data.frame(table(models)))
  rank.top3 <- temp[order(temp[,2], decreasing = T),]
  
  count <- 0
  while(count<3){
    sum.freq <- sum(rank.top3[,2]==rank.top3[count+1,2] )
    
    if((count+sum.freq)<=3){
      Best.Models.Index[(count+1):(count+sum.freq),i] <- as.numeric(as.vector(
        rank.top3$models[(count+1):(count+sum.freq)]))
      count <- count + sum.freq
      #index <- index+sum.freq
    } else {
      Best.Models.Index[(count+1):3,i] <- sort(as.numeric(as.vector(
        rank.top3$models[(count+1):(count+sum.freq)])))[1:(3-count)]
      count <- 4
    }
    
  }
}
colnames(Best.Models.Index) <- names.endo
Best.Models.Index
Best.Models <- matrix(0, ncol=n.endo, nrow=top3)
for(i in 1:n.endo){
  count <- 1
  for(j in Best.Models.Index[,i]){
    Best.Models[count,i] <- chosenModel[j,i]
    count <- count+1
  }
}

Best.Models

################### CONVERT BEST MODELS TO RESMAT DATA #######################

#Convert names and matrix into resmat:
names<-c(dat.names, "const", "trend")
chosen<-rep(0, length(names))
model.x.arls <- vector(mode="list", top3)
model.y.arls <- vector(mode="list", top3)

for(i in 1:top3){
  mod.x.mat <- matrix(0, ncol=length(strDat), nrow=n.endo)
  mod.y.mat <- matrix(0, ncol=length(strDat), nrow=n.endo)
  split.model<-strsplit(Best.Models[i,], " ")
  for(j in 1:n.endo){
    n<-length(split.model[[j]])
    for(k in 1:n){
      var.name<-split.model[[j]][k]
      if(var.name %in% strDat){
        mod.x.mat[j,which(strDat == var.name)] = 1
      }
      var.noInt <- gsub('[[:digit:]]+', '', var.name)
      if(var.noInt %in% names){
        mod.y.mat[j,which(names == var.noInt)] = 1
      }
    }
  }
  model.y.arls[[i]] <- mod.y.mat
  model.x.arls[[i]] <- mod.x.mat
}

#################################  ARLS  #####################################

#ADAPTIVE RECURSIVE LEAST SQUARES
# CREDITS GIVEN TO LASSE ENGBO CHRISTIANSEN.
# As the ARLS function was mainly implemented by him.
# small changes are made though.
# y is the data
# x is the exogeneous data for each variable chosen
# lambda is the forgetting

arls <- function(y, x, lambda){
  n<-dim(y)[1]
  np <- ncol(x)
  I <- diag(rep(1,np))
  theta1 <- matrix(0,ncol=1, nrow=np)
  store <- matrix(0, nrow=n, ncol=2+2*np)
  colnames(store) <- c("e",paste("theta1",1:np,sep=""),paste("x",1:np,sep=""),"lambda")
  store[1,] <- c(0,t(theta1), x[1,], lambda)
  P<-I*0.1
  for(i in 2:n){
    xt<-t(x[i,,drop=FALSE] )
    k <- P%*%xt / as.numeric((lambda + t(xt)%*%P%*%xt))
    e <- y[i] - t(xt)%*%theta1
    theta1 <- theta1 + k%*%e
    Ikx=I-k%*%t(xt)
    P=lambda^-1*Ikx%*%P;
    store[i,] <- c(e,t(theta1),t(xt),lambda)
  }
  return(store)
}

# Objective function to optimize lambda

obj.fun <- function(lambda, ynew, xnew){
  tmp <- arls(y=ynew, x=xnew, lambda=lambda)
  return(sum(tmp[-(5),1]^2))
}


######### PREPARE DATA #########
y<-as.matrix(dat[,c(-1)])                     # Save dat in y
x<-embed(y, dimension = 1 + p)[, -(1:n.exo)]  # Make a lagged matrix of y
var.const<-rep(1,N-p)                         # Make const variable
var.trend<-seq(p+1, length=N-p)               # Make trend variable
x<-cbind(x, var.const, var.trend)             # Save in x
lasty<-cbind(y, rep(1,N),seq(p+1, length=N))  # Contains all data including trend, const

###### Optimize lambda ######
lambda <- matrix(0, nrow=n.endo, ncol=top3)
for(i in 1:top3){
  for(j in 1:n.endo){
    x.choose<-x[,which(model.x.arls[[i]][j,]==1)]
    yend<-as.matrix(y[-c(1:p), j])
    opt<-optimise(obj.fun, interval = c(0.3,1), maximum=F, 
                  ynew=yend, xnew=as.matrix(x.choose))
    if(opt$minimum<0.99909){
      lambda[j,i]<-opt$minimum
    }else{
      lambda[j,i]<-1
    }
  }
}

rownames(lambda) <- c(1:n.endo)
colnames(lambda) <- c(1:top3)

if(choose.dataset!=48){
  frame()
  grob1<-tableGrob(round(lambda,digits=5))
  title <- textGrob("Forgetting Factor For Each Model",gp=gpar(fontsize=10))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(
    grob1, 
    heights = grobHeight(title) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    title, 
    1, 1, 1, ncol(table))
  grid.draw(table)
} else if(choose.dataset==48){
  layout(matrix(c(rep(1:12)), nrow = 4, ncol = 3, byrow = TRUE))
  
  for(i in 1:12){
    frame()
    grob1<-tableGrob(round(lambda[(4*(i-1)+1):(4*i),],digits=5))
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    title <- textGrob("",gp=gpar(fontsize=10))
    padding <- unit(5,"mm")
    table <- gtable_add_rows(
      grob1, 
      heights = grobHeight(title) + padding,
      pos = 0)
    table <- gtable_add_grob(
      table, 
      title, 
      1, 1, 1, ncol(table))
    grid.draw(table)
    popViewport(3)
  }
  mtext("Forgetting Factor for the 48 industries",3, outer=T, line=-1.5)
}


#memory
print(1/(1-lambda))

##################### LOOK AT LAMBDA FOR SHORT WINDOW ########################
# WARNING WILL TAKE A LONG TIME TO RUN
#Try to optimise lambda for a shorter window, to see a difference

short.window = F
if(short.window){
  par(mfrow=c(1,1))
  par(mgp=c(2,0.8,0), mar=c(3,3,2,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
  for(j in 1:top3){
    for(i in 1:n.endo){
      end <- init.block
      start <- 1
      x.choose<-x[,which(model.x.arls[[j]][i,]==1)]
      yend<-as.matrix(y[-c(1:p), i])
      lambda.slide <- c()
      
      while (end < N) {
        if(length(x.choose)<=936){
          opt<-optimise(obj.fun, interval = c(0.3,1), maximum=F, 
                        ynew=as.matrix(yend[start:end]), 
                        xnew=as.matrix(x.choose[start:end]))
        } else {
          opt<-optimise(obj.fun, interval = c(0.3,1), maximum=F, 
                        ynew=as.matrix(yend[start:end]), 
                        xnew=as.matrix(x.choose[start:end,]))
        }
        
        lambda.slide[start]<-opt$minimum
        start <- start + n.ahead
        end <- end + n.ahead
      }
      if(i==1){
        plot(lambda.slide, xaxt='n', col=col1[i], ylim=c(0.75,1), 
             main=paste("Forgetting Factor over time for Model", j), 
             xlab="Date", ylab="Fortgetting Factor - lambda")
        axis(1, seq(1,dim.mat[1],length.out=15), 
             format(dat$Date[c(seq(init.block+1,
                                   init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
             cex.axis = 1)
      }else{
        points(lambda.slide, col=col1[i])
      }
    }
  }
}

#################### LOOK AT CHANGES WITH ADAPTIVE MODE ######################

pred.adapt <- matrix(0, nrow=N-init.block, ncol=n.endo)
pred.adapt.list <- vector(mode='list', top3)
pred.res <- matrix(0, nrow=N-init.block, ncol=n.endo)

for(i in 1:top3){
  for(j in 1:n.endo){
    
    x.choose<-as.matrix(x[,which(model.x.arls[[i]][j,]==1)]) #Choose data structure for each variable
    yend<-as.matrix(y[-c(1:p), j]) #Prepare endogenous input variable
    n.theta <- length(which(model.x.arls[[i]][j,]==1))
    store1<-arls(y=yend, x=x.choose, lambda=lambda[j,i])
    count<-0
    
    
    for(k in init.block:(N-1)){
      count <-count+1
      pred.adapt[count, j]<- t(lasty[k,which(model.x.arls[[i]][j,]==1)])%*%store1[k-1,2:(1+n.theta)]
      
    }
  }
  pred.adapt.list[[i]] <- pred.adapt
}

d2<-dim(pred.adapt.list[[1]])
abs.error.list <- vector(mode="list", top3)
for(i in 1:top3){ 
  abs.error.list[[i]] <- (y[(init.block+1):N,1:n.endo]-pred.adapt.list[[i]])^2
}

############################# MAKE ORDER #####################################

temp.list <- vector(mode='list', top3)
temp.mat <- matrix(0, ncol=top3, nrow=(N-init.block))
order.list <- vector(mode='list', n.endo)

for(i in 1:n.endo){
  temp<-rapply(abs.error.list, classes = 'matrix', how = 'list', 
               f = function(x) x[, i, drop = FALSE])
  for(j in 1:top3){
    temp.mat[,j] <- temp[[j]]
  }
  temp.list[[i]] <- temp.mat
}
temp.mat <- 0

temp <- matrix(0, ncol=top3, nrow=d[1])

rank.arls <- vector(mode="list", n.endo)
for(i in 1:n.endo){
  rank.arls[[i]] <- t(apply(temp.list[[i]],1, rank))
  t1<-which(rank.arls[[i]]!=1,arr.ind = T)
  rank.arls[[i]][t1]<-0
}

############################# MAKE PLOT ######################################
par(mfrow=c(5,1))
par(mgp=c(2,0.8,0), mar=c(3,3,2,4),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
#Make ready for plot
tmp <-vector(mode="list", n.endo)
for(i in 1:n.endo){
  tmp[[i]] <- cbind(rank.arls[[i]][,1], 2*rank.arls[[i]][,2], 3*rank.arls[[i]][,3])
}

#look at number of changes
if(choose.dataset==5){
  layout(matrix(c(rep(1:5),0), nrow = 2, ncol = 3, byrow = TRUE))
}else if(choose.dataset==10){
  layout(matrix(c(rep(1:10),0,0), nrow = 3, ncol = 4, byrow = TRUE))
}else if(choose.dataset==17){
  layout(matrix(c(rep(1:17),0,0,0), nrow = 5, ncol = 4, byrow = TRUE))
}else if(choose.dataset==48){
  layout(matrix(c(rep(1:24)), nrow = 4, ncol = 6, byrow = TRUE))
}

for(i in 1:n.endo){
  t1 = as.vector(t(tmp[[i]]))
  t1 = t1[t1 != 0]
  trans = data.frame(from = t1[-length(t1)], to = t1[-1])
  print(xtable(with(trans, table(from, to))))
  
  frame()
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grob<-tableGrob(with(trans, table(from, to)))
  title <- textGrob(dat.names[i],gp=gpar(fontsize=10))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(
    grob, 
    heights = grobHeight(title) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    title, 
    1, 1, 1, ncol(table))
  grid.draw(table)
  popViewport(3)
}


par(ps=18)
par(mgp=c(2,0.8,0), mar=c(3.5,4,3,6.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
par(mfrow=c(5,1))

for(i in 1:n.endo){
  
  tmp[[i]][tmp[[i]]==0] <- NA
  matplot(tmp[[i]], main=paste("Ranked SSE for ARLS of:", 
                               dat.names[[i]], "(Forgetting Factor:", 
                               round(lambda[i,1], digits=4), "[1],", round(lambda[i,2], 
                                                                           digits=4),
                               "[2],", round(lambda[i,3], digits=4), "[3])"), 
          col=c(col1[i]), xlab="Date", ylab="Model", pch=3,
          yaxt='n', xaxt='n', ylim=c(0.90, 3.1))
  axis(2, at=1:top3, 1:top3)
  axis(1, seq(1,dim.mat[1],length.out=15), 
       format(dat$Date[c(seq(init.block+1,
                             init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
       cex.axis = 1)
  
  
  axis(4, c(1,2,3), round(c(mean(abs.error.list[[1]][which(rank.arls[[i]][,1]==1),i]),
                            mean(abs.error.list[[2]][which(rank.arls[[i]][,2]==1),i]),
                            mean(abs.error.list[[3]][which(rank.arls[[i]][,3]==1),i])), 
                          digits=2),las=1
  )
  mtext("MSE",4, outer=TRUE, line=-1.5)
}

par(mfrow=c(1,1))

############################## NOW FOR CRPS ##################################

pred.adapt <- matrix(0, nrow=N-init.block, ncol=n.endo)
pred.adapt.list <- vector(mode='list', top3)
pred.res <- matrix(0, nrow=N-init.block, ncol=n.endo)
var.1<-matrix(0, nrow=N-init.block, ncol=n.endo)
sd.adapt.list <- vector(mode='list', top3)
for(i in 1:top3){
  for(j in 1:n.endo){
    x.choose<-as.matrix(x[,which(model.x.arls[[i]][j,]==1)])
    yend<-as.matrix(y[-c(1:p), j])
    n.theta <- length(which(model.x.arls[[i]][j,]==1))
    store1<-arls(y=yend, x=x.choose, lambda=lambda[j,i])
    count<-0
    SIGMA<-solve(diag(1/(lambda[j,i]^((N-2):0))))
    
    for(k in init.block:(N-1)){
      count <-count+1
      pred.adapt[count, j]<- t(lasty[k,which(model.x.arls[[i]][j,]==1)])%*%store1[k-1,2:(1+n.theta)]
      df <- k-n.theta
      
      e<-c()
      
      
      #find all residuals for the model
      for(m in 1:(k-1)){
        e[m] <- lasty[m+1, n.endo]-store1[k,2:(1+n.theta)]%*%x.choose[m,]
        
      }
      
      F<-as.matrix(t(store1[,(2+n.theta):(2*n.theta+1)]))%*%SIGMA%*%
        as.matrix(store1[,(2+n.theta):(2*n.theta+1)])
      
      var.1[count,j] <- (crossprod(e)/df)*(1+t(x.choose[k,])%*%solve(F)%*%x.choose[k,])
    }
    
    
  }
  pred.adapt.list[[i]] <- pred.adapt
  sd.adapt.list[[i]] <- sqrt(var.1)
}


CRPS2 <- matrix(0, ncol=n.endo, nrow= top3)
crps.list2 <- vector(mode='list', top3)

for(i in 1:top3){ 
  crps <- matrix(0, ncol=n.endo, nrow=nrow(pred))
  for(j in 1:n.endo){
    mu <- pred.adapt.list[[i]][,j]
    sigma <- sd.adapt.list[[i]][,j]
    obs<-dat[(block2+1):N,(1+j)]
    z <- (obs - mu)/sigma
    crps[,j] <- sigma * (z * (2 * pnorm(z, 0, 1) - 1) + 2 * dnorm(z, 0, 1) - 1/sqrt(pi))
    CRPS2[i,j]<-mean(crps[,j])
  }
  crps.list2[[i]] <- crps
  
}
colnames(CRPS2) <- names.endo


################################# MAKE PLOTS #################################

temp.list <- vector(mode='list', top3)
temp.mat <- matrix(0, ncol=top3, nrow=(N-init.block))
order.list <- vector(mode='list', n.endo)

for(i in 1:n.endo){
  temp<-rapply(crps.list2, classes = 'matrix', how = 'list', 
               f = function(x) x[, i, drop = FALSE])
  for(j in 1:top3){
    temp.mat[,j] <- temp[[j]]
  }
  temp.list[[i]] <- temp.mat
}
temp.mat <- 0

temp <- matrix(0, ncol=top3, nrow=d[1])

rank.crps.arls <- vector(mode="list", n.endo)
for(i in 1:n.endo){
  rank.crps.arls[[i]] <- t(apply(temp.list[[i]],1, rank))
  t1<-which(rank.crps.arls[[i]]!=1,arr.ind = T)
  rank.crps.arls[[i]][t1]<-0
}

############################# MAKE PLOT ######################################
par(mfrow=c(5,1))

#Make ready for plot
tmp <-vector(mode="list", n.endo)
for(i in 1:n.endo){
  tmp[[i]] <- cbind(rank.crps.arls[[i]][,1], 2*rank.crps.arls[[i]][,2],
                    3*rank.crps.arls[[i]][,3])
}

#look at number of changes
if(choose.dataset==5){
  layout(matrix(c(rep(1:5),0), nrow = 2, ncol = 3, byrow = TRUE))
}else if(choose.dataset==10){
  layout(matrix(c(rep(1:10),0,0), nrow = 3, ncol = 4, byrow = TRUE))
}else if(choose.dataset==17){
  layout(matrix(c(rep(1:17),0,0,0), nrow = 5, ncol = 4, byrow = TRUE))
}else if(choose.dataset==48){
  layout(matrix(c(rep(1:24)), nrow = 4, ncol = 6, byrow = TRUE))
}

for(i in 1:n.endo){
  t1 = as.vector(t(tmp[[i]]))
  t1 = t1[t1 != 0]
  trans = data.frame(from = t1[-length(t1)], to = t1[-1])
  print(xtable(with(trans, table(from, to))))
  
  frame()
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grob<-tableGrob(with(trans, table(from, to)))
  title <- textGrob(dat.names[i],gp=gpar(fontsize=10))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(
    grob, 
    heights = grobHeight(title) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    title, 
    1, 1, 1, ncol(table))
  grid.draw(table)
  popViewport(3)
}


par(ps=18)
par(mgp=c(2,0.8,0), mar=c(3.5,4,3,6.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
par(mfrow=c(5,1))
for(i in 1:n.endo){
  
  tmp[[i]][tmp[[i]]==0] <- NA
  matplot(tmp[[i]], main=paste("Ranked CRPS for ARLS of:", 
                               dat.names[[i]], "(Forgetting Factor:", 
                               round(lambda[i,1], digits=4), "[1],", 
                               round(lambda[i,2], digits=4),
                               "[2],", round(lambda[i,3], digits=4), "[3])"), 
          col=col1[i], xlab="Date", ylab="Model", pch=3,
          yaxt='n', xaxt='n', ylim=c(0.9, 3.1))
  axis(2, at=1:top3, 1:top3)
  axis(1, seq(1,dim.mat[1],length.out=15), 
       format(dat$Date[c(seq(init.block+1,
                             init.block+dim.mat[1],length.out=15))],"%Y-%m"), 
       cex.axis = 1)
  
  axis(4, c(1,2,3), round(c(mean(crps.list2[[1]][which(rank.crps.arls[[i]][,1]==1),i]),
                            mean(crps.list2[[2]][which(rank.crps.arls[[i]][,2]==1),i]),
                            mean(crps.list2[[3]][which(rank.crps.arls[[i]][,3]==1),i])), 
                          digits=2)
       ,las=1)
  mtext("Mean CRPS",4, outer=TRUE, line=-1.5)
}
par(mfrow=c(1,1))

########################SMALL RESIDUAL ANALYSIS SECTION##########################
sign.test<-function(x){
  res <- x
  (N.sign.changes <- sum( res[-(1)] * 
                            res[-length(res)]<0 ))
  print(binom.test(N.sign.changes, 
                   (length(res))-1))
}

if(choose.dataset==5){
  var.final <- vars::VAR(dat[,2:(n.endo+1)], type="both", p=1)
  
  final.resmat<-model.x.arls[[2]]
  final.resmat[3,]<-model.x.arls[[3]][3,]
  
  var.final1 <- restrict2(var.final, method="manual", resmat=final.resmat)
  
  
  par(mgp=c(2,0.8,0), mar=c(3.5,4,3,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=FALSE, ps=12)
  par(mfrow=c(1,1))
  
  plot(residuals(var.final1$varresult[[1]]), ylab="Residual", xlab="Time", xaxt='n', 
       main="Residuals of Model F5", type='l',col=col1[1], ylim=c(-25,28))
  for(i in 2:n.endo){
    lines(residuals(var.final1$varresult[[i]]),col=col1[i])
  }
  abline(mean(residuals(var.final1)),0, col=1)
  axis(1, c(seq(1,N,length.out=12)), 
       format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  legend("topright", legend=names.endo, col=col1, ncol=choose.dataset/5, lty=1, cex=1.3)
  
  par(ps=12)
  par(mgp=c(2,0.8,0), mar=c(3.5,4,3,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
  par(mfrow=c(2,3))
  for(i in 1:n.endo){
    qqPlot(residuals(var.final1$varresult[[i]]), ylab="Residual", main=dat.names[i])
  }
  
  
  par(mfrow=c(1,1), mar=c(3.5,3,2,1), mgp=c(2,0.8,0))
  lag.cf <- 30
  plot(acf(residuals(var.final1$varresult[[1]]),plot=FALSE, 
           lag.max=lag.cf,na.action=na.pass),col=col1[1],
       type="l", max.mfrow=1, ylim=c(-0.15,1), main=" ")
  title("ACF of the Residuals for Model F5", line=0.5)
  points(0:lag.cf,acf(residuals(var.final1$varresult[[1]]),plot=FALSE, 
                      lag.max=lag.cf,na.action=na.pass)$acf, 
         col=col1[1], pch=1, lwd=2)
  for(i in 2:n.endo){
    lines(0:lag.cf,acf(residuals(var.final1$varresult[[i]]),plot=FALSE, 
                       lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i])
    points(0:lag.cf,acf(residuals(var.final1$varresult[[i]]),plot=FALSE, 
                        lag.max=lag.cf,na.action=na.pass)$acf, col=col1[i], 
           pch=1, lwd=2)
  }
  legend("topright", legend=names.endo, col=col1, 
         pch=1, ncol=choose.dataset/5, lty=1, cex=1.3)
  
  for(i in 1:n.endo){
    sign.test(residuals(var.final1$varresult[[i]]))
  }
  
  par(mfrow=c(1,1))
  for(i in 1:n.endo){
    yend<-as.matrix(y[-c(1:p), i])
    rownames(yend)<-NULL
    x.test<-x[,which(final.resmat[i,]==1)]
    fit <- lm(yend~(-1+x.test))
    if(i==1){
      plot(1:N, type="n", ylim=c(0,0.12),xaxt='n', xlab="Date", 
           ylab="Cook's Distance", main="Cook's Distance for Model F5")
    }
    lines(cooks.distance(fit), col=col1[i])
  } 
  axis(1, c(seq(1,N,length.out=12)), 
       format(dat$Date[c(seq(1,N,length.out=12))],"%Y-%m"), 
       cex.axis = 1)
  legend("topleft", legend=names.endo, col=col1, ncol=choose.dataset/5, lty=1, cex=1.3)
  
  par(mfrow=c(2,5))
  for(i in 1:n.endo){
    yend<-as.matrix(y[-c(1:p), i])
    rownames(yend)<-NULL
    x.test<-x[,which(final.resmat[i,]==1)]
    fit <- lm(yend~(-1+x.test))
    plot(fit, which=3,caption=NULL, main=dat.names[i], col=col1[i])
    plot(fit, which=1,caption=NULL, main=dat.names[i], col=col1[i])
  }
}

