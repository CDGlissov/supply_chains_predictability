rm(list=ls())
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
par(mfrow=c(1,1))

####################################Packages##################################
library(readr)
library(car)
library(reshape2)
library(MASS)
library(vars)
library(readxl)
library(tseries)
library(xtable)
library(plyr)
library(verification)
library(corrplot)
library(Hmisc)

#################################Set-up#######################################
#setwd("C:/Users/Christian_2/Dropbox/Bachelor/R_code")
setwd("C:/Users/Christian/Dropbox/Bachelor/R_code")
#setwd("C:/Users/Fanny/Dropbox/DTU/6.Var_2018-Sjatte_semestern/Bachelor/R_code")
source("functions/VARextension.R")

include.predictor <- T
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

####PREPARE PREDICTOR DATA####
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
  
  sum(is.na(PredData16))
  #Remove last NA observation
  PredData16<-PredData16[-nrow(PredData16),]
  DatePred <- PredData16$Date
  PredData16 <- PredData16[, c(2,3,12)]
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
  temp2<-PredData16$Return[2:length(PredData16$Return)]
  temp3<-PredData16$infl[2:length(PredData16$infl)]
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
N.att<-5
###########################################################################
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



###########################Get variance################################
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

############################# SLIDING MODEL  #################################
# init.block  <-  Initialize block length
# n.ahead     <-  Prediction ahead
# p           <-  Order of model
# threshold   <-  Threshold of significant parameter
# res.x       <-  List of residuals 
# est.x       <-  List of estimates
# estNorm.mat <-  Normalized estimates
# strDat      <-  Save names of each attribute with lag.
# dim.mat     <-  Dimensions of residual matrix

nb.blocks <- 20
error.crps.list.pred <- vector(mode = 'list', nb.blocks-1)
error.mse.list.pred <- vector(mode = 'list', nb.blocks-1)

for(year.block in 2:nb.blocks){
 
  init.block <- year.block*12
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
  
  ##############################################################################
  
  while (end < N) {
    if(include.predictor){
      mod <-VAR(dat[(start:end),2:(n.endo+1)], exogen=dat[start:end,(n.endo+2):(n.exo+1)], p=p, type="both")
    }else{
      mod <-VAR(dat[(start:end),2:(n.endo+1)], p=p, type="both")
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
    
    start <- start+n.ahead
    end <- end+n.ahead
  }
  
  #loop over each RESPONSE variable
  for(variable in 1:n.endo){
    for(i in 1:length(res.list)){
      res.mat[i, ] <- res.list[[i]][variable,]
      est.mat[i, ] <- est.list[[i]][variable,]
      
    }
    dim.mat<-dim(res.mat)
    #Remove 0 values
    est.mat[which(est.mat == 0)] <- NA
    res.mat[which(res.mat == 0)] <- NA
    
    
    
    
    res.x[[variable]] <- res.mat
    est.x[[variable]] <- est.mat
  }
  
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

  nb.models <- 4
  
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
  
  par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
  
  chosenModel <- specific.best 
  
  block2 <- init.block # Choose desired initial block length in months.
  n.ahead<-1      # steps ahead for predictions.
  pred.residuals <- vector(mode="list", nb.models)
  pred.list <- vector(mode="list", nb.models)
  count<-0;
  pred.list.sd <- vector(mode="list", nb.models)
  pred.standardized <-  vector(mode="list", nb.models)
  
  #Make predictions and standardize predictions for skill scores.
  for(i in 1:nb.models){
    print(chosenModel[i,]) #Print the chosen model
    #count <- count+1
    resmat <- make.resmat(unname(chosenModel[i,])) #make restrict matrix
    end <- block2
    start <- 1
    pred    <- matrix(0, ncol=n.endo, nrow = n.models)
    pred.sd <- matrix(0, ncol=n.endo, nrow = n.models)
    
    while (end < N) {
      var.fit <-VAR(dat[(start:end),2:(n.endo+1)], 
                    exogen=dat[start:end,7:9], p=p, type="both")
      var.fitRed <-restrict2(var.fit, method = "manual", 
                             resmat = resmat)
      
      # Forecasting for one attribute
      var.pred <- predict(var.fitRed, n.ahead = n.ahead, ci = 0.95, 
                          dumvar=matrix(as.numeric(dat[end,7:9]),
                                        nrow=n.ahead, ncol= n.exo-n.endo))
      
      pred.cov <- sig(var.fitRed, n.ahead=n.ahead)
      
      #SAVE PREDS
      for(j in 1:n.endo){
        pred[start,j] <- var.pred$fcst[[j]][1]
        pred.sd[start,j] <- sqrt(diag(pred.cov[, , 1]))[j]
      }
      
      start <- start+n.ahead
      end <- end+n.ahead
    }
    pred.list.sd[[i]]<-pred.sd
    pred.list[[i]] <- pred
    pred.residuals[[i]] <- dat[(block2+1):N,2:(n.endo+1)]-pred

  }
  
  
  #####################SKILL SCORES############################
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
      crps[,j] <- sigma * (z * (2 * pnorm(z, 0, 1) - 1) + 2 * dnorm(z, 0, 1) - 1/sqrt(pi))
      CRPS[i,j]<-mean(crps[,j])
    }
    crps.list[[i]] <- crps
    
  }
  colnames(CRPS) <- names.endo
  
  
  ######################MSE########################
  precision <- vector(mode='list', nb.models)
  for(i in 1:nb.models){
    precision[[i]] <- pred.residuals[[i]]^2
  }
  
  ref <- matrix(0, nrow=N-1, ncol=n.endo)
  for(i in 1:n.endo){
    ref[,i]<-(diff(dat[,i+1], 1))^2
  }
  
  mse <- matrix(0, ncol=n.endo, nrow=nb.models)
  
  for(i in 1:nb.models){
    for(j in 1:n.endo){
      mse[i,j]<-mean(precision[[i]][,j])
    }
  }
  print(year.block)
  
  error.crps.list.pred[[year.block-1]] <- CRPS
  error.mse.list.pred[[year.block-1]] <- mse
}


########################### PLOT THE CRPS AND MSE CROSSVAL ################################

par(mfrow=c(1,1), ps=12)
cross.means2 <- rowMeans(t(sapply(rapply(error.mse.list.pred, classes = 'matrix', how = 'list', f = function(x) x[, 1:5, drop = FALSE]),cbind)))
index.test2 <- rep(0,length(cross.means2))
index.test2[which.min(cross.means2)] <- cross.means2[which.min(cross.means2)]
barplot(cross.means2[1:18], col=1,ylim=c(0,28), add=F, names.arg=c(2:19), xlab="Years", ylab="MSE", main="Mean of the Models MSE")

error.mse.list[[20]]<-NULL
cross.means <- rowMeans(t(sapply(rapply(error.mse.list, classes = 'matrix', how = 'list', f = function(x) x[, 1:5, drop = FALSE]),cbind)))
index.test <- rep(0,length(cross.means))
index.test[which.min(cross.means)] <- cross.means[which.min(cross.means)]
barplot(cross.means[1:18],add=T, col=rgb(0.745,0.745,0.745, alpha=0.7))

#barplot(c(rep(NA, 12),cross.means2[13:18]),add=T, col=1)

#barplot(index.test2[1:18], add=T, col="red")
barplot(index.test[1:18], add=T, col="red")
text(9.12, 0.7, paste(round(cross.means[which.min(cross.means)],digits=2)))
box(which = "plot", lty = "solid")
legend("topright", c("With Predictor Data", "Without Predictor Data"),fill=c("black", rgb(0.745,0.745,0.745, alpha=0.7)))


par(mfrow=c(1,1))
cross.means2 <- rowMeans(t(sapply(rapply(error.crps.list.pred, classes = 'matrix', how = 'list', f = function(x) x[, 1:5, drop = FALSE]),cbind)))
index.test2 <- rep(0,length(cross.means2))
index.test2[which.min(cross.means2)] <- cross.means2[which.min(cross.means2)]
barplot(cross.means2[1:18], col=1,ylim=c(0,3.5), names.arg=c(2:19), xlab="Years", ylab="CRPS", main="Mean of the Models CRPS")

error.crps.list[[20]]<-NULL
cross.means <- rowMeans(t(sapply(rapply(error.crps.list, classes = 'matrix', how = 'list', f = function(x) x[, 1:5, drop = FALSE]),cbind)))
index.test <- rep(0,length(cross.means))
index.test[which.min(cross.means)] <- cross.means[which.min(cross.means)]
barplot(cross.means[1:18],add=T, col=rgb(0.745,0.745,0.745, alpha=0.7))

barplot(index.test[1:18], add=T, col="red")
text(9.12, 0.1, paste(round(cross.means[which.min(cross.means)],digits=2)))
#barplot(index.test2[1:18], add=T, col="purple")
box(which = "plot", lty = "solid")
legend("topright", c("With Predictor Data", "Without Predictor Data"),fill=c("black", rgb(0.745,0.745,0.745, alpha=0.7)))




