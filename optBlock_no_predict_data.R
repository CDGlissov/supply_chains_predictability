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
#################################Set-up#######################################
#setwd("C:/Users/Christian_2/Dropbox/Bachelor/R_code")
setwd("C:/Users/Christian/Dropbox/Bachelor/R_code")
#setwd("C:/Users/Fanny/Dropbox/DTU/6.Var_2018-Sjatte_semestern/Bachelor/R_code")
#setwd("E:/Dropbox/Fanny och Peter/Fanny Bachelor")
#setwd("C:/Users/Fanny/Dropbox/Fanny Bachelor")

source("./functions/VARextension.R")

####PREPARE INDUSTRY DATA####
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
dat.names <- names(dat[2:N.col])
N.att <- length(dat.names)


###############FUNCTIONS##################
make.resmat <- function(attribute.names){
  resmat <- matrix(data=0, nrow=N.att, ncol=p*N.att+2)
  count <- 1
  for (model in attribute.names) {
    temp <- strsplit(model, " ")[[1]]
    ind.names <- match(temp, strDat)
    resmat[count, ind.names] <- 1
    count <- count+1
  }
  return(resmat)
}

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
##############################################################################
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
error.crps.list <- vector(mode = 'list', nb.blocks)
error.mse.list <- vector(mode = 'list', nb.blocks)

for(year.block in 1:nb.blocks){
  print(year.block)
  init.block <- year.block*12
  n.ahead<-1 
  p<-1
  n.models <- N - init.block
  threshold <- 1.96
  res.list<-vector(mode="list", n.models)
  est.list<-vector(mode="list", n.models)
  res.mat <- matrix(0, nrow=n.models, p*N.att+2)
  est.mat <- matrix(0, nrow=n.models, p*N.att+2)
  estNorm.mat <- matrix(0, nrow=n.models, p*N.att+2)
  end <- init.block
  start <- 1
  res.x <- vector(mode="list", N.att)
  est.x <- vector(mode="list", N.att)
  
  ##############################Prepare strDat##################################
  strDat <- c()
  k<-0
  for(i in 1:p){
    for(j in 1:N.att){
      k<-k+1
      strDat[k] <- paste(dat.names[j], i,sep="")
    }
  }
  strDat[k+1] <- "Const"
  strDat[k+2] <- "Trend"
  ##############################################################################
  
  while (end < N) {
    mod <-VAR(dat[(start:end),2:N.col], p=p, type="both")
    mod.Red <-restrict2(mod, method="ser", 
                        thresh=threshold, m=NULL)
    
    for(variable in 1:N.att){
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
  
  #make plot ready:
  par(mfrow=c(5,1))
  par(mgp=c(2,0.8,0), mar=c(0,3,1.1,7.1), oma=c(3,0,1,1) ,las=0, pty="m", xpd=F)
  
  #loop over each RESPONSE variable
  for(variable in 1:N.att){
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
    
    res.x[[variable]] <- res.mat
    est.x[[variable]] <- est.mat
  }
  
  
  ################################################################
  # Tab         <-  Tabulator matrix
  # Tab.list    <-  list of tabulator matrix
  # combos.list <-  List of combinations
  # combo       <-  combinations of a given model
  # name.combos <-  Combinations of names
  
  tab.list <- vector(mode="list", N.att)
  Tab <- matrix(data=0,nrow=dim.mat[2],ncol=dim.mat[2])
  combos.list <- vector(mode="list", N.att)
  for(variable in 1:N.att){
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
                               res.x[[variable]][i, 1:(N.att*p+2)]))
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
  
  #########################################################
  #top models for each attribute of sliding model
  # specific.best      <-  Best attribute for exogeneous variables
  
  specific.best <- matrix(data=0, nrow=nb.models, ncol=N.att)
  for (i in 1:N.att){
    temp <- rownames(combos.list[[i]])
    if(sum(match(temp, ""), na.rm = TRUE)==1){
      temp <- temp[-which(match(temp, "")==1)]
    }
    specific.best[,i] <- temp[1:nb.models]
  }
  colnames(specific.best) <- dat.names
  
  
  
  #########################################################
  ## What model to use: Top for each attributes or combined top.
  # chosenModel       <-  Choose between specific.best or combined.best
  # pred.residuals    <-  save residuals in list for each chosen model
  # pred.list.sd      <-  Save standard deviations in list
  # pred.list         <-  Save predictions in list
  # pred.cov          <-  Save covariance matrix
  
  par(mfrow=c(1,1))
  
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
    pred    <- matrix(0, ncol=N.att, nrow = n.models)
    pred.CI <- matrix(0, ncol=N.att, nrow = n.models)
    pred.sd <- matrix(0, ncol=N.att, nrow = n.models)
    
    while (end < N) {
      var.fit <-VAR(dat[(start:end),2:N.col], p=p, type="both")
      var.fitRed <-restrict2(var.fit, method = "manual", 
                             resmat = resmat)
      
      # Forecasting for one attribute
      var.pred <- predict(var.fitRed, n.ahead = n.ahead, ci = 0.95)
      pred.cov <- sig(var.fitRed, n.ahead=n.ahead)
      
      #SAVE PREDS
      for(j in 1:N.att){
        pred[start,j] <- var.pred$fcst[[j]][1]
        pred.sd[start,j] <- sqrt(diag(pred.cov[, , 1]))[j]
      }
      
      
      start <- start+n.ahead
      end <- end+n.ahead
    }
    pred.list.sd[[i]]<-pred.sd
    pred.list[[i]] <- pred
    pred.residuals[[i]] <- dat[(block2+1):N,2:(N.att+1)]-pred
  }
  
  
  #####################SKILL SCORES############################
  # skill.score   <-  Value of error for skill score
  
  CRPS <- matrix(0, ncol=N.att, nrow= nb.models)
  crps.list <- vector(mode='list', nb.models)
  
  #Method 1
  for(i in 1:nb.models){ #for each model
    crps <- matrix(0, ncol=N.att, nrow=nrow(pred))
    for(j in 1:N.att){ #for each attribute
      mu <- pred.list[[i]][,j]
      sigma <- pred.list.sd[[i]][,j]
      obs<-dat[(block2+1):N,(1+j)]
      z <- (obs - mu)/sigma
      crps[,j] <- sigma * (z * (2 * pnorm(z, 0, 1) - 1) + 2 * dnorm(z, 0, 1) - 1/sqrt(pi))
      CRPS[i,j]<-mean(crps[,j])
    }
    crps.list[[i]] <- crps
    
  }
  
  precision <- vector(mode='list', nb.models)
  for(i in 1:nb.models){
    precision[[i]] <- pred.residuals[[i]]^2
  }
  
  mse <- matrix(0, ncol=N.att, nrow=nb.models)
  
  for(i in 1:nb.models){
    for(j in 1:N.att){
      mse[i,j]<-mean(precision[[i]][,j])
    }
  }
  
  error.crps.list[[year.block]] <- CRPS
  error.mse.list[[year.block]] <- mse
}


## For the saved enviroments, start here

#error.crps.list <- error.crps.list[-1] #10 and 17
#error.mse.list <- error.mse.list[-1] #10 and 17
#
#error.crps.list <- error.crps.list[-c(1:4)] #48
#error.mse.list <- error.mse.list[-c(1:4)] #48

#choose appropriate h
h<- 0

par(mfrow=c(1,1))
par(mgp=c(2,0.8,0), mar=c(3.2,3,2,0.5),oma=c(0,0,0,0),las=0, pty="m", xpd=F)

cross.means <- rowMeans(t(sapply(rapply(error.mse.list, classes = 'matrix', 
                                        how = 'list', f = function(x) x[, 1:N.att, drop = FALSE]),cbind)))
cross.means.CRPS <- rowMeans(t(sapply(rapply(error.crps.list, classes = 'matrix', 
                                             how = 'list', f = function(x) x[, 1:N.att, drop = FALSE]),cbind)))

#Have to manually do some adjusting, not recommended
for(i in 1:N.att){
  temp<-rapply(error.crps.list, classes = 'matrix', how = 'list', f = function(x) x[, i, drop = FALSE])
  if(i == 1){
    min.y<-min(unlist(error.crps.list))
    max.y <- max(unlist(error.crps.list))
    matplot(t(sapply(temp, cbind)),lty=rep(1, nb.models), type='b', col=col1[i], 
            main="CRPS over different block lengths", 
            ylim=c(min.y-min.y*0.2,max.y ),
            xlim=c(1, nb.blocks-h), ylab="CRPS", xlab="Block length in years", xaxt="n", yaxt="n")
    scale.min <- norm.scale(cross.means.CRPS, min.y-min.y*0.2, min.y-min.y*0.05)
    
    x.low <- rep(min.y-(min.y*0.2+0.2),nb.blocks) #gotta adjust here for bars to hit x axis
    min.low <- x.low
    min.low[which.min(scale.min)] <- scale.min[which.min(scale.min)] 
    rect(seq(0.5, nb.blocks-0.5, by=1), x.low, seq(1.5, nb.blocks+0.5, by=1), 
         scale.min, col=rgb(0.5,0.5,0.5,alpha=0.2))
    rect(seq(0.5, nb.blocks-0.5, by=1), x.low, seq(1.5, nb.blocks+0.5, by=1), 
         min.low, col=rgb(1,0,0,alpha=0.5))
    abline(scale.min[which.max(scale.min)],0)
  }
  axis(1, seq(1,(nb.blocks-h), length.out=(nb.blocks-h)), (1+h):(nb.blocks), cex.axis=1)
  axis(2, seq(min.y,max.y, length.out=8),digits=2, round(seq(min.y,max.y, 
                                                             length.out=8), digits=2), cex.axis=1)
  #axis(1, seq(1,(nb.blocks-1), length.out=(nb.blocks-1)), 2:nb.blocks, cex.axis=1)
  #axis(1, seq(1,nb.blocks, length.out=nb.blocks), 1:nb.blocks, cex.axis=1)
  matlines(t(sapply(temp, cbind)),lty=rep(1, nb.models), type='b', col=col1[i])
}
legend("topright",c(dat.names, "Model Number, 1-4"), text.col=c(col1[1:N.att],1), ncol=2)

#Have to manually do some adjusting, not recommended
for(i in 1:N.att){
  temp<-rapply(error.mse.list, classes = 'matrix', how = 'list', f = function(x) x[, i, drop = FALSE])
  if(i == 1){
    min.y<-min(unlist(error.mse.list))
    max.y <- max(unlist(error.mse.list))
    matplot(t(sapply(temp, cbind)),lty=rep(1, nb.models), type='b', col=col1[i], 
            main="MSE over different block lengths", 
            ylim=c(min.y-min.y*0.2,max.y ),
            xlim=c(1, nb.blocks-h), ylab="MSE", xlab="Block length in years", xaxt="n", yaxt="n")
    scale.min <- norm.scale(cross.means, min.y-min.y*0.2, min.y-min.y*0.1)
    
    x.low <- rep(min.y-(min.y*0.2+2),nb.blocks) #adjust
    min.low <- x.low
    min.low[which.min(scale.min)] <- scale.min[which.min(scale.min)] 
    rect(seq(0.5, nb.blocks-0.5, by=1), x.low, seq(1.5, nb.blocks+0.5, by=1), 
         scale.min, col=rgb(0.5,0.5,0.5,alpha=0.2))
    rect(seq(0.5, nb.blocks-0.5, by=1), x.low, seq(1.5, nb.blocks+0.5, by=1), 
         min.low, col=rgb(1,0,0,alpha=0.5))
    abline(scale.min[which.max(scale.min)],0)
  }
  axis(1, seq(1,(nb.blocks-h), length.out=(nb.blocks-h)), (1+h):(nb.blocks), cex.axis=1)
  axis(2, round(seq(min.y,max.y, length.out=8)), round(seq(min.y,max.y, length.out=8)), cex.axis=1)
  #axis(1, seq(1,(nb.blocks-1), length.out=(nb.blocks-1)), 2:nb.blocks, cex.axis=1)
  #axis(1, seq(1,nb.blocks, length.out=nb.blocks), 1:nb.blocks, cex.axis=1)
  matlines(t(sapply(temp, cbind)),lty=rep(1, nb.models), type='b', col=col1[i])
  
  #abline(v=which.min(cross.means), lwd=2, lty=5, col="black")
  #rect(x.left, ybot, xright,ytop)
}
legend("topright",c(dat.names, "Model Number, 1-4"), text.col=c(col1[1:N.att],1), ncol=2)



cross.means <- rowMeans(t(sapply(rapply(error.mse.list, classes = 'matrix', how = 'list',
                                        f = function(x) x[, 1:10, drop = FALSE]),cbind)))
index.test <- rep(0,length(cross.means))
index.test[which.min(cross.means)] <- cross.means[which.min(cross.means)]
barplot(cross.means)
axis(1, seq(1,(nb.blocks), length.out=(nb.blocks)), 1:nb.blocks, cex.axis=1)
barplot(index.test, add=T, col="red")

legend("topright",c(dat.names, "Model Number, 1-4"), text.col=c(col1[1:N.att],1), ncol=2)


par(mgp=c(2,0.8,0), mar=c(1,1,1,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)
#Make legend, insets manually to place correctly
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft",c(dat.names, "Model Number, 1-4"), text.col=c(col1[1:N.att],1), ncol=5)
par(mgp=c(2,0.8,0), mar=c(3,3,2,1),oma=c(0,0,0,0),las=0, pty="m", xpd=F)















