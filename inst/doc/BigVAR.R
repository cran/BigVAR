## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install_github("wbnicholson/BigVAR/BigVAR")

## -----------------------------------------------------------------------------
library(BigVAR)
data(Y)

## -----------------------------------------------------------------------------
# 3 x 7 coefficient matrix
B = BigVAR.fit(Y,struct='Basic',p=2,lambda=1)[,,1]
# construct 7 x 99 lag matrix of Y
Z = VARXLagCons(Y,p=2,oos=TRUE)$Z
# obtain out of sample forecasts
yhat = B%*%Z[,ncol(Z),drop=F]

## ----echo=FALSE---------------------------------------------------------------
suppressMessages(library(BigVAR,quietly=TRUE,warn.conflicts=FALSE))
#onorm=c()
library(lattice)
#Oracle=diag(3)
#SigmaU=diag(3)
SparsityPlot <- function (B, p, k,s,m, title = NULL) 
{

    text <- c()
    for (i in 1:p) {
        text1 <- as.expression(bquote(bold(Phi)^(.(i))))
        text <- append(text, text1)
    }
    ## text <- c()
   if(s>0){
     for (i in (p+1):(p+s+1)) {
        text1 <- as.expression(bquote(bold(beta)^(.(i-p))))
        text <- append(text, text1)
    }
     }
    f <- function(m) t(m)[, nrow(m):1]
    
    rgb.palette <- colorRampPalette(c("white", "grey" ),space = "Lab")
    ## rgb.palette <- colorRampPalette(c("white", "blue"), space = "Lab")
    at <- seq(k/2 + 0.5, p * (k)+ 0.5, by = k)
    at2 <- seq(p*k+s/2+.5,p*k+s*m+.5,by=s)
    at <- c(at,at2)
    se2 = seq(1.75, by = k, length = k)
    L2 <- levelplot(f(abs(B)), col.regions = rgb.palette, colorkey = NULL, 
        xlab = NULL, ylab = NULL, main = list(label = title, 
            cex = 1), panel = function(...) {
            panel.levelplot(...)
            panel.abline(a = NULL, b = 1, h = seq(1.5, m*s+p* k + 
                0.5, by = 1), v = seq(1.5, by = 1, length = p * 
                k+m*s))
            bl1 <- seq(k + 0.5, p * 
                k + 0.5, by = k)
            bl2 <- seq(p*k + 0.5, p * 
                k + 0.5+s*m, by = m)
            b1 <- c(bl1,bl2)
            panel.abline(a = NULL, b = 1, v = p*k+.5, lwd = 7)
            panel.abline(a = NULL, b = 1, v = b1, lwd = 3)
        }, scales = list(x = list(alternating = 1, labels = text, 
            cex = 1, at = at, tck = c(0, 0)), y = list(alternating = 0, 
            tck = c(0, 0))))
    return(L2)
}

B1 <- matrix(rep(1,57)*rbinom(57,1,.6),nrow=3,ncol=19)
B2 <-matrix(0,nrow=3,ncol=19)
B2[,1:3] <- 1
B2[,10:12] <- 1
B2[,16] <- 1
B2[,19] <- 1

B3 <-matrix(0,nrow=3,ncol=19)
diag(B3[,1:3])<- 1
B3[,10:12] <- 1
diag(B3[,10:12])<- 0
B2[,16] <- 1
B2[,19] <- 1


B4 <-matrix(0,nrow=3,ncol=19)
B4[,1:3] <- rep(1,9)*rbinom(9,1,.6)
B4[,10:12] <- rep(1,9)*rbinom(9,1,.4)

B4[,18] <- c(1,0,1)

p=5;k=3
Lasso <- SparsityPlot(B1,p,k,m=2,s=2,title="Basic VARX-L")
Group <- SparsityPlot(B2,p,k,m=2,s=2,title="Lag VARX-L")
OO <- SparsityPlot(B3,p,k,m=2,s=2,title="Own/Other VARX-L")
Sparse <- SparsityPlot(B4,p,k,m=2,s=2,title="Sparse Lag VARX-L")

## ----echo=FALSE---------------------------------------------------------------
library(gridExtra,quietly=TRUE)    
grid.arrange(Lasso,Group,OO,Sparse,ncol=2)

## ----echo=FALSE---------------------------------------------------------------
k=3;p=5
HLAGC <- matrix(0,nrow=k,ncol=k*p)
HLAGC[1,] <- 1
HLAGC[2,1:6] <- 1
HLAGC[3,1:12] <- 1

HLAGOO <- matrix(0,nrow=k,ncol=k*p)
HLAGOO[1,1:13] <- 1
HLAGOO[2,1:6] <- 1
HLAGOO[3,c(1:9,12)] <- 1

HLAGELEM <- matrix(0,nrow=k,ncol=k*p)
HLAGELEM[1,c(1:10,12,13)] <- 1
HLAGELEM[2,c(1,3,4,6,7,9,10,12,13,15)] <- 1
HLAGELEM[3,c(1:8,10:11,13:14)] <- 1




SparsityPlot <-  
function (B, p, k,s,m, title = NULL) 
{

    text <- c()
    for (i in 1:p) {
        text1 <- as.expression(bquote(bold('\U03A6')^(.(i))))
        text <- append(text, text1)
    }
    ## text <- c()
    if(m>0){
     for (i in (p+1):(p+s+1)) {
        text1 <- as.expression(bquote(bold('\U03B2')^(.(i-p))))
        text <- append(text, text1)
    }
     }
    f <- function(m) t(m)[, nrow(m):1]
    
    rgb.palette <- colorRampPalette(c("white", "grey" ),space = "Lab")
    ## rgb.palette <- colorRampPalette(c("white", "blue"), space = "Lab")
    at <- seq(k/2 + 0.5, p * (k)+ 0.5, by = k)
    if(m>0){
    at2 <- seq(p*k+m/2+.5,p*k+s*m+.5,by=m)}else{at2=c()}
    at <- c(at,at2)
    se2 = seq(1.75, by = k, length = k)
    L2 <- levelplot(f(abs(B)), col.regions = rgb.palette, colorkey = NULL, 
        xlab = NULL, ylab = NULL, main = list(label = title, 
            cex = 1), panel = function(...) {
            panel.levelplot(...)
            panel.abline(a = NULL, b = 1, h = seq(1.5, m*s+p* k + 
                0.5, by = 1), v = seq(1.5, by = 1, length = p * 
                k+m*s))
            bl1 <- seq(k + 0.5, p * 
                k + 0.5, by = k)
            if(m>0){
            bl2 <- seq(p*k + 0.5, p * 
                k + 0.5+s*m, by = m)}else(bl2=c())
            b1 <- c(bl1,bl2)
            panel.abline(a = NULL, b = 1, v = p*k+.5, lwd = 3)
            panel.abline(a = NULL, b = 1, v = b1, lwd = 3)
        }, scales = list(x = list(alternating = 1, labels = text, 
            cex = 1, at = at, tck = c(0, 0)), y = list(alternating = 0, 
            tck = c(0, 0))))
    return(L2)
}
set.seed(1986)
B5 <-matrix(0,nrow=3,ncol=15)
B5[,1:3] <- rep(1,9)*rbinom(9,1,.85)
B5[,4:6] <- rep(1,9)*rbinom(9,1,.65)
B5[,7:9] <- rep(1,9)*rbinom(9,1,.45)
B5[,10:12] <- rep(1,9)*rbinom(9,1,.25)
B5[,13:15] <- rep(1,9)*rbinom(9,1,.05)


HV4 <- SparsityPlot(B5,5,k,0,0,title='Lag-Weighted Lasso')

HVC=SparsityPlot(HLAGC,p,k,0,0,title="Componentwise HLAG")
HVOO=SparsityPlot(HLAGOO,p,k,0,0,title="Own/Other HLAG")
HVELEM=SparsityPlot(HLAGELEM,p,k,0,0,title="Elementwise HLAG")
grid.arrange(HVC,HVOO,HVELEM,HV4,ncol=2)

## -----------------------------------------------------------------------------
data(Y)
# Create a Basic VAR-L (Lasso Penalty) with maximum lag order p=4, 10 grid points with lambda optimized according to rolling validation of 1-step ahead MSFE
mod1<-constructModel(Y,p=4,"Basic",gran=c(150,10),h=1,cv="Rolling",verbose=FALSE,IC=TRUE,model.controls=list(intercept=TRUE))

## -----------------------------------------------------------------------------
results=cv.BigVAR(mod1)
results

## -----------------------------------------------------------------------------
str(results)

## -----------------------------------------------------------------------------
plot(results)

## -----------------------------------------------------------------------------

mod2<-constructModel(Y,p=4,"Basic",gran=c(5,10),h=1,cv="Rolling",verbose=FALSE,IC=FALSE)
res2=cv.BigVAR(mod2)
plot(res2)


## -----------------------------------------------------------------------------

mod3<-constructModel(Y,p=4,"Basic",gran=c(500,10),h=1,cv="Rolling",verbose=FALSE,IC=FALSE)
res3=cv.BigVAR(mod3)
plot(res3)


## -----------------------------------------------------------------------------
SparsityPlot.BigVAR.results(results)

## -----------------------------------------------------------------------------
predict(results,n.ahead=1)

## -----------------------------------------------------------------------------
predict(results,n.ahead=1, confint=TRUE)

## -----------------------------------------------------------------------------
coef(results)

## ----echo=TRUE----------------------------------------------------------------
data(Y) # simulated multivariate time series
# coefficient matrix used to generate Y
data(Generator)
# note that coefficients with a darker shade are larger in magnitude
SparsityPlot(A[1:3,],p=4,3,s=0,m=0,title="Sparsity Structure of Generator Matrix")

## -----------------------------------------------------------------------------
# fit a Basic VARX-L with k=2,m=1,s=2,p=4,lambda=.01
VARX=list(k=2,s=2)
#returns k x (kp+ms+1) coefficient matrix
model=BigVAR.fit(Y,p,"Basic",lambda=1e-2,VARX=VARX,intercept=TRUE)
model

## -----------------------------------------------------------------------------

# N-fold cross validation for VAR
# Y: data
# nfolds: number of cross validation folds
# struct: penalty structure
# p: lag order 
# nlambdas: number of lambdas:
# gran1: depth of lambda grid
# seed: set to make it reproducible
NFoldcv <- function(Y,nfolds,struct,p,nlambdas,gran1,seed)
{
    MSFE <- matrix(0,nrow=nrow(Y),ncol=10)
    A <- constructModel(Y,p,struct=struct,gran=c(gran1,nlambdas),verbose=F)
                                        # construct lag matrix										
    Z1 <- VARXLagCons(Y,X=NULL,s=0,p=p,0,0)
    trainZ <- Z1$Z[2:nrow(Z1$Z),]


    trainY <- matrix(Y[(p+1):nrow(Y),],ncol=ncol(Y)) 
    set.seed(seed)
    
	inds <- sample(nrow(trainY))

	B <- BigVAR.est(A)
	lambda.grid <- B$lambda
    folds <- cut(inds,breaks=nfolds,labels=FALSE)

    MSFE <- matrix(0,nrow=nfolds,ncol=nlambdas)
    for(i in 1:nfolds){
        
        test <- trainY[which(folds==i),]
        train <- trainY[which(folds!=i),]
        testZ <-t(t(trainZ)[which(folds!=i),])
        
	    B=BigVAR.fit(train,p=p,lambda=lambda.grid,struct='Basic')
	
                                                #iterate over lambdas
        for(j in 1:nlambdas){


            MSFETemp <- c()
            for(k in 1:nrow(test))    {
                tempZ <- testZ[,k,drop=FALSE]
                bhat <- matrix(B[,2:dim(B)[2],j],nrow=ncol(Y),ncol=(p*ncol(Y)))
                preds <- B[,1,j]+bhat%*%tempZ

                MSFETemp <- c(MSFETemp,sum(abs(test[k,]-preds))^2)
                
            }
            MSFE[i,j] <- mean(MSFETemp)
            

        }


    }

    return(list(MSFE=MSFE,lambdas=lambda.grid))
}
# 10 fold cv
MSFEs<-NFoldcv(Y,nfolds=10,"Basic",p=5,nlambdas=10,gran1=50,seed=2000)
# choose smaller lambda in case of ties (prevents extremely sparse solutions)
opt=MSFEs$lambda[max(which(colMeans(MSFEs$MSFE)==min(colMeans(MSFEs$MSFE))))]
opt

## -----------------------------------------------------------------------------
data(Y)
p <- 4
T1 <- floor(nrow(Y))/3
T2 <- floor(2*nrow(Y))/3
#Matrix of zeros for X
X <- matrix(0,nrow=nrow(Y),ncol=ncol(Y))
BICMSFE <- VARXForecastEval(Y,X,p,0,T1,T2,"BIC",1)

## -----------------------------------------------------------------------------
mod <- VARXFit(Y,3,NULL,NULL)
pred <-PredictVARX(mod)
pred

## -----------------------------------------------------------------------------
library(MASS)
k=3;p=6
B=matrix(0,nrow=k,ncol=p*k)
A1<- matrix(c(.4,-.07,.08,-.06,-.7,.07,-.08,.07,-.4),ncol=3,nrow=3)
A2 <- matrix(c(-.6,0,0,0,-.4,0,0,0,.5),ncol=3,nrow=3)
B[,1:k]=A1
B[,(5*k+1):(6*k)]=A2
A <- VarptoVar1MC(B,p,k)
set.seed(2000)
Y <-MultVarSim(k,A,p,.005*diag(k),500)
SparsityPlot(B,p,k,0,0, title='Sparsity Plot of VAR Coefficient Matrix')

## ----cache=TRUE---------------------------------------------------------------
library(MCS)
# train on first 250 observations
YTrain=Y[1:250,]
Loss <- c()
T1=1*floor(nrow(YTrain)/3);T2=2*floor(nrow(YTrain)/3)
p=8

structures<-c("Basic","BasicEN","Lag","SparseLag","OwnOther","HLAGC","HLAGOO","HLAGELEM","MCP","SCAD")

for(i in structures){
	# construct BigVAR object; we will perform a dual grid search for the sparse lag and sparse own/other models
	if(i%in%c("SparseLag","SparseOO")){
		alpha=seq(0,1,length=10)
	}else{
		alpha=0
	}
    
    A<- constructModel(YTrain,p=p,struct=i,gran=c(100,10),T1=T1,T2=T2,verbose=FALSE,model.controls=list(intercept=FALSE,alpha=alpha))
	# perform rolling cv
    res<- cv.BigVAR(A)
    # save out of sample loss for each structure
    Loss <- cbind(Loss,res@OOSMSFE)    
}
# construct AIC and BIC benchmarks
BIC <- VARXForecastEval(YTrain,matrix(0,nrow=nrow(YTrain)),p,0,T2,nrow(YTrain),"BIC",1)$MSFE
AIC <- VARXForecastEval(YTrain,matrix(0,nrow=nrow(YTrain)),p,0,T2,nrow(YTrain),"AIC",1)$MSFE

Loss <- as.data.frame(Loss)
names(Loss) <- structures
Loss <- cbind(Loss,BIC,AIC)

names(Loss)[(ncol(Loss)-1):ncol(Loss)] <- c("BIC","AIC")
names(Loss) <- paste0(names(Loss),"L")
mcs.test <- MCSprocedure(as.matrix(Loss),verbose=FALSE)
mcs.test

## -----------------------------------------------------------------------------
suppressMessages(library(expm))

# Phi k x kp coefficient matrix
# sigma kxk residual covariance matrix
# n number of time steps to run IRF
# p lag order
# k number of series
# Y0: k dimensional vector reflecting initialization of the IRF									   
generateIRF <- function(Phi,Sigma,n,k,p,Y0)
{


if(p>1){

A <-VarptoVar1MC(Phi,p,k) 

}else{

A <- Phi

}
J <- matrix(0,nrow=k,ncol=k*p)
diag(J) <- 1
P <- t(chol(Sigma))
IRF <- matrix(0,nrow=k,ncol=n+1)
for(i in 0:n)
{

phi1 <- J%*%(A%^%i)%*%t(J)

theta20 <- phi1%*%P

IRF[,i+1] <- (theta20%*%Y0)

}


return(IRF)

}

## ----echo=TRUE,eval=TRUE,cache=TRUE-------------------------------------------
require(quantmod)
require(zoo)
# get GDP, Federal Funds Rate, CPI from FRED
#Gross Domestic Product (Relative to 2000)
getSymbols('GDP',src='FRED',type='xts')
GDP<- aggregate(GDP,as.yearqtr,mean)
GDP <- GDP/mean(GDP["2000"])*100
# Transformation Code: First Difference of Logged Variables
GDP <- diff(log(GDP))
index(GDP) <- as.yearqtr(index(GDP))
# Federal Funds Rate
getSymbols('FEDFUNDS',src='FRED',type='xts')
FFR <- aggregate(FEDFUNDS,as.yearqtr,mean)
# Transformation Code: First Difference
FFR <- diff(FFR)
# CPI ALL URBAN CONSUMERS, relative to 1983
getSymbols('CPIAUCSL',src='FRED',type='xts')
CPI <- aggregate(CPIAUCSL,as.yearqtr,mean)
CPI <- CPI/mean(CPI['1983'])*100
# Transformation code: difference of logged variables
CPI <- diff(log(CPI))                             
# Seasonally Adjusted M1
getSymbols('M1SL',src='FRED',type='xts')
M1<- aggregate(M1SL,as.yearqtr,mean)
# Transformation code, difference of logged variables
M1 <- diff(log(M1))
# combine series
Y <- cbind(CPI,FFR,GDP,M1)
names(Y) <- c("CPI","FFR","GDP","M1")
Y <- na.omit(Y)
k=ncol(Y)
T <- nrow(Y)
# start/end of rolling validation
T1 <- which(index(Y)=="1985 Q1")
T2 <- which(index(Y)=="2005 Q1")

#Demean
    Y <- Y - (c(rep(1, nrow(Y))))%*%t(c(apply(Y[1:T1,], 2, mean))) 
#Standarize Variance 
for (i in 1:k) {
        Y[, i] <- Y[, i]/apply(Y[1:T1,], 2, sd)[i]
    }
library(expm)
# Fit an Elementwise HLAG model
Model1=constructModel(as.matrix(Y),p=4,struct="HLAGELEM",gran=c(25,10),verbose=FALSE,VARX=list(),T1=T1,T2=T2)
Model1Results=cv.BigVAR(Model1)

# generate IRF for 10 quarters following a 1 percent increase in the federal funds rate
IRFS <- generateIRF(Phi=Model1Results@betaPred[,2:ncol(Model1Results@betaPred)],Sigma=cov(Model1Results@resids),n=10,k=ncol(Y),p=4,Y0=c(0,.01,0,0))

IRFS <- generateIRF(Model1Results@betaPred[,2:ncol(Model1Results@betaPred)],cov(Model1Results@resids),10,4,4,c(0,.01,0,0))    

## ----functions, include=FALSE, echo=FALSE-------------------------------------
# A function for captioning and referencing images
fig <- local({
    i <- 0
    ref <- list()
    list(
        cap=function(refName, text) {
            i <<- i + 1
            ref[[refName]] <<- i
            paste("Figure ", i, ": ", text, sep="")
        },
        ref=function(refName) {
            ref[[refName]]
        })
})

## ----irf_plot,fig.cap='\\label{fig:irf_plot} Generated Impulse Reponses', echo=FALSE----
# Plot IRFs
par(mfrow=c(2,2))
for(i in 1:4)
    {

        plot(IRFS[i,],type='l',main=names(Y)[i],ylab="",xlab="")

        
        }


