############################################################################
############################################################################
###                                                                      ###
###                          DEFINICIJE FUNKCIJ                          ###
###                                                                      ###
############################################################################
############################################################################

##----------------------------------------------------------------
##                      Potrebne knjižnice                      --
##----------------------------------------------------------------

library(dplyr)
library(knitr)
library(mvtnorm)
library(ROCR)


##--------------------------------------------------------------------
##  Funkcije za generiranje podatkov, risanje ROC in računanje AUC  --
##--------------------------------------------------------------------

doloci.mejo <- function(mu1, mu2, ro, b1, b2){
  korelacije <- matrix(ro, nrow=2, ncol=2)
  diag(korelacije) <- 1
  x <- rmvnorm(n=10000, mean=c(mu1,mu2), sigma=korelacije)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n=10000,0,1)
  round(median(y))
}

get.data <- function(n, mu1, mu2, ro, b1, b2){
  korelacije <- matrix(ro, nrow=2, ncol=2)
  diag(korelacije) <- 1
  x <- rmvnorm(n=n, mean=c(mu1,mu2), sigma=korelacije)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n,0,1)
  meja <- doloci.mejo(mu1, mu2, ro, b1, b2)
  y[y<meja] <- 0
  y[y>=meja] <- 1
  
  return(data.frame(y,x))
}

plot.roc <- function(df){
  pred1 <- prediction(df$X1, df$y) 
  perf1 <- performance(pred1,"tpr","fpr")
  plot(perf1,col="orange", main = naslov)
  
  pred2 <- prediction(df$X2, df$y) 
  perf2 <- performance(pred2,"tpr","fpr")
  plot(perf2,col="blue", add=TRUE)
  legend(0.83,0.2,c('Marker 1','Marker 2'),col=c('orange','blue'),lwd=1)
}

get.AUC <- function(df){
  pred1 <- prediction(df$X1, df$y) 
  auc_ROCR1 <- performance(pred1, measure = "auc")
  auc_ROCR1 <- auc_ROCR1@y.values[[1]]
  
  pred2 <- prediction(df$X2, df$y) 
  auc_ROCR2 <- performance(pred2, measure = "auc")
  auc_ROCR2 <- auc_ROCR2@y.values[[1]]
  
  list("AUC1"=auc_ROCR1, "AUC2" = auc_ROCR2,
       "razlika"=auc_ROCR1 - auc_ROCR2,
       "razmerje" = auc_ROCR1/auc_ROCR2)
}

##----------------------------------------------------------------
##                Funkcije za testiranje - TESTI                --
##----------------------------------------------------------------

## PRVI TEST 
## Permutiramo markerje, razlika

##  Funkcija, s pomočjo katere dobimo porazdelitev testne statistike potrebuje   
##  več vhodnih parametrov: 
##    * podatki (df)
##    * kaj permutiramo (y/x)
##    * kaj preverjamo (razmerje/razlike)             
##    * n (št. ponovitev izračuna)  

permutiraj <- function(df, perm.cols, m.type){
  #Najprej permutiramo podatke
  df[, perm.cols] <- df[sample(1:nrow(df)), perm.cols]
  get.AUC(df)[m.type] %>% as.numeric()
}

testiraj <- function(df, perm.cols, m.type, n=5000){
  test.stat <- get.AUC(df)[m.type] %>% as.numeric()
  porazdelitev <- replicate(n, permutiraj(df, perm.cols, m.type))
  p.vr <- sum(porazdelitev > test.stat)/n
  
  return(list("porazdelitev" = porazdelitev,
              "t" = test.stat,
              "p" = p.vr))
}





