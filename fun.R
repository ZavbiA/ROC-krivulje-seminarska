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

source("lib.r")

##--------------------------------------------------------------------
##  Funkcije za generiranje podatkov, risanje ROC in računanje AUC  --
##--------------------------------------------------------------------

#funkcija za dolocanje meje na podatkih
doloci.mejo <- function(mu1, mu2, ro, b1, b2){
  korelacije <- matrix(ro, nrow=2, ncol=2)
  diag(korelacije) <- 1
  x <- rmvnorm(n=10000, mean=c(mu1,mu2), sigma=korelacije)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n=10000,0,1)
  round(median(y))
}

#funkcija za generiranje podatkov
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

#izrise obe ROC krivulji
plot.roc <- function(df){
  pred1 <- prediction(df$X1, df$y) 
  perf1 <- performance(pred1,"tpr","fpr")
  plot(perf1,col="orange")
  
  pred2 <- prediction(df$X2, df$y) 
  perf2 <- performance(pred2,"tpr","fpr")
  plot(perf2,col="blue", add=TRUE)
  legend(0.75,0.2,c('Marker 1','Marker 2'),col=c('orange','blue'),lwd=1)
}

# izracuna oba AUC-ja, vrne list(AUC1,AUC2,razlika,razmerje)
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
##    * perm.cols: kaj permutiramo (c("y") ali c("X1","X2))
##    * m.type: kaj preverjamo ("razmerje"/"razlika")             
##    * n (št. ponovitev izračuna)  



permutiraj <- function(df, perm.cols, m.type){
  #Najprej permutiramo podatke
  for(i in perm.cols){
    df[, i] <- df[sample(1:nrow(df)), i]
  }
  get.AUC(df)[m.type] %>% as.numeric()
}

#vrne porazdelitev testne statistike
porazdelitev <- function(df, perm.cols, m.type, n=1000){
  porazdelitev <- replicate(n, permutiraj(df, perm.cols, m.type))
  
  return(list("dist" = porazdelitev,
              "m.type" = m.type))
} #m.type = razlika ali razmerje, dist pa so dejanske vrednosti razlike/razmerja
  
testiraj <- function(df, porazdelitev){
  test.stat <- get.AUC(df)[porazdelitev$m.type] %>% as.numeric()
  if (porazdelitev$m.type=="razlika"){
    p.vr <- sum(abs(porazdelitev$dist) > test.stat)/length(porazdelitev$dist)
  }
  else{
    druga.meja = 1/test.stat
    zgornja = max(druga.meja,test.stat)
    spodnja = min(druga.meja,test.stat)
    p.vr <- (sum(porazdelitev$dist > zgornja)+sum(porazdelitev$dist < spodnja))/length(porazdelitev$dist)
  }
  return(list("porazdelitev" = porazdelitev$dist,
              "t" = test.stat,
              "p" = p.vr))
} #porazdelitev vrne to kar vrne funkcija porazdelitev
#t je originalna vrednost


plot.test <- function(data, iz=FALSE, p.val=FALSE){
  #hist(data$porazdelitev, freq=FALSE,
  #     xlab = "x", ylab = expression(f[X]))
  plt <- density(data$porazdelitev)

  sp.meja <- quantile(data$porazdelitev, probs = c(0.025))
  zg.meja <- quantile(data$porazdelitev, probs = c(0.975))
  
  plot(plt, main="", xlab="x",
       ylab = expression(f[X]) )
  if(p.val){
  polygon(c(zg.meja, plt$x[plt$x>=zg.meja]),
          c(0,plt$y[plt$x>=zg.meja]), col="grey")
  polygon(c(plt$x[plt$x<=sp.meja], sp.meja),
          c(plt$y[plt$x<=sp.meja], 0), col="grey")
  }

  if(p.val){
    #abline(v=data$t, col="red", add=TRUE)
    arrows(x0=data$t, y0=max(plt$y)/8, y1=0, length=0.1,
           col="red") 
    text(data$t, max(plt$y)/6.5, paste0("p = ", data$p),
         cex = .8, col="red")
  }
  
} #izrise graf density vzorca, lahko se p-vrednost in kriticno obmocje


### PRIMER:

#df <- get.data(100,0,2,0.4,6,2)
#data.test <- testiraj(df, c("y"), "razlika", n=1000)
#plot.test(data.test, 1, iz=TRUE, p.val=TRUE)

