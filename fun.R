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

doloci.mejo <- function(mu1, mu2, ro, b1, b2){
  
  # Funkcija za dolocanje meje na podatkih za NORMALNO PORAZD.
  # (uporablja se znotraj generiranja podatkov)
  #---------------------------------------------------------------------
  # INPUT: 
  #   mu1...povprečje za marker 1
  #   mu2...povprečje za marker 2
  #   ro...korelacijski faktor med markerjema
  #   b1...vpliv na bolezen za marker 1
  #   b2...vpliv na bolezen za marker 2
  # OUTPUT:
  #   meja (int) za določitev bolezni
  #---------------------------------------------------------------------
  
  korelacije <- matrix(ro, nrow=2, ncol=2)
  diag(korelacije) <- 1
  x <- rmvnorm(n=10000, mean=c(mu1,mu2), sigma=korelacije)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n=10000,0,5)
  round(median(y))
}


doloci.mejo.gamma <- function(b1, b2){
  
  # Funkcija za dolocanje meje na podatkih za GAMMA PORAZD.
  # (uporablja se znotraj generiranja podatkov)
  #---------------------------------------------------------------------
  # INPUT: 
  #   b1...vpliv na bolezen za marker 1
  #   b2...vpliv na bolezen za marker 2
  # OUTPUT:
  #   meja (int) za določitev bolezni
  #---------------------------------------------------------------------
  
  e1 = rexp(10000, rate=2)
  e2 = rexp(10000, rate=2)
  e3 = rexp(10000, rate=2)
  
  X1 = e1 + e3 # prvi marker
  X2 = e2 + e3 # drugi marker
  y <- b1*x[,1]+b2*x[,2] + rnorm(n=10000,0,1)
  round(median(y))
}


get.data <- function(n, mu1, mu2, ro, b1, b2){
  
  # Funkcija za generiranje vzorca - NORMALNA PORAZDELITEV
  #---------------------------------------------------------------------
  # INPUT: 
  #   n...velikost vzorca
  #   mu1...povprečje za marker 1
  #   mu2...povprečje za marker 2
  #   ro...korelacijski faktor med markerjema
  #   b1...vpliv na bolezen za marker 1
  #   b2...vpliv na bolezen za marker 2
  # OUTPUT:
  #   tabela s stolpci:
  #     y...binarna(0/1) označuje ali ima posameznik bolezen (0-zdrav,1-bolen)
  #     X1...vrednost markerja 1
  #     X2...vrednost markerja 2
  #---------------------------------------------------------------------
  
  korelacije <- matrix(ro, nrow=2, ncol=2)
  diag(korelacije) <- 1
  x <- rmvnorm(n=n, mean=c(mu1,mu2), sigma=korelacije)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n,0,5)
  meja <- doloci.mejo(mu1, mu2, ro, b1, b2)
  
  zdravi <- y<meja
  bolni <- y>=meja
  y[zdravi] <- 0
  y[bolni] <- 1
  
  return(data.frame(y,x))
}

get.data.gamma <- function(n, b1, b2){
  
  # Funkcija za generiranje vzorca - GAMMA PORAZDELITEV
  #---------------------------------------------------------------------
  # INPUT: 
  #   n...velikost vzorca
  #   ro...korelacijski faktor med markerjema
  #   b1...vpliv na bolezen za marker 1
  #   b2...vpliv na bolezen za marker 2
  # OUTPUT:
  #   tabela s stolpci:
  #     y...binarna(0/1) označuje ali ima posameznik bolezen (0-zdrav,1-bolen)
  #     X1...vrednost markerja 1
  #     X2...vrednost markerja 2
  #---------------------------------------------------------------------
  
  e1 = rexp(n, rate=2)
  e2 = rexp(n, rate=2)
  e3 = rexp(n, rate=2)

  X1 = e1 + e3 # prvi marker
  X2 = e2 + e3 # drugi marker
  x <- cbind(X1,X2)
  y <- b1*x[,1]+b2*x[,2] + rnorm(n,0,1)
  
  meja <- doloci.mejo.gamma(b1, b2)
  y[y<meja] <- 0
  y[y>=meja] <- 1
  
  return(data.frame(y,x))
}


plot.roc <- function(df){
  # Funkcija, ki izriše obe ROC krivulji
  #---------------------------------------------------------------------
  # INPUT: 
  #   df...vzorec s stolpci y, X1, X2
  # OUTPUT:
  #   Graf z ROC krivuljama in oznakami Markerjev
  #---------------------------------------------------------------------
  pred1 <- prediction(df$X1, df$y) 
  perf1 <- performance(pred1,"tpr","fpr")
  plot(perf1,col="orange")
  
  pred2 <- prediction(df$X2, df$y) 
  perf2 <- performance(pred2,"tpr","fpr")
  plot(perf2,col="blue", add=TRUE)
  legend(0.75,0.2,c('Marker 1','Marker 2'),col=c('orange','blue'),lwd=1)
}

get.AUC <- function(df){
  
  # Funkcija, ki na vzorcu izračuna več statistik
  #---------------------------------------------------------------------
  # INPUT: 
  #   df...vzorec s stolpci y, X1, X2
  # OUTPUT:
  #   list (4) z vrednostmi:
  #     AUC1...AUC za Marker 1 (X1)
  #     AUC2...AUC za Marker 2 (X2)
  #     razlika...AUC1-AUC2
  #     razmerje...AUC1/AUC2
  #---------------------------------------------------------------------
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

permutiraj <- function(df, perm.cols, m.type){
  
  # Funkcija, ki na vzorcu permutira podatke in vrne željeno statistiko
  #---------------------------------------------------------------------
  # INPUT: 
  #   df...vzorec s stolpci y, X1, X2
  #   perm.cols...katere vrednosti permutiramo. Možni: c("y") ali c("X1","X2")
  #   m.type...Kaj računamo. Možni: "razlika"/"razmerje"
  # OUTPUT:
  #   vrednost željene statistike (razmerje ali razlika AUC)
  #---------------------------------------------------------------------
  
  if(length(perm.cols)>1){
    
    }
  
  #for(i in perm.cols){
  #  df[, i] <- df[sample(1:nrow(df)), i]
  #}
  get.AUC(df)[m.type] %>% as.numeric()
}

permutiraj2 <- function(df, m.type){
  ### DRUGA MOŽNOST ZA PERMUTIRANJE
  df_m <- df %>% reshape2::melt(id.vars=c("y"))
  df_m <- as.data.table(df_m)
  df_m[, variable := sample(variable), by = y] 
  df_perm <- df_m %>% as.data.frame() 
  df_perm$seq <- with(df_perm, ave(value, y, variable, FUN = seq_along))
  df_perm <- reshape2::dcast(y + seq ~ variable, data = df_perm, value.var = "value")
  df_perm$seq <- NULL
  get.AUC(df)[m.type] %>% as.numeric()df
}
  
testiraj <- function(df, perm.cols, m.type, n=1000){
  
  # Funkcija, ki zgenerira porazdelitev pod ničelno domnevo (permutacije)
  #---------------------------------------------------------------------
  # INPUT: 
  #   df...vzorec s stolpci y, X1, X2
  #   perm.cols...katere vrednosti permutiramo. Možni: c("y") ali c("X1","X2")
  #   m.type...Kaj računamo. Možni: "razlika"/"razmerje"
  #   n...število permutacij za generiranje porazdelitve
  # OUTPUT:
  #   list (3) z elementi:
  #     porazdelitev...vektor dolžine n, dobljena porazdelitev testne stat.
  #     t...vrednost testne statistike na vzorcu
  #     p...vrednost p
  #---------------------------------------------------------------------
  
  porazdelitev <- replicate(n, permutiraj(df, perm.cols, m.type))
  
  test.stat <- get.AUC(df)[m.type] %>% as.numeric()
  
  if (m.type=="razlika"){
    p.vr <- sum(abs(porazdelitev) > abs(test.stat))/length(porazdelitev)
  }
  else{
    druga.meja = 1/test.stat
    zgornja = max(druga.meja,test.stat)
    spodnja = min(druga.meja,test.stat)
    p.vr <- (sum(porazdelitev > zgornja)+sum(porazdelitev < spodnja))/length(porazdelitev)
  }
  
  return(list("porazdelitev" = porazdelitev,
              "t" = test.stat,
              "p" = p.vr))
} 


plot.test <- function(data, iz=TRUE, p.val=TRUE){
  
  # Funkcija, ki nariše rezultate testa
  #---------------------------------------------------------------------
  # INPUT: 
  #   data...podatki dobljeni s funkcijo testiraj
  #   iz...ali naj nariše kritično območje. Možni: TRUE/FALSE
  #   p.val...ali naj označi testno stat. in vr.p. Možni: TRUE/FALSE
  # OUTPUT:
  #   grafični prikaz testa (density, poljubno p.vr in testna stat.)
  #---------------------------------------------------------------------

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
  
}

