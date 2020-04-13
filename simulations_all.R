############################################################################
############################################################################
###                                                                      ###
###                         PARALELNE SIMULACIJE                         ###
###                       VELIKOSTI IN MOČI TESTOV                       ###
###                                                                      ###
############################################################################
############################################################################



##################################################################
##         Osnovni podatki (enaka porazdelitev)                 ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti1 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster

##################################################################
##       Različni pričakovani vrednosti markerjev               ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 4; ro <- 0.4; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti2 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster



##################################################################
##       Neodvisna markerja                                     ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti3 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster

##################################################################
##           Velika korelacija                                  ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.9; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti4 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster



##################################################################
##           Velika korelacija                                  ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.9; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti5 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster


##################################################################
##           Podobni beti                                       ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 5

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti6 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster


##################################################################
##           Podobni beti,neodvisna                             ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0; b1 <- 6; b2 <- 5

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti7 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster


##################################################################
##                      Majhen vzorec                           ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 20; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data","doloci.mejo","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data(n, mu1, mu2, ro, b1, b2),"razmerje",1000)$p})


lastnosti8 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster



##################################################################
##                      GAMMA                                   ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data.gamma","doloci.mejo.gamma","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.gamma(n, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.gamma(n, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.gamma(n, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.gamma(n, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.gamma(n, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.gamma(n, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.gamma(n, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.gamma(n, b1, b2),"razmerje",1000)$p})


lastnosti9 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster


##################################################################
##                      POISS                                   ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; b1 <- 6; b2 <- 2

# EXPORTAMO VSE POTREBNE FUNKCIJE
clusterExport(cl,c("get.data.pois","doloci.mejo.pois","testiraj.rank.X","testiraj.rank.y","auroc",
                   "n","mu1","mu2","ro","b1","b2"))

# Simulacije za velikost
pvred1.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.pois(n, 3, 3),"razlika",1000)$p})
pvred2.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.pois(n, 3, 3),"razlika",1000)$p})
pvred3.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.pois(n, 3, 3),"razmerje",1000)$p})
pvred4.vel <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.pois(n, 3, 3),"razmerje",1000)$p})


# Simulacije za moc
pvred1.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.pois(n, b1, b2),"razlika",1000)$p})
pvred2.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.pois(n, b1, b2),"razlika",1000)$p})
pvred3.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.X(get.data.pois(n, b1, b2),"razmerje",1000)$p})
pvred4.moc <- parSapply(cl, 1:1000, function(i,...){testiraj.rank.y(get.data.pois(n, b1, b2),"razmerje",1000)$p})


lastnosti10 <- data.frame("Velikost" = c(round(sum(pvred1.vel<0.05)/1000,3),
                                        round(sum(pvred2.vel<0.05)/1000,3),
                                        round(sum(pvred3.vel<0.05)/1000,3),
                                        round(sum(pvred4.vel<0.05)/1000,3)),
                         "Moč" = c(round(sum(pvred1.moc<0.05)/1000,3),
                                   round(sum(pvred2.moc<0.05)/1000,3),
                                   round(sum(pvred3.moc<0.05)/1000,3),
                                   round(sum(pvred4.moc<0.05)/1000,3)),
                         row.names = paste0("Test ",1:4))

stopCluster(cl)  #stop the cluster



dump(c("lastnosti1","lastnosti2","lastnosti3","lastnosti4","lastnosti5",
       "lastnosti6","lastnosti7","lastnosti8","lastnosti9","lastnosti10"), "results_all.R")

