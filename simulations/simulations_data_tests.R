############################################################################
############################################################################
###                                                                      ###
###                     PODATKI IN  TESTI                                ###
###                                                                      ###
############################################################################
############################################################################

source("fun.R")

##################################################################
##         Osnovni podatki (enaka porazdelitev)                 ##
##################################################################

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 2
podatki1 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.1 <- testiraj.rank.X(podatki1,"razlika",n.perm=1000)
test2.1 <- testiraj.rank.y(podatki1,"razlika",n.perm=1000)
test3.1 <- testiraj.rank.X(podatki1,"razmerje",n.perm=1000)
test4.1 <- testiraj.rank.y(podatki1,"razmerje",n.perm=1000)


##################################################################
##       Razlicni pricakovani vrednosti markerjev               ##
##################################################################

n <- 100; mu1 <- 0; mu2 <- 4; ro <- 0.4; b1 <- 6; b2 <- 2


podatki2 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.2 <- testiraj.rank.X(podatki2,"razlika",n.perm=1000)
test2.2 <- testiraj.rank.y(podatki2,"razlika",n.perm=1000)
test3.2 <- testiraj.rank.X(podatki2,"razmerje",n.perm=1000)
test4.2 <- testiraj.rank.y(podatki2,"razmerje",n.perm=1000)



##################################################################
##       Neodvisna markerja                                     ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0; b1 <- 6; b2 <- 2

podatki3 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.3 <- testiraj.rank.X(podatki3,"razlika",n.perm=1000)
test2.3 <- testiraj.rank.y(podatki3,"razlika",n.perm=1000)
test3.3 <- testiraj.rank.X(podatki3,"razmerje",n.perm=1000)
test4.3 <- testiraj.rank.y(podatki3,"razmerje",n.perm=1000)

##################################################################
##           Velika korelacija                                  ##
##################################################################

cl <- makeCluster(detectCores()-1)  

# EXPORTAMO VSE POTREBNE KNIŽNICE
clusterEvalQ(cl,library(mvtnorm))

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.9; b1 <- 6; b2 <- 2

podatki4 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.4 <- testiraj.rank.X(podatki4,"razlika",n.perm=1000)
test2.4 <- testiraj.rank.y(podatki4,"razlika",n.perm=1000)
test3.4 <- testiraj.rank.X(podatki4,"razmerje",n.perm=1000)
test4.4 <- testiraj.rank.y(podatki4,"razmerje",n.perm=1000)


##################################################################
##           Podobni beti                                       ##
##################################################################

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 5

podatki5 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.5 <- testiraj.rank.X(podatki5,"razlika",n.perm=1000)
test2.5 <- testiraj.rank.y(podatki5,"razlika",n.perm=1000)
test3.5 <- testiraj.rank.X(podatki5,"razmerje",n.perm=1000)
test4.5 <- testiraj.rank.y(podatki5,"razmerje",n.perm=1000)


##################################################################
##           Podobni beti,neodvisna                             ##
##################################################################

n <- 100; mu1 <- 0; mu2 <- 0; ro <- 0; b1 <- 6; b2 <- 5

podatki6 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.6 <- testiraj.rank.X(podatki6,"razlika",n.perm=1000)
test2.6 <- testiraj.rank.y(podatki6,"razlika",n.perm=1000)
test3.6 <- testiraj.rank.X(podatki6,"razmerje",n.perm=1000)
test4.6 <- testiraj.rank.y(podatki6,"razmerje",n.perm=1000)


##################################################################
##                      Majhen vzorec                           ##
##################################################################

n <- 20; mu1 <- 0; mu2 <- 0; ro <- 0.4; b1 <- 6; b2 <- 2

podatki7 <- get.data(n, mu1, mu2, ro, b1, b2)

test1.7 <- testiraj.rank.X(podatki7,"razlika",n.perm=1000)
test2.7 <- testiraj.rank.y(podatki7,"razlika",n.perm=1000)
test3.7 <- testiraj.rank.X(podatki7,"razmerje",n.perm=1000)
test4.7 <- testiraj.rank.y(podatki7,"razmerje",n.perm=1000)

##################################################################
##                      GAMMA                                   ##
##################################################################

n <- 100; b1 <- 6; b2 <- 2

podatki8 <- get.data.gamma(n, b1, b2)

test1.8 <- testiraj.rank.X(podatki8,"razlika",n.perm=1000)
test2.8 <- testiraj.rank.y(podatki8,"razlika",n.perm=1000)
test3.8 <- testiraj.rank.X(podatki8,"razmerje",n.perm=1000)
test4.8 <- testiraj.rank.y(podatki8,"razmerje",n.perm=1000)

##################################################################
##                      POISS                                   ##
##################################################################

n <- 100; b1 <- 6; b2 <- 2

podatki9 <- get.data.pois(n, b1, b2)

test1.9 <- testiraj.rank.X(podatki9,"razlika",n.perm=1000)
test2.9 <- testiraj.rank.y(podatki9,"razlika",n.perm=1000)
test3.9 <- testiraj.rank.X(podatki9,"razmerje",n.perm=1000)
test4.9 <- testiraj.rank.y(podatki9,"razmerje",n.perm=1000)

##################################################################
##                      RAZLIČNI                                ##
##################################################################

n <- 100; b1 <- 6; b2 <- 2

podatki10 <- get.data.razlicna(n, b1, b2)

test1.10 <- testiraj.rank.X(podatki10,"razlika",n.perm=1000)
test2.10 <- testiraj.rank.y(podatki10,"razlika",n.perm=1000)
test3.10 <- testiraj.rank.X(podatki10,"razmerje",n.perm=1000)
test4.10 <- testiraj.rank.y(podatki10,"razmerje",n.perm=1000)


# dump(c("podatki1","test1.1","test2.1","test3.1","test4.1",
#        "podatki2","test1.2","test2.2","test3.2","test4.2",
#        "podatki3","test1.3","test2.3","test3.3","test4.3",
#        "podatki4","test1.4","test2.4","test3.4","test4.4",
#        "podatki5","test1.5","test2.5","test3.5","test4.5",
#        "podatki6","test1.6","test2.6","test3.6","test4.6",
#        "podatki7","test1.7","test2.7","test3.7","test4.7",
#        "podatki8","test1.8","test2.8","test3.8","test4.8",
#        "podatki9","test1.9","test2.9","test3.9","test4.9",
#        "podatki10","test1.10","test2.10","test3.10","test4.10"), "podatki_in_testi.R")

dump(c("podatki1","test1.1","test2.1","test3.1","test4.1",
       "podatki2",
       "podatki3",
       "podatki4",
       "podatki5",
       "podatki6",
       "podatki7",
       "podatki8",
       "podatki9",
       "podatki10"), "podatki_in_testi.R")


