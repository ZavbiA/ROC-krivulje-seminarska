###########################################################################
###########################################################################
###                                                                     ###
###               PODATKI, SIMULACIJE IN LASTNOSTI TESTOV               ###
###                                                                     ###
###########################################################################
###########################################################################

source("lib.r")
source("fun.r")

n <- 100 # velikost vzorca
N <- 1000 # Število simulacij za generiranje porazdelitev
m <- 500 # Število simulacij za vsak test

##----------------------------------------------------------------
##               POISSONOVA - ENAKA PORAZDELITEV MARKERJEV                             --
##----------------------------------------------------------------


## TESTI ZA PODATKE IZ POISSONOVE
b1<-6; b2<-2

podatki <- get.data.pois(n, b1, b2)

test1 <- testiraj(podatki, perm.cols=c("X1","X2"), m.type="razlika", n=N)
test2 <- testiraj(podatki, perm.cols=c("y"), m.type="razlika", n=N)
test3 <- testiraj(podatki, perm.cols=c("X1","X2"), m.type="razmerje", n=N)
test4 <- testiraj(podatki, perm.cols=c("y"), m.type="razmerje", n=N)


##----------------------------------------------------------------
##         Velikost testa NORMAL - enaka porazdelitev           --
##----------------------------------------------------------------

# Podatki pod H0
 b1 <- 3; b2 <- 3

# Test 1
pvred1 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                                 perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
vel1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                                  perm.cols=c("y"), m.type="razlika", n=N)$p)
vel2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                                  perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
vel3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                                  perm.cols=c("y"), m.type="razmerje", n=N)$p)
vel4 <- round(sum(pvred4<0.05)/m,3)

velikosti.pois <- data.frame("Velikost" = c(vel1,vel2,vel3,vel4),
                        row.names = paste0("Test ",1:4))

##---------------------------------------------------------------
##          Moč testa  POISSON - enaki porazdelitvi             --
##---------------------------------------------------------------

# Podatki pod H1 (isti kot prej)
b1 <- 6; b2 <- 2

# Test 1
pvred1 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
moc1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                               perm.cols=c("y"), m.type="razlika", n=N)$p)
moc2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
moc3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data.pois(n, b1, b2),
                               perm.cols=c("y"), m.type="razmerje", n=N)$p)
moc4 <- round(sum(pvred4<0.05)/m,3)

moci.pois <- data.frame("Moc testa" = c(moc1,moc2,moc3,moc4),
                   row.names = paste0("Test ",1:4))


dump(c("podatki","test1","test2","test3","test4",
       "velikosti.pois","moci.pois"), "rezultati_diskretna.R")

