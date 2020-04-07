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
##                         GAMMA                                --
##----------------------------------------------------------------


## TESTI ZA GAMMA

n<-100; b1<-6; b2<-2
podatki.gamma <- get.data.gamma(n, b1, b2)

test.gamma1 <- testiraj(podatki.gamma, perm.cols=c("X1","X2"), m.type="razlika", n=N)
test.gamma2 <- testiraj(podatki.gamma, perm.cols=c("y"), m.type="razlika", n=N)
test.gamma3 <- testiraj(podatki.gamma, perm.cols=c("X1","X2"), m.type="razmerje", n=N)
test.gamma4 <- testiraj(podatki.gamma, perm.cols=c("y"), m.type="razmerje", n=N)

##----------------------------------------------------------------
##                 Velikost testa GAMMA                         --
##----------------------------------------------------------------

# Podatki pod H0
b1 <- 3; b2 <- 3

# Test 1
pvred1 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
vel1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("y"), m.type="razlika", n=N)$p)
vel2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
vel3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("y"), m.type="razmerje", n=N)$p)
vel4 <- round(sum(pvred4<0.05)/m,3)

velikosti.gamma <- data.frame("Velikost" = c(vel1,vel2,vel3,vel4),
                              row.names = paste0("Test ",1:4))

##---------------------------------------------------------------
##                    Moč testa  GAMMA                         --
##---------------------------------------------------------------

# Podatki pod H1 (isti kot prej)
b1 <- 6; b2 <- 2

# Test 1
pvred1 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
moc1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("y"), m.type="razlika", n=N)$p)
moc2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
moc3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data.gamma(n, b1, b2),
                               perm.cols=c("y"), m.type="razmerje", n=N)$p)
moc4 <- round(sum(pvred4<0.05)/m,3)

moci.gamma <- data.frame("Moc testa" = c(moc1,moc2,moc3,moc4),
                         row.names = paste0("Test ",1:4))


dump(c("podatki.gamma","test.gamma1","test.gamma2","test.gamma3","test.gamma4",
       "velikosti.gamma","moci.gamma"), "rezultati_gamma.R")