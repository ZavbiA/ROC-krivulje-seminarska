###########################################################################
###########################################################################
###                                                                     ###
###               PODATKI, SIMULACIJE IN LASTNOSTI TESTOV               ###
###                                                                     ###
###########################################################################
###########################################################################

source("lib.r")
source("fun.r")

##----------------------------------------------------------------
##                         Porazdelitve                         --
##----------------------------------------------------------------

n<-100; mu1<-0; mu2<-2; ro<-0.4; b1<-6; b2<-2
podatki <- get.data(n, mu1, mu2, ro, b1, b2)

por1 <- porazdelitev(podatki, perm.cols=c("X1","X2"), m.type="razlika", n=5000)
por2 <- porazdelitev(podatki, perm.cols=c("y"), m.type="razlika", n=5000)
por3 <- porazdelitev(podatki, perm.cols=c("X1","X2"), m.type="razmerje", n=5000)
por4 <- porazdelitev(podatki, perm.cols=c("y"), m.type="razmerje", n=5000)

# Določimo podatke pod H0
n <- 100; mu1 <- 0; mu2 <-2; ro <-0.4; b1 <- 3; b2 <- 3

# Porazdelitve pod ničelno domnevo
podatki2 <- get.data(n, mu1, mu2, ro, b1, b2)
por21 <- porazdelitev(podatki2, perm.cols=c("X1","X2"), m.type="razlika", n=1000)
por22 <- porazdelitev(podatki2, perm.cols=c("y"), m.type="razlika", n=1000)
por23 <- porazdelitev(podatki2, perm.cols=c("X1","X2"), m.type="razmerje", n=1000)
por24 <- porazdelitev(podatki2, perm.cols=c("y"), m.type="razmerje", n=1000)



dump(c("por1","por2","por3","por4","por21","por22","por23","por24"), "porazdelitve.R")
dump(c("podatki","podatki2"), "podatki.R")

##----------------------------------------------------------------
##                        Velikost testa                        --
##----------------------------------------------------------------

n <- 100; mu1 <- 0; mu2 <-2; ro <-0.4; b1 <- 3; b2 <- 3

# Test 1
pvred1 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                  por21)$p)
vel1 <- round(sum(pvred1<0.05)/1000,2)

# Test 2
pvred2 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                  por22)$p)
vel2 <- round(sum(pvred2<0.05)/1000,2)

# Test 3
pvred3 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                  por23)$p)
vel3 <- round(sum(pvred3<0.05)/1000,2)

# Test 4
pvred4 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                  por24)$p)
vel4 <- round(sum(pvred4<0.05)/1000,2)

velikosti <- data.frame("Velikost" = c(vel1,vel2,vel3,vel4),
                        row.names = paste0("Test ",1:4))

##---------------------------------------------------------------
##                          Moč testa                          --
##---------------------------------------------------------------

n <- 100; mu1 <- 0; mu2 <-2; ro <-0.4; b1 <- 6; b2 <- 2

# Porazdelitve: uporabimo tiste od prej

# Test 1
pvred21 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                   por1)$p)
moc1 <- round(sum(pvred21<0.05)/1000,2)

# Test 2
pvred22 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                   por2)$p)
moc2 <- round(sum(pvred22<0.05)/1000,2)

# Test 3
pvred23 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                   por3)$p)
moc3 <- round(sum(pvred23<0.05)/1000,2)

# Test 4
pvred24 <- replicate(1000,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                                   por4)$p)
moc4 <- round(sum(pvred24<0.05)/1000,2)

moci <- data.frame("Moč testa" = c(moc1,moc2,moc3,moc4),
                   row.names = paste0("Test ",1:4))

dump(c("velikosti","moci"), "lastnostiTestov.R")
