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
##                         KORELACIJA: 0                        --
##----------------------------------------------------------------

## TESTI ZA NORMALNE PODATKE
mu1<-0; mu2<-0; ro<-0; b1<-6; b2<-2

podatki <- get.data(n, mu1, mu2, ro, b1, b2)

test.kor01 <- testiraj(podatki, perm.cols=c("X1","X2"), m.type="razlika", n=N)
test.kor02 <- testiraj(podatki, perm.cols=c("y"), m.type="razlika", n=N)
test.kor03 <- testiraj(podatki, perm.cols=c("X1","X2"), m.type="razmerje", n=N)
test.kor04 <- testiraj(podatki, perm.cols=c("y"), m.type="razmerje", n=N)


##----------------------------------------------------------------
##                 Velikost testa NORMAL                        --
##----------------------------------------------------------------

# Podatki pod H0
mu1 <- 0; mu2 <-0; ro <-0; b1 <- 3; b2 <- 3

# Test 1
pvred1 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
vel1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("y"), m.type="razlika", n=N)$p)
vel2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
vel3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("y"), m.type="razmerje", n=N)$p)
vel4 <- round(sum(pvred4<0.05)/m,3)

velikosti.kor0 <- data.frame("Velikost" = c(vel1,vel2,vel3,vel4),
                             row.names = paste0("Test ",1:4))

##---------------------------------------------------------------
##                    Moč testa  NORMAL                        --
##---------------------------------------------------------------

# Podatki pod H1 (isti kot prej)
mu1 <- 0; mu2 <-0; ro <-0; b1 <- 6; b2 <- 2

# Test 1
pvred1 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razlika", n=N)$p)
moc1 <- round(sum(pvred1<0.05)/m,3)

# Test 2
pvred2 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("y"), m.type="razlika", n=N)$p)
moc2 <- round(sum(pvred2<0.05)/m,3)

# Test 3
pvred3 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("X1","X2"), m.type="razmerje", n=N)$p)
moc3 <- round(sum(pvred3<0.05)/m,3)

# Test 4
pvred4 <- replicate(m,testiraj(get.data(n, mu1, mu2, ro, b1, b2),
                               perm.cols=c("y"), m.type="razmerje", n=N)$p)
moc4 <- round(sum(pvred4<0.05)/m,3)

moci.kor0 <- data.frame("Moc testa" = c(moc1,moc2,moc3,moc4),
                        row.names = paste0("Test ",1:4))


dump(c("podatki.kor0","test1.kor0","test2.kor0","test3.kor0","test4.kor0",
       "velikosti.kor0","moci.kor0"), "rezultati_kor0.R")






