library(phangorn)

#Algoritem funkcije F

#Izračuna pričakovano vrednost naključne projekcije, ki
#je odvisna le od dimenzije prostora.

#Vhod: dimenzija prostora označena z d
#Izhod: pričakovana vrednost naključne projekcije

F <- function(d) {
  dfactorial(d-2) / dfactorial(d-1) * sqrt(2/pi)
}

#Algoritem za naključen vektor

#Ustvari naključen vektor dimenzije 2, pri čemer sta
#njegovi komponenti porazdeljeni z normalno porazdelitvijo
#N(0, 1) in ju izračunamo z Box-Mullerjevo metodo.

#Vhod:
#Izhod: naključen vektor dimenzije 2

Nakljucen_vektor <- function() {
  U <- runif(1)
  V <- runif(1)
  M <- sqrt(-2*log(U)) * cos(2*pi*V)
  N <- sqrt(-2*log(U)) * sin(2*pi*V)
  return(c(M,N)/norm(c(M,N), type="2"))
}

#Algoritem za izračun približka razdalje povprečne razdalje med točkami

#Izračuna približek povprečne razdalje med točkami v matriki S.

#Vhod: tabela točk oz. matrika S, zahtevana natančnost e
#Izhod: približek povprečne razdalje med pari točk v S

Priblizek_povprecne_razdalje <- function(S, e=0.1) {
  d <- ncol(S)
  n <- nrow(S)
  f <- F(d)
  m <- ceiling(1/e^2)
  skupna_razdalja <- 0
  #  print(list(n,d,f,m))
  for (i in 1:m) {
    r <- Nakljucen_vektor()
    S_r <- sort(S %*% r)
    skupna_razdalja <- skupna_razdalja + sum(S_r * seq(1-n, n-1, 2))
  }
  return(2*skupna_razdalja/(f*n^2*m))
}

#Algoritem za izračun točne povprečne razdalje med točkami

#Izračuna točno povprečno razdaljo med točkami v matriki S.

#Vhod: tabela točk oz. matrika S
#Izhod: povprečna razdalja med pari točk v S

Tocna_povprecna_razdalja <- function(S) {
  s <- 0
  n <- dim(S)[1]
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      s <- s + sqrt(sum((S[i,] - S[j,])^2))
    }
  }
  povprecna_razdalja <- 2*s/(n^2)
  return(list(povprecna_razdalja))
}

#Generiranje naključnih točk

Nakljucne_tocke <- function(b = 20, c = -100.0, d = 100.0) {
  stevilo_tock <- b
  S <- matrix(runif(2*stevilo_tock, c, d),
              nrow=stevilo_tock, ncol=2)
  return(S)
}

#Koliko ponovitev je potrebnih za želeno natančnost

Test_natancnosti <- function(S, napaka) {
  tocna <- as.numeric(Tocna_povprecna_razdalja(S))
  priblizek <- 0
  priblizek2 <- 0
  stevec <- 0
  while (abs((priblizek/tocna) - 1) > napaka && stevec < 50) {
    stevec <- stevec + 1
    priblizek2 <- priblizek2 + Priblizek_povprecne_razdalje(S)
    priblizek <- priblizek2/stevec
  }
  return(stevec)
}

#Vrne vektor s številom ponovitev v 100 različnih ponovitvah testa natančnosti

Test <- function(S, napaka = 0.02, dolzina = 100) {
  stevci <- c()
  for (i in 1:dolzina) {
    stevci[i] <- Test_natancnosti(S, napaka)
  }
  return(stevci)
}


#Preizkusi

#1.) Spreminjanje števila točk

pet_tock <- Nakljucne_tocke(5)
petnajst_tock <- Nakljucne_tocke(15)
petdeset_tock <- Nakljucne_tocke(50)
sto_tock <- Nakljucne_tocke(100)

napaka <- 0.02
 
test5 <- Test(pet_tock, napaka)
test15 <- Test(petnajst_tock, napaka)
test50 <- Test(petdeset_tock, napaka)
test100 <- Test(sto_tock, napaka)
 
hist(test5, breaks=c(0:max(test5)),main='Spreminjanje števila točk: 5 točk',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test15, breaks=c(0:max(test15)),main='Spreminjanje števila točk: 15 točk',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test50, breaks=c(0:max(test50)),main='Spreminjanje števila točk: 50 točk',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test100, breaks=c(0:max(test100)),main='Spreminjanje števila točk: 100 točk',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')

#aggregate(data.frame(count = test5), list(value = test5), length)


#2.) Spreminjanje števila točk

napaka1 <- 0.05
napaka2 <- 0.03
napaka3 <- 0.02
napaka4 <- 0.01

tocke <- Nakljucne_tocke(10)

test1 <- Test(tocke, napaka1)
test2 <- Test(tocke, napaka2)
test3 <- Test(tocke, napaka3)
test4 <- Test(tocke, napaka4)

hist(test1, breaks=c(0:max(test1)),main='Spreminjanje natančnosti: 5%',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test2, breaks=c(0:max(test2)),main='Spreminjanje natančnosti: 3%',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test3, breaks=c(0:max(test3)),main='Spreminjanje natančnosti: 2%',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')
hist(test4, breaks=c(0:max(test4)),main='Spreminjanje natančnosti: 1%',xlab='Število ponovitev',ylab='Frekvenca', col='paleturquoise3')



