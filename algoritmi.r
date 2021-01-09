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

Nakljucne_tocke <- function() {
  stevilo_tock <- sample(1:100, 1)
  S <- matrix(runif(2*stevilo_tock, -100.0, 100.0),
              nrow=stevilo_tock, ncol=2)
  return(S)
}







