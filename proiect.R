install.packages("e1071")
library(e1071)
install.packages("normalr")
library("normalr")
install.packages("scales")
library("scales")
install.packages("formattable")
library("formattable")
# BITCOIN -----------------------------------------------------------------
# Incarcare date Bitcoin 21/12/2017 - 21/12/2022
btc <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/BTC.csv")
btc$Data <- as.Date(btc$Data, format = "%d/%m/%Y")
summary(btc$Pret)
# Mediana = 10970
mean(btc$Pret)
# Media = 20323.89
sd(btc$Pret)
# Abaterea standard = 16985.25
sd(btc$Pret) / mean(btc$Pret)
# Coeficientul de variatie = 83.57%
# Media nu este reprezentativa pentru perioada studiata.
skewness(btc$Pret)
# Coeficientul de asimetrie = 1.027
# Distributia este asimetrica la dreapta.
kurtosis(btc$Pret)
# Coeficientul de aplatizare = -0.288
# Distributia este platicurtica.
hist(btc$Pret, col = "orange", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(btc$Pret, col = "orange", ylab = "Pre?? (USD)")
# Nu avem outlieri.

plot(btc$Data, btc$Pret, type = "l", col="orange", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabBTC <- c()
for(i in 2 : length(btc$Pret)){
  rentabBTC[i] <- log(btc$Pret[i] / btc$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(btc$Pret) - 1){
  rentabBTC[i] <- rentabBTC[i + 1]
}
rentabBTC
plot(btc$Data, rentabBTC, type = "l", col = "red")
percent(summary(rentabBTC))
# Mediana = 0.0988%
percent(mean(rentabBTC))
# Media = 0.0034%
percent(sd(rentabBTC))
# Abaterea standard = 3.95%
percent(sd(rentabBTC) / mean(rentabBTC))
# Coeficientul de variatie = 115870.75%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabBTC)
# Coeficientul de asimetrie = -1.03
# Distributia este asimetrica la dreapta.
kurtosis(rentabBTC)
# Coeficientul de aplatizare = 12.59693
# Distributia este leptocurtica.
hist(rentabBTC, col = "orange", xlab = "Rata de rentabilitate", ylab = "Nr. observa??ii", main =
       "")
boxplot(rentabBTC, col = "orange", ylab = "Rata de rentabilitate")

# ETHEREUM ----------------------------------------------------------------
# Incarcare date Ethereum 21/12/2017 - 21/12/2022
eth <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/ETH.csv")
eth$Data <- as.Date(eth$Data, format = "%d/%m/%Y")
summary(eth$Pret)
# Mediana = 518.89
mean(eth$Pret)
# Media = 1144.476
sd(eth$Pret)
# Abaterea standard = 1200.336
sd(eth$Pret) / mean(eth$Pret)
# Coeficientul de variatie = 104,88%
# Media nu este reprezentativa pentru perioada studiata.
skewness(eth$Pret)
# Coeficientul de asimetrie = 1.16
# Distributia este asimetrica la dreapta.
kurtosis(eth$Pret)
# Coeficientul de aplatizare = 0.22
# Distributia este platicurtica.
hist(eth$Pret, col = "gray", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(eth$Pret, col = "gray", ylab = "Pre?? (USD)")
# Avem outlieri pe valorile mari.
# Identificam outlierii.
a = which(eth$Pret > quantile(eth$Pret)[4] + 1.5 * IQR(eth$Pret), arr.ind = TRUE)
outethpret <- data.frame(eth$Data[a],eth$Pret[a])
colnames(outethpret) <- c("Data", "Pret")
outethpret
plot(eth$Data, eth$Pret, type = "l", col="gray", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabETH <- c()
for(i in 2 : length(eth$Pret)){
  rentabETH[i] <- log(eth$Pret[i] / eth$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(eth$Pret) - 1){
  rentabETH[i] <- rentabETH[i + 1]
}
rentabETH
plot(eth$Data, rentabETH, type = "l", col = "red")
percent(summary(rentabETH))
# Mediana = 0.075%
percent(mean(rentabETH))
# Media = 0.21%
percent(sd(rentabETH))
# Abaterea standard = 5.12%
percent(sd(rentabETH) / mean(rentabETH))
# Coeficientul de variatie = 23922.33%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabETH)
# Coeficientul de asimetrie = -0.998
# Distributia este asimetrica la dreapta.
kurtosis(rentabETH)
# Coeficientul de aplatizare = 9.72
# Distributia este leptocurtica.
hist(rentabETH, col = "gray", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabETH, col = "gray", ylab = "Rentabilitate")

# CARDANO -----------------------------------------------------------------
# Incarcare date Cardano 01/10/2017 - 23/12/2019
ada <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/ADA.csv")
ada$Data <- as.Date(ada$Data, format = "%d/%m/%Y")
summary(ada$Pret)
# Mediana = 0.16
mean(ada$Pret)
# Media = 0.499
sd(ada$Pret)
# Abaterea standard = 0.63
sd(ada$Pret) / mean(ada$Pret)
# Coeficientul de variatie = 126%
# Media nu este reprezentativa pentru perioada studiata.
skewness(ada$Pret)
# Coeficientul de asimetrie = 1.60
# Distributia este asimetrica la dreapta.
kurtosis(ada$Pret)
# Coeficientul de aplatizare = 1.90
# Distributia este platicurtica.
hist(ada$Pret, col = "blue", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(ada$Pret, col = "blue", ylab = "Pre?? (USD)")
# Avem outlieri pe valorile mari.
# Identificam outlierii.
a = which(ada$Pret > quantile(ada$Pret)[4] + 1.5 * IQR(ada$Pret), arr.ind = TRUE)
outadapret <- data.frame(ada$Data[a],ada$Pret[a])
colnames(outadapret) <- c("Data", "Pret")
outadapret
plot(ada$Data, ada$Pret, type = "l", col="blue", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabADA <- c()
for(i in 2 : length(ada$Pret)){
  rentabADA[i] <- log(ada$Pret[i] / ada$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(ada$Pret) - 1){
  rentabADA[i] <- rentabADA[i + 1]
}
rentabADA
plot(ada$Data, rentabADA, type = "l", col = "blue")
percent(summary(rentabADA))
# Mediana = -0.036%
percent(mean(rentabADA))
# Media = -0.04%
percent(sd(rentabADA))
# Abaterea standard = 5.96%
percent(sd(rentabADA) / mean(rentabADA))
# Coeficientul de variatie = -16492.50%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabADA)
# Coeficientul de asimetrie = -0.002
# Distributia este usor asimetrica la stanga.
kurtosis(rentabADA)
# Coeficientul de aplatizare = 5.28
# Distributia este leptocurtica.
hist(rentabADA, col = "blue", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabADA, col = "blue", ylab = "Rentabilitate")

# BINANCE COIN ------------------------------------------------------------
# Incarcare date Binance Coin 21/12/2017 - 21/12/2022
bnb <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/BNB.csv")
bnb$Data <- as.Date(bnb$Data, format = "%d/%m/%Y")
summary(bnb$Pret)
# Mediana = 27.089
mean(bnb$Pret)
# Media = 149.60
sd(bnb$Pret)
# Abaterea standard = 183.98
sd(bnb$Pret) / mean(bnb$Pret)
# Coeficientul de variatie = 122.97%
# Media nu este reprezentativa pentru perioada studiata.
skewness(bnb$Pret)
# Coeficientul de asimetrie = 0.98
# Distributia este asimetrica la dreapta.
kurtosis(bnb$Pret)
# Coeficientul de aplatizare = -0.41
# Distributia este platicurtica.
hist(bnb$Pret, col = "orange", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(bnb$Pret, col = "orange", ylab = "Pre?? (USD)")
# Nu avem outlieri.
plot(bnb$Data, bnb$Pret, type = "l", col="orange", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabBNB <- c()
for(i in 2 : length(bnb$Pret)){
  rentabBNB[i] <- log(bnb$Pret[i] / bnb$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(bnb$Pret) - 1){
  rentabBNB[i] <- rentabBNB[i + 1]
}
rentabBNB
plot(bnb$Data, rentabBNB, type = "l", col = "red")
percent(summary(rentabBNB))
# Mediana = 0.066%
percent(mean(rentabBNB))
# Media = 0.21%
percent(sd(rentabBNB))
# Abaterea standard = 5.81%
percent(sd(rentabBNB) / mean(rentabBNB))
# Coeficientul de variatie = 2760.04%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabBNB)
# Coeficientul de asimetrie = 0.33
# Distributia este asimetrica la dreapta.
kurtosis(rentabBNB)
# Coeficientul de aplatizare = 15.67
# Distributia este leptocurtica.
hist(rentabBNB, col = "orange", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabBNB, col = "orange", ylab = "Rentabilitate")

# TETHER ------------------------------------------------------------
# Incarcare date Tether 21/12/2017 - 21/12/2022
usdt <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/USDT.csv")
usdt$Data <- as.Date(usdt$Data, format = "%d/%m/%Y")
summary(usdt$Pret)
# Mediana = 1.0005
mean(usdt$Pret)
# Media = 1.0014
sd(usdt$Pret)
# Abaterea standard = 0.0053
sd(usdt$Pret) / mean(usdt$Pret)
# Coeficientul de variatie = 0.52%
# Media este reprezentativa pentru perioada studiata.
skewness(usdt$Pret)
# Coeficientul de asimetrie = 1.11
# Distributia este asimetrica la dreapta.
kurtosis(usdt$Pret)
# Coeficientul de aplatizare = 15.33
# Distributia este leptocurtica.
hist(usdt$Pret, col = "magenta", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(usdt$Pret, col = "magenta", ylab = "Pre?? (USD)")
#Avem outlieri.
# Identificam outlierii.
a = which(usdt$Pret > quantile(usdt$Pret)[4] + 1.5 * IQR(usdt$Pret), arr.ind = TRUE)
outusdtpret <- data.frame(usdt$Data[a],usdt$Pret[a])
colnames(outusdtpret) <- c("Data", "Pret")
outusdtpret

plot(usdt$Data, usdt$Pret, type = "l", col="magenta", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabUSDT <- c()
for(i in 2 : length(usdt$Pret)){
  rentabUSDT[i] <- log(usdt$Pret[i] / usdt$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(usdt$Pret) - 1){
  rentabUSDT[i] <- rentabUSDT[i + 1]
}
rentabUSDT
plot(usdt$Data, rentabUSDT, type = "l", col = "magenta")
percent(summary(rentabUSDT))
# Mediana = -0.0006%
percent(mean(rentabUSDT))
# Media = -0.0005%
percent(sd(rentabUSDT))
# Abaterea standard = 0.40%
percent(sd(rentabUSDT) / mean(rentabUSDT))
# Coeficientul de variatie = -80871.02%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabUSDT)
# Coeficientul de asimetrie = 0.38
# Distributia este asimetrica la dreapta.
kurtosis(rentabUSDT)
# Coeficientul de aplatizare = 39.03
# Distributia este leptocurtica.
hist(rentabUSDT, col = "magenta", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabUSDT, col = "magenta", ylab = "Rentabilitate")

# RIPPLE ------------------------------------------------------------
# Incarcare date Ripple 21/12/2017 - 21/12/2022
xrp <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/XRP.csv")
xrp$Data <- as.Date(xrp$Data, format = "%d/%m/%Y")
summary(xrp$Pret)
# Mediana = 0.3963
mean(xrp$Pret)
# Media = 0.5296
sd(xrp$Pret)
# Abaterea standard = 0.366
sd(xrp$Pret) / mean(xrp$Pret)
# Coeficientul de variatie = 69.28%
# Media este nereprezentativa pentru perioada studiata.
skewness(xrp$Pret)
# Coeficientul de asimetrie = 2.34
# Distributia este asimetrica la dreapta.
kurtosis(xrp$Pret)
# Coeficientul de aplatizare = 9.288
# Distributia este leptocurtica.
hist(xrp$Pret, col = "green", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(xrp$Pret, col = "green", ylab = "Pre?? (USD)")
#Avem outlieri pe valorile mari.
# Identificam outlierii.
a = which(xrp$Pret > quantile(xrp$Pret)[4] + 1.5 * IQR(xrp$Pret), arr.ind = TRUE)
outxrppret <- data.frame(xrp$Data[a],xrp$Pret[a])
colnames(outxrppret) <- c("Data", "Pret")
outxrppret

plot(xrp$Data, xrp$Pret, type = "l", col="green", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabXRP <- c()
for(i in 2 : length(xrp$Pret)){
  rentabXRP[i] <- log(xrp$Pret[i] / xrp$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(xrp$Pret) - 1){
  rentabXRP[i] <- rentabXRP[i + 1]
}
rentabXRP
plot(xrp$Data, rentabUSDT, type = "l", col = "green")
percent(summary(rentabXRP))
# Mediana = -0.11%
percent(mean(rentabXRP))
# Media = -0.07%
percent(sd(rentabXRP))
# Abaterea standard = 6.05%
percent(sd(rentabXRP) / mean(rentabXRP))
# Coeficientul de variatie = -8763.03%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabXRP)
# Coeficientul de asimetrie = 0.11
# Distributia este asimetrica la dreapta.
kurtosis(rentabXRP)
# Coeficientul de aplatizare = 13.18
# Distributia este leptocurtica.
hist(rentabXRP, col = "green", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabXRP, col = "green", ylab = "Rentabilitate")

# SOLANA ------------------------------------------------------------
# Incarcare date Solana 10/04/2020 - 21/12/2022
sol <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/SOL.csv")
sol$Data <- as.Date(sol$Data, format = "%d/%m/%Y")
summary(sol$Pret)
# Mediana = 32.08
mean(sol$Pret)
# Media = 51.44
sd(sol$Pret)
# Abaterea standard = 61.13
sd(sol$Pret) / mean(sol$Pret)
# Coeficientul de variatie = 118.83%
# Media este nereprezentativa pentru perioada studiata.
skewness(sol$Pret)
# Coeficientul de asimetrie = 1.42
# Distributia este asimetrica la dreapta.
kurtosis(sol$Pret)
# Coeficientul de aplatizare = 1.04
# Distributia este platicurtica.
hist(sol$Pret, col = "#00FFFF", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(sol$Pret, col = "#00FFFF", ylab = "Pre?? (USD)")
#Avem outlieri pe valorile mari.
# Identificam outlierii.
a = which(sol$Pret > quantile(sol$Pret)[4] + 1.5 * IQR(sol$Pret), arr.ind = TRUE)
outsolpret <- data.frame(sol$Data[a],sol$Pret[a])
colnames(outsolpret) <- c("Data", "Pret")
outsolpret

plot(sol$Data, sol$Pret, type = "l", col="#00FFFF", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabSOL <- c()
for(i in 2 : length(sol$Pret)){
  rentabSOL[i] <- log(sol$Pret[i] / sol$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(sol$Pret) - 1){
  rentabSOL[i] <- rentabSOL[i + 1]
}
rentabSOL
plot(sol$Data, rentabSOL, type = "l", col = "#00FFFF")
percent(summary(rentabSOL))
# Mediana = 0,06%
percent(mean(rentabSOL))
# Media = -0.25%
percent(sd(rentabSOL))
# Abaterea standard = 7.94%
percent(sd(rentabSOL) / mean(rentabSOL))
# Coeficientul de variatie = 3120.68%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabSOL)
# Coeficientul de asimetrie = -0.36
# Distributia este asimetrica la stanga.
kurtosis(rentabSOL)
# Coeficientul de aplatizare = 5.39
# Distributia este leptocurtica.
hist(rentabSOL, col = "#00FFFF", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabSOL, col = "#00FFFF", ylab = "Rentabilitate")

# POLKADOT ------------------------------------------------------------
# Incarcare date Polkadot 20/08/2020 - 21/12/2022
dot <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/DOT.csv")
dot$Data <- as.Date(dot$Data, format = "%d/%m/%Y")
summary(dot$Pret)
# Mediana = 16.223
mean(dot$Pret)
# Media = 18.06
sd(dot$Pret)
# Abaterea standard = 12.67
sd(dot$Pret) / mean(dot$Pret)
# Coeficientul de variatie = 70.15%
# Media este nereprezentativa pentru perioada studiata.
skewness(dot$Pret)
# Coeficientul de asimetrie = 0.68
# Distributia este asimetrica la dreapta.
kurtosis(dot$Pret)
# Coeficientul de aplatizare = -0.64
# Distributia este platicurtica.
hist(dot$Pret, col = "#FF6600", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(dot$Pret, col = "#FF6600", ylab = "Pre?? (USD)")
#Nu avem outlieri pe valorile mari.

plot(dot$Data, dot$Pret, type = "l", col="#FF6600", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabDOT <- c()
for(i in 2 : length(dot$Pret)){
  rentabDOT[i] <- log(dot$Pret[i] / dot$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(dot$Pret) - 1){
  rentabDOT[i] <- rentabDOT[i + 1]
}
rentabDOT
plot(dot$Data, rentabDOT, type = "l", col = "#FF6600")
percent(summary(rentabDOT))
# Mediana = -0.082%
percent(mean(rentabDOT))
# Media = 0.05%
percent(sd(rentabDOT))
# Abaterea standard = 6.68%
percent(sd(rentabDOT) / mean(rentabDOT))
# Coeficientul de variatie = 14388.34%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabDOT)
# Coeficientul de asimetrie = 0.21
# Distributia este asimetrica la dreapta.
kurtosis(rentabDOT)
# Coeficientul de aplatizare = 7.24
# Distributia este leptocurtica.
hist(rentabDOT, col = "#FF6600", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabDOT, col = "#FF6600", ylab = "Rentabilitate")

# USD COIN ------------------------------------------------------------
# Incarcare date USD Coin 08/10/2018 - 21/12/2022
usdc <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/USDC.csv")
usdc$Data <- as.Date(usdc$Data, format = "%d/%m/%Y")
summary(usdc$Pret)
# Mediana = 1.0002
mean(usdc$Pret)
# Media = 1.002
sd(usdc$Pret)
# Abaterea standard = 0.005
sd(usdc$Pret) / mean(usdc$Pret)
# Coeficientul de variatie = 0.57%
# Media este reprezentativa pentru perioada studiata.
skewness(usdc$Pret)
# Coeficientul de asimetrie = 2.17
# Distributia este asimetrica la dreapta.
kurtosis(usdc$Pret)
# Coeficientul de aplatizare = 8.93
# Distributia este leptocurtica.
hist(usdc$Pret, col = "yellow", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(usdc$Pret, col = "yellow", ylab = "Pre?? (USD)")
#Avem outlieri pe valorile mari, cat si pe cele mici.
# Identificam outlierii.
a = which(usdc$Pret > quantile(usdc$Pret)[4] + 1.5 * IQR(usdc$Pret), arr.ind = TRUE)
outusdcpret <- data.frame(usdc$Data[a],usdc$Pret[a])
colnames(outusdcpret) <- c("Data", "Pret")
outusdcpret

plot(usdc$Data, usdc$Pret, type = "l", col="yellow", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabUSDC <- c()
for(i in 2 : length(usdc$Pret)){
  rentabUSDC[i] <- log(usdc$Pret[i] / usdc$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(usdc$Pret) - 1){
  rentabUSDC[i] <- rentabUSDC[i + 1]
}
rentabUSDC
plot(usdc$Data, rentabUSDC, type = "l", col = "yellow")
percent(summary(rentabUSDC))
# Mediana = 0,0014%
percent(mean(rentabUSDC))
# Media = -0.00015%
percent(sd(rentabUSDC))
# Abaterea standard = 0.37%
percent(sd(rentabUSDC) / mean(rentabUSDC))
# Coeficientul de variatie = -244236.11%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabUSDC)
# Coeficientul de asimetrie = 0.53
# Distributia este asimetrica la dreapta.
kurtosis(rentabUSDC)
# Coeficientul de aplatizare = 30.34
# Distributia este leptocurtica.
hist(rentabUSDC, col = "yellow", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabUSDC, col = "yellow", ylab = "Rentabilitate")
# TERRA ------------------------------------------------------------
# Incarcare date TERRA 29/05/2022 - 21/12/2022
luna <- read.csv("C:/Users/asus/Desktop/Cibernetica sistemelor economice/LUNA.csv")
luna$Data <- as.Date(luna$Data, format = "%d/%m/%Y")
summary(luna$Pret)
# Mediana = 2.066
mean(luna$Pret)
# Media = 2.35
sd(luna$Pret)
# Abaterea standard = 1.21
sd(luna$Pret) / mean(luna$Pret)
# Coeficientul de variatie = 51.55%
# Media este nereprezentativa pentru perioada studiata.
skewness(luna$Pret)
# Coeficientul de asimetrie = 3.67
# Distributia este asimetrica la dreapta.
kurtosis(luna$Pret)
# Coeficientul de aplatizare = 16.46
# Distributia este leptocurtica.
hist(luna$Pret, col = "#999999", xlab = "Pre?? (USD)", ylab = "Nr. observa??ii", main = "")
boxplot(sol$Pret, col = "#999999", ylab = "Pre?? (USD)")
#Avem outlieri pe valorile mari.
# Identificam outlierii.
a = which(luna$Pret > quantile(luna$Pret)[4] + 1.5 * IQR(luna$Pret), arr.ind = TRUE)
outlunapret <- data.frame(luna$Data[a],luna$Pret[a])
colnames(outlunapret) <- c("Data", "Pret")
outlunapret

plot(luna$Data, luna$Pret, type = "l", col="#999999", xlab = "Data", ylab = "Pre?? (USD)")
# Calcul rentabilitate
rentabLUNA <- c()
for(i in 2 : length(luna$Pret)){
  rentabLUNA[i] <- log(luna$Pret[i] / luna$Pret[i - 1])
}
#Eliminam valoarea NA
for(i in 1 : length(luna$Pret) - 1){
  rentabLUNA[i] <- rentabLUNA[i + 1]
}
rentabLUNA
plot(luna$Data, rentabLUNA, type = "l", col = "#999999")
percent(summary(rentabLUNA))
# Mediana = -0.3%
percent(mean(rentabLUNA))
# Media = -0.8%
percent(sd(rentabLUNA))
# Abaterea standard = 12.41%
percent(sd(rentabLUNA) / mean(rentabLUNA))
# Coeficientul de variatie = -1560.14%
# Media nu este reprezentativa pentru perioada studiata.
skewness(rentabLUNA)
# Coeficientul de asimetrie = 2.38
# Distributia este asimetrica la dreapta.
kurtosis(rentabLUNA)
# Coeficientul de aplatizare = 23.85
# Distributia este leptocurtica.
hist(rentabLUNA, col = "#999999", xlab = "Rentabilitate", ylab = "Nr. observa??ii", main = "")
boxplot(rentabLUNA, col = "#999999", ylab = "Rentabilitate")
