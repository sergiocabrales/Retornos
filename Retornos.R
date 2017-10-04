# INGENIERÍA FINANCIERA
#################################################

# Se cargan las libreria o paquetes que se necesitan

library(timeDate)
library(timeSeries)
library(fBasics)
library(readr)

# Cargar los datos desde un archivo csv
COLCAP <- read_csv("COLCAP.csv")
View(COLCAP)

n <- length(COLCAP$Valor);
R_COLCAP <- log(COLCAP$Valor[-1]/COLCAP$Valor[-n])
N=length(R_COLCAP)

plot(1:N,R_COLCAP,"l")

hist(R_COLCAP)

K=kurtosis(R_COLCAP)
S=skewness(R_COLCAP)

#Autocorrelación de los retornos

autocorrelation=as.matrix(acf(R_COLCAP,lag.max=12,type=c("correlation"))$acf)

#Autocorrelación de los retornos al cuadrado (varianza)

retsquare=R_COLCAP^2

Autocorr_retsquare=as.matrix(acf(retsquare,lag.max=12,type=c("correlation"))$acf)

#Test de Normalidad

jarqueberaTest(R_COLCAP)
