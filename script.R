install.packages("nortest")
install.packages("rapportools")
library(readr)
library("nortest")
library("rapportools")

g6 <- read_delim("g6.csv", ";", escape_double = FALSE, 
                 trim_ws = TRUE)
aov <- read_delim("aov.csv", ";", escape_double = FALSE, 
                  +     col_types = cols(var = col_factor(levels = c("norma", 
                                                                     +         "geo", "flot"))), trim_ws = TRUE)
g6.aov <- aov
d300<-g6

sum_d300<-summary(d300)
tab_d300<-table(d300$norma)
View(sum_d300)

summary(d300)
pairs(d300)

#Anális exploratorio
#A un n.s. del 5%=0.05
#H0: Los datos tienen una distribución normal
#H1: Los datos no tienen una distribución normal

shapiro.test(d300$norma)
ad.test(d300$norma)

shapiro.test(d300$geo)
ad.test(d300$geo)

shapiro.test(d300$flot)
ad.test(d300$flot)


#Anális exploratorio
#A un n.s. del 5%=0.05
#H0: No existe diferencia significativa entre las varianzas
#H1: Existe diferencia significativa entre las varianzas

a<-d300$norma
b<-d300$geo
c<-d300$flot


bartlett.test(list(a,b,c))

#ANOVA

anova<- aov(densidad ~ var, data=g6.aov)
summary(anova)

#Test de comparación
#Tukey

TukeyHSD(anova)