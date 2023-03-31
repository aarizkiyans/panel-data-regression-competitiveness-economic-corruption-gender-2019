rm(list = ls())

#input data
library(readxl)

dataku2 <- read_excel("C:/Users/Rzyns/Desktop/Belajar R/dataku2.xlsx")
View(dataku2)

str(dataku2)

names(dataku2)

head(dataku2)

nrow(dataku2)

ncol(dataku2)

summary(dataku2)

summary(data2)
NROW(na.omit(data))
require(dplyr)

dataku2 %>% count(NGR)
df <- dataku2[-1:-2]
df

#korelasi
library(corrplot)
corrl<-cor(df)
corrl
corrplot(corrl, method = 'number')

library(gplots)
plotmeans(DS~TH , main = 'Heterogeneity Across Airline', data = dataku2)
install.packages("corrplot")
library(corrplot)
corrplot::corrplot(corrl, method = "number")

#Uji CEM
install.packages("plm")
library(plm)
ols <- plm(DS ~ KG + KE + PK, data = dataku2, model = "pooling")
summary(ols)

#Uji FEM
library(plm)
fixed.dum <- lm(DS ~ KG + KE + PK + factor(NGR)-1, data = dataku2)
summary(fixed.dum)
fixed <- plm(DS ~ KG + KE + PK,data = dataku2, effect = "individual", model = "within")
summary(fixed)

#Uji Chow
pFtest(fixed,ols)

#Uji REM
random <- plm(DS ~ KG + KE + PK,data =dataku2, index = c("NGR","TH"),model ="random")
summary(random)

#Uji Haussmann
phtest(random, fixed)

#Uji Heteroskesdasititas
library(lmtest)
bptest(DS ~ KG + KE + PK+factor(NGR)+factor(TH), data=dataku2, studentize=F)
lmMod <- lm(DS ~ KG + KE + PK+factor(NGR)+factor(TH), data=dataku2)
plot(lmMod)
modelreg <- lm(DS ~ KG + KE + PK, data=dataku2)
bptest(modelreg)
lmtest::bptest(modelreg)

