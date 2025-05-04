#definir diret?rio de trabalho e importar dados da tabela excel
setwd("C:/Users/Larissa/OneDrive - unb.br/Materias/Processamento de imagens/trabalho/dados")
setwd("C:/Users/l_v_v/OneDrive - unb.br/Materias/Processamento de imagens/trabalho/dados")
library(readxl)
#reservatorios
serra_mesa <- as.data.frame(read.csv('serra_mesa.csv'))
cana_brava <- as.data.frame(read.csv('cana_brava.csv'))
sao_salv <- as.data.frame(read.csv('sao_salvador.csv'))
peixe_ang <- as.data.frame(read.csv('peixe_angical.csv'))
lajeado <- as.data.frame(read.csv('lajeado.csv'))
estreito <- as.data.frame(read.csv('estreito.csv'))
tucurui <- as.data.frame(read.csv('tucurui.csv'))

#libraries
library(funtimes)
library(dplyr)
library(Kendall)
#library(tseries)

#Serra da Mesa


#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(serra_mesa, Outflow), start = 2000, end = 2020, frequency = 1)

tsarea <-  ts(select(serra_mesa, Area), start = 2000, end = 2020, frequency = 1)

tsprecipitation <- ts(select(serra_mesa, Precipitation), start = 2000, end = 2020, frequency = 1)

tsliq_evap <- ts(select(serra_mesa, Liq_evaporation), start = 2000, end = 2020, frequency = 1)

tsagriculture <- ts(select(serra_mesa, Agriculture), start = 2000, end = 2020, frequency = 1)

tsurban <- ts(select(serra_mesa, Urban), start = 2000, end = 2020, frequency = 1)

tsnatural <- ts(select(serra_mesa, Natural), start = 2000, end = 2020, frequency = 1)

tspasture <- ts(select(serra_mesa, Pasture), start = 2000, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend

plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa
notrend_test(tsoutflow, test = "WAVK", Window = 5)

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
notrend_test(tsarea)
MannKendall(tsarea)#n?o-significativa
notrend_test(tsarea, test = "MK")
notrend_test(tsarea, test = "WAVK", Window = 5)

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window =5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#n?o-significativa

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#n?o-significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2, ar.method = "yw")
MannKendall(tsurban)#n?o-significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#n?o-significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#n?o-significativa

#sub-set para 2000 a 2010 e 2010 a 2020
tsoutflow1<-ts(tsoutflow[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsoutflow1)
lines(lowess(time(tsoutflow1), tsoutflow1), lw=2, col = 2)
MannKendall(tsoutflow1)#n?o-significativa
notrend_test(tsoutflow1, test = "WAVK", Window = 5)
tsoutflow2<-ts(tsoutflow[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)#n?o-significativa
notrend_test(tsoutflow2, test = "MK")
notrend_test(tsoutflow2, test = "WAVK", Window = 5)


tsarea1<-ts(tsarea[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsarea1)
lines(lowess(time(tsarea1), tsarea1), lw=2, col = 2)
MannKendall(tsarea1)#n?o-significativa
tsarea2<-ts(tsarea[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)#n?o-significativa

tsprecipitation1<-ts(tsprecipitation[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsprecipitation1)
lines(lowess(time(tsprecipitation1), tsprecipitation1), lw=2, col = 2)
MannKendall(tsprecipitation1)#n?o-significativa
notrend_test(tsprecipitation1, test = "WAVK", Window = 5)
tsprecipitation2<-ts(tsprecipitation[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5)

tsliq_evap1<-ts(tsliq_evap[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsliq_evap1)
lines(lowess(time(tsliq_evap1), tsliq_evap1), lw=2, col = 2)
MannKendall(tsliq_evap1)#n?o-significativa
notrend_test(tsliq_evap1, test = "WAVK", Window = 10)
tsliq_evap2<-ts(tsliq_evap[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)#n?o-significativa

tsurban1<-ts(tsurban[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsurban1)
lines(lowess(time(tsurban1), tsurban1), lw=2, col = 2)
MannKendall(tsurban1)#n?o-significativa
tsurban2<-ts(tsurban[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)#n?o-significativa

tsnatura1<-ts(tsnatural[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsnatura1)
lines(lowess(time(tsnatura1), tsnatura1), lw=2, col = 2)
MannKendall(tsnatura1)#n?o-significativa
tsnatura2<-ts(tsnatural[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)#n?o-significativa

tsagriculture1<-ts(tsagriculture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsagriculture1)
lines(lowess(time(tsagriculture1), tsagriculture1), lw=2, col = 2)
MannKendall(tsagriculture1)#n?o-significativa
tsagriculture2<-ts(tsagriculture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)#n?o-significativa

tspasture1<-ts(tspasture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tspasture1)
lines(lowess(time(tspasture1), tspasture1), lw=2, col = 2)
MannKendall(tspasture1)#n?o-significativa
tspasture2<-ts(tspasture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)#n?o-significativa

#Cana Brava

#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(cana_brava, Outflow), start = 2003, end = 2020, frequency = 1)

tsarea <-  ts(select(cana_brava, Area), start = 2003, end = 2020, frequency = 1)

tsprecipitation <- ts(select(cana_brava, Precipitation), start = 2003, end = 2020, frequency = 1)

tsliq_evap <- ts(select(cana_brava, Liq_evaporation), start = 2003, end = 2020, frequency = 1)

tsagriculture <- ts(select(cana_brava, Agriculture), start = 2003, end = 2020, frequency = 1)

tsurban <- ts(select(cana_brava, Urban), start = 2003, end = 2020, frequency = 1)

tsnatural <- ts(select(cana_brava, Natural), start = 2003, end = 2020, frequency = 1)

tspasture <- ts(select(cana_brava, Pasture), start = 2003, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa
notrend_test(tsoutflow, test = "WAVK", Window = 5)

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#n?o-significativa
notrend_test(tsliq_evap, test = "WAVK", Window = 5)

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa
notrend_test(tsagriculture, test = "WAVK", Window = 5)

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2003 a 2010 e 2010 a 2020

tsoutflow2<-ts(tsoutflow[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)


tsarea2<-ts(tsarea[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)


tsprecipitation2<-ts(tsprecipitation[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5)


tsliq_evap2<-ts(tsliq_evap[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)
notrend_test(tsliq_evap2, test = "WAVK", Window = 5)

tsurban2<-ts(tsurban[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)


tsnatura2<-ts(tsnatural[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)


tsagriculture2<-ts(tsagriculture[8:18], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)

tspasture2<-ts(tspasture[8:18], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)#n?o-significativa
notrend_test(tspasture2, test = "WAVK", Window = 5)


#São Salvador
#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(sao_salv, Outflow), start = 2009, end = 2020, frequency = 1)

tsarea <-  ts(select(sao_salv, Area), start = 2009, end = 2020, frequency = 1)

tsprecipitation <- ts(select(sao_salv, Precipitation), start = 2009, end = 2020, frequency = 1)

tsliq_evap <- ts(select(sao_salv, Liq_evaporation), start = 2009, end = 2020, frequency = 1)

tsagriculture <- ts(select(sao_salv, Agriculture), start = 2009, end = 2020, frequency = 1)

tsurban <- ts(select(sao_salv, Urban), start = 2009, end = 2020, frequency = 1)

tsnatural <- ts(select(sao_salv, Natural), start = 2009, end = 2020, frequency = 1)

tspasture <- ts(select(sao_salv, Pasture), start = 2009, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#n?o-significativa
notrend_test(tsliq_evap, test = "WAVK", Window = 5)

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2010 a 2020
tsoutflow2<-ts(tsoutflow[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)

tsarea2<-ts(tsarea[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)

tsprecipitation2<-ts(tsprecipitation[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5)

tsliq_evap2<-ts(tsliq_evap[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)
notrend_test(tsliq_evap2, test = "WAVK", Window = 5)

tsurban2<-ts(tsurban[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura2<-ts(tsnatural[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture2<-ts(tsagriculture[2:12], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)

tspasture2<-ts(tspasture[2:12], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)

#Peixe angical
#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(peixe_ang, Outflow), start = 2006, end = 2020, frequency = 1)

tsarea <-  ts(select(peixe_ang, Area), start = 2006, end = 2020, frequency = 1)

tsprecipitation <- ts(select(peixe_ang, Precipitation), start = 2006, end = 2020, frequency = 1)

tsliq_evap <- ts(select(peixe_ang, Liq_evaporation), start = 2006, end = 2020, frequency = 1)

tsagriculture <- ts(select(peixe_ang, Agriculture), start = 2006, end = 2020, frequency = 1)

tsurban <- ts(select(peixe_ang, Urban), start = 2006, end = 2020, frequency = 1)

tsnatural <- ts(select(peixe_ang, Natural), start = 2006, end = 2020, frequency = 1)

tspasture <- ts(select(peixe_ang, Pasture), start = 2006, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa
notrend_test(tsoutflow, test = "WAVK", Window = 5)

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#n?o-significativa
notrend_test(tsliq_evap, test = "WAVK", Window = 5)

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2006 a 2010 e 2010 a 2020

tsoutflow2<-ts(tsoutflow[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)

tsarea2<-ts(tsarea[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)#n?o-significativa
notrend_test(tsarea2, test = "WAVK", Window = 7)

tsprecipitation2<-ts(tsprecipitation[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5)

tsliq_evap2<-ts(tsliq_evap[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)#n?o-significativa
notrend_test(tsliq_evap2, test = "WAVK", Window = 5)

tsurban2<-ts(tsurban[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura2<-ts(tsnatural[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture2<-ts(tsagriculture[5:15], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)
notrend_test(tsagriculture2, test = "WAVK", Window = 5, ar.method = "yw")

tspasture2<-ts(tspasture[5:15], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
notrend_test(tspasture2, ar.method = "yw")
MannKendall(tspasture2)#n?o-significativa
notrend_test(tspasture2, test = "MK")
notrend_test(tspasture2, test = "WAVK", Window = 5, ar.method = "yw")

#Lajeado

#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(lajeado, Outflow), start = 2002, end = 2020, frequency = 1)

tsarea <-  ts(select(lajeado, Area), start = 2002, end = 2020, frequency = 1)

tsprecipitation <- ts(select(lajeado, Precipitation), start = 2002, end = 2020, frequency = 1)

tsliq_evap <- ts(select(lajeado, Liq_evaporation), start = 2002, end = 2020, frequency = 1)

tsagriculture <- ts(select(lajeado, Agriculture), start = 2002, end = 2020, frequency = 1)

tsurban <- ts(select(lajeado, Urban), start = 2002, end = 2020, frequency = 1)

tsnatural <- ts(select(lajeado, Natural), start = 2002, end = 2020, frequency = 1)

tspasture <- ts(select(lajeado, Pasture), start = 2002, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#significativa

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
notrend_test(tspasture, ar.method = "yw")
MannKendall(tspasture)#n?o-significativa
notrend_test(tspasture, test = "MK")
notrend_test(tspasture, test = "WAVK", Window = 5)

#sub-set para 2002 a 2010 e 2010 a 2020

tsoutflow2<-ts(tsoutflow[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)

tsarea2<-ts(tsarea[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)#n?o-significativa
notrend_test(tsarea2, test = "WAVK", Window = 9)

tsprecipitation2<-ts(tsprecipitation[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5)

tsliq_evap2<-ts(tsliq_evap[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)#n?o-significativa
notrend_test(tsliq_evap2, test = "WAVK", Window = 5)

tsurban2<-ts(tsurban[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura2<-ts(tsnatural[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture2<-ts(tsagriculture[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)

tspasture2<-ts(tspasture[9:19], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)

#Estreito

#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(estreito, Outflow), start = 2012, end = 2020, frequency = 1)

tsarea <-  ts(select(estreito, Area), start = 2012, end = 2020, frequency = 1)

tsprecipitation <- ts(select(estreito, Precipitation), start = 2012, end = 2020, frequency = 1)

tsliq_evap <- ts(select(estreito, Liq_evaporation), start = 2012, end = 2020, frequency = 1)

tsagriculture <- ts(select(estreito, Agriculture), start = 2012, end = 2020, frequency = 1)

tsurban <- ts(select(estreito, Urban), start = 2012, end = 2020, frequency = 1)

tsnatural <- ts(select(estreito, Natural), start = 2012, end = 2020, frequency = 1)

tspasture <- ts(select(estreito, Pasture), start = 2012, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
notrend_test(tsoutflow, ar.method = "yw")
MannKendall(tsoutflow)#n?o-significativa
notrend_test(tsoutflow, test = "MK")
notrend_test(tsoutflow, test = "WAVK", Window = 5)

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 4, ar.method = "yw")

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
notrend_test(tsliq_evap, ar.method = "yw")
MannKendall(tsliq_evap)#n?o-significativa
notrend_test(tsliq_evap, test = "MK")
notrend_test(tsliq_evap, test = "WAVK", Window = 6)

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2012 a 2010 e 2010 a 2020

tsoutflow2<-ts(tsoutflow[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)

tsarea2<-ts(tsarea[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)

tsprecipitation2<-ts(tsprecipitation[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)

tsliq_evap2<-ts(tsliq_evap[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)

tsurban2<-ts(tsurban[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura2<-ts(tsnatural[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture2<-ts(tsagriculture[9:19], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)

tspasture2<-ts(tspasture[9:19], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)

#Tucuru?

#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(tucurui, Outflow), start = 2000, end = 2020, frequency = 1)

tsinflow <- ts(select(tucurui, Inflow), start = 2000, end = 2020, frequency = 1)

tsarea <-  ts(select(tucurui, Area), start = 2000, end = 2020, frequency = 1)

tsprecipitation <- ts(select(tucurui, Precipitation), start = 2000, end = 2020, frequency = 1)

tsliq_evap <- ts(select(tucurui, Liq_evaporation), start = 2000, end = 2020, frequency = 1)

tsagriculture <- ts(select(tucurui, Agriculture), start = 2000, end = 2020, frequency = 1)

tsurban <- ts(select(tucurui, Urban), start = 2000, end = 2020, frequency = 1)

tsnatural <- ts(select(tucurui, Natural), start = 2000, end = 2020, frequency = 1)

tspasture <- ts(select(tucurui, Pasture), start = 2000, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
notrend_test(tsarea, ar.method = "yw")
MannKendall(tsarea)#n?o-significativa
notrend_test(tsarea, test = "MK")
notrend_test(tsarea, test = "WAVK", Window = 5)

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "WAVK", Window = 5)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#n?o-significativa
notrend_test(tsliq_evap, test = "WAVK", Window = 5)

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2000 a 2010 e 2010 a 2020
tsoutflow1<-ts(tsoutflow[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsoutflow1)
lines(lowess(time(tsoutflow1), tsoutflow1), lw=2, col = 2)
MannKendall(tsoutflow1)#n?o-significativa
notrend_test(tsoutflow1, test = "WAVK", Window = 5, ar.method = "yw")
tsoutflow2<-ts(tsoutflow[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)#n?o-significativa
notrend_test(tsoutflow2, test = "WAVK", Window = 4)

tsarea1<-ts(tsarea[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsarea1)
lines(lowess(time(tsarea1), tsarea1), lw=2, col = 2)
MannKendall(tsarea1)#n?o-significativa
notrend_test(tsarea1, test = "WAVK", Window = 8)
tsarea2<-ts(tsarea[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)#n?o-significativa


tsprecipitation1<-ts(tsprecipitation[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsprecipitation1)
lines(lowess(time(tsprecipitation1), tsprecipitation1), lw=2, col = 2)
MannKendall(tsprecipitation1)#n?o-significativa
notrend_test(tsprecipitation1, test = "WAVK", Window = 5, ar.method = "yw")
tsprecipitation2<-ts(tsprecipitation[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "WAVK", Window = 5, ar.method = "yw")

tsliq_evap1<-ts(tsliq_evap[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsliq_evap1)
lines(lowess(time(tsliq_evap1), tsliq_evap1), lw=2, col = 2)
MannKendall(tsliq_evap1)#n?o-significativa
notrend_test(tsliq_evap1, test = "WAVK", Window = 10)
tsliq_evap2<-ts(tsliq_evap[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
MannKendall(tsliq_evap2)#n?o-significativa
notrend_test(tsliq_evap2, test = "WAVK", Window = 5)

tsurban1<-ts(tsurban[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsurban1)
lines(lowess(time(tsurban1), tsurban1), lw=2, col = 2)
MannKendall(tsurban1)
tsurban2<-ts(tsurban[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura1<-ts(tsnatural[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsnatura1)
lines(lowess(time(tsnatura1), tsnatura1), lw=2, col = 2)
MannKendall(tsnatura1)
tsnatura2<-ts(tsnatural[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture1<-ts(tsagriculture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsagriculture1)
lines(lowess(time(tsagriculture1), tsagriculture1), lw=2, col = 2)
notrend_test(tsagriculture1, ar.method = "yw")
MannKendall(tsagriculture1)#n?o-significativa
notrend_test(tsagriculture1, test = "MK")
notrend_test(tsagriculture1, test = "WAVK", Window = 3, ar.method = "yw")
tsagriculture2<-ts(tsagriculture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
notrend_test(tsagriculture2, ar.method = "yw")
MannKendall(tsagriculture2)#n?o-significativa
notrend_test(tsagriculture2, test = "MK")
notrend_test(tsagriculture2, test = "WAVK", Window = 7, ar.method = "yw")

tspasture1<-ts(tspasture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tspasture1)
lines(lowess(time(tspasture1), tspasture1), lw=2, col = 2)
MannKendall(tspasture1)
tspasture2<-ts(tspasture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)


#################################################################################
#Tocantins
library(readxl)
tocantins <- as.data.frame(read_excel("novos_dados.xlsx", 
                                      sheet = "tocantins"))
tocantins <- tocantins[11:21,]
#selecionar 1 coluna e transformar em time series object
tsoutflow <- ts(select(tocantins, OutflowTuc), start = 2010, end = 2020, frequency = 1)

tsinflow <- ts(select(tocantins, OutflowSM), start = 2010, end = 2020, frequency = 1)

tsarea <-  ts(select(tocantins, Area), start = 2010, end = 2020, frequency = 1)

tsprecipitation <- ts(select(tocantins, Precipitation), start = 2010, end = 2020, frequency = 1)

tsliq_evap <- ts(select(tocantins, Liq_evaporation), start = 2010, end = 2020, frequency = 1)

tsagriculture <- ts(select(tocantins, Agriculture), start = 2010, end = 2020, frequency = 1)

tsurban <- ts(select(tocantins, Urban), start = 2010, end = 2020, frequency = 1)

tsnatural <- ts(select(tocantins, Natural), start = 2010, end = 2020, frequency = 1)

tspasture <- ts(select(tocantins, Pasture), start = 2010, end = 2020, frequency = 1)

#plotar as s?ries
#incluir lowes trend
plot(tsoutflow)
lines(lowess(time(tsoutflow), tsoutflow), lw=2, col = 2)
MannKendall(tsoutflow)#n?o-significativa

plot(tsinflow)
lines(lowess(time(tsinflow), tsinflow), lw=2, col = 2)
MannKendall(tsinflow)#significativa
notrend_test(tsinflow, test = "WAVK", Window =10)

plot(tsarea)
lines(lowess(time(tsarea), tsarea), lw=2, col = 2)
MannKendall(tsarea)#n?o-significativa

plot(tsprecipitation)
lines(lowess(time(tsprecipitation), tsprecipitation), lw=2, col = 2)
notrend_test(tsprecipitation, ar.method = "yw")
MannKendall(tsprecipitation)#n?o-significativa
notrend_test(tsprecipitation, test = "MK")
notrend_test(tsprecipitation, test = "WAVK", Window =4)

plot(tsliq_evap)
lines(lowess(time(tsliq_evap), tsliq_evap), lw=2, col = 2)
MannKendall(tsliq_evap)#significativa

plot(tsagriculture)
lines(lowess(time(tsagriculture), tsagriculture), lw=2, col = 2)
MannKendall(tsagriculture)#significativa

plot(tsurban)
lines(lowess(time(tsurban), tsurban), lw=2, col = 2)
MannKendall(tsurban)#significativa

plot(tsnatural)
lines(lowess(time(tsnatural), tsnatural), lw=2, col = 2)
MannKendall(tsnatural)#significativa

plot(tspasture)
lines(lowess(time(tspasture), tspasture), lw=2, col = 2)
MannKendall(tspasture)#significativa

#sub-set para 2000 a 2010 e 2010 a 2020
##outflow w inflow são os mesmos de SM e TUC

tsarea1<-ts(tsarea[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsarea1)
lines(lowess(time(tsarea1), tsarea1), lw=2, col = 2)
MannKendall(tsarea1)
tsarea2<-ts(tsarea[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsarea2)
lines(lowess(time(tsarea2), tsarea2), lw=2, col = 2)
MannKendall(tsarea2)
notrend_test(tsarea2, test = "WAVK", Window = 4)

tsprecipitation1<-ts(tsprecipitation[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsprecipitation1)
lines(lowess(time(tsprecipitation1), tsprecipitation1), lw=2, col = 2)
notrend_test(tsprecipitation1, ar.method = "yw")
MannKendall(tsprecipitation1)#n?o-significativa
notrend_test(tsprecipitation1, test = "MK")
notrend_test(tsprecipitation1, test = "WAVK", Window = 4)
tsprecipitation2<-ts(tsprecipitation[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsprecipitation2)
lines(lowess(time(tsprecipitation2), tsprecipitation2), lw=2, col = 2)
notrend_test(tsprecipitation2, ar.method = "yw")
MannKendall(tsprecipitation2)#n?o-significativa
notrend_test(tsprecipitation2, test = "MK")
notrend_test(tsprecipitation2, test = "WAVK", Window = 4)

tsliq_evap1<-ts(tsliq_evap[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsliq_evap1)
lines(lowess(time(tsliq_evap1), tsliq_evap1), lw=2, col = 2)
notrend_test(tsliq_evap1, ar.method = "yw")
MannKendall(tsliq_evap1)#n?o-significativa
notrend_test(tsliq_evap1, test = "MK")
notrend_test(tsliq_evap1, test = "WAVK", Window =4)
tsliq_evap2<-ts(tsliq_evap[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsliq_evap2)
lines(lowess(time(tsliq_evap2), tsliq_evap2), lw=2, col = 2)
notrend_test(tsliq_evap2, ar.method = "yw")
MannKendall(tsliq_evap2)#n?o-significativa
notrend_test(tsliq_evap2, test = "MK")
notrend_test(tsliq_evap2, test = "WAVK", Window = 4)

tsurban1<-ts(tsurban[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsurban1)
lines(lowess(time(tsurban1), tsurban1), lw=2, col = 2)
MannKendall(tsurban1)
tsurban2<-ts(tsurban[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsurban2)
lines(lowess(time(tsurban2), tsurban2), lw=2, col = 2)
MannKendall(tsurban2)

tsnatura1<-ts(tsnatural[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsnatura1)
lines(lowess(time(tsnatura1), tsnatura1), lw=2, col = 2)
MannKendall(tsnatura1)
tsnatura2<-ts(tsnatural[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsnatura2)
lines(lowess(time(tsnatura2), tsnatura2), lw=2, col = 2)
MannKendall(tsnatura2)

tsagriculture1<-ts(tsagriculture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsagriculture1)
lines(lowess(time(tsagriculture1), tsagriculture1), lw=2, col = 2)
MannKendall(tsagriculture1)
tsagriculture2<-ts(tsagriculture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsagriculture2)
lines(lowess(time(tsagriculture2), tsagriculture2), lw=2, col = 2)
MannKendall(tsagriculture2)

tspasture1<-ts(tspasture[1:11], start = 2000, end = 2010, frequency = 1)
plot(tspasture1)
lines(lowess(time(tspasture1), tspasture1), lw=2, col = 2)
MannKendall(tspasture1)
tspasture2<-ts(tspasture[11:21], start = 2010, end = 2020, frequency = 1)
plot(tspasture2)
lines(lowess(time(tspasture2), tspasture2), lw=2, col = 2)
MannKendall(tspasture2)
notrend_test(tspasture2, test = "WAVK", Window = 5)

tsoutflow1<-ts(tsoutflow[1:11], start = 2000, end = 2010, frequency = 1)
plot(tsoutflow1)
lines(lowess(time(tsoutflow1), tsoutflow1), lw=2, col = 2)
MannKendall(tsoutflow1)
notrend_test(tsoutflow1, test = "WAVK", Window = 5, ar.method = 'yw')
tsoutflow2<-ts(tsoutflow[11:21], start = 2010, end = 2020, frequency = 1)
plot(tsoutflow2)
lines(lowess(time(tsoutflow2), tsoutflow2), lw=2, col = 2)
MannKendall(tsoutflow2)
notrend_test(tsoutflow2, test = "WAVK", Window = 5)

tsinflow1<-ts(tsinflow, start = 2000, end = 2010, frequency = 1)
plot(tsinflow1)
lines(lowess(time(tsinflow1), tsinflow1), lw=2, col = 2)
MannKendall(tsinflow1)
notrend_test(tsinflow1, test = "WAVK", Window = 4, ar.method = 'yw')

tsinflow2<-ts(select(tocantins, OutflowSM), start = 2010, end = 2020, frequency = 1)
plot(tsinflow2)
lines(lowess(time(tsinflow2), tsinflow2), lw=2, col = 2)
MannKendall(tsinflow2)
notrend_test(tsinflow2, test = "WAVK", Window = 4, ar.method = 'yw')
