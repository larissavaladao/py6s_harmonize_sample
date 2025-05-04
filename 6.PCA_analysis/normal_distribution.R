## dados dos mosaicos com periodos de água definidos por cotas em Obidos ####

#definir diretório de trabalho e importar dados ##################

setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")
library(readr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_cota_lulc.csv"))
head(data)
summary(data)

#imputar valores que estao faltando ###################
#500 iterations of predictive mean mapping for imputing 
#5 datasets
library(mice)
library(dplyr)


#imputar dados faltantes faltantes
selecao <- select(data, mean_u__wind  , mean_v__wind  , anthropic_km2, area_km2,mean_SPM,mean_precipitation)
dados_imp <- mice(selecao, m = 5, maxit = 100, method = 'pmm', seed = 500)
dados_comp <- complete(dados_imp, 2)#numero do dataset cujas imputa??es vc quer usar
dados_comp


selecao <- select(data, -X,-mean_u__wind,-mean_v__wind,-anthropic_km2)

selecao$mean_u__wind <- dados_comp$mean_u__wind
selecao$mean_v__wind <- dados_comp$mean_v__wind
selecao$anthropic_km2 <- dados_comp$anthropic_km2

write.csv(selecao, file = "filled_data_cota_lulc.csv")


#ler os arquivos ja substituidos para calcular as correlacoes e normalidade ###########################
#todos os periodos de agua incluidos
data_select <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
data_select <- select(data_select,-X,-class_name)
head(data_select)
summary(data_select)

#testes preliminares #####################################
library(ggpubr)
library(moments)


# tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
#testar normalidade 
#Area
shapiro.test(data_select$area_km2) #dados normais
ks.test(data_select$area_km2, "pnorm", mean(data_select$area_km2), sd(data_select$area_km2))
ggqqplot(data_select$area_km2)
# Distribution of CONT variable
ggdensity(data_select, x = "area_km2", fill = "lightgray", title = "Area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$area_km2)
skewness(data_select$area_km2)

#SPM
shapiro.test(data_select$mean_SPM) #dados normais
ks.test(data_select$mean_SPM, "pnorm", mean(data_select$mean_SPM), sd(data_select$mean_SPM))
ggqqplot(data_select$mean_SPM)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_SPM", fill = "lightgray", title = "SPM") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_SPM)
skewness(data_select$mean_SPM)

#Precipitation
shapiro.test(data_select$mean_precipitation) #dados normais
ks.test(data_select$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_select$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_precipitation)
skewness(data_select$mean_precipitation)

# discharge
shapiro.test(data_select$mean_discharge) #dados normais
ks.test(data_select$mean_discharge, "pnorm", mean(data_select$mean_discharge), sd(data_select$mean_discharge))
ggqqplot(data_select$mean_discharge)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_discharge)
skewness(data_select$mean_discharge)

#wind U
shapiro.test(data_select$mean_u__wind) #dados normais
ks.test(data_select$mean_u__wind, "pnorm", mean(data_select$mean_u__wind), sd(data_select$mean_u__wind))
ggqqplot(data_select$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_u__wind)
skewness(data_select$mean_u__wind)

#wind V
shapiro.test(data_select$mean_v__wind) #dados normais
ks.test(data_select$mean_v__wind, "pnorm", mean(data_select$mean_v__wind), sd(data_select$mean_v__wind))
ggqqplot(data_select$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_v__wind)
skewness(data_select$mean_v__wind)

#anthropic_km2
shapiro.test(data_select$anthropic_km2) #dados normais
ks.test(data_select$anthropic_km2, "pnorm", mean(data_select$anthropic_km2), sd(data_select$anthropic_km2))
ggqqplot(data_select$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_select, x = "anthropic_km2", fill = "lightgray", title = "anthropic_km2") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$anthropic_km2)
skewness(data_select$anthropic_km2)

#calcular correla??o##############################################################
#pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

#testar se a covari?ncia ? linear - verificar scatter plot
#Serra da Mesa
ggscatter(data_select, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear
ggscatter(data_select, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#matriz de corela??o############################################################

data_select<-select(data_select, area_km2 , mean_SPM  , mean_precipitation,
                    mean_discharge ,mean_u__wind,mean_v__wind,anthropic_km2)
correlation <- cor(data_select, method = 'spearman')

corrplot::corrplot.mixed(correlation, upper = 'ellipse', lower = 'number', las = 1)


write.csv(correlation, file = "correlation_cota_lulc.csv")

#verificar se as correla??es s?o significativas##################################

cor.test(data_select$mean_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_discharge, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_v__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$anthropic_km2, method = 'spearman')





### DADOS Separados por periodos##################################################################################

data_select$water_period <- as.factor(data_select$water_period)

# checking which values are from the period
data_LW <- filter(data_select,water_period=='LW') 
data_LW
summary(data_LW)

data_HW<- filter(data_select,water_period=='HW') 
data_HW
summary(data_HW)

data_R<- filter(data_select,water_period=='R') 
data_R
summary(data_R)

data_F<- filter(data_select,water_period=='F') 
data_F
summary(data_F)


#testes preliminares #####################################

# tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
#testar normalidade 
#Area####
#LW
shapiro.test(data_LW$area_km2) #dados normais
ks.test(data_LW$area_km2, "pnorm", mean(data_LW$area_km2), sd(data_LW$area_km2))
ggqqplot(data_LW$area_km2)
# Distribution of CONT variable
ggdensity(data_LW, x = "area_km2", fill = "lightgray", title = "Area LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$area_km2)
skewness(data_LW$area_km2)

#HW
shapiro.test(data_HW$area_km2) #dados normais
ks.test(data_HW$area_km2, "pnorm", mean(data_HW$area_km2), sd(data_HW$area_km2))
ggqqplot(data_HW$area_km2)
# Distribution of CONT variable
ggdensity(data_HW, x = "area_km2", fill = "lightgray", title = "Area HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$area_km2)
skewness(data_HW$area_km2)

#R
shapiro.test(data_R$area_km2) #dados normais
ks.test(data_R$area_km2, "pnorm", mean(data_R$area_km2), sd(data_R$area_km2))
ggqqplot(data_R$area_km2)
# Distribution of CONT variable
ggdensity(data_R, x = "area_km2", fill = "lightgray", title = "Area R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$area_km2)
skewness(data_R$area_km2)

#F
shapiro.test(data_F$area_km2) #dados normais
ks.test(data_F$area_km2, "pnorm", mean(data_F$area_km2), sd(data_F$area_km2))
ggqqplot(data_F$area_km2)
# Distribution of CONT variable
ggdensity(data_F, x = "area_km2", fill = "lightgray", title = "Area F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$area_km2)
skewness(data_F$area_km2)

#SPM########
#LW
shapiro.test(data_LW$mean_SPM) #dados normais
ks.test(data_LW$mean_SPM, "pnorm", mean(data_LW$mean_SPM), sd(data_LW$mean_SPM))
ggqqplot(data_LW$mean_SPM)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_SPM", fill = "lightgray", title = "SPM LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_SPM)
skewness(data_LW$mean_SPM)

#HW
shapiro.test(data_HW$mean_SPM) #dados normais
ks.test(data_HW$mean_SPM, "pnorm", mean(data_HW$mean_SPM), sd(data_HW$mean_SPM))
ggqqplot(data_HW$mean_SPM)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_SPM", fill = "lightgray", title = "SPM HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_SPM)
skewness(data_HW$mean_SPM)

#R
shapiro.test(data_R$mean_SPM) #dados normais
ks.test(data_R$mean_SPM, "pnorm", mean(data_R$mean_SPM), sd(data_R$mean_SPM))
ggqqplot(data_R$mean_SPM)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_SPM", fill = "lightgray", title = "SPM R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_SPM)
skewness(data_R$mean_SPM)

#F
shapiro.test(data_F$mean_SPM) #dados normais
ks.test(data_F$mean_SPM, "pnorm", mean(data_F$mean_SPM), sd(data_F$mean_SPM))
ggqqplot(data_F$mean_SPM)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_SPM", fill = "lightgray", title = "SPM F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_SPM)
skewness(data_F$mean_SPM)

#Precipitation ####
#LW
shapiro.test(data_LW$mean_precipitation) #dados normais
ks.test(data_LW$mean_precipitation, "pnorm", mean(data_LW$data_LW), sd(data_LW$mean_precipitation))
ggqqplot(data_LW$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_precipitation", fill = "lightgray", title = "Precipitation LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_precipitation)
skewness(data_LW$mean_precipitation)

#Precipitation ####
shapiro.test(data_LW$mean_precipitation) #dados normais
ks.test(data_LW$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_LW$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_precipitation)
skewness(data_LW$mean_precipitation)

#Precipitation ####
shapiro.test(data_select$mean_precipitation) #dados normais
ks.test(data_select$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_select$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_precipitation)
skewness(data_select$mean_precipitation)

#Precipitation ####
shapiro.test(data_select$mean_precipitation) #dados normais
ks.test(data_select$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_select$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_precipitation)
skewness(data_select$mean_precipitation)

# discharge
shapiro.test(data_select$discharge_mean) #dados normais
ks.test(data_select$discharge_mean, "pnorm", mean(data_select$discharge_mean), sd(data_select$discharge_mean))
ggqqplot(data_select$discharge_mean)
# Distribution of CONT variable
ggdensity(data_select, x = "discharge_mean", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$discharge_mean)
skewness(data_select$discharge_mean)

#wind U
shapiro.test(data_select$mean_u__wind) #dados normais
ks.test(data_select$mean_u__wind, "pnorm", mean(data_select$mean_u__wind), sd(data_select$mean_u__wind))
ggqqplot(data_select$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_u__wind)
skewness(data_select$mean_u__wind)

#wind V
shapiro.test(data_select$mean_v__wind) #dados normais
ks.test(data_select$mean_v__wind, "pnorm", mean(data_select$mean_v__wind), sd(data_select$mean_v__wind))
ggqqplot(data_select$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_v__wind)
skewness(data_select$mean_v__wind)

#calcular correla??o##############################################################
#pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

#testar se a covari?ncia ? linear - verificar scatter plot
#Serra da Mesa
ggscatter(data_select, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'discharge_mean', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'discharge_mean') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

#matriz de corela??o############################################################

data_select<-select(data_select, area_km2  , mean_SPM  , mean_precipitation,
                    discharge_mean ,mean_u__wind,mean_v__wind)
correlation <- cor(data_select, method = 'spearman')

corrplot::corrplot.mixed(correlation, upper = 'ellipse', lower = 'number', las = 1)


write.csv(correlation, file = "correlation.csv")

#verificar se as correla??es s?o significativas##################################

cor.test(data_select$mean_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$discharge_mean, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_v__wind, method = 'spearman')



#####################################################################################
## dividir por periodos



