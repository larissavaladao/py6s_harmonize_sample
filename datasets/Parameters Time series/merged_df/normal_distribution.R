#definir diret?rio de trabalho e importar dados da tabela excel##################
#setwd("C:/Users/l_v_v/Documents/GitHub/py6s_harmonize_sample/8.PCA_analysis")

setwd("C:/Users/l_v_v/Documents/GitHub/py6s_harmonize_sample/datasets/Parameters Time series/merged_df")
library(readr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/py6s_harmonize_sample/datasets/Parameters Time series/merged_df/df_merged.csv"))
head(data)
