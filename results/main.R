library(xlsx)
library(dplyr)

setwd("~/Documentos/ds4all/gcee/results")
source("graph_results.R")


df_patents <- read.xlsx("../dados-processados/patentes.xlsx", sheetName = 'Sheet1')
df_marcas <- read.xlsx("../dados-processados/marca.xlsx", sheetName = 'Sheet1')
df_software <- read.xlsx("../dados-processados/software.xlsx", sheetName = 'Sheet1')
df_topografia <- read.xlsx("../dados-processados/topografia.xlsx", sheetName = 'Sheet1')
df_desenho <- read.xlsx("../dados-processados/desenho.xlsx", sheetName = 'Sheet1')
df_cultivar_reg <- read.xlsx("../dados-processados/cultivar_registrada.xlsx", sheetName = 'Sheet1')
df_cultivar_prot <- read.xlsx("../dados-processados/cultivar_protegida.xlsx", sheetName = 'Sheet1')

# Turn ano_desenvolvimento into number
df_patents <- transform(df_patents, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_marcas <- transform(df_marcas, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_software <- transform(df_software, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_topografia <- transform(df_topografia, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_desenho <- transform(df_desenho, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_cultivar_reg <- transform(df_cultivar_reg, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))
df_cultivar_prot <- transform(df_cultivar_prot, ano_desenvolvimento = as.numeric(as.character(ano_desenvolvimento)))

graph_by_year <- GraphByYear(df_patents)
graph_by_mean_year <- GraphByMeanYear(df_patents)
graph_by_country <- GraphByCountry(df_patents)
graph_by_category <- GraphByCategory(df_patents)
keywords_cloud <- CloudKeyWords(df_patents)