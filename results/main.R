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

# Cultivar Protegido -> ano_solicitacao
# Cultivar Registrado -> ano_solicitacao
# Desenho Industrial -> ano_desenvolvimento
# Marcas -> ano_desenvolvimento
# Topografia -> ano_desenvolvimento
# Software -> ano_desenvolvimento
# Patentes -> ano_desenvolvimento
# Turn ano_desenvolvimento into number
df_patents <- transform(df_patents, ano = as.numeric(as.character(ano_desenvolvimento)))
df_marcas <- transform(df_marcas, ano = as.numeric(as.character(ano_desenvolvimento)))
df_software <- transform(df_software, ano = as.numeric(as.character(ano_desenvolvimento)))
df_topografia <- transform(df_topografia, ano = as.numeric(as.character(ano_desenvolvimento)))
df_desenho <- transform(df_desenho, ano = as.numeric(as.character(ano_desenvolvimento)))
df_cultivar_reg <- transform(df_cultivar_reg, ano = as.numeric(as.character(ano_solicitacao)))
df_cultivar_prot <- transform(df_cultivar_prot, ano = as.numeric(as.character(ano_solicitacao)))


# Create a merged df with all types of patents
df_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(df_patents, df_marcas, df_software, df_topografia, df_desenho, df_cultivar_prot, df_cultivar_reg))

graph_by_year <- GraphByYear(df_all)
graph_by_mean_year <- GraphByMeanYear(df_patents)
graph_by_country <- GraphByCountry(df_patents)
graph_by_category <- GraphByCategory(df_patents)
keywords_cloud <- CloudKeyWords(df_patents)