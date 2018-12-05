library(xlsx)
library(dplyr)

setwd("~/Documentos/ds4all/gcee/results")
source("graph_results.R")


df_patents <- read.xlsx("../dados-processados/patentes.xlsx", sheetName = 'Sheet1')

graph_by_year <- GraphByYear(df_patents)
graph_by_mean_year <- GraphByMeanYear(df_patents)
graph_by_country <- GraphByCountry(df_patents)
graph_by_category <- GraphByCategory(df_patents)
keywords_cloud <- CloudKeyWords(df_patents)

