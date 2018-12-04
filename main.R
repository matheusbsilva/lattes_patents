library(XML)
library(xlsx)
library(dplyr)

setwd("~/Documentos/ds4all/gcee/")
source("read_patents.R")
source("read_marca.R")
source("read_topografia.R")
source("read_software.R")
source("read_cultivar_registrada.R")
source("read_cultivar_protegida.R")
source("read_desenho.R")
source("merge_inct.R")
source("graph_results.R")

folder_path <- "~/Documentos/ds4all/gcee/teste/"
incts_ids_path <- "~/Documentos/ds4all/gcee/data/Lista_IDLattes_patentes_membros_03102018.xlsx"

df_pat <- ReadPatents(folder_path)
df_marca <- ReadMarcas(folder_path)
df_topo <- ReadTopografias(folder_path)
df_software <- ReadSoftware(folder_path)
df_cultivar_reg <- ReadCultivarRegistrada(folder_path)
df_cultivar_proteg <-ReadCultivarProtegida(folder_path)
df_desenho <- ReadDesenho(folder_path)


df_pat <- MergeWithINCT(df_pat, incts_ids_path)
# df_marca <- MergeWithINCT(df_marca, incts_ids_path)
# df_topo <- MergeWithINCT(df_topo, incts_ids_path)
# df_software <- MergeWithINCT(df_software, incts_ids_path)
# df_cultivar_reg <- MergeWithINCT(df_cultivar_reg, incts_ids_path)
# df_cultivar_proteg <- MergeWithINCT(df_cultivar_proteg, incts_ids_path)
# df_desenho <- MergeWithINCT(df_desenho, incts_ids_path)

graph_by_year <- GraphByYear(df_pat)
graph_by_mean_year <- GraphByMeanYear(df_pat)
graph_by_country <- GraphByCountry(df_pat)
graph_by_category <- GraphByCategory(df_pat)
keywords_cloud <- CloudKeyWords(df_pat)