library(XML)
library(xlsx)
library(dplyr)

setwd("~/Documentos/ds4all/gcee/")
source("read_patents.R")
source("read_marca.R")
source("read_topografia.R")
source("merge_inct.R")

df_pat <- ReadPatents("~/Documentos/ds4all/gcee/teste/")
df_marca <- ReadMarcas("~/Documentos/ds4all/gcee/teste/")
df_topo <- ReadTopografias("~/Documentos/ds4all/gcee/teste/")
df_merged <- MergeWithINCT(df_pat, "~/Documentos/ds4all/gcee/data/Lista_IDLattes_patentes_membros_03102018.xlsx")
