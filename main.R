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

folder_path <- "~/Documentos/ds4all/gcee/teste/"

df_pat <- ReadPatents(folder_path)
df_marca <- ReadMarcas(folder_path)
df_topo <- ReadTopografias(folder_path)
df_software <- ReadSoftware(folder_path)
df_cultivar_reg <- ReadCultivarRegistrada(folder_path)
df_cultivar_proteg <-ReadCultivarProtegida(folder_path)
df_desenho <- ReadDesenho(folder_path)
df_merged <- MergeWithINCT(df_pat, "~/Documentos/ds4all/gcee/data/Lista_IDLattes_patentes_membros_03102018.xlsx")
