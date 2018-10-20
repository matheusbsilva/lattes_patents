#install.packages("xml2")
#install.packages("dplyr")
#install.packages("XML")

library(xml2)
library(dplyr)
library(XML)
library(tidyverse)

# Define diretório padrão
# setwd("~/Documentos/ds4all/gcee/")
# setwd("~/Documentos/UNB/2 18/ds/git/lattes_patents/")

# Caminho de todos os curriculos
# all.xmls <- list.files("curriculos/", full = TRUE)
all.xmls <- list.files("teste/", full = TRUE)
lista.patentes <- list()

for (path.xml in all.xmls) {
  lista.xml <- read_xml(path.xml)

  # Busca pela TAG patente em todo o xml
  patentes <- xml_find_all(lista.xml, "//PATENTE")

  #TODO: tratar casos em que o atributo NUMERO-IDENTIFICADOR está vazio
  if (length(patentes) != 0) {
    lista.patentes[[xml_attrs(lista.xml)[["NUMERO-IDENTIFICADOR"]]]] <- as_list(patentes)
  }
}




# Teste com um arquivo
xmlobj_ds <- read_xml("teste/0021199964477362.xml")

# Assim converte para DataFrame, mas tem que ver porque não ta vindo todas as informações corretas, como o numero de sequencia e a quantidade e os nomes dos autores

df_items_ds <- data.frame(
  seq_prod   = xml_find_all( xmlobj_ds, ".//PATENTE" ) %>% xml_attr( "SEQUENCIA-PRODUCAO" ),
  titulo = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>%  xml_attr( "TITULO" ),
  ano = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("ANO-DESENVOLVIMENTO"),
  categoria = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE" ) %>% xml_attr("CATEGORIA"),
  autor1 = xml_find_all( xmlobj_ds, ".//PATENTE/AUTORES" ) %>% xml_attr("NOME-COMPLETO-DO-AUTOR"),
  stringsAsFactors = FALSE )

# Assim tem que colocar em lista os dados pedidos: autores, ids

oddsetds <- xml_find_all(xmlobj_ds, ".//PATENTE") %>%
  xml_children() %>% map(xml_attrs) %>% map_df(~as.list(.))
