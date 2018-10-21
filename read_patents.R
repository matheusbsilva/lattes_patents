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
  # seq_prod   = xml_find_all( xmlobj_ds, ".//PATENTE" ) %>% xml_attr( "SEQUENCIA-PRODUCAO" ),
  id_lattes = xml_find_all( xmlobj_ds, "." ) %>%  xml_attr( "NUMERO-IDENTIFICADOR" ),
  nome_completo = xml_find_all( xmlobj_ds, "./DADOS-GERAIS" ) %>%  xml_attr( "NOME-COMPLETO" ),
  titulo = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>%  xml_attr( "TITULO" ),
  ano_desenvolvimento = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("ANO-DESENVOLVIMENTO"),
  pais = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("PAIS"),
  flag_relevancia = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("FLAG-RELEVANCIA"),
  titulo_ingles = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("TITULO-INGLES"),
  flag_pontecial_inovacao = xml_find_all( xmlobj_ds, ".//PATENTE/DADOS-BASICOS-DA-PATENTE" ) %>% xml_attr("FLAG-POTENCIAL-INOVACAO"),
  finalidade = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE" ) %>% xml_attr("FINALIDADE"),
  instituicao = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE" ) %>% xml_attr("INSTITUICAO-FINANCIADORA"),
  finalidade_ingles = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE" ) %>% xml_attr("FINALIDADE-INGLES"),
  categoria = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE" ) %>% xml_attr("CATEGORIA"),
  tipo_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("TIPO-PATENTE"),
  id_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("CODIGO-DO-REGISTRO-OU-PATENTE"),
  titulo_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("TITULO-PATENTE"),
  data_pedido_deposito = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("DATA-PEDIDO-DE-DEPOSITO"),
  data_pedido_exame = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("DATA-PEDIDO-DE-EXAME"),
  data_concessao = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("DATA-DE-CONCESSAO"),
  instituicao_deposito_registro = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("INSTITUICAO-DEPOSITO-REGISTRO"),
  numero_deposito_pct = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("NUMERO-DEPOSITO-PCT"),
  formato_data_deposito_pct = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("FORMATO-DATA-DEPOSITO-PCT"),
  data_deposito_pct = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("DATA-DEPOSITO-PCT"),
  nome_do_titular = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("NOME-DO-TITULAR"),
  nome_do_depositante = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("NOME-DO-DEPOSITANTE"),
  sta_validado = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE" ) %>% xml_attr("STA-VALIDADO"),
  descricao_sistuacao_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/HISTORICO-SITUACOES-PATENTE" ) %>% xml_attr("DESCRICAO-SITUACAO-PATENTE"),
  data_situacao_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/HISTORICO-SITUACOES-PATENTE" ) %>% xml_attr("DATA-SITUACAO-PATENTE"),
  status_situacao_patente = xml_find_all( xmlobj_ds, ".//PATENTE/DETALHAMENTO-DA-PATENTE/HISTORICO-SITUACOES-PATENTE" ) %>% xml_attr("STATUS-SITUACAO-PATENTE"),
  palavra_chave_1 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-1"),
  palavra_chave_2 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-2"),
  palavra_chave_3 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-3"),
  palavra_chave_4 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-4"),
  palavra_chave_5 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-5"),
  palavra_chave_6 = xml_find_all( xmlobj_ds, ".//PATENTE/PALAVRAS-CHAVE" ) %>% xml_attr("PALAVRA-CHAVE-6"),
  nome_grande_area_conhecimento = xml_find_all( xmlobj_ds, ".//PATENTE/AREAS-DO-CONHECIMENTO/AREA-DO-CONHECIMENTO-1" ) %>% xml_attr("NOME-GRANDE-AREA-DO-CONHECIMENTO"),
  nome_area_conhecimento = xml_find_all( xmlobj_ds, ".//PATENTE/AREAS-DO-CONHECIMENTO/AREA-DO-CONHECIMENTO-1" ) %>% xml_attr("NOME-DA-AREA-DO-CONHECIMENTO"),
  nome_sub_area_conhecimento = xml_find_all( xmlobj_ds, ".//PATENTE/AREAS-DO-CONHECIMENTO/AREA-DO-CONHECIMENTO-1" ) %>% xml_attr("NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
  nome_especialidade = xml_find_all( xmlobj_ds, ".//PATENTE/AREAS-DO-CONHECIMENTO/AREA-DO-CONHECIMENTO-1" ) %>% xml_attr("NOME-DA-ESPECIALIDADE"),
  setor_atividade_1 = xml_find_all( xmlobj_ds, ".//PATENTE/SETORES-DE-ATIVIDADE" ) %>% xml_attr("SETOR-DE-ATIVIDADE-1"),
  setor_atividade_2 = xml_find_all( xmlobj_ds, ".//PATENTE/SETORES-DE-ATIVIDADE" ) %>% xml_attr("SETOR-DE-ATIVIDADE-2"),
  setor_atividade_3 = xml_find_all( xmlobj_ds, ".//PATENTE/SETORES-DE-ATIVIDADE" ) %>% xml_attr("SETOR-DE-ATIVIDADE-3"),
  descricao_informacoes_adicionais = xml_find_all( xmlobj_ds, ".//PATENTE/INFORMACOES-ADICIONAIS" ) %>% xml_attr("DESCRICAO-INFORMACOES-ADICIONAIS"),
  descricao_informacoes_adicionais_ingles = xml_find_all( xmlobj_ds, ".//PATENTE/INFORMACOES-ADICIONAIS" ) %>% xml_attr("DESCRICAO-INFORMACOES-ADICIONAIS-INGLES"),
  #autor1 = xml_find_all( xmlobj_ds, ".//PATENTE/AUTORES" ) %>% xml_attr("NOME-COMPLETO-DO-AUTOR"),
  stringsAsFactors = FALSE )

# Area de conhecimento se tiver mais de um

# Assim tem que colocar em lista os dados pedidos: autores, ids

oddsetds <- xml_find_all(xmlobj_ds, ".//PATENTE") %>%
  xml_children() %>% map(xml_attrs) %>% map_df(~as.list(.))
