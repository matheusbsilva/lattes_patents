setwd("~/Documentos/UNB/2-18/ds/git/lattes_patents/curriculos")


library(XML)
library(xlsx)
library(dplyr)


#################################################
# Função para validar xml attr do E-lattes
Validate <- function (element, field) {
  result <- ""
  
  tryCatch(
    result <- xmlGetAttr(element, field)  ,
    error = function(e) {
      
    }
  )
  
  if (is.null(result) || is.na(result))
    result <- ""
  result
}

# Função para ponto vírgula e espaço
SemiColon <- function (field, target){ 
  if (is.null(field) || field == "") {
    target <- paste(target, "VAZIO", sep = "; ")
    
  }
  else{
    if (target != "VAZIO") {
      target <- paste(target, field, sep = "; ")
    }
    else{
      target <- paste(field, sep = "; ")
    }
  }
  
  target
}
################################################

all.xmls <- list.files(".")
#path <- "0000325690951570.xml"
df_pat <- data.frame()

for (path.xml in all.xmls) {
  file <- xmlParse(path.xml)
  #file <- xmlParse(path)
  root <- xmlRoot(file)
  df_aux <- NULL
  
  marcas <- root[["PRODUCAO-TECNICA"]]["MARCA", all = TRUE]
  nome_autores <- "VAZIO"
  nome_citacao_autores <- "VAZIO"
  ordem_autores <- "VAZIO"
  id_autores <- "VAZIO"
  
  for (marca in marcas) {
    marca_autores <- marca["AUTORES", all = TRUE]
    for (autor in marca_autores) {
      nome_completo <- Validate(autor, "NOME-COMPLETO-DO-AUTOR")
      nome_citacao <- Validate(autor, "NOME-PARA-CITACAO")
      ordem <- Validate(autor, "ORDEM-DE-AUTORIA")
      idcnpq <- Validate(autor, "NRO-ID-CNPQ")
      
      nome_autores <- SemiColon(nome_completo, nome_autores)
      nome_citacao_autores <- SemiColon(nome_citacao, nome_citacao_autores)
      ordem_autores <- SemiColon(ordem, ordem_autores)
      id_autores <- SemiColon(idcnpq, id_autores)
      
      
    }
    # variáveis das tags
    
    DADOS_BASICOS <- "DADOS-BASICOS-DA-MARCA"
    DETALHAMENTO <- "DETALHAMENTO-DA-MARCA"
    
    df_aux <- data.frame(
      # Informações básicas
      id_lattes = gsub(".xml", "", path.xml),
      nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
      id_patente = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
      
      # TAG: DADOS-BASICOS-DA-MARCA
      titulo_marca = Validate(marca[[DADOS_BASICOS]], "TITULO"),
      ano_desenvolvimento = Validate(marca[[DADOS_BASICOS]], "ANO-DESENVOLVIMENTO"),
      pais = Validate(marca[[DADOS_BASICOS]], "PAIS"),
      flag_relevancia = Validate(marca[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
      titulo_ingles = Validate(marca[[DADOS_BASICOS]], "TITULO-INGLES"),
      flag_potencial_inovacao = Validate(marca[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
      
      # TAG: DETALHAMENTO-DA-MARCA
      finalidade = Validate(marca[[DETALHAMENTO]], "FINALIDADE"),
      finalidade_ingles = Validate(marca[[DETALHAMENTO]], "FINALIDADE-INGLES"),
      natureza = Validate(marca[[DETALHAMENTO]], "NATUREZA"),
      
      # TAG: DETALHAMENTO-DA-TOPOGRAFIA-DE-CIRCUITO-INTEGRADO > REGISTRO-OU-PATENTE
      tipo_patente = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
      #WARN: Informação duplicada, TITULO-PATENTE
      titulo_patente_reg = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
      data_pedido_de_deposito = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
      data_pedido_de_exame = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
      data_de_concessao = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
      instituicao_deposito_registro = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
      numero_deposito_pct = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
      formato_data_deposito_pct = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
      data_deposito_pct = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
      nome_titular = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
      nome_depositante = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
      sta_validacao = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
      
      # TAG: AREA-DO-CONHECIMENTO 
      # TODO: se tiver mais de uma?
      nome_grande_area_do_conhecimento = Validate(marca[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
      nome_da_area_do_conhecimento = Validate(marca[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
      nome_da_sub_area_do_conhecimento = Validate(marca[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
      nome_da_especialidade = Validate(marca[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-ESPECIALIDADE"),
      
      # TAG: PALAVRAS-CHAVE
      palavra_chave_1 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-1"),
      palavra_chave_2 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-2"),
      palavra_chave_3 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-3"),
      palavra_chave_4 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-4"),
      palavra_chave_5 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-5"),
      palavra_chave_6 = Validate(marca[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-6"),
      
      # TAG: SETORES-DE-ATIVIDADE
      setor_de_atividade_1 = Validate(marca[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
      setor_de_atividade_2 = Validate(marca[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
      setor_de_atividade_3 = Validate(marca[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
      
      # AUTORES
      nome_completo_autores = nome_autores,
      nome_citacao_autores = nome_citacao_autores,
      ordem_autoria_autores = I(list(ordem_autores)),
      id_autores = I(list(id_autores)),
      
      stringsAsFactors = FALSE
    )
    df_pat <- rbind(df_pat, df_aux)
  }
}
