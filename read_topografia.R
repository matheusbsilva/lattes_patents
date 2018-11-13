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
  
  topografias <- root[["PRODUCAO-TECNICA"]]["TOPOGRAFIA-DE-CIRCUITO-INTEGRADO", all = TRUE]
  nome_autores <- "VAZIO"
  nome_citacao_autores <- "VAZIO"
  ordem_autores <- "VAZIO"
  id_autores <- "VAZIO"
  
  for (topografia in topografias) {
    topografia_autores <- topografia["AUTORES", all = TRUE]
    for (autor in topografia_autores) {
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
    
    DADOS_BASICOS <- "DADOS-BASICOS-DA-TOPOGRAFIA-DE-CIRCUITO-INTEGRADO"
    DETALHAMENTO <- "DETALHAMENTO-DA-TOPOGRAFIA-DE-CIRCUITO-INTEGRADO"
    
    df_aux <- data.frame(
      # Informações básicas
      id_lattes = gsub(".xml", "", path.xml),
      nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
      id_patente = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
      
      # TAG: DADOS-BASICOS-DA-TOPOGRAFIA
      titulo_topografia = Validate(topografia[[DADOS_BASICOS]], "TITULO"),
      ano_desenvolvimento = Validate(topografia[[DADOS_BASICOS]], "ANO-DESENVOLVIMENTO"),
      pais = Validate(topografia[[DADOS_BASICOS]], "PAIS"),
      flag_relevancia = Validate(topografia[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
      titulo_ingles = Validate(topografia[[DADOS_BASICOS]], "TITULO-INGLES"),
      flag_potencial_inovacao = Validate(topografia[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
      
      # TAG: DETALHAMENTO-DA-TOPOGRAFIA-DE-CIRCUITO-INTEGRADO
      finalidade = Validate(topografia[[DETALHAMENTO]], "FINALIDADE"),
      instituicao_financiadora = Validate(topografia[[DETALHAMENTO]], "INSTITUICAO-FINANCIADORA"),
      finalidade_ingles = Validate(topografia[[DETALHAMENTO]], "FINALIDADE-INGLES"),
      
      # TAG: DETALHAMENTO-DA-TOPOGRAFIA-DE-CIRCUITO-INTEGRADO > REGISTRO-OU-PATENTE
      tipo_patente = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
      #WARN: Informação duplicada, TITULO-PATENTE
      titulo_patente_reg = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
      data_pedido_de_deposito = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
      data_pedido_de_exame = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
      data_de_concessao = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
      instituicao_deposito_registro = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
      numero_deposito_pct = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
      formato_data_deposito_pct = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
      data_deposito_pct = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
      nome_titular = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
      nome_depositante = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
      sta_validacao = Validate(topografia[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
      
      # TAG: INFORMACOES-ADICIONAIS
      descricao_info_adicionais = Validate(topografia[["INFORMACOES-ADICIONAIS"]], "DESCRICAO-INFORMACOES-ADICIONAIS"),
      
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
