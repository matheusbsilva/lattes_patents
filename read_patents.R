
# Define diretório padrão
setwd("~/Documentos/ds4all/gcee/")
setwd("~/Documentos/UNB/2 18/ds/git/lattes_patents/")


library(XML)
setwd("~/Documentos/ds4all/gcee/teste/")

file_path <- "0021199964477362.xml"
file <- xmlParse(file_path)
root <- xmlRoot(file)

patentes <- root[["PRODUCAO-TECNICA"]]["PATENTE", all = TRUE]
nome_autores <- NULL
nome_citacao_autores <- NULL
ordem_autores <- list()
id_autores <- list()
df_pat <- data.frame()
for (patente in patentes) {
  patentes_autores <- patente["AUTORES", all = TRUE]
  for (autor in patentes_autores) {
    nome_completo <- xmlAttrs(autor)[["NOME-COMPLETO-DO-AUTOR"]]
    nome_citacao <- xmlAttrs(autor)[["NOME-PARA-CITACAO"]]
    ordem <- xmlAttrs(autor)[["ORDEM-DE-AUTORIA"]]
    idcnpq <- xmlAttrs(autor)[["NRO-ID-CNPQ"]]
    
    if (nome_completo != "") {
      if (!is.null(nome_autores)) {
        nome_autores <- paste(nome_autores, nome_completo, sep = "; ")
      }
      else{
        nome_autores <- paste(nome_completo, sep = "; ")
      }
    }
    else
      nome_autores <- cat(nome_autores, "VAZIO", sep = "; ")
    
    if (nome_citacao != "") {
      if (!is.null(nome_citacao_autores)) {
        nome_citacao_autores <-
          paste(nome_citacao_autores, nome_citacao, sep = "; ")
      }
      else{
        nome_citacao_autores <- paste(nome_citacao, sep = "; ")
      }
    }
    else{
      nome_citacao_autores <- cat(nome_citacao_autores, "VAZIO", sep = "; ")
    }
    
    
    if (ordem != "") {
      ordem_autores <- c(ordem_autores, ordem)
    }
    else
      ordem_autores <- c(ordem_autores, "VAZIO")
    
    if (idcnpq != "") {
      id_autores <- c(id_autores, idcnpq)
    }
    else
      id_autores <- c(id_autores, "VAZIO")
    
  }
  
  df_aux <- data.frame(
    # Informações básicas
    id_lattes = gsub(".xml", "", file_path),
    nome_completo = xmlAttrs(root[["DADOS-GERAIS"]])[["NOME-COMPLETO"]],
    id_patente = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["CODIGO-DO-REGISTRO-OU-PATENTE"]],
    
    # TAG: DADOS-BASICOS-DA-PATENTE
    titulo_patente = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["TITULO"]],
    ano_desenvolvimento = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["ANO-DESENVOLVIMENTO"]],
    pais = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["PAIS"]],
    meio_de_divulgacao = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["MEIO-DE-DIVULGACAO"]],
    flag_relevancia = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["FLAG-RELEVANCIA"]],
    flag_potencial_inovacao = xmlAttrs(patente[["DADOS-BASICOS-DA-PATENTE"]])[["FLAG-POTENCIAL-INOVACAO"]],
    
    # TAG: DETALHAMENTO-DA-PATENTE
    finalidade = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]])[["FINALIDADE"]],
    instituicao_financiadora = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]])[["INSTITUICAO-FINANCIADORA"]],
    categoria = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]])[["CATEGORIA"]],
    
    # TAG: DETALHAMENTO-DA-PATENTE > REGISTRO-OU-PATENTE
    tipo_patente = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["TIPO-PATENTE"]],
    #WARN: Informação duplicada, TITULO-PATENTE
    titulo_patente_reg = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["TITULO-PATENTE"]],
    data_pedido_de_deposito = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["DATA-PEDIDO-DE-DEPOSITO"]],
    data_pedido_de_exame = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["DATA-DE-PEDIDO-DE-EXAME"]],
    data_de_concessao = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["DATA-DE-CONCESSAO"]],
    instituicao_deposito_registro = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["INSTITUICAO-DEPOSITO-REGISTRO"]],
    numero_deposito_pct = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["NUMERO-DEPOSITO-PCT"]],
    formato_data_deposito_pct = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["FORMATO-DATA-DEPOSITO-PCT"]],
    data_deposito_pct = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["DATA-DEPOSITO-PCT"]],
    nome_titular = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["NOME-DO-TITULAR"]],
    nome_depositante = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["NOME-DO-DEPOSITANTE"]],
    sta_validacao = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]])[["STA-VALIDADO"]],
    
    # TAG: DETALHAMENTO-DA-PATENTE > HISTORICO-SITUACOES-PATENTE
    descricao_situacao = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]])[["DESCRICAO-SITUACAO-PATENTE"]],
    data_situacao = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]])[["DATA-SITUACAO-PATENTE"]],
    status_situacao = xmlAttrs(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]])[["STATUS-SITUACAO-PATENTE"]],
    
    # TAG: PALAVRAS-CHAVE
    palavra_chave_1 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-1"]],
    palavra_chave_2 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-2"]],
    palavra_chave_3 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-3"]],
    palavra_chave_4 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-4"]],
    palavra_chave_5 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-5"]],
    palavra_chave_6 = xmlAttrs(patente[["PALAVRAS-CHAVE"]])[["PALAVRA-CHAVE-6"]],
    
    # TAG: SETORES-DE-ATIVIDADE
    setor_de_atividade_1 = xmlAttrs(patente[["SETORES-DE-ATIVIDADE"]])[["SETOR-DE-ATIVIDADE-1"]],
    setor_de_atividade_2 = xmlAttrs(patente[["SETORES-DE-ATIVIDADE"]])[["SETOR-DE-ATIVIDADE-2"]],
    setor_de_atividade_3 = xmlAttrs(patente[["SETORES-DE-ATIVIDADE"]])[["SETOR-DE-ATIVIDADE-3"]],
    
    # TAG: INFORMACOES-ADICIONAIS
    descricao_info_adicionais = xmlAttrs(patente[["INFORMACOES-ADICIONAIS"]])[["DESCRICAO-INFORMACOES-ADICIONAIS"]],
    
    # AUTORES
    nome_completo_autores = nome_autores,
    nome_citacao_autores = nome_citacao_autores,
    ordem_autoria_autores = I(list(ordem_autores)),
    id_autores = I(list(id_autores)),
    
    stringsAsFactors = FALSE
  )
  df_pat <- rbind(df_pat, df_aux)
}
