source("utils.R")

ReadDesenho <- function(path){
  setwd(path)
  all.xmls <- list.files(".")
  df_desenho <- data.frame()
  
  for (path.xml in all.xmls) {
    file <- xmlParse(path.xml)
    root <- xmlRoot(file)
    df_aux <- NULL
    
    desenhos <- root[["PRODUCAO-TECNICA"]]["DESENHO-INDUSTRIAL", all = TRUE]
    nome_autores <- "VAZIO"
    nome_citacao_autores <- "VAZIO"
    ordem_autores <- "VAZIO"
    id_autores <- "VAZIO"
    
    for (desenho in desenhos) {
      desenho_autores <- desenho["AUTORES", all = TRUE]
      for (autor in desenho_autores) {
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
      
      DADOS_BASICOS <- "DADOS-BASICOS-DO-DESENHO-INDUSTRIAL"
      DETALHAMENTO <- "DETALHAMENTO-DO-DESENHO-INDUSTRIAL"
      
      df_aux <- data.frame(
        # Informações básicas
        id_lattes = gsub(".xml", "", path.xml),
        nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
        id_patente = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
        
        # TAG: DADOS-BASICOS-DO-DESENHO-INDUSTRIAL
        titulo_marca = Validate(desenho[[DADOS_BASICOS]], "TITULO"),
        ano_desenvolvimento = Validate(desenho[[DADOS_BASICOS]], "ANO-DESENVOLVIMENTO"),
        pais = Validate(desenho[[DADOS_BASICOS]], "PAIS"),
        flag_relevancia = Validate(desenho[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
        titulo_ingles = Validate(desenho[[DADOS_BASICOS]], "TITULO-INGLES"),
        flag_potencial_inovacao = Validate(desenho[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
        
        # TAG: DETALHAMENTO-DO-DESENHO-INDUSTRIAL
        finalidade = Validate(desenho[[DETALHAMENTO]], "FINALIDADE"),
        instituicao_financiadora = Validate(desenho[[DETALHAMENTO]], "INSTITUICAO-FINANCIADORA"),
        finalidade_ingles = Validate(desenho[[DETALHAMENTO]], "FINALIDADE-INGLES"),
        
        # TAG: DETALHAMENTO-DO-DESENHO-INDUSTRIAL > REGISTRO-OU-PATENTE
        tipo_patente = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
        #WARN: Informação duplicada, TITULO-PATENTE
        titulo_patente_reg = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
        data_pedido_de_deposito = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
        data_pedido_de_exame = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
        data_de_concessao = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
        instituicao_deposito_registro = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
        numero_deposito_pct = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
        formato_data_deposito_pct = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
        data_deposito_pct = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
        nome_titular = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
        nome_depositante = Validate(desenho[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
        #sta_validacao = Validate(marca[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
        
        # TAG: AREA-DO-CONHECIMENTO 
        # TODO: se tiver mais de uma?
        nome_grande_area_do_conhecimento = Validate(desenho[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
        nome_da_area_do_conhecimento = Validate(desenho[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
        nome_da_sub_area_do_conhecimento = Validate(desenho[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
        nome_da_especialidade = Validate(desenho[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-ESPECIALIDADE"),
        
        # AUTORES
        nome_completo_autores = nome_autores,
        nome_citacao_autores = nome_citacao_autores,
        ordem_autoria_autores = I(list(ordem_autores)),
        id_autores = I(list(id_autores)),
        
        stringsAsFactors = FALSE
      )
      df_desenho <- rbind(df_desenho, df_aux)
    }
  }
  rm(df_aux)
  
  df_desenho$id_patente <- RemoveNonAlNum(df_desenho$id_patente)
  
  df_desenho
}
