# Define diretório padrão
setwd("~/Documentos/ds4all/gcee/curriculos/")
#setwd("~/Documentos/UNB/2 18/ds/git/lattes_patents/")


library(XML)
library(xlsx)
library(dplyr)

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

RemoveNonAlNum <- function (column){
  regex <- "[^[:alnum:]]"
  gsub(regex, "", column)
}

all.xmls <- list.files(".")
df_pat <- data.frame()

for (path.xml in all.xmls) {
  file <- xmlParse(path.xml)
  root <- xmlRoot(file)
  df_aux <- NULL
  
  patentes <- root[["PRODUCAO-TECNICA"]]["PATENTE", all = TRUE]
  nome_autores <- "VAZIO"
  nome_citacao_autores <- "VAZIO"
  ordem_autores <- "VAZIO"
  id_autores <- "VAZIO"
  
  for (patente in patentes) {
    patentes_autores <- patente["AUTORES", all = TRUE]
    for (autor in patentes_autores) {
      nome_completo <- Validate(autor, "NOME-COMPLETO-DO-AUTOR")
      nome_citacao <- Validate(autor, "NOME-PARA-CITACAO")
      ordem <- Validate(autor, "ORDEM-DE-AUTORIA")
      idcnpq <- Validate(autor, "NRO-ID-CNPQ")
      
      nome_autores <- SemiColon(nome_completo, nome_autores)
      nome_citacao_autores <- SemiColon(nome_citacao, nome_citacao_autores)
      ordem_autores <- SemiColon(ordem, ordem_autores)
      id_autores <- SemiColon(idcnpq, id_autores)
      
      
    }
    df_aux <- data.frame(
      # Informações básicas
      id_lattes = gsub(".xml", "", path.xml),
      nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
      id_patente = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
      
      # TAG: DADOS-BASICOS-DA-PATENTE
      titulo_patente = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "TITULO"),
      ano_desenvolvimento = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "ANO-DESENVOLVIMENTO"),
      pais = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "PAIS"),
      meio_de_divulgacao = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "MEIO-DE-DIVULGACAO"),
      flag_relevancia = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "FLAG-RELEVANCIA"),
      flag_potencial_inovacao = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "FLAG-POTENCIAL-INOVACAO"),
      
      # TAG: DETALHAMENTO-DA-PATENTE
      finalidade = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "FINALIDADE"),
      instituicao_financiadora = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "INSTITUICAO-FINANCIADORA"),
      categoria = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "CATEGORIA"),
      
      # TAG: DETALHAMENTO-DA-PATENTE > REGISTRO-OU-PATENTE
      tipo_patente = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
      #WARN: Informação duplicada, TITULO-PATENTE
      titulo_patente_reg = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
      data_pedido_de_deposito = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
      data_pedido_de_exame = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
      data_de_concessao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
      instituicao_deposito_registro = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
      numero_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
      formato_data_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
      data_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
      nome_titular = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
      nome_depositante = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
      sta_validacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
      
      # TAG: DETALHAMENTO-DA-PATENTE > HISTORICO-SITUACOES-PATENTE
      descricao_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "DESCRICAO-SITUACAO-PATENTE"),
      data_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "DATA-SITUACAO-PATENTE"),
      status_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "STATUS-SITUACAO-PATENTE"),
      
      # TAG: PALAVRAS-CHAVE
      palavra_chave_1 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-1"),
      palavra_chave_2 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-2"),
      palavra_chave_3 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-3"),
      palavra_chave_4 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-4"),
      palavra_chave_5 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-5"),
      palavra_chave_6 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-6"),
      
      # TAG: SETORES-DE-ATIVIDADE
      setor_de_atividade_1 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
      setor_de_atividade_2 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
      setor_de_atividade_3 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
      
      # TAG: INFORMACOES-ADICIONAIS
      descricao_info_adicionais = Validate(patente[["INFORMACOES-ADICIONAIS"]], "DESCRICAO-INFORMACOES-ADICIONAIS"),
      
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

# Check unique patents id

unique_patents_id <- df_pat %>% distinct(id_patente) %>% count()

# Convert to .xls file 
#write.xlsx(df_pat, file="patentes.xlsx", sheetName="sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

path.ids <- "../Lista_IDLattes_patentes_membros_03102018.xlsx"
df_ids <- read.xlsx(path.ids, sheetIndex = 1)
colnames(df_ids)[colnames(df_ids)=="nro_id_cnpq"] <- "id_lattes"

# Remove duplicated ids
df_ids <- subset(df_ids, !duplicated(df_ids$id_lattes))

result <- merge(df_ids, df_pat, by = "id_lattes")

newers <- filter(result, ano_desenvolvimento >= 2010)
incts <- data.frame(table(newers$inct))
colnames(incts) <- c('inct', 'frequência')

# Graph
library(ggplot2)

result_after_2010 <- filter(result, ano_desenvolvimento >= 2010)

greater_1996 <- filter(result, ano_desenvolvimento >= 1996)
year_df <- data.frame(table(greater_1996$ano_desenvolvimento), stringsAsFactors = FALSE)
colnames(year_df) <- c('ano', 'qtd_patentes')

# Graph by year
year_bar <- ggplot(year_df, aes(x=ano, y=qtd_patentes)) +
  geom_bar(stat='identity', fill='#388E8E') +
  labs(title = "Quantidade de patentes por ano", x = "Ano de desenvolvimento", y = "Número de patentes")


before_2010 <- filter(greater_1996, ano_desenvolvimento >= 2002 & ano_desenvolvimento < 2010)
after_2010 <- filter(greater_1996, ano_desenvolvimento >= 2010 & ano_desenvolvimento < 2018)

before_2010 <- data.frame(table(before_2010$ano_desenvolvimento), stringsAsFactors = FALSE)
colnames(before_2010) <- c('ano', 'qtd_patentes')

after_2010 <- data.frame(table(after_2010$ano_desenvolvimento), stringsAsFactors = FALSE)
colnames(after_2010) <- c('ano', 'qtd_patentes')

means_years <- data.frame("year" = c("2002-2009","2010-2017"), "media" = c(mean(before_2010$qtd_patentes), mean(after_2010$qtd_patentes)))

# Graph mean of years before and after 2010
mean_year_bar <- ggplot(means_years, aes(x=year, y=media)) +
  geom_bar(stat='identity', fill='#388E8E') +
  labs(title = "Média de patentes antes e após 2010", x = "Ano", y = "Média do número de patentes")


# By country graph
country_df <- data.frame(table(result_after_2010$pais))
colnames(country_df) <- c('pais', 'qtd_patentes')
filtered_countries <- filter(country_df, qtd_patentes > 1 & pais != 'Brasil')
country_bar <- ggplot(filtered_countries, aes(x=reorder(pais, qtd_patentes, sum), y=qtd_patentes)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 550, 25)) +
  geom_bar(stat='identity', fill = '#0d627a') +
  labs(title = "Patentes por países", x = "País", y = "Número de patentes")


# By category
pat_category <- data.frame(table(result_after_2010$categoria))
colnames(pat_category) <- c('categoria', 'qtd_patentes')
filtered_category <- filter(pat_category, qtd_patentes > 2 & categoria != "")
category_bar <- ggplot(filtered_category, aes(x=categoria, y=qtd_patentes)) +
  geom_bar(stat='identity', fill = '#0d627a') +
  labs(title = "Patentes por categoria", x = "Categoria", y = "Número de patentes")


# Key words cloud
library(wordcloud2)

keyword_df <- select(result_after_2010, palavra_chave_1, palavra_chave_2, 
                     palavra_chave_3, palavra_chave_4, palavra_chave_5, palavra_chave_6)

keyword_list <- unlist(keyword_df, use.names = FALSE)
keyword_list <- keyword_list[keyword_list != ""]
df_key <- data.frame(table(keyword_list))
wordcloud_key <- wordcloud2(df_key)

# By INCT
df_incts <- data.frame(table(result_after_2010$inct))
colnames(df_incts) <- c('inct', 'qtd_patentes')
# TODO DEFINE CHART