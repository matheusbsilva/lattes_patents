
# Graph
library(ggplot2)

FilterGreaterThan1996 <- function(df){
  greater_1996 <- filter(df, ano >= 1996)
  greater_1996
}

FilterGreaterThan2010 <- function(df) {
  result_after_2010 <- filter(df, ano >= 2010)
  result_after_2010
}

GraphByYear <- function(df) {
  greater_1996 <- FilterGreaterThan1996(df)
  year_df <- data.frame(table(greater_1996$ano), stringsAsFactors = FALSE)
  colnames(year_df) <- c('ano', 'qtd_patentes')
  
  year_bar <- ggplot(year_df, aes(x=ano, y=qtd_patentes)) +
    geom_bar(stat='identity', fill='#388E8E') +
    labs(title = "Quantidade de patentes por ano", x = "Ano de desenvolvimento", y = "Número de patentes")
  
}

GraphByMeanYear <- function(df) {
  greater_1996 <- FilterGreaterThan1996(df)
  before_2010 <- filter(greater_1996, ano >= 2002 & ano < 2010)
  after_2010 <- filter(greater_1996, ano >= 2010 & ano < 2018)
  
  before_2010 <- data.frame(table(before_2010$ano), stringsAsFactors = FALSE)
  colnames(before_2010) <- c('ano', 'qtd_patentes')
  
  after_2010 <- data.frame(table(after_2010$ano), stringsAsFactors = FALSE)
  colnames(after_2010) <- c('ano', 'qtd_patentes')
  
  means_years <- data.frame("year" = c("2002-2009","2010-2017"), "media" = c(mean(before_2010$qtd_patentes), mean(after_2010$qtd_patentes)))
  
  # Graph mean of years before and after 2010
  mean_year_bar <- ggplot(means_years, aes(x=year, y=media)) +
    geom_bar(stat='identity', fill='#388E8E') +
    labs(title = "Média de patentes antes e após 2010", x = "Ano", y = "Média do número de patentes")
  
}

GraphByCountry <- function(df) {
  result_after_2010 <- FilterGreaterThan2010(df)
  
  # By country graph
  country_df <- data.frame(table(result_after_2010$pais))
  colnames(country_df) <- c('pais', 'qtd_patentes')
  
  filtered_countries <- filter(country_df, qtd_patentes > 1 & pais != 'Brasil')
  country_bar <- ggplot(filtered_countries, aes(x=reorder(pais, qtd_patentes, sum), y=qtd_patentes)) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 550, 25)) +
    geom_bar(stat='identity', fill = '#0d627a') +
    labs(title = "Patentes por países", x = "País", y = "Número de patentes")
  
}

GraphByCategory <- function(df) {
  result_after_2010 <- FilterGreaterThan2010(df)
  # By category
  pat_category <- data.frame(table(result_after_2010$categoria))
  colnames(pat_category) <- c('categoria', 'qtd_patentes')
  filtered_category <- filter(pat_category, qtd_patentes > 2 & categoria != "")
  category_bar <- ggplot(filtered_category, aes(x=categoria, y=qtd_patentes)) +
    geom_bar(stat='identity', fill = '#0d627a') +
    labs(title = "Patentes por categoria", x = "Categoria", y = "Número de patentes")
}

CloudKeyWords <- function(df) {
  
  # Key words cloud
  library(wordcloud2)
  
  result_after_2010 <- FilterGreaterThan2010(df)
  
  keyword_df <- select(result_after_2010, palavra_chave_1, palavra_chave_2, 
                       palavra_chave_3, palavra_chave_4, palavra_chave_5, palavra_chave_6)
  
  keyword_list <- unlist(keyword_df, use.names = FALSE)
  keyword_list <- keyword_list[keyword_list != ""]
  df_key <- data.frame(table(keyword_list))
  wordcloud_key <- wordcloud2(df_key) 
}