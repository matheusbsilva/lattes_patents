# Params:
#   df = Dataframe with patente information
#   filepath = Filepath of the xlsx file with INCT info
MergeWithINCT <- function(df, filepath) {
  path.ids <- filepath
  df_incts <- read.xlsx(path.ids, sheetIndex = 1)
  colnames(df_incts)[colnames(df_incts)=="nro_id_cnpq"] <- "id_lattes"
  
  # Remove duplicated ids
  df_incts <- subset(df_incts, !duplicated(df_incts$id_lattes))
  
  result <- merge(df_incts, df, by = "id_lattes")
  
  result
}