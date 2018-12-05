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
