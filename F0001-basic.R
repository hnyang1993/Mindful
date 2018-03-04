# extractVar: extracting variables and the column number from the list of variables
extractVar <- function(pattern, name = data.label$name, ...) {
  index = grep(pattern, name, ...)
  return(data.frame(no=index, variable = name[index], stringsAsFactors = FALSE))
}

# alpha.ci: getting CI for cronbach's alpha
alpha.ci <- function(x, digits = 2) {
  round(c(x$total$raw_alpha - 1.96 * x$total$ase,                 
          x$total$raw_alpha, 
          x$total$raw_alpha + 1.96 * x$total$ase), digits = digits)
}
