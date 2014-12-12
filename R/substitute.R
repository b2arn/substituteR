require("evaluate")
require("RJDBC")

#! Substitute function
#! 
#! Allows to substitute R variables in text file
#! @param x text to be substituted
#! @keywords substitute 
 
substitute <- function (x) {
  m <- regexpr("`.*`", x)
  handler <- new_output_handler(value = function(x) {
    sprintf("%s", x)
  })
  regmatches(x, m) <- lapply(regmatches(x, m), function(expr) {
    real.expr <- substr(expr, 2, nchar(expr) - 1)
    evaluate(real.expr, new_device = F, 
             output_handler = handler)[[2]]
  })
  x
}

#! Execute function
#! 
#! Allows to run query with R variables in it
#! @param con RJDBC connection
#! @param file Sql file

execute <- function(con, file, has.result = T) {
  f <- dbGetQuery
  if (has.result) {
    f <- dbSendUpdate
  }
  f(con, paste(substitute(readLines(file)), collapse = " "))
}
