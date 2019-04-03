names <- c("combustion", "recycling", "craiglist")
values <- c(20, 30, 50)

df <- data.frame(names = names, values = values)

test1 <- lapply(X = names, FUN = function(x, df){
  paste("range", x, paste(df$names, collapse = ","))
}, df = df)

test2 <- mapply(x = names, y = values, FUN = function(x, y, df){
  paste("range", x, y, paste(df$names, collapse = ","))
}, MoreArgs = list(df = df)
, SIMPLIFY = FALSE, USE.NAMES = FALSE
)
