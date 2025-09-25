\dontrun{
x <- 1:10000
f <- function(y) y * y + 12
result <- TmParallelApply(x, f)
}
