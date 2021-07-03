x <- rchisq(n = 100, df = 8)
y <- x^2
calc_js_divergence(x = x, y = y)

mymat <- rbind(x, y)
calc_js_divergence(x = mymat)
