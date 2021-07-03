x <- rchisq(n = 100, df = 8)
y <- x^2
calc_hellinger_dist(x = x, y = y)

mymat <- rbind(x, y)
CalcHellingerDist(x = mymat)
