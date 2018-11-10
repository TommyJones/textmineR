library(textmineR)

set.seed(90210)

d <- CreateDtm(nih_sample$ABSTRACT_TEXT,
               doc_names = nih_sample$APPLICATION_ID,
               ngram_window = c(1,2))

d <- d[,colSums(d) > 1]


m1 <- FitLdaModel(d, 25, 500, 450,
                  alpha = 0.1, beta = 0.05,
                  optimize_alpha = TRUE,
                  calc_likelihood = TRUE,
                  calc_r2 = TRUE)

a <- rgamma(25, shape = 1)

a <- a / sum(a) # * (0.1 * 25)

m2 <- FitLdaModel(d, 25, 500, 450,
                  alpha = a * 20, beta = 0.05,
                  optimize_alpha = TRUE,
                  calc_likelihood = TRUE,
                  calc_r2 = TRUE)

b <- colSums(d) / sum(d) * (0.05 * ncol(d))

m3 <- FitLdaModel(d, 25, 500, 450,
                  alpha = 0.1, beta = b,
                  optimize_alpha = TRUE,
                  calc_likelihood = TRUE,
                  calc_r2 = TRUE)

m4 <- FitLdaModel(d, 25, 500, 450,
                  alpha = a * 20, beta = b,
                  optimize_alpha = TRUE,
                  calc_likelihood = TRUE,
                  calc_r2 = TRUE)


plot(m1$log_likelihood, type = "l", col = "red", 
     ylim = range(c(m1$log_likelihood$log_likelihood, m4$log_likelihood$log_likelihood,
                    m2$log_likelihood$log_likelihood, m3$log_likelihood$log_likelihood)),
     lwd = 2, lty = 4, main = "Priors Matter")
lines(m2$log_likelihood, col = "blue", lwd = 2, lty = 3)
lines(m3$log_likelihood, col = "green", lwd = 2, lty = 2)
lines(m4$log_likelihood, col = "yellow", lwd = 2)
legend("bottomright",
       legend = c("             R2  Coh.",
                  paste("SA SB:", round(m1$r2,2), round(mean(m1$coherence),2)),
                  paste("AA SB:", round(m2$r2,2), round(mean(m2$coherence),2)),
                  paste("SA AB:", round(m3$r2,2), round(mean(m3$coherence),2)),
                  paste("AA AB:", round(m4$r2,2), round(mean(m4$coherence),2))),
       col = c("white", "red", "blue", "green", "yellow"),
       lwd = 2, lty = 5:1)
