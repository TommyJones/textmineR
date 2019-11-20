
# want to randomly initialize topics in theta

theta_counts <- gtools::rdirichlet(n = nrow(dtm),
                                   alpha = alpha)
theta_counts <- theta_counts * (sum(alpha) + rowSums(object$data))
theta_counts <- t(theta_counts) - object$alpha 
theta_counts <- t(theta_counts)

# want to reprodue topic-word distribution from previous model
theta_counts_sum <- t(theta_counts) %>% round() %>% colSums() # need to set up phi
phi_counts <- object$phi * (sum(beta) + theta_counts_sum)
phi_counts <- phi_counts - beta # assumes beta is a matrix




