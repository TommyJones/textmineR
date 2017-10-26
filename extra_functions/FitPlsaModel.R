#### Just chicken scratch for now. This is my workspace to create a Plsa model
# fitted by the EM algorithm. Likely implementation will actually be in C++.
# ideally, I'd like to approximate it by chunking the data, calculating parameters
# in parallel, then reconciling by averaging. Wash, rinse, repeat.

### As of 10/26/2017, this is garbage that doesn't converge. More work to follow!

rm(list = ls())

library(textmineR)

### initialize some stuff with toy data ----------------------------------------
dtm <- as.matrix(nih_sample_dtm)

# these will (later) be initialized at random
# phi <- nih_sample_topic_model$phi
# theta <- nih_sample_topic_model$theta

k <- 10

phi <- gtools::rdirichlet(n = k, alpha = colSums(dtm) / sum(dtm) * 250) # do.call(rbind, lapply(1:k, function(x) colSums(dtm) / sum(dtm)))

theta <- do.call(rbind, lapply(seq_len(nrow(dtm)), function(x) rep(1 / k, k))) # gtools::rdirichlet(n = nrow(dtm), alpha = rep(0.1, k)) # 

p_w <- colSums(dtm) / sum(dtm)

p_d <- rowSums(dtm) / sum(dtm)

max_iters <- 200 # something small for now

cost_history <- numeric(max_iters)

### declare the main calculating functions -------------------------------------
GetPhi <- function(gamma, theta, p_d, p_w){ # 
  
  p_t <- p_d %*% theta
  
  # p_w <- p_t %*% phi
  
  result <- matrix(0, ncol = length(p_w),
                   nrow = length(p_w))
  
  diag(result) <- p_w
  
  result <- gamma %*% result
  
  p_t <- p_d %*% theta
  
  result <- result / as.numeric(p_t)
  
  result <- result / rowSums(result)
  
}

GetGamma <- function(phi, theta, p_d, p_w){ # 
  
  p_t <- p_d %*% theta
  
  # p_w <- p_t %*% phi
  
  result <- matrix(0, ncol = ncol(p_t), nrow = ncol(p_t))
  
  diag(result) <- p_t
  
  result <- result %*% phi
  
  result <- t(apply(result, 1, function(x) x / p_w))
  
  result <- t(result) / colSums(result)

  result <- t(result)
}

GetTheta <- function(gamma, dtm){
  (dtm / rowSums(dtm)) %*% t(gamma)
}

GetSS <- function(phi, theta, dtm){
  
  yhat <- (theta %*% phi) * rowSums(dtm)
  
  ss <- (yhat - dtm) ^ 2
  
  ss <- sum(rowSums(ss))
  
  ss
}


### do some iterations ---------------------------------------------------------

iter <- 1

while (iter <= max_iters) {
  
  # get gamma
  gamma <- GetGamma(phi = phi, theta = theta, p_d = p_d, p_w = p_w) #
  
  # get theta
  theta <- GetTheta(gamma = gamma, dtm = dtm)
  
  # get phi
  phi <- GetPhi(gamma = gamma, theta = theta, p_d = p_d, p_w = p_w) # 
  
  # get names lined up
  rownames(theta) <- rownames(dtm)
  colnames(phi) <- colnames(dtm)
  rownames(phi) <- paste0("t_", seq_len(nrow(phi)))
  colnames(theta) <- rownames(phi)
  
  # get cost
  cost_history[ iter ] <- GetSS(phi = phi, theta = theta, dtm = dtm) # CalcTopicModelR2(dtm = Matrix(dtm, sparse = T), phi, theta) # mean(CalcProbCoherence(phi, dtm, 5)) # 
  
  iter <- iter + 1
  
}


plot(cost_history, type = "l")
