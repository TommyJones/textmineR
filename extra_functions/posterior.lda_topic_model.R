

library(textmineR)
library(magrittr)
data("nih_sample_topic_model")

posterior <- function(object, ...) UseMethod("posterior")

posterior.lda_topic_model <- function(object, which = "theta", num_samples = 100, ...) {
  
  ### check inputs ----
  if (! class(object) == "lda_topic_model") 
    stop("object must be of class lda_topic_model")
  
  if (num_samples <= 0) {
    stop("num_samples must be an integer greater than 0")
  }
  
  num_samples <- round(num_samples) # in case someone is cheeky
  
  
  ### set up objects to extract ----
  if (which == "theta") {
    # extract dirichlet parameter for theta
    par <- object$theta * (sum(object$alpha) + rowSums(object$data))
    
  } else if (which == "phi") {

    # need to recover approximate theta count mat to get number of times
    # each topic was sampled
    theta_count <- object$theta * (sum(object$alpha) + rowSums(object$data)) 
    theta_count <- t(theta_count) - object$alpha 
    theta_count <- t(theta_count) %>% round %>% colSums()
    
    # now get the right parameter matrix
    par <- object$phi * (sum(object$beta) + theta_count)
    
  } else {
    stop("which must be one of 'theta' or 'phi'")
  }
  
  # get appropriate dim names to use later
  cols <- colnames(par)
  
  rows <- rownames(par)
  
  
  ### take samples ----
  
  # sample from each row (document or topic)
  samples <- lapply(seq_len(nrow(par)), function(j) par[j,]) %>%
    textmineR::TmParallelApply(function(x){
      p <- gtools::rdirichlet(n = num_samples, alpha = x)
      
      colnames(p) <- cols
      
      as.data.frame(p)
    }, export = c("num_samples", "cols"), libraries = "gtools", ...)
  
  
  samples <- mapply(function(x,y){
    x$var <- y
    x
  },x = samples, y = rows,
  SIMPLIFY = FALSE)
  
  # names(samples) <- rows
  # 
  # class(samples) <- "lda_posterior"
  
  samples <- do.call(cbind, samples)
  
  samples
  
}

### plot that sucker ----

library(ggjoy)
library(hrbrthemes)
library(ggplot2)
library(reshape2)

p <- posterior.lda_topic_model(nih_sample_topic_model)

p <- melt(p, variable.name = "topic")

r <- range(p$value)


ggplot(p[ p$var == "8777693" , ], aes(x = value, y = topic, height = ..density..)) + 
  geom_joy(scale = 3) +
  scale_x_continuous(limits = r) + 
  theme_ipsum(grid = FALSE) + 
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1)) +
  labs(title = "blah",
       subtitle = "blerg")


