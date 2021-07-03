\dontrun{
a <- posterior(object = nih_sample_topic_model, which = "theta", num_samples = 20)

plot(density(a$t1[a$var == "8693991"]))

b <- posterior(object = nih_sample_topic_model, which = "phi", num_samples = 20)

plot(denisty(b$research[b$var == "t_5"]))
}
