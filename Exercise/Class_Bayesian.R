options(cores = parallel::detectCores()) ; library("patchwork") ; library("brms")

ggplot(data = wooldridge::wine, aes(x = alcohol, y = heart)) + geom_point(size = 3, shape = 21, fill = "white", col = "black") +
  theme_minimal() +
  stat_smooth(method = "lm", linetype = "dashed", color = "red") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
        panel.background = element_rect(fill="#6666CC"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

reg  <- brms::brm(formula = heart ~ alcohol, data = wooldridge::wine,
                  chains = 3, iter = 20000, warmup = 2500, cores = 3,
                  prior = prior(normal(-15, 5), class = "b", coef = "alcohol"))

brms::bayes_R2(reg)

chains <- as.array(reg)[, , ]
traceplot_1 <- ggplot() + 
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 1]), aes(x = row, y = X1), size = 2, col = "#99000050") +
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 1]), aes(x = row, y = X2), size = 2, col = "#FF330050") +
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 1]), aes(x = row, y = X3), size = 2, col = "#FF990050") +
  theme_minimal() +
  labs(x = "", y = "estimate") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
        panel.background = element_rect(fill="#6666CC"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

traceplot_2 <- ggplot() + 
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 2]), aes(x = row, y = X1), size = 2, col = "#99000050") +
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 2]), aes(x = row, y = X2), size = 2, col = "#FF330050") +
  geom_line(data = data.frame(row = c(1:nrow(chains)), chains[, , 2]), aes(x = row, y = X3), size = 2, col = "#FF990050") +
  theme_minimal() +
  labs(x = "", y = "estimate") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
        panel.background = element_rect(fill="#6666CC"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

distribplot_1 <- data.frame(value = c(rnorm(500000, 232.92, 16.0))) %>% 
  ggplot(aes(x = value)) +
  geom_density(alpha = 0.5, fill = 'gold') +
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Intercept") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
        panel.background = element_rect(fill="#6666CC"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

distribplot_2 <- data.frame(value = c(rnorm(500000, -17.29, 3.62))) %>% 
  ggplot(aes(x = value)) +
  geom_density(alpha = 0.5, fill = 'gold') +
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "alcohol") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
        panel.background = element_rect(fill="#6666CC"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

(distribplot_1 + traceplot_1) / (distribplot_2 + traceplot_2)

data.frame(
  value = c(rnorm(100000, -17.29, 3.62), rnorm(100000, -19.683, 5.121), rnorm(100000, -15, 5)),
  distribution = rep(c("Posterior", "Likelihood", "Prior"), each = 100000)) %>% 
  ggplot(aes(x = value, fill = distribution)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Posterior" = "chartreuse3", "Likelihood" = "gold", "Prior" = "cornflowerblue")) +
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Distribution Comparison") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size=1),
          panel.background = element_rect(fill="#6666CC"),
          panel.grid = element_line(colour = NA),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.margin = unit(c(.5, .5, .5, .5), "cm"))
