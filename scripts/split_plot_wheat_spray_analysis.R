library(agricolae)
require(tidyverse)

# (Note: same data can be fitted as split or split plot designs. It is therefore important to note the field plan before fitting the model)
# split plot
factorial_split_plot_df <- readxl::read_xlsx("../data/split_plot_wheat_spray_variety.xlsx")

factorial_split_plot_df <- factorial_split_plot_df %>% 
  janitor::clean_names() %>% 
  mutate_at(c("treatment", "var", "treat", "rep"), as_factor)
  
factorial_split_plot_df %>% 
  count(var)

factorial_split_plot_df %>% 
  count(treat)

factorial_split_plot_model <- sp.plot(block = factorial_split_plot_df$rep,
        pplot = factorial_split_plot_df$treat,
        splot = factorial_split_plot_df$var,
        Y = factorial_split_plot_df$fb)

factorial_split_plot_aov_model_real <- aov(fb ~ rep + treat*var + Error(rep/treat), data = factorial_split_plot_df)
factorial_split_plot_aov_model_real <- aov(fb ~ rep + treat/var + rep/treat, data = factorial_split_plot_df)
factorial_split_plot_aov_model_real <- aov(fb ~ rep + treat:var + rep/treat, data = factorial_split_plot_df)
factorial_split_plot_aov_model_nest <- aov(fb ~ rep + rep/treat + Error(treat/var), data = factorial_split_plot_df)
factorial_split_plot_aov_full <- aov(fb ~ rep*treat*var, data = factorial_split_plot_df)

factorial_split_plot_aov_model_real
factorial_split_plot_aov_model_nest
factorial_split_plot_aov_model_full
factorial_split_plot_aov_model

factorial_split_plot_model$ANOVA

factorial_split_plot_aov_full
B <- suppressWarnings(anova(factorial_split_plot_aov_full))
W <- NULL
W <- B[c(1, 2, 7, 3, 6, 7), ]
for (j in 1:2) {
  W[3, j] <- B[4, j]
  W[6, j] <- B[5, j] + B[7, j]
}
W[, 3] <- W[, 2]/W[, 1]
W[2, 4] <- W[2, 3]/W[3, 3]
W[4:5, 4] <- W[4:5, 3]/W[6, 3]
W[2, 5] <- 1 - pf(W[2, 4], W[2, 1], W[3, 1])
W[4:5, 5] <- 1 - pf(W[4:5, 4], W[4:5, 1], W[6, 1])

# Get first error df
eda<-factorial_split_plot_model$gl.a
edb<-factorial_split_plot_model$gl.b

# Get first error MS
ea <-factorial_split_plot_model$Ea
eb <-factorial_split_plot_model$Eb

out1 <- LSD.test(factorial_split_plot_df$fb,
                 factorial_split_plot_df$treat, eda, ea,
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

# "var" mean comparison is useless because it has non-significant variance component
out2 <- LSD.test(factorial_split_plot_df$fb,
                 factorial_split_plot_df$var, edb, eb,
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

# "interaction" mean comparison is useless because it has non-significant variance component
out3 <- LSD.test(factorial_split_plot_df$fb,
                 interaction(factorial_split_plot_df$treat, factorial_split_plot_df$var), edb, eb, 
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

