library(agricolae)
require(tidyverse)

# (Note: same data can be fitted as split or split plot designs. It is therefore important to note the field plan before fitting the model)
# strip plot
factorial_strip_plot_df <- readxl::read_xlsx("../data/split_plot_wheat_spray_variety.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_at(c("treatment", "var", "treat", "rep"), as_factor)
  

factorial_strip_plot_model <- strip.plot(BLOCK = factorial_strip_plot_df$rep,
                                         COL = factorial_strip_plot_df$treat,
                                         ROW = factorial_strip_plot_df$var,
                                         Y = factorial_strip_plot_df$fb)

# what's the correct model spec ?
factorial_strip_plot_aov_model <- aov(fb ~ rep + treat + var + rep:var + rep:treat + var:treat,
    data = factorial_strip_plot_df) # this is correct formulation

factorial_strip_plot_aov_model %>% 
  anova()

factorial_strip_plot_model$ANOVA

# Get first error df
eda <- factorial_strip_plot_model$gl.a
edb <- factorial_strip_plot_model$gl.b

# Get first error MS
ea <- factorial_strip_plot_model$Ea
eb <- factorial_strip_plot_model$Eb

out1 <- LSD.test(factorial_strip_plot_df$fb,
                 factorial_strip_plot_df$treat, eda, ea,
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

# "var" mean comparison is useless because it has non-significant variance component
out2 <- LSD.test(factorial_strip_plot_df$fb,
                 factorial_strip_plot_df$var, edb, eb,
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

# "interaction" mean comparison is useless because it has non-significant variance component
out3 <- LSD.test(factorial_strip_plot_df$fb,
                 interaction(factorial_strip_plot_df$treat, factorial_strip_plot_df$var), edb, eb, 
                 alpha=0.05, p.adj="none", group=TRUE, console = TRUE)

