require(tidyverse)
### t distribution versus standard normal distribution (Z distribution).

# Let T be a t random variable with 3 degrees of freedom, and let Z be a
# standard normal random variable. Compare P(T < - 2.5) with P(Z < - 2.5)

# The absolute difference between the two probabilities is given by:

pt(-2.5, df = 3) - pnorm(-2.5)

# The relative magnitude of the probabilites is given by:

pt(-2.5, df = 3) / pnorm(-2.5)

# This implies that P(T < -2.5) is approximately 7 times larger! than P(Z < -2.5)

# How many degrees of freedom for a t random variable T do you need before
# P(T < -2.5) is only 50% larger than P(Z < -2.5) ?

mag_relative_p_tz <- function(q_value, df_t = 3){
  pr_t_dist <- pt(q_value, df_t)
  pr_n_dist <- pnorm(q_value)
  mag_relative <- pr_t_dist/pr_n_dist
  return(mag_relative)
}

minimum_zero_diff <- which.min(abs(outer(mag_relative_p_tz(df_t = 1:20), 2, "-")))
mag_relative_p_tz(df_t = 1:20)[minimum_zero_diff]
mag_relative_p_tz(df_t = minimum_zero_diff)

# why is relative magnitidue of T and Z probabilities at higher positive
# quantiles equal to 1 ?

map_dfr(seq(-2, 10, by = 0.1) %>%
          set_names(seq(-2, 10, by = 0.1)),
        ~mag_relative_p_tz(.x, df_t = 1:30), .id = "name") %>% 
  mutate(df_value = 1:30) %>% 
  pivot_longer(cols = -df_value, names_to = "quantile_value", values_to = "relative_magnitidue") %>% 
  ggplot(aes(x= df_value, y = relative_magnitidue, color = quantile_value)) +
  geom_line() +
  theme_bw() +
  guides(color = "none")
