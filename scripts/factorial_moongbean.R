require(agricolae)
require(tidyverse)

# moong_data <- readxl::read_xlsx("./data_moong.xlsx", na = c("0")) %>%
#   janitor::clean_names() %>%
#   mutate_at(c("factor_a", "factor_b", "replication"), as_factor)

ecoyldwp_aov_model <- aov(ecoyldwp ~ factor_a + factor_b + replication + factor_a:factor_b, data = moong_data)

anova(ecoyldwp_aov_model) # anova shows non-significant variance components! so not necessary to go to mean comparison

LSD.test(ecoyldwp_aov_model, trt = "factor_a", console = TRUE)
LSD.test(ecoyldwp_aov_model, trt = "factor_b", console = TRUE)

LSD.test(moong_data$ecoyldwp,
         interaction(moong_data$factor_a,
                     moong_data$factor_b),
         console = TRUE, DFerror = ecoyldwp_aov_model$df.residual,
         MSerror = anova(ecoyldwp_aov_model)[4,3])
