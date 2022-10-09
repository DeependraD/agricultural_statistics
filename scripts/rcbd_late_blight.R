library(agricolae)

######## RCBD

## Old school way

dat<-read.csv(file = "clipboard", sep = "\t", header=TRUE)
dat

model1<-aov(Mean.AUDPC ~  Rep +  Treatment, data = dat)
model1
anova(model1)
comp4<-LSD.test(model1, "Treatment",console=TRUE)
comp4

#factorial()

dat<-read.csv(file = "clipboard", sep = "\t", header=TRUE)
dat
dat$Treat.Var <- with(dat,interaction(Treat, Var))
library(agricolae)
modle8<-aov(Yield..t.ha.~Rep+Treat+Var+Treat.Var,data=dat)
anova(modle8)
library(agricolae)
comp6<-LSD.test(modle8,"Treat.Var",alpha=0.001,console=TRUE)

comp6<-LSD.test(modle8,"Treat",alpha=0.001,console=TRUE)

comp6<-LSD.test(modle8,"Var",alpha=0.001,console=TRUE)


## Cooler way

require(tidyverse)
data <- readxl::read_xlsx("../data/rcbd_late_blight.xlsx", "Fungicide combination", "A2:J26")

names(data)
str(data)

data$Rep <- as.factor(data$Rep)
data$T.N <- as.factor(data$`T.N`)
data$Treatment <- as.factor(data$Treatment)

data <- janitor::clean_names(data)

var_mod_total_yield_kg_plot <- aov(`total_yield_kg_plot` ~ rep + treatment, data = data)
var_mod_mean_severity <- aov(`mean_severity` ~ rep + treatment, data = data)
var_mod_audpc_i <- aov(`audpc_i` ~ rep + treatment, data = data)
var_mod_total_number_of_tubers <- aov(`total_number_of_tubers` ~ rep + treatment, data = data)
var_mod_percent_of_infected_tubers <- aov(`percent_of_infected_tubers` ~ rep + treatment, data = data)

map(list(var_mod_total_yield_kg_plot,
      var_mod_mean_severity,
      var_mod_audpc_i,
      var_mod_total_number_of_tubers,
      var_mod_percent_of_infected_tubers) %>% set_names("yield", "severity",
                                                        "audpc_i", "audpc_ii",
                                                        "infected_tubers"), ~anova(.x))

map(list(var_mod_total_yield_kg_plot,
         var_mod_mean_severity,
         var_mod_audpc_i,
         var_mod_total_number_of_tubers,
         var_mod_percent_of_infected_tubers) %>% set_names("yield", "severity",
                                                           "audpc_i", "audpc_ii",
                                                           "infected_tubers"), ~resid(.x) %>% hist())



