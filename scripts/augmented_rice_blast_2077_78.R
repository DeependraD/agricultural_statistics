# Augmented design

rm(list =ls())
#shyam <- read.csv("C:/Users/vijay/Desktop/NARC/ANNUAL_LIBRARY/Rice blast Data 2077.78.csv")
dat8<-read.csv(file = "clipboard", sep = "\t", header=TRUE)
dat8
library(augmentedRCBD)
dat8$Block<-as.factor(dat8$Block)
dat8$Entry <-as.factor(dat8$Entry)
dat8$Genotype <-as.factor(dat8$Genotype)
dat8$Score<-as.numeric(dat8$Score)
str(dat8)
ram<-augmentedRCBD(dat8$Block, dat8$Genotype, dat8$Score, method.comp = "lsd",group = TRUE, console=TRUE, alpha = 0.05)
report.augmentedRCBD(ram, file.path(tempdir(),
                                    "augmentedRCBDoutput.docx"))