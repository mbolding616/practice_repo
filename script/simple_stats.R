# Libraries -----------------------------------------------------------------

library(tidyverse)
library(vegan)
library(patchwork)


# data --------------------------------------------------------------------
invert_data <- read.csv("data/invert_data.csv")
str(invert_data)
colnames(invert_data)


# Make into Univariate Dataframe ------------------------------------------

invert_data <- invert_data %>% 
  separate(site, c('site','sample'))
invert <- invert_data %>% select(Nemata:Unionidae)
env <- invert_data %>% select(site:sample)

#Vegan functions
richness <- specnumber(invert)
abundance <- rowSums(invert)
#Shannon-weaver
H <- diversity(invert, index = "shannon")
#Simpsons
D <- diversity(invert, "simpson")
#Pielous' evenness
J <- H/log(richness)

univariate <-  env
univariate$richness <- richness
univariate$shannon <- H
univariate$abundance <- abundance
univariate$simpson <- D
univariate$evenness <- J

write.csv(univariate, "data/univariate_data.csv", row.names = FALSE)
