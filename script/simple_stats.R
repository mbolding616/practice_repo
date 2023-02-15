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
invert_1m <- invert*10

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

# Summary Table -----------------------------------------------------------

univariate %>% 
  group_by(site) %>%
  summarise(S_mean = mean(richness),
            S_sd = sd(richness))
sum_table <- univariate %>% 
  group_by(site) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, min = min), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
sum_table <- sum_table %>% t %>% as.data.frame()
write.csv(sum_table, "data/univariate_summary.csv", row.names = TRUE)


# Figures -----------------------------------------------------------------

richness_plot <- ggplot(univariate, aes(x = site, y = richness)) +
  geom_boxplot(lwd = 0.75, aes(fill= site)) +
  geom_jitter(aes(shape = site, fill = site), 
              size = 4, stroke = 1.5, width = 0.05, height = 0.05)+
  theme_bw(14) +
  scale_shape_manual(values = c(21, 24, 22))+
  scale_fill_manual(values = c("#BA251E","#F2C73B", "#48484B")) +
  theme(legend.position = "none") +
  labs(x = "", 
       y = (expression(paste("Density per 1"," ",m^2)))) +
  scale_x_discrete(labels = c("station1" = "Station 1", 
                              "station2" = "Station 2", 
                              "station3" = "Station 3"))

ggsave("output/richness_plot.jpg", richness_plot)  

density_plot <- ggplot(univariate, aes(x = site, y = abundance)) +
  geom_boxplot(lwd = 0.75, aes(fill= site)) +
  geom_jitter(aes(shape = site, fill = site), 
              size = 4, stroke = 1.5, width = 0.05, height = 0.05)+
  theme_bw(14) +
  scale_shape_manual(values = c(21, 24, 22))+
  scale_fill_manual(values = c("#BA251E","#F2C73B", "#48484B")) +
  theme(legend.position = "none") +
  labs(x = "", 
       y = (expression(paste("Density per 1"," ",m^2)))) +
  scale_x_discrete(labels = c("station1" = "Station 1", 
                              "station2" = "Station 2", 
                              "station3" = "Station 3"))

ggsave("output/density_plot.jpg", density_plot)  

richness_plot / density_plot + plot_annotation(tag_levels = "A")


# multivariate ------------------------------------------------------------

invert_long <- invert_data %>% 
  pivot_longer(Nemata:Unionidae, 
               names_to = "species", 
               values_to = "count")

ggplot(invert_long, aes(x = site, y = count, fill = site)) +
  geom_col() +
  facet_wrap(.~species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "", 
       y = "")
