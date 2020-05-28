# Premium Pricing Test
# Maj 2020
# Peer Christensen

library(tidyverse)
library(readxl)
library(ggridges)

df <- read_excel("pricing_test_premium2.xlsx") %>%
  mutate(rank = as.numeric(rank),
         conversionrate = as.numeric(conversionrate),
         pageviews = as.numeric(pageviews),
         adw_cac = as.numeric(adw_cac)) %>% 
  filter(rank <= 1000,conversionrate <101) %>%
  mutate(rank_group = factor(case_when(rank >= 601 ~ "gr4",
                                 rank <= 100 ~ "gr1",
                                 rank >= 101 & rank <= 600 ~ "gr3"))) %>%
  select(rank_group, rank,conversionrate, pageviews,adw_cac)

table(df$rank_group)

p1 <- df %>%
  ggplot(aes(y=conversionrate, x = rank_group, fill = rank_group)) +
  #geom_density_ridges()
  geom_jitter(alpha=.7) +
  geom_boxplot(outlier.shape = NA,alpha=.7) +
  theme_minimal()

p2 <- df %>%
  ggplot(aes(x=rank_group,y=pageviews, fill = rank_group)) +
  geom_jitter(alpha=.7) +
  geom_boxplot(outlier.shape = NA,alpha=.7)  +
  theme_minimal()

gridExtra::grid.arrange(p1,p2,ncol=2)

df %>%
  ggplot(aes(x=rank_group,y=adw_cac, fill = rank_group)) +
  geom_jitter(alpha=.7) +
  geom_boxplot(outlier.shape = NA,alpha=.7)  +
  theme_minimal()

df <- within(df, rank_group <- relevel(rank_group, ref = "gr3"))
m <- lm(adw_cac ~ rank_group,data=df)
summary(m)
