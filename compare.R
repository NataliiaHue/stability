library(ggplot2) # for plots, incl. histograms
library(patchwork) # for saving multiple plots in one file
library(tidyverse) # for data restructuring
library(readr)

# set the directory to where the dataframe with the features lies - needed to filter old stats
setwd("/Users/neshcheret/Documents/GitHub/hueblerstability")

categories <- read_tsv("etc/features_with_categories.tsv")

# set the directory to where the results lie
setwd("/Users/neshcheret/Documents/GitHub/stability")

# read in the old stats
old_stats <- read_tsv("SI_summary_table.csv") # 224 features
old_stats <- old_stats[old_stats$Feature %in% categories$ID == TRUE, ] # 171 features
summary_rates_2020 <- old_stats %>%
  select(Feature, q10_2020 = Median_rate_loss, q01_2020 = Median_rate_gain,
         loglikelihood_2020 = LogLikelihood)

# read in the newest rates - Oct 29th
df_rates_oct_29 <- read.csv('results_asr_rates.csv', header=TRUE)
df_rates_oct_29 <- df_rates_oct_29[df_rates_oct_29$Model == 'ARD', ]

summary_rates_oct_29 <- df_rates_oct_29 %>%
  group_by(Feature) %>%
  summarise(q01_oct_29 = median(q01),
            q10_oct_29 = median(q10),
            loglikelihood_oct_29 = median(LogLikelihood))

# read in the second newest rates - Oct 22nd
df_rates_oct_22 <- read.csv('results_asr_rates_oct_22.csv', header=TRUE)
df_rates_oct_22 <- df_rates_oct_22[df_rates_oct_22$Model == 'ARD', ]

summary_rates_oct_22 <- df_rates_oct_22 %>%
  group_by(Feature) %>%
  summarise(q01_oct_22 = median(q01),
            q10_oct_22 = median(q10),
            loglikelihood_oct_22 = median(LogLikelihood))

compare <- summary_rates_2020 %>%
  inner_join(summary_rates_oct_22, by = c("Feature" = "Feature")) %>%
  inner_join(summary_rates_oct_29, by = c("Feature" = "Feature"))
  
compare_oct_rate_loss <- ggplot(compare, aes(q01_oct_22,q01_oct_29)) +
  geom_point()
compare_oct_rate_loss
compare_oct_rate_gain <- ggplot(compare, aes(q10_oct_22, q10_oct_29)) +
  geom_point()
compare_oct_rate_gain

compare_rate_loss <- ggplot(compare, aes(q10_2020, q10_oct_29)) +
  geom_point()
compare_rate_loss
compare_rate_gain <- ggplot(compare, aes(q01_2020, q01_oct_29)) +
  geom_point()
compare_rate_gain

ggsave("compare.pdf", compare_rate_loss / compare_oct_rate_loss | compare_rate_gain  / compare_oct_rate_gain,height=10,width=10)

compare_likelihood <- ggplot(compare, aes(loglikelihood_oct_29, loglikelihood_2020)) +
  geom_point()
compare_likelihood

compare_oct_likelihood <- ggplot(compare, aes(loglikelihood_oct_29, loglikelihood_oct_22)) +
  geom_point()
compare_oct_likelihood
