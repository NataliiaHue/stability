library(reshape) # for melt()
library(ggplot2) # for plots, incl. histograms
library(ggridges) # for ridgeplot
library(patchwork) # for saving multiple plots in one file
library(tidyverse) # for data restructuring
library(readr)

# This scripts needs the following data:
# - results: d, rates, states
# - values from raw cldf
# - categories from etc cld

# The feature set in categories represents the final version of the "informative" features (not absent across the data set)
# The analysis was run for the feature set in the categories file
# match the raw values with categoriea and remove all features NOT present in categories

setwd("/Users/neshcheret/Documents/GitHub/hueblerstability")

# read in the categories
# categories <- read_tsv("https://raw.githubusercontent.com/cldf-datasets/hueblerstability/main/etc/features_with_categories.tsv")

# read in coded values
# values <- read_csv("https://raw.githubusercontent.com/cldf-datasets/hueblerstability/main/cldf/values.csv")

categories <- read_tsv("etc/features_with_categories.tsv")

features <- categories$ID # Save feature names in a variable
description <- categories$Feature # Save the feature question in variable
PoS <- categories$PoS # Save the part of speech in a variable
Function <- categories$Function # Save the functional category in a variable
Level <- categories$Level # Save the language level in a variable
Feature_short <- categories$Feature_short

values <- read_csv("cldf/values.csv")

# Merge the data
data <- values %>%
  select(Parameter_ID, Language_ID, Value) %>%
  rename(ID = Parameter_ID, variable = Language_ID, value = Value) %>%
  mutate(value = replace(value, which(value == "?"), NA))

data <- data[data$ID %in% categories$ID == TRUE, ]
length(unique(data$ID)) # 171

# convert from tibble to a data frame
data <- as.data.frame(data)

data$value <- as.numeric(data$value)

setwd("/Users/neshcheret/Documents/GitHub/stability")

# load phylogenetic signal (D) results
df_d <- read.csv('results_d.csv', header = TRUE)

# collect rates results
asr_rates_oct29 <- lapply(list.files('asr_29_oct', 'asr_rates.*.csv', full.names=TRUE), read_csv) %>% bind_rows()
#write.csv(asr.oct29, 'results_asr_rates.csv', quote=FALSE, row.names=FALSE)

# the previous command produces a tibble -> convert to data.frame
df_rates <- as.data.frame(asr_rates_oct29)
# df_rates <- as.data.frame(df_rates)
# df_rates <- read.csv('results_asr_rates.csv', header=TRUE)

# throw away the other models except for the q01 and q10 rates found by the ARD model
df_rates <- df_rates[df_rates$Model == 'ARD', ]

# collect states results
asr_states_oct29 <- lapply(list.files('asr_29_oct', 'asr_states.*.csv', full.names=TRUE), read_csv) %>% bind_rows()

# the previous command produces a tibble -> convert to data.frame
df_states <- as.data.frame(asr_states_oct29)
#df_states <- read.csv('results_asr_states.csv', header = TRUE)

# throw away the other models except for the states found by the ARD model
df_states <- df_states[df_states$Model == 'ARD', ]

length(unique(df_rates$Feature)) # 171
length(unique(df_states$Feature)) # 171
length(unique(df_d$Feature)) # 171
length(unique(df_states$Feature)) # 171
length(categories$ID) # 171

# Create a dataframe with summaries of the results
stats <- data.frame(
    Feature = features,
    Feature_short = Feature_short,
    Description = description,
    Function = Function,
    Level = Level,
    PoS = PoS,
    Values = sapply(features, function(f) length(na.exclude(data[data$ID == f, 'value']))), # Number of data points (non-missing data)
    Present = sapply(features, function(f) sum(na.exclude(data[data$ID == f, 'value']))), # Number of 1's
    Proportion_present = sapply(features, function(f) round(sum(na.exclude(data[data$ID == f, 'value']))/length(na.exclude(data[data$ID == f, 'value'])), digits = 2)), # Proportion of 1's
    Proportion_missing = sapply(features, function(f) round(sum(is.na(data[data$ID == f, 'value']))/length(is.na(data[data$ID == f, 'value'])), digits = 2)), # Proportion of NA's
    Median_D = sapply(features, function(f) round(median(df_d[df_d$Feature == f, 'D']), digits = 2)),
    SD_D = sapply(features, function(f) round(sd(df_d[df_d$Feature == f, 'D']), digits = 2)),
    Min_D = sapply(features, function(f) round(min(df_d[df_d$Feature == f, 'D']), digits = 2)),
    Max_D = sapply(features, function(f) round(max(df_d[df_d$Feature == f, 'D']), digits = 2)),
    LogLikelihood_rates = sapply(features, function(f) round(mean(df_rates[df_rates$Feature == f, 'LogLikelihood']), digits = 2)),
    AICc_rates = round(sapply(features, function(f) round(median(df_rates[df_rates$Feature == f, 'AICc']), digits = 2))),
    Median_rate_q10 = sapply(features, function(f) round(median(df_rates[df_rates$Feature == f, 'q10'], na.rm = TRUE), digits = 2)),
    SD_rate_q10 = sapply(features, function(f) round(sd(df_rates[df_rates$Feature == f, 'q10']), digits = 2)),
    Median_rate_q01 = sapply(features, function(f) round(median(df_rates[df_rates$Feature == f, 'q01'], na.rm = TRUE), digits = 2)),
    SD_rate_q01 = sapply(features, function(f) round(sd(df_rates[df_rates$Feature == f, 'q01']), digits = 2)),
    Min_rate_q10 = sapply(features, function(f) round(min(df_rates[df_rates$Feature == f, 'q10'], na.rm=TRUE), digits = 2)),
    Min_rate_q01 = sapply(features, function(f) round(min(df_rates[df_rates$Feature == f, 'q01'], na.rm=TRUE), digits = 2)),
    Max_rate_q10 = sapply(features, function(f) round(max(df_rates[df_rates$Feature == f, 'q10'], na.rm=TRUE), digits = 2)),
    Max_rate_q01 = sapply(features, function(f) round(max(df_rates[df_rates$Feature == f, 'q01'], na.rm=TRUE), digits = 2)),
    LogLikelihood_states = sapply(features, function(f) round(mean(df_states[df_states$Feature == f, 'LogLikelihood']), digits = 2)),
    AICc_states = round(sapply(features, function(f) round(median(df_states[df_states$Feature == f, 'AICc']), digits = 2))),
    p1_turkic = sapply(features, function(f) round(mean(df_states[df_states$Feature == f & df_states$Clade == "Turkic", 'p1']), digits = 2)),
    p1_mongolic = sapply(features, function(f) round(mean(df_states[df_states$Feature == f & df_states$Clade == "Mongolic", 'p1']), digits = 2)),
    p1_tungusic = sapply(features, function(f) round(mean(df_states[df_states$Feature == f & df_states$Clade == "Tungusic", 'p1']), digits = 2)),
    p1_koreanic = sapply(features, function(f) round(mean(df_states[df_states$Feature == f & df_states$Clade == "Koreanic", 'p1']), digits = 2)),
    p1_japonic = sapply(features, function(f) round(mean(df_states[df_states$Feature == f & df_states$Clade == "Japonic", 'p1']), digits = 2))
   )

setwd("/Users/neshcheret/Documents/GitHub/stability")
write.table(stats,"SI_summary_table_Oct_29.csv", sep = "\t", row.names=FALSE, quote = TRUE)

# Check if there are uninformative features with all absent
stats[stats$Present==0,]

# Check if there are cases with missing values
stats[!complete.cases(stats),]

# Set theme_classic() as default for all plots
theme_set(theme_classic())

###################### Phylogenetic signal and evolutionary rate ######################

# Histograms on median rate q01, q10 and phylogenetic signal

h1 <- ggplot(df_d, aes(x = D)) + 
  geom_histogram(binwidth = 3) + 
  xlab('D')
h1

h2 <- ggplot(df_rates, aes(q01)) + 
  geom_histogram(binwidth = 10) + 
  xlab('Rate of feature gain (0 -> 1)')
h2

h3 <- ggplot(df_rates, aes(q10)) +
  geom_histogram(binwidth = 10) +
  xlab('Rate of feature loss (1 -> 0)')
h3

ggsave("histograms.pdf", (h1 | h2 | h3), height=5, width=12)

# what is the relationship between the median _D_ score and the number of present characters
present_d <- ggplot(stats, aes(x = Present, y = Median_D, color = Values)) +
    geom_point(show.legend = FALSE) +
    ggtitle('D vs. "1"') +
    xlab('Proportion present') +
    ylab('D') +
    scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")

present_q10 <- ggplot(stats, aes(x = Present, y = Median_rate_q10, color = Values)) +
  geom_point(show.legend = FALSE) +
  ggtitle('Feature loss vs. "1"') +
  xlab('Proportion present') +
  ylab('Rate of feature loss') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")

present_q01 <- ggplot(stats, aes(x = Present, y = Median_rate_q01, color = Values)) +
  geom_point() +
  ggtitle('Feature gain vs. "1"') +
  xlab('Proportion present') +
  ylab('Rate of feature gain') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")

ggsave("present.pdf", (present_d | present_q10 | present_q01), height=2, width=8)

# what is the relationship between the median _D_ score and the amount of missing data?
missing_d <- ggplot(stats, aes(x = Proportion_missing, y = Median_D, color = Values)) +
  geom_point(show.legend = FALSE) +
  ggtitle('D vs. NA') +
  xlab('Proportion missing') +
  ylab('D') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")
missing_d

# what is the relationship between the median rate of loss score and the amount of missing data?
missing_q10 <- ggplot(stats, aes(x = Proportion_missing, y = Median_rate_q10, color = Values)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Feature loss vs. NA") +
  xlab('Proportion missing') +
  ylab('Rate of feature loss') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")
missing_q10

# what is the relationship between the median rate of gain score and the amount of missing data?
missing_q01 <- ggplot(stats, aes(x = Proportion_missing, y = Median_rate_q01, color = Values)) +
  geom_point() +
  ggtitle("Feature gain vs. NA") +
  xlab('Proportion missing') +
  ylab('Rate of feature gain') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue")
missing_q01

ggsave("missing-data.pdf", (missing_d | missing_q10 | missing_q01), height=2, width=8)

cor_d_missing <-cor.test(stats$Proportion_missing, stats$Median_D, method="kendall")
cor_d_missing # 0.06

cor_q01_missing <-cor.test(stats$Proportion_missing, stats$Median_rate_q01, method="kendall")
cor_q01_missing # 0.22

cor_q10_missing <-cor.test(stats$Proportion_missing, stats$Median_rate_q10, method="kendall")
cor_q10_missing # 0.22

# How many features could I code for less than 50% of languages? -> 9 features, or 5 %
stats %>%
  count(Proportion_missing < 0.5) %>%
  mutate(Percentage = n/sum(n))

stats %>%
  count(Proportion_missing < 0.3) %>%
  mutate(Percentage = n/sum(n))

stats_missing_less_fifty <- stats %>%
  filter(Proportion_missing < 0.5)

# > 1 overdispersed
# = 1 random
# = 0 brownian
# < 0 extremely clumped

# Parallel boxplots for function and rate
b1 <- ggplot(stats, aes(x = Function, y = log10(Median_rate_q01))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())
b1

b2 <- ggplot(stats, aes(x = Function, y = log10(Median_rate_q10))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())
b2

b3 <- ggplot(stats, aes(x = Function, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Function') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=1), axis.ticks.x = element_blank())
b3

ggsave("boxplots-function.pdf", b1  /  b2 / b3,height=10,width=7)

# Parallel boxplots for level and rate
b4 <- ggplot(stats, aes(x = Level, y = log10(Median_rate_q01))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())
b4

b5 <- ggplot(stats, aes(x = Level, y = log10(Median_rate_q10))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())
b5

b6 <- ggplot(stats, aes(x = Level, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Level') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2), axis.ticks.x = element_blank())
b6

ggsave("boxplots-level.pdf", b4  /  b5 / b6 ,height=10,width=7)

# Parallel boxplots for PoS and rate

b7 <- ggplot(stats, aes(x = PoS, y = log10(Median_rate_q01))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())

b8 <- ggplot(stats, aes(x = PoS, y = log10(Median_rate_q10))) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank())

b9 <- ggplot(stats, aes(x = PoS, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Part of speech') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2), axis.ticks.x = element_blank())

ggsave("boxplots-pos.pdf", b7 / b8 / b9, height = 10, width = 7)


###################### Ridgeplots for phylogenetic signal and rate ######################

#feature_set <- read.csv("feature_set.txt", sep="\t")

df_d_feature_short <- df_d %>% 
  inner_join(categories,by=c("Feature"="ID")) %>%
  select(Tree, Feature_short, D, PVal1, PVal0)

# feature_order <- reorder(df_d_feature_short$Feature_short, df_d_feature_short$D)

# feature_order_factor <- unique(feature_order)

# reorder feature by median _D_ value to help plotting later
#df_d$Feature <- with(df_d, reorder(Feature, D, median))
#df_d_feature_short$Feature_short <- with(df_d_feature_short, reorder(Feature_short, D, median))

p_d <- ggplot(df_d_feature_short, aes(x = D, y = reorder(Feature_short, D), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale=2) +
  scale_fill_gradient(low="orange", high="blue") +
  theme(axis.title.y=element_blank()) +
  geom_vline(xintercept=0) +
  geom_vline(xintercept=1) +
  xlim(-5, 5) +
  guides(fill="none")

ggsave('ridgeplot-D.pdf', height=20, width=10)

# delete rows with infinite values (intrudoced through log10 transformation)
#df_rates <- df_rates[!df_rates$Log10_q01==(-Inf),]
#df_rates <- df_rates[!df_rates$Log10_q10==(-Inf),]

df_rates_feature_short <- df_rates %>% 
  inner_join(categories,by=c("Feature"="ID")) %>%
  select(Tree, Feature_short, q01, q10)

#df_rates_feature_short <- df_rates_feature_short[!df_rates_feature_short$Log10q01==(-Inf),]
#df_rates_feature_short <- df_rates_feature_short[!df_rates_feature_short$q10==(-Inf),]

# reorder feature by median q01 rate value to help plotting later
#df_rates$Feature <- with(df_rates, reorder(Feature, q01, median))

p_rate_01 <- ggplot(df_rates_feature_short, aes(x = log10(q01), y = reorder(Feature_short, log10(q01)), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale = 2) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient(low="orange", high="blue") +
  geom_vline(xintercept=0) +
  guides(fill="none") +
  xlab("Log10 0-1 transition rate (feature gain)")
p_rate_01
ggsave('ridgeplot-q01.pdf', height=20, width=10)

p_rate_10 <- ggplot(df_rates_feature_short, aes(x = log10(q10), y = reorder(Feature_short, log10(q10)), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale=2) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient(low="orange", high="blue") +
  geom_vline(xintercept=0) +
  guides(fill="none") +
  xlab("1-0 transition rate (feature loss)")
p_rate_10
ggsave('ridgeplot-q10.pdf', height=20, width=10)

# Save the median D values in a variable
median_d <- stats$Median_D
rate_loss <- log10(stats$Median_rate_q10)
rate_gain <- log10(stats$Median_rate_q01)

# correlation between D and 10 transition rate
corr_q10 <- cor.test(median_d,rate_loss, method="kendall")
corr_q10 # 0.5099
# correlation between D and 01 transition rate
corr_q01 <- cor.test(median_d,rate_gain, method="kendall")
corr_q01 # 0.4981

# plot the correlation between phylogenetic signal and q10 transition rate

ph_df_rates_q10 <- ggplot(stats,aes(Median_rate_q10, Median_D, color=Present)) +
  geom_point() +
  scale_x_log10() +
  theme_classic() +
  geom_smooth(method='lm', formula= y~x) +
  annotate(geom = "text", x = 1, y = 2, label = paste("tau = ",round(corr_q10$estimate, digits = 2))) +
  xlab("Log10 rate (transition from 1 to 0, feature loss)") +
  ylab("Median (D)")
ph_df_rates_q10

# plot the correlation between phylogenetic signal and q01 transition rate
round(corr_q01$estimate, digits = 2)
ph_df_rates_q01 <- ggplot(stats,aes(Median_rate_q01,Median_D, color=Present)) +
  geom_point() +
  scale_x_log10() +
  theme_classic() +
  geom_smooth(method='lm', formula= y~x) +
  annotate(geom="text", x = 1, y = 2.5, label = paste("tau = ",round(corr_q01$estimate, digits = 2))) +
  xlab("Log10 rate (transition from 0 to 1, feature gain)") +
  ylab("Median (D)")
ph_df_rates_q01

ggsave("correlation.pdf", ph_df_rates_q10 / ph_df_rates_q01, height=5,width=5)

num_features <- length(stats$Feature)

# stats for the results: phylogenetic signal
x1 <- round(sum(median_d < 0) / num_features, digits = 2) # overclumped
x1
x2 <- round((sum(median_d >= 0 & median_d < 0.5) / num_features), digits = 2) # phylogenetic signal
x2
x3 <- round(sum(median_d >= 0.5 & median_d <= 1) / num_features, digits = 2) # random
x3
x4 <- round(sum(median_d > 1) / num_features, digits = 2) # overdispersed
x4

# Check whether the proportions sum up to 1
x1 + x2 + x3 + x4
x1 + x2 # number of features with a phylogenetic signal
x3 + x4 # number of features without a phylogenetic signal

# stats for the results: rate. What is the proportion of features that evolve slowly and fast?
lost_slowly <- round(sum(stats$Log10Rate_q10 < 0) / num_features, digits = 2)
gained_slowly <- round(sum(stats$Log10Rate_q01 < 0) / num_features, digits = 2)

lost_fast <- round(sum(stats$Log10Rate_q10 > 0) / num_features, digits = 2)
gained_fast <- round(sum(stats$Log10Rate_q01 > 0) / num_features, digits = 2)

# stats for the results: rate (slow, medium, fast)

y1_q01 <- round(sum(log10(stats$Median_rate_q01) < (-0.5)) / num_features, digits = 2) # slow
y1_q01
y2_q01  <- round(sum(log10(stats$Median_rate_q01) > (-0.5) & log10(stats$Median_rate_q01) < 0.5) / num_features, digits = 2) # medium
y2_q01
y3_q01  <- round(sum(log10(stats$Median_rate_q01) > 0.5) / num_features, digits = 2) # fast
y3_q01

# Check whether the proportions sum up to 1
y1_q01 + y2_q01 + y3_q01

y1_q10 <- round(sum(log10(stats$Median_rate_q10) < (-0.5))/num_features, digits = 2) # slow
y2_q10  <- round(sum(log10(stats$Median_rate_q10) > (-0.5) & log10(stats$Median_rate_q10) < 0.5)/num_features, digits = 2) # medium
y3_q10  <- round(sum(log10(stats$Median_rate_q10) > 0.5) / num_features, digits = 2) # fast

# Check whether the proportions sum up to 1
y1_q10 + y2_q10 + y3_q10

# stats for results: reports on rate of gain
range_min_gain <- range(stats$Min_rate_q01)
median_min_gain <- median(stats$Min_rate_q01)
sd_min_gain <- sd(stats$Min_rate_q01)

range_max_gain <- range(stats$Max_rate_q01)
median_max_gain <- median(stats$Max_rate_q01)
sd_max_gain <- sd(stats$Max_rate_q01)

range_median_gain <- range(stats$Median_rate_q01)
median_median_gain <- median(stats$Median_rate_q01)
sd_median_gain <- sd(stats$Median_rate_q01)

# stats for results: reports on rate of loss

range_min_loss <- range(stats$Min_rate_q10)
median_min_loss <- median(stats$Min_rate_q10)
sd_min_loss <- sd(stats$Min_rate_q10)

range_max_loss <- range(stats$Max_rate_q10)
median_max_loss <- median(stats$Max_rate_q10)
sd_max_loss <- sd(stats$Max_rate_q10)

range_median_loss <- range(stats$Median_rate_q10)
median_median_loss <- median(stats$Median_rate_q10)
sd_median_loss <- sd(stats$Median_rate_q10)


# basic stats for the results

min_d <- round(min(df_d$D), digits = 2)
max_d <- round(max(df_d$D), digits = 2)
median_d <- round(median(df_d$D), digits = 2)
sd_d <- round(sd(df_d$D), digits = 2)

min_loss <- round(min(df_rates$q10), digits = 2)
max_loss <- round(max(df_rates$q10), digits = 2)
median_loss <- round(median(df_rates$q10), digits = 2)
sd_loss <- round(sd(df_rates$q10), digits = 2)

min_gain <- round(min(df_rates$q01), digits = 2)
max_gain <- round(max(df_rates$q01), digits = 2)
median_gain <- round(median(df_rates$q01), digits = 2)
sd_gain <-round(sd(df_rates$q01), digits = 2)

basic_statistics<- data.frame(
  Min = c(min_d, min_loss, min_gain),
  Median = c(median_d, median_loss, median_gain),
  Max = c(max_d, max_loss, max_gain),
  Sd = c(sd_d, sd_loss, sd_gain)
)

rownames(basic_statistics) <- c("D", "Rate of loss", "Rate of gain")

write.table(basic_statistics,"basic_statistics.csv", sep = " & ", quote = FALSE)

# plots: correlation between proportion present and rates

p_q10 <- ggplot(stats,aes(Median_rate_q10,Proportion_present)) +
  geom_point(col="red", alpha=0.9)
p_q10
p_q01 <- ggplot(stats,aes(Median_rate_q01,Proportion_present)) +
  geom_point(col="red", alpha=0.9)
p_q01

# coefficients: correlation between proportion present and rates
cor_q01 <- cor.test(stats$Median_rate_q01, stats$Proportion_present, method="kendall") 
cor_q01 # -0.34 - the feature is gained slower if more languages have the feature
cor_q10 <- cor.test(stats$Median_rate_q10, stats$Proportion_present, method="kendall") 
cor_q10 # no correlation between the rate of loss and proportion present

# set of stable features: phylogenetic signal below 0.5 and rate below or equal 0
stable_features <- stats[median_d < 0.5 & log10(stats$Median_rate_q01) <= 0,]
stable_features <- stable_features[log10(stable_features$Median_rate_q10) <= 0,]

# what percentage is stable?
round(length((rownames(stable_features)))/num_features, digits = 2) # 0.67

# get stable features with rate values below/above (add ! (NOT) for above) -2.5 -- these are almost invariable features or features present in all languages
stable_features <- stable_features[((stable_features$Log10Rate_q01 + stable_features$Log10Rate_q10)/2)<(-4),] 

# Plot stable features
p_stable <- ggplot(stable_features,aes((Log10Rate_q01+Log10Rate_q10)/2,Median_D, color=PoS)) +
  geom_point() +
  theme_classic() +
  xlab("Rate") +
  ylab("Median of D")

# Why take into account both phylogenetic signal and rate?
# In some features there is no correlation.
# Which features are these?

stats$Feature[stats$Median_D < 0.5 & log10(stats$Median_rate_q01) > 0] # TE029
stats$Feature[stats$Median_D < 0.5 & log10(stats$Median_rate_q10) > 0] # "GB023" "GB103" "GB275" "GB276" # 4
stats$Feature[stats$Median_D > 0.5 & log10(stats$Median_rate_q10) < 0] #  "GB042" "GB043" "GB074" "GB110" "GB117" "GB150" "GB166" "GB302" "GB316" "GB318" "GB326" "GB431" "TE054" # 13
stats$Feature[stats$Median_D > 0.5 & log10(stats$Median_rate_q01) < 0] # "GB020" "GB021" "GB037" "GB042" "GB043" "GB074" "GB110" "GB117" "GB138" "GB140" "GB150" "GB166" "GB167" "GB264" "GB302" "GB316" "GB318" "GB322" "GB400" "GB431" "GB520" "TE050" # 22

###################### Ancestral state reconstruction ######################

# Plot the distribution of ancestral states reconstructed as 1

h10 <- ggplot(stats, aes(x = p1_turkic)) + geom_histogram(binwidth = 0.1) + xlab('Turkic')
h10
h11 <- ggplot(stats, aes(x = p1_mongolic)) + geom_histogram(binwidth = 0.1) + xlab('Mongolic')
h12 <- ggplot(stats, aes(x = p1_tungusic)) + geom_histogram(binwidth = 0.1) + xlab('Tungusic')
h13 <- ggplot(stats, aes(x = p1_japonic)) + geom_histogram(binwidth = 0.1) + xlab('Japonic')
h14 <- ggplot(stats, aes(x = p1_koreanic)) + geom_histogram(binwidth = 0.1) + xlab('Koreanic')

ggsave("histograms-p1.pdf",(h10 | h11 | h12 | h13 | h14),  height=3, width=9)  # needs `patchwork` library

# Which features can be reconstructed to the proto-language level with 95% probability?

stats$Feature_short[stats$p1_turkic >= 0.95]
stats$Feature_short[stats$p1_mongolic >= 0.95]
stats$Feature_short[stats$p1_tungusic >= 0.95]
stats$Feature_short[stats$p1_japonic >= 0.95]
stats$Feature_short[stats$p1_koreanic >= 0.95]

# How many features can be reconstructed as present to the proto-language level with 75% probability?

sum(stats$p1_turkic >= 0.75) * 100 / num_features
sum(stats$p1_mongolic >= 0.75) * 100 / num_features
sum(stats$p1_tungusic >= 0.75) * 100 / num_features
sum(stats$p1_koreanic >= 0.75) * 100 / num_features
sum(stats$p1_japonic >= 0.75) * 100 / num_features

# How many features can be reconstructed as present to the proto-language level with 95% probability?

sum(stats$p1_turkic >= 0.95) * 100 / num_features
sum(stats$p1_mongolic >= 0.95) * 100 / num_features
sum(stats$p1_tungusic >= 0.95) * 100 / num_features
sum(stats$p1_koreanic >= 0.95) * 100 / num_features
sum(stats$p1_japonic >= 0.95) * 100 / num_features

# How many features can be reconstructed as absent to the proto-language level with 95% probability?

sum(stats$p1_turkic <= 0.05) * 100 / num_features
sum(stats$p1_mongolic <= 0.05) * 100 / num_features
sum(stats$p1_tungusic <= 0.05) * 100 / num_features
sum(stats$p1_koreanic  <=  0.05) * 100 / num_features
sum(stats$p1_japonic  <=  0.05) * 100 / num_features

# How many features can be reconstructed as absent to the proto-language level with 75% probability?

sum(stats$p1_turkic <= 0.25) * 100 / num_features
sum(stats$p1_mongolic <=  0.25) * 100 / num_features
sum(stats$p1_tungusic <= 0.25) * 100 / num_features
sum(stats$p1_koreanic <= 0.25) * 100 / num_features
sum(stats$p1_japonic <= 0.25) * 100 / num_features

# Pairwise comparisons between proto-languages

turkic_mongolic <- round(sum(stats$p1_turkic >= 0.95 & stats$p1_mongolic >= 0.95) * 100 / num_features, digits = 2)
turkic_tungusic <- round(sum(stats$p1_turkic >= 0.95 & stats$p1_tungusic >= 0.95) * 100 / num_features, digits = 2)
turkic_koreanic <- round(sum(stats$p1_turkic >= 0.95 & stats$p1_koreanic >= 0.95) * 100 / num_features, digits = 2)
turkic_japonic <- round(sum(stats$p1_turkic >= 0.95 & stats$p1_japonic >= 0.95) * 100 / num_features, digits = 2)

mongolic_tungusic <- round(sum(stats$p1_mongolic >= 0.95 & stats$p1_tungusic >= 0.95) * 100 / num_features, digits = 2)
mongolic_koreanic <- round(sum(stats$p1_mongolic >= 0.95 & stats$p1_koreanic >= 0.95) * 100 / num_features, digits = 2)
mongolic_japonic <- round(sum(stats$p1_mongolic >= 0.95 & stats$p1_japonic >= 0.95) * 100 / num_features, digits = 2)

tungusic_koreanic <- round(sum(stats$p1_tungusic >= 0.95 & stats$p1_koreanic >= 0.95) * 100 / num_features, digits = 2)
tungusic_japonic <- round(sum(stats$p1_tungusic >= 0.95 & stats$p1_japonic >= 0.95) * 100 / num_features, digits = 2)

koreanic_japonic <- round(sum(stats$p1_koreanic >= 0.95 & stats$p1_japonic>=0.95) * 100 / num_features, digits = 2)

overlaps<- data.frame(
  Turkic = c("X",turkic_mongolic, turkic_tungusic, turkic_koreanic, turkic_japonic),
  Mongolic = c(turkic_mongolic,"X", mongolic_tungusic, mongolic_koreanic, mongolic_japonic),
  Tungusic = c(turkic_tungusic, mongolic_tungusic, "X", tungusic_koreanic, tungusic_japonic),
  Koreanic = c(turkic_koreanic, mongolic_koreanic, tungusic_koreanic, "X", koreanic_japonic),
  Japonic = c(turkic_japonic, mongolic_japonic, tungusic_japonic, koreanic_japonic, "X")
)

rownames(overlaps) <- c("Turkic", "Mongolic", "Tungusic", "Koreanic", "Japonic")

write.table(overlaps,"overlaps.csv", sep = " & ", quote = FALSE)



