library(reshape) # for melt()
library(ggplot2) # for plots, incl. histograms
library(ggridges) # for ridgeplot
library(patchwork) # for saving multiple plots in one file
library(dplyr) # for data restructuring

setwd("/Users/neshcheret/Documents/GitHub/articles/stability")

# load phylogenetic signal (D) results
res <- read.csv('results.csv', header = TRUE)

# load rate results
rates <- read.csv('asr_rates.csv.gz', header=TRUE)

# throw away the other models except for the q01 and q10 rates found by the ARD model
rates <- rates[rates$Model == 'ARD', ]

# load states results
states <- read.csv('asr_states.csv', header = TRUE)

# throw away the other models except for the states found by the ARD model
states <- states[states$Model == 'ARD', ]

rates$Log10q01 <- log10(rates$q01)
rates$Log10q10 <- log10(rates$q10)


# save unnecessary features in a vector to further delete them from the data
FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)

# Load the data
data <- read.csv(
  "all_languages_data_categories_full_names.csv",
  sep = ";",
  strip.white = TRUE,
  na.strings = c("?", "-"),
  stringsAsFactors = FALSE
)

# Prepare the data
data <- data[! data$ID %in% FEATURES_TO_IGNORE, ] # remove unnecessary features
features <- data$ID # Save feature names in a variable
description <- data$Feature # Save the feature question in variable
PoS <- data$PoS # Save the part of speech in a variable
Function <- data$Function # Save the functional category in a variable
Level <- data$Level # Save the language level in a variable

# Make the data tidy
data <- melt(data, id=c("ID", "Feature", "PoS", "Function", "Level"))

# Create a dataframe with summaries of the results
stats <- data.frame(
    Feature = features,
    Description = description,
    Function = Function,
    Level = Level,
    PoS = PoS,
    Values = sapply(features, function(f) length(na.exclude(data[data$ID == f, 'value']))), # Number of data points (non-missing data)
    Present = sapply(features, function(f) sum(na.exclude(data[data$ID == f, 'value']))), # Number of 1's
    Proportion_present = sapply(features, function(f) round(sum(na.exclude(data[data$ID == f, 'value']))/length(na.exclude(data[data$ID == f, 'value'])), digits = 2)), # Proportion of 1's
    Median_D = sapply(features, function(f) round(median(res[res$Feature == f, 'D']), digits = 2)),
    SD_D = sapply(features, function(f) round(sd(res[res$Feature == f, 'D']), digits = 2)),
    Median_rate_q10 = sapply(features, function(f) round(median(rates[rates$Feature == f, 'q10'], na.rm = TRUE), digits = 2)),
    SD_rate_q10 = sapply(features, function(f) round(sd(rates[res$Feature == f, 'q10']), digits = 2)),
    Median_rate_q01 = sapply(features, function(f) round(median(rates[rates$Feature == f, 'q01'], na.rm = TRUE), digits = 2)),
    SD_rate_q01 = sapply(features, function(f) round(sd(rates[res$Feature == f, 'q01']), digits = 2)),
    Min_D = sapply(features, function(f) round(min(res[res$Feature == f, 'D']), digits = 2)),
    Max_D = sapply(features, function(f) round(max(res[res$Feature == f, 'D']), digits = 2)),
    Min_rate_q10 = sapply(features, function(f) round(min(rates[rates$Feature == f, 'q10'], na.rm=TRUE), digits = 2)),
    Min_rate_q01 = sapply(features, function(f) round(min(rates[rates$Feature == f, 'q01'], na.rm=TRUE), digits = 2)),
    Max_rate_q10 = sapply(features, function(f) round(max(rates[rates$Feature == f, 'q01'], na.rm=TRUE), digits = 2)),
    Max_rate_q01 = sapply(features, function(f) round(max(rates[rates$Feature == f, 'q10'], na.rm=TRUE), digits = 2)),
    LogLikelihood = sapply(features, function(f) round(mean(states[states$Feature == f, 'LogLikelihood']), digits = 2)),
    AICc = round(sapply(features, function(f)median(states[states$Feature == f, 'AICc'])), digits = 2),
    p1_turkic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Turkic", 'p1']), digits = 2)),
    p1_mongolic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Mongolic", 'p1']), digits = 2)),
    p1_tungusic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Tungusic", 'p1']), digits = 2)),
    p1_koreanic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Koreanic", 'p1']), digits = 2)),
    p1_japonic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Japonic", 'p1']), digits = 2))   
   )

# Delete uninformative features with all absent
stats <- stats[!stats$Feature=="GB095",] #delete feature with 0 present
stats <- stats[!stats$Present==0,]

# add columns with log10 transformed rate values to the stats summary table
stats$Log10Rate_q10 <- log10(stats$Median_rate_q10)
stats$Log10Rate_q01 <- log10(stats$Median_rate_q01)

# Remove cases with missing values
stats <- stats[complete.cases(stats),]

# Set theme_classic() as default for all plots
theme_set(theme_classic())

###################### Phylogenetic signal and evolutionary rate ######################

# Histograms on median rate q01, q10 and phylogenetic signal

h1 <- ggplot(stats, aes(x = Median_D)) + 
  geom_histogram(binwidth = 0.3) + 
  xlab('Median (D)')

h2 <- ggplot(stats, aes(x = Median_rate_q01)) + 
  geom_histogram(binwidth = 0.1) + 
  scale_x_log10() +
  xlab('log10(Median rate: 0 -> 1, feature gain)')

h3 <- ggplot(stats, aes(x = Median_rate_q10)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_log10() +
  xlab('log10(Median rate: 1 -> 0, feature loss)')


ggsave("histograms.pdf", (h1 | h2 | h3), height=5, width=12)

# what is the relationship between the median _D_ score and the number of present characters
m <- ggplot(stats, aes(x = Present, y = Median, color = Values)) +
    geom_point() +
    ggtitle("Median _D_ score and number of 1's") +
    scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue") +
    theme_classic()
ggsave('scatter_Present_vs_Median.pdf', m)


# > 1 overdispersed
# = 1 random
# = 0 brownian
# < 0 extremely clumped

# Parallel boxplots for function and rate
b1 <- ggplot(stats, aes(x = Function, y = Log10Rate_q01)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())

b2 <- ggplot(stats, aes(x = Function, y = Log10Rate_q10)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())

b3 <- ggplot(stats, aes(x = Function, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Function') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=1))
ggsave("boxplots-function.pdf", b1  /  b2 / b3,height=10,width=7)

# Parallel boxplots for level and rate
b4 <- ggplot(stats, aes(x = Level, y = Log10Rate_q01)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())
b5 <- ggplot(stats, aes(x = Level, y = Log10Rate_q10)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())
b6 <- ggplot(stats, aes(x = Level, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Level') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))
ggsave("boxplots-level.pdf", b4  /  b5 / b6 ,height=10,width=7)

# Parallel boxplots for PoS and rate

b7 <- ggplot(stats, aes(x = PoS, y = Log10Rate_q01)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 0 to 1, feature gain') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())

b8 <- ggplot(stats, aes(x = PoS, y = Log10Rate_q10)) + 
  geom_boxplot() +
  geom_hline(yintercept=0, col="red") +
  ylab('Transition from 1 to 0, feature loss') +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())

b9 <- ggplot(stats, aes(x = PoS, y = Median_D)) + 
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red") +
  xlab('Part of speech') +
  ylab('Phylogenetic signal (D)') +
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))

ggsave("boxplots-pos.pdf", b7  /  b8 / b9,height=10,width=7)

# Save the median D values in a variable
median_d <- stats$Median_D

# correlation between D and 10 transition rate
corr_q10 <- cor.test(median_d,stats$Log10Rate_q10, method="kendall")

# correlation between D and 01 transition rate
corr_q01 <- cor.test(median_d,stats$Log10Rate_q01, method="kendall")

# plot the correlation between phylogenetic signal and q10 transition rate

ph_rates_q10 <- ggplot(stats,aes(Log10Rate_q10,Median_D, color=Present)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method='lm', formula= y~x) +
  annotate("text", x = -1.5, y=2, label = paste("tau=",round(corr_q10$estimate, digits = 2))) +
  xlab("Log10 rate (transition from 1 to 0, feature loss)") +
  ylab("Median (D)") +
  expand_limits(x = -2)

plotly::ggplotly()

# plot the correlation between phylogenetic signal and q01 transition rate

ph_rates_q01 <- ggplot(stats,aes(Log10Rate_q01,Median_D, color=Present)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method='lm', formula= y~x) +
  annotate("text", x = -1.5, y=2, label = paste("tau=",round(corr_q01$estimate, digits = 2))) +
  xlab("Log10 rate (transition from 0 to 1, feature gain)") +
  ylab("Median (D)") +
  expand_limits(x = -2)

plotly::ggplotly()

ggsave("correlation.pdf", ph_rates_q10 / ph_rates_q01, height=5,width=5)

num_features <- length(stats$Feature)

# stats for the results: phylogenetic signal
x1 <- round(sum(median_d < 0) / num_features, digits = 2) # overclumped
x2 <- round((sum(median_d > 0 & median_d < 0.5) / num_features), digits = 2) # phylogenetic signal
x3 <- round(sum(median_d > 0.5 & median_d < 1) / num_features, digits = 2) # random
x4<- round(sum(median_d > 1) / num_features, digits = 2) # overdispersed

# Check whether the proportions sum up to 1
x1 + x2 + x3 + x4
x1 + x2 # number of features wtih a phylogenetic signal
x3 + x4 # number of features without a phylogenetic signal

# stats for the results: rate. What is the proportion of features that evolve slowly and fast?
lost_slowly <- round(sum(stats$Log10Rate_q10 < 0) / num_features, digits = 2)
gained_slowly <- round(sum(stats$Log10Rate_q01 < 0) / num_features, digits = 2)

lost_fast <- round(sum(stats$Log10Rate_q10 > 0) / num_features, digits = 2)
gained_fast <- round(sum(stats$Log10Rate_q01 > 0) / num_features, digits = 2)

# stats for the results: rate (slow, medium, fast)

y1_q01 <- round(sum(stats$Log10Rate_q01 < (-0.5)) / num_features, digits = 2) # slow
y2_q01  <- round(sum(stats$Log10Rate_q01 > (-0.5) & stats$Log10Rate_q01 < 0.5) / num_features, digits = 2) # medium
y3_q01  <- round(sum(stats$Log10Rate_q01>0.5) / num_features, digits = 2) # fast

# Check whether the proportions sum up to 1
y1_q01 + y2_q01 + y3_q01

y1_q10 <- round(sum(stats$Log10Rate_q10<(-0.5))/num_features, digits = 2) # slow
y2_q10  <- round(sum(stats$Log10Rate_q10 > (-0.5) & stats$Log10Rate_q10 < 0.5)/num_features, digits = 2) # medium
y3_q10  <- round(sum(stats$Log10Rate_q10>0.5)/num_features, digits = 2) # fast
y1_q10+y2_q10+y3_q10

# stats for results: reports on rate of gain
range(stats$Min_rate_q01)
median(stats$Min_rate_q01)
sd(stats$Min_rate_q01)

range(stats$Max_rate_q01)
median(stats$Max_rate_q01)
sd(stats$Max_rate_q01)

range(stats$Median_rate_q01)
median(stats$Median_rate_q01)
sd(stats$Median_rate_q01)

# stats for results: reports on rate of loss

range(stats$Min_rate_q10)
median(stats$Min_rate_q10)
sd(stats$Min_rate_q10)

range(stats$Max_rate_q10)
median(stats$Max_rate_q10)
sd(stats$Max_rate_q10)

range(stats$Median_rate_q10)
median(stats$Median_rate_q10)
sd(stats$Median_rate_q10)

# plots: correlation between proportion present and rates

p_q10 <- ggplot(stats,aes(Median_rate_q10,Proportion_present)) +
  geom_point(col="red", alpha=0.9)

p_q01 <- ggplot(stats,aes(Median_rate_q01,Proportion_present)) +
  geom_point(col="red", alpha=0.9)

p_log10q10 <- ggplot(stats,aes(Log10Rate_q10,Proportion_present)) +
  geom_point(col="red", alpha=0.9)

p_log10q01 <- ggplot(stats,aes(Log10Rate_q01,Proportion_present)) +
  geom_point(col="red", alpha=0.9)

# coefficients: correlation between proportion present and rates
cor_q01 <- cor.test(stats$Median_rate_q01, stats$Proportion_present, method="kendall") # no correlation
cor_q10 <- cor.test(stats$Median_rate_q10, stats$Proportion_present, method="kendall") # no correlation

# set of stable features: phylogenetic signal below 0.5 and rate below or equal 0
stable_features <- stats[median_d < 0.5 & stats$Log10Rate_q01 <= 0,]
stable_features <- stable_features[stable_features$Log10Rate_q10 <= 0,]

# what percentage is stable?
round(length((rownames(stable_features)))/num_features, digits = 2)

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

stats$Feature[median_d<0.5 & stats$Log10Rate_q01>0] # GB187
length(stats$Feature[median_d<0.5 & stats$Log10Rate_q10>0]) # "GB023" "GB103" "GB250" "GB275" "GB276" # 5
length(stats$Feature[median_d>0.5 & stats$Log10Rate_q10<0]) # "GB021" "GB042" "GB043" "GB074" "GB086" "GB110" "GB166" "GB302" "GB316" "GB318" "GB431" "GB520" "TE050" "TE054" "TS001" # 15
length(stats$Feature[median_d>0.5 & stats$Log10Rate_q01<0]) # "GB020" "GB021" "GB037" "GB042" "GB043" "GB074" "GB110" "GB138" "GB140" "GB166" "GB167" "GB264" "GB302" "GB316" "GB318" "GB322" "GB400" "GB431" "GB520" "TE050" "TS010"# 21


###################### Ridgeplots for phylogenetic signal and rate ######################

feature_set <- read.csv("feature_set.txt", sep="\t")

res_feature_short <- res %>% 
  inner_join(feature_set,by=c("Feature"="ID")) %>%
  select(Tree, Feature_short, D, PVal1, PVal0)

# reorder feature by median _D_ value to help plotting later
#res$Feature <- with(res, reorder(Feature, D, median))
#res_feature_short$Feature_short <- with(res_feature_short, reorder(Feature_short, D, median))

p <- ggplot(res_feature_short, aes(x = D, y=reorder(Feature_short, D), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale=2) +
  scale_fill_gradient(low="orange", high="blue") +
  theme(axis.title.y=element_blank()) +
  geom_vline(xintercept=0) +
  geom_vline(xintercept=1) +
  xlim(-5, 5) +
  guides(fill="none")

ggsave('ridgeplot-D-vs-Feature.pdf', height=20, width=10)

# delete rows with infinite values (intrudoced through log10 transformation)
rates <- rates[!rates$Log10q01==(-Inf),]
rates <- rates[!rates$Log10q10==(-Inf),]

rates_feature_short <- rates %>% 
  inner_join(feature_set,by=c("Feature"="ID")) %>%
  select(Tree, Feature_short, Log10q01, Log10q10)

#rates_feature_short <- rates_feature_short[!rates_feature_short$Log10q01==(-Inf),]
#rates_feature_short <- rates_feature_short[!rates_feature_short$q10==(-Inf),]

# reorder feature by median q01 rate value to help plotting later
#rates$Feature <- with(rates, reorder(Feature, q01, median))

p_rate_01 <- ggplot(rates_feature_short, aes(x = Log10q01, y = reorder(Feature_short, Log10q01), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale=2) +
  theme(axis.title.y=element_blank()) +
  scale_fill_gradient(low="orange", high="blue") +
  guides(fill="none") +
  xlab("Log10 0-1 transition rate (feature gain")

ggsave('ridgeplot-q01-vs-Feature.pdf', height=20, width=10)

p_rate_10 <- ggplot(rates_feature_short, aes(x = Log10q01, y=reorder(Feature_short, Log10q10), fill=..x..)) +
  geom_density_ridges_gradient(rel_min_height=0.01, scale=2) +
  theme(axis.title.y=element_blank()) +
  scale_fill_gradient(low="orange", high="blue") +
  guides(fill="none") +
  xlab("1-0 transition rate (feature loss)")

ggsave('ridgeplot-q10-vs-Feature.pdf', height=20, width=10)


###################### Ancestral state reconstruction ######################

# Plot the distribution of ancestral states reconstructed as 1

h10 <- ggplot(stats, aes(x = p1_turkic)) + geom_histogram() + xlab('Turkic')
h11 <- ggplot(stats, aes(x = p1_mongolic)) + geom_histogram() + xlab('Mongolic')
h12 <- ggplot(stats, aes(x = p1_tungusic)) + geom_histogram() + xlab('Tungusic')
h13 <- ggplot(stats, aes(x = p1_japonic)) + geom_histogram() + xlab('Japonic')
h14 <- ggplot(stats, aes(x = p1_koreanic)) + geom_histogram() + xlab('Koreanic')

ggsave("histograms-p1.pdf",(h10 | h11 | h12 | h13 | h14),  height=3, width=9)  # needs `patchwork` library

# Which features can be reconstructed to the proto-language level with 95% probability?

stats$Description[stats$p1_turkic >= 0.95]
stats$Description[stats$p1_mongolic >= 0.95]
stats$Description[stats$p1_tungusic >= 0.95]
stats$Description[stats$p1_japonic >= 0.95]
stats$Description[stats$p1_koreanic >= 0.95]

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


######################  Write table for the supplementary materials ###################### 

si_table <- data.frame(
  Feature = features,
  Description = description,
  Median_D = sapply(features, function(f) round(median(res[res$Feature == f, 'D']),digits = 2 )),
  SD_D = sapply(features, function(f) round(sd(res[res$Feature == f, 'D']),digits = 2)),
  Median_rate_loss = sapply(features, function(f) round(median(rates[rates$Feature == f, 'q10'], na.rm=TRUE),digits = 2)),
  SD_rate_loss = sapply(features, function(f) round(sd(rates[res$Feature == f, 'q10']),digits = 2)),
  Median_rate_gain = sapply(features, function(f) round(median(rates[rates$Feature == f, 'q01'], na.rm=TRUE),digits = 2)),
  SD_rate_gain = sapply(features, function(f) round(sd(rates[res$Feature == f, 'q01']),digits = 2)),
  LogLikelihood = sapply(features, function(f) round(mean(states[states$Feature == f, 'LogLikelihood']),digits = 2)),
  AICc=round(sapply(features, function(f)median(states[states$Feature == f, 'AICc'])),digits = 2),
  p1_turkic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Turkic", 'p1']),digits = 2)),
  p1_mongolic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Mongolic", 'p1']),digits = 2)),
  p1_tungusic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Tungusic", 'p1']),digits = 2)),
  p1_koreanic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Koreanic", 'p1']),digits = 2)),
  p1_japonic = sapply(features, function(f) round(mean(states[states$Feature == f & states$Clade == "Japonic", 'p1']),digits = 2))
)

write.table(si_table,"SI_summary_table.csv", sep = "\t", row.names=FALSE, quote = TRUE)
