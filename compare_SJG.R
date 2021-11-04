library(tidyverse)
library(ggplot2)
library(patchwork)

setwd("/Users/neshcheret/Documents/GitHub/stability")

# I got this out of the repository by going back through the commits to get one 3 months ago
asr.orig <- read.csv('asr_rates_2020.csv', header=TRUE)
#asr.orig <- asr.orig[asr.orig$Feature == 'GB022' & asr.orig$Model == 'ARD',]
asr.orig <- asr.orig[asr.orig$Model == 'ARD',]
asr.orig$When <- 'Original'

#results_asr_rates_oct_22

asr.oct22 <- lapply(list.files('asr_22_oct', 'asr_rates.*.csv', full.names=TRUE), read_csv) %>% bind_rows()
asr.oct29 <- lapply(list.files('asr_29_oct', 'asr_rates.*.csv', full.names=TRUE), read_csv) %>% bind_rows()

asr.oct22$When <- 'Oct22'
asr.oct29$When <- 'Oct29'

asr <- rbind(rbind(asr.orig, asr.oct22), asr.oct29)

asr1gb22 <- asr[asr$Model == 'ARD' & asr$Feature == "TE005", ]

asr1gb22 <- asr[asr$Model == 'ARD', ]

theme_set(theme_classic())

p <- ggplot(asr1gb22, aes(x=LogLikelihood, group=When, fill=When)) + geom_histogram() + facet_grid(When~.) + guides(fill="none")
q <- ggplot(asr1gb22, aes(x=AICc, group=When, fill=When)) + geom_histogram() + facet_grid(When~.) + guides(fill="none")
r <- ggplot(asr1gb22, aes(x=q01, group=When, fill=When)) + geom_histogram() + facet_grid(When~.) + guides(fill="none")
s <- ggplot(asr1gb22, aes(x=q10, group=When, fill=When)) + geom_histogram() + facet_grid(When~.) + guides(fill="none")

((p + q ) / (r + s))
