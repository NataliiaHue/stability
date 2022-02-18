#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)

asr_states_oct29 <- lapply(list.files('asr_29_oct', 'asr_states.*.csv', full.names=TRUE), read_csv) %>% bind_rows()

# select a model
df_states <- asr_states_oct29[asr_states_oct29$Model != 'ARD', ]

# read in features with short names
categories <- read_tsv("./hueblerstability/etc/features_with_categories.tsv")

# average and collect results
df <- df_states %>%
    inner_join(categories, by=c("Feature"="ID")) %>%
    group_by(Feature_short, Clade) %>%
    summarise(Mean=mean(p1), Max=max(p1), Min=min(p1), Median=median(p1), Std=sd(p1))
    

# split into 4 groups to make the plot more readable
df$Dataset <- cut(1:855, 3, labels=1:3)

p1 <- ggplot(df[df$Dataset=='1',], aes(reorder(Feature_short, Median), Clade, fill=Median)) +
    geom_tile()

p2 <- ggplot(df[df$Dataset=='2',], aes(reorder(Feature_short, Median), Clade, fill=Median)) +
    geom_tile()

p3 <- ggplot(df[df$Dataset=='3',], aes(reorder(Feature_short, Median), Clade, fill=Median)) +
    geom_tile()

pw <- (p1 / p2 / p3 ) + plot_layout(guides = 'collect')
pw <- pw & theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
    )
pw <- pw & scale_fill_viridis(option="plasma", discrete=FALSE)

ggsave(filename="heatmap.pdf", pw, width=12, height=12)

