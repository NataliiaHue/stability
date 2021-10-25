#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)

setwd( "/Users/neshcheret/Documents/GitHub")

df_states <- read.csv("stability/results_asr_states.csv", header = TRUE)

# select a model
df_states <- df_states[df_states$Model != 'ARD', ]

# read in features with short names
categories <- read_tsv("hueblerstability/etc/features_with_categories.tsv")

# average and collect results
df <- df_states %>%
    inner_join(categories, by=c("Feature"="ID")) %>%
    group_by(Feature_short, Clade)%>%
    summarise(Mean=mean(p1), Max=max(p1), Min=min(p1), Median=median(p1), Std=sd(p1))
    

# split into 4 groups to make the plot more readable
df$Dataset <- cut(1:850, 5, labels=1:5)

p1 <- ggplot(df[df$Dataset=='1',], aes(Feature_short, Clade, fill=Median)) +
    geom_tile()

p2 <- ggplot(df[df$Dataset=='2',], aes(Feature_short, Clade, fill=Median)) +
    geom_tile()

p3 <- ggplot(df[df$Dataset=='3',], aes(Feature_short, Clade, fill=Median)) +
    geom_tile()

p4 <- ggplot(df[df$Dataset=='4',], aes(Feature_short, Clade, fill=Median)) +
    geom_tile()

p5 <- ggplot(df[df$Dataset=='5',], aes(Feature_short, Clade, fill=Median)) +
    geom_tile()

pw <- (p1 / p2 / p3 / p4 / p5) + plot_layout(guides = 'collect')
pw <- pw & theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
    )
pw <- pw & scale_fill_viridis(option="plasma", discrete=FALSE)


# I do not know why there are two guides. Well, I think it's because the guides are not
# identical because of different ranges...
setwd( "/Users/neshcheret/Documents/GitHub/stability")
ggsave(filename="heatmap.pdf", pw, width=12, height=12)
