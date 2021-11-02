loss_gain <- ggplot(compare, aes(q01_2020, q10_2020)) +
  geom_point()
loss_gain

compare <- compare %>%
  mutate(diff_median_rate_loss = Median_rate_loss - q10,
         diff_median_rate_gain = Median_rate_gain - q01) %>%
  select(Feature, Feature_short, diff_median_rate_loss, diff_median_rate_gain)


# Look at the correlations with the missing data

missing_d_old <- ggplot(compare, aes(x = Proportion_missing, y = Median_D.x, color = Values)) +
  geom_point() +
  ggtitle("Median D and amount of NA's") +
  xlab('Proportion missing') +
  ylab('Median D') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue") +
  theme_classic()
missing_d_old

missing_q01_old <- ggplot(compare, aes(x = Proportion_missing, y = Median_rate_gain, color = Values)) +
  geom_point() +
  ggtitle("Median rate of feature gain and amount of NA's") +
  xlab('Proportion missing') +
  ylab('Median rate of feature gain') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue") +
  theme_classic()
missing_q01_old

missing_q10_old <- ggplot(compare, aes(x = Proportion_missing, y = Median_rate_loss, color = Values)) +
  geom_point() +
  ggtitle("Median rate of feature loss and amount of NA's") +
  xlab('Proportion missing') +
  ylab('Median rate of feature loss') +
  scale_color_gradient('Amount of Data', low = "tomato", high = "steelblue") +
  theme_classic()
missing_q10_old

ggsave("missing_data_old.pdf", (missing_d_old | missing_q01_old | missing_q10_old), height=5, width=15)
