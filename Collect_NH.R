library(dplyr)
library(readr)

df_d <- list.files("/Users/neshcheret/Documents/GitHub/stability/d", "results.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_d <- df_d %>%
  select(-X1)

write.csv(df, 'results_d.csv', quote=FALSE, row.names=FALSE)

df_rates <- list.files("/Users/neshcheret/Documents/GitHub/stability/asr", "asr_rates.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_rates, 'results_asr_rates.csv', quote=FALSE, row.names=FALSE)

df_states <- list.files("/Users/neshcheret/Documents/GitHub/stability/asr", "asr_states.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_states, 'results_asr_states.csv', quote=FALSE, row.names=FALSE)
