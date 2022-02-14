library(dplyr)
library(readr)

df_d <- list.files("./d", "results.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_d <- df_d %>%
  select(-X1)

write.csv(df, 'results_d.csv', quote=FALSE, row.names=FALSE)

df_rates <- list.files("./asr_29_oct", "asr_rates.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_rates, 'results_asr_rates.csv', quote=FALSE, row.names=FALSE)

df_states <- list.files("./asr_29_oct", "asr_states.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_states, 'results_asr_states.csv', quote=FALSE, row.names=FALSE)

# Collect old results

df_rates_22_oct <- list.files("./asr_22_oct", "asr_rates.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_rates, 'results_asr_rates_22_oct.csv', quote=FALSE, row.names=FALSE)

df_states_22_oct <- list.files("./asr_22_oct", "asr_states.*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

write.csv(df_states_22_oct, 'results_asr_states_22_oct.csv', quote=FALSE, row.names=FALSE)

