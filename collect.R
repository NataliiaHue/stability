#!/usr/bin/env Rscript
# collects results
library(vroom)

cat("collecting D results\n")

df <- vroom(
  list.files("/Users/neshcheret/Documents/GitHub/stability/d", "results.*.csv", full.names=TRUE),
  delim=",",
  num_threads=4
)

write.csv(df, 'results_d.csv', quote=FALSE, row.names=FALSE)
# vroom keeps file handles open, which means we risk 'too many files open'.
# Running gc closes them.
rm(df)
gc()

cat("collecting ASR rates results\n")
df <- vroom(
  list.files("/Users/neshcheret/Documents/GitHub/stability/asr", "asr_rates.*.csv", full.names=TRUE),
  delim=",",
  num_threads=4
)
write.csv(df, 'results_asr_rates.csv', quote=FALSE, row.names=FALSE)
rm(df)
gc()

cat("collecting ASR states results\n")
df <- vroom(
        list.files("/Users/neshcheret/Documents/GitHub/stability/asr", "asr_states.*.csv", full.names=TRUE),
        delim=",",
        num_threads=4
)
write.csv(df, 'results_asr_states.csv', quote=FALSE, row.names=FALSE)
rm(df)
gc()