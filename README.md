# R DataFrame/Tibble Library
- R library designed to explore information about an R data frame, such as column names, types, presence of NAs, and unique/distinct values

# Usages
```
# Load library
source("https://raw.githubusercontent.com/phampn13/r-libs/main/data_frame.R")

# Discover a data frame or tibble
lp_df_sum(data_frame_or_tibble, type="spec")

lp_df_sum(data_frame_or_tibble, type="data", columns=c("column1", "column2"))
```
