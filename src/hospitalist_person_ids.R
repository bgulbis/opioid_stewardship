library(tidyverse)
library(readxl)

df <- read_excel("data/external/hospitalists.xlsx")

mbo_id <- edwr::concat_encounters(df$person_id)
print(mbo_id)
