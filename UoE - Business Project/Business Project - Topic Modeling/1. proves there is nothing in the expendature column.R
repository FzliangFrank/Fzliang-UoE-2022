library(readr)
library(tidyverse)
read_csv("raw/AIbyTOpic.csv") %>%
  select(ExpenditurePounds, AwardPounds) %>%
  select(ExpenditurePounds) %>%
  filter(!!NA)
