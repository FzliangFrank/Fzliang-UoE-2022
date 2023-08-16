load("RData/stm_20.RData")

library(LDAvis)
library(stm)
library(tidytext)

toLDAvis(stm_20, out$documents, R = 10, reorder.topics = FALSE)

plot(stm_20)


estimateEffect(
  formula = 1:20 ~ date, 
  stmobj = stm_20, 
  metadata = meta, 
  uncertainty = "None") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate)

prep <- estimateEffect(
  formula = 1:20 ~ s(date), 
  stmobj = stm_20, 
  metadata = meta, 
  uncertainty = "None")
plot(prep, "date", method = "continuous", model = z, topics = 6, printlegend = FALSE)
plot(prep, "date", 
     method = "continuous", 
     model = stm_20, 
     topics = c(6,8,19), 
     printlegend = TRUE)

plot(prep, "date", 
     method = "continuous", 
     model = stm_20, 
     topics = c(20, 2, 11), 
     printlegend = TRUE, xaxt = "n")

seq <- seq(from = min(meta$StartDate), to = max(meta$StartDate), by = "year") 
library(lubridate)
yearnames <- year(seq)
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

meta$StartDate %>% class()

plot(prep, "date", 
     method = "continuous", 
     model = stm_20, 
     topics = c(20, 2, 11), 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

plot(prep, "date", 
     linecol = "blue",
     method = "continuous", 
     model = stm_20, 
     topics = c(8), 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

plot(prep, "date", 
     linecol = "red",
     method = "continuous", 
     model = stm_20, 
     topics = c(19), 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

plot(prep, "date", 
     linecol = "blue",
     method = "continuous", 
     model = stm_20, 
     topics = 20, 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

plot(prep, "date", 
     linecol = "blue",
     method = "continuous", 
     model = stm_20, 
     topics = c(2), 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

plot(prep, "date", 
     linecol = "blue",
     method = "continuous", 
     model = stm_20, 
     topics = c(11), 
     printlegend = TRUE, xaxt = "n")
axis(1, at = as.numeric(seq) - min(as.numeric(seq)), labels = yearnames)

