## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 6)


## ----gen_data-----------------------------------------------------------------
library(marketr)
library(dplyr)
library(magrittr)
library(ggplot2)

needs <- sample(2:5, 1000, replace = T)
ease <- sample(2:5, 1000, replace = T)
emotion <- sample(2:5, 1000, replace = T)
nps_question <- sample(3:10, 1000, replace = T)
grps <- c("a", "b", "c")
months <- sample(1:12, 1000, replace = T)

survey_data <- tibble::as_tibble(cbind(needs, ease, emotion, nps_question, grps, months)) %>%
  mutate(month = as.numeric(months))

head(survey_data)

## ----cxi----------------------------------------------------------------------
# Overall CXi
cxi_calc(survey_data) %>% knitr::kable()

## CXi by group
cxi_calc(survey_data, grps, cx_high = 4, cx_low = 2) %>% knitr::kable()

# Overall CXi trend
cxi_trend(survey_data, month) %>% knitr::kable() 

# Overall CXi trend by group - plotted
cxi_trend(survey_data, month, grps, cx_high = 4, cx_low = 2, min_surveys = 1, avg_surveys = 0) %>% 
  ggplot(aes(x = month, y = cxi)) +
  geom_line() +
  facet_wrap(grps ~ ., nrow = 3)

## ----NPS----------------------------------------------------------------------
# Overall NPS
nps_calc(survey_data) %>% knitr::kable()

## NPS by group
nps_calc(survey_data, grps) %>% knitr::kable()

# Overall NPS trend
nps_trend(survey_data, month) %>% knitr::kable()

# Overall NPS trend by group - plotted
nps_trend(survey_data, month, grps, min_surveys = 1, avg_surveys = 0) %>% 
  ggplot(aes(x = month, y = nps)) +
  geom_line() +
  facet_wrap(grps ~ ., nrow = 3)

