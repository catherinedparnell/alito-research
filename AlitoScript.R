library(tidyverse)
library(dplyr)
library(stringr)

setwd("/Users/catherineparnell/alito-research")

df <- read.csv("AlitoDataset.csv")

justices <- list('Alito','Roberts','Stevens','Scalia','Kennedy','Souter','Thomas','Ginsberg','Breyer','Sotomayor','Kagan','Gorsuch')

df <- df %>% 
  mutate(Roberts = if_else(str_detect(Majority, "Roberts"), 1, 0)) %>%
  mutate(Stevens = if_else(str_detect(Majority, "Stevens"), 1, 0)) %>%
  mutate(Scalia = if_else(str_detect(Majority, "Scalia"), 1, 0)) %>%
  mutate(Kennedy = if_else(str_detect(Majority, "Kennedy"), 1, 0)) %>%
  mutate(Souter = if_else(str_detect(Majority, "Souter"), 1, 0)) %>%
  mutate(Thomas = if_else(str_detect(Majority, "Thomas"), 1, 0)) %>%
  mutate(Ginsberg = if_else(str_detect(Majority, "Ginsberg"), 1, 0)) %>%
  mutate(Breyer = if_else(str_detect(Majority, "Breyer"), 1, 0)) %>%
  mutate(Sotomayor = if_else(str_detect(Majority, "Sotomayor"), 1, 0)) %>%
  mutate(Kagan = if_else(str_detect(Majority, "Kagan"), 1, 0)) %>%
  mutate(Gorsuch = if_else(str_detect(Majority, "Gorsuch"), 1, 0))

df <- df %>%
  mutate(AnswerFlag = case_when(
    str_detect(Characterized.Answer, "Yes")~1,
    str_detect(Characterized.Answer, "No")~0,
    TRUE~2
  ))

print(df)
