library(ggplot2)
library(openintro)
library(caret)
library(reshape2)
library(tidyverse)

set.seed(6)
set.seed(23)

## Parte 1
data(starbucks)
attach(starbucks)
summary(starbucks)
data <- select(starbucks, protein, calories)
