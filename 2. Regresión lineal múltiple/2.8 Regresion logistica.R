library(ISLR2)
library(tidyverse)

default <- Default$default
balance <- Default$balance
  
default <- recode(default, "No"=0, "Yes"=1)
datos <- data.frame(balance,default)
mod_log <- glm(default ~ balance, data = datos, family = "binomial")
mod_log
summary(mod_log)

