## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,message=F,results=F------------------------------------------------
library(rbenvo)
library(ggplot2)
theme_set(theme_bw())
data("FFR_distances")
data("FFR_subjects")

## ----subjdf-------------------------------------------------------------------
head(FFR_subjects,5)

## ----befdf--------------------------------------------------------------------
head(FFR_distances,5)

## ----mkbenvo------------------------------------------------------------------
bdf <- benvo(subject_data = FFR_subjects,
             sub_bef_data = list(FFR=FFR_distances),by='id')
summary(bdf)

## ----summary------------------------------------------------------------------
bdf

## ----verb_demo----------------------------------------------------------------
bdf %>% activate(FFR) %>% filter(id<=5)

## ----plot---------------------------------------------------------------------
plot(bdf)

## -----------------------------------------------------------------------------
joinvo(bdf,"FFR")

## ----subjectdesign_example----------------------------------------------------
str(subject_design(bdf,BMI ~ sex))

## ----longitudinal_example-----------------------------------------------------
data("longitudinal_HFS")
longitudinal_HFS

## ----longitudinal_summary-----------------------------------------------------
summary(longitudinal_HFS)

## ----long_plot----------------------------------------------------------------
plot(longitudinal_HFS,
     plotfun = "pointrange",
     term ="HFS",
     component = "Distance",
     p = 0.9)

## ----longitudinal_design_ex---------------------------------------------------
str(longitudinal_design(longitudinal_HFS,BMI ~ sex + (1|id)),max.level=1)

