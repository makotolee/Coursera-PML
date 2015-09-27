rm(list=ls())
library(knitr)
library(markdown)


setwd("C:\\Users\\Brandon\\Desktop\\mdec")


knit2html("Coursera_PML_Project.Rmd")
browseURL("Coursera_PML_Project.html")
