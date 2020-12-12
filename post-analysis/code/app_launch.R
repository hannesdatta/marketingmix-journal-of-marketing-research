rm(list=ls())
# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(shiny)

load('../temp/simworkspace.RData')

shinyApp(ui = ui, server = server)

