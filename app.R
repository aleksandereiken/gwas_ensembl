## Clean workspace
rm(list=ls())
source("creds.R")

#Load libraries
library(shiny)
library(gwasrapidd)
library(stringr)
library(dplyr)
library(openxlsx)
library(shinyalert)
options(repos = BiocInstaller::biocinstallRepos())
# getOption("repos")

#Set working directory to where the files for the program are located
runApp()

# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='hablab',
#                           token='4AED754589D3E00D5EDD9DAC16C7B185',
#                           secret='I7TLXQpJt4wfDd8f+DYf5nPTNXm5a1BXyDsXr3DF')
# accounts()
# connectUser(account = "hablab",server = "shinyapps.io", quiet = FALSE)
# deployApp(account = "aleksandereiken") Y


