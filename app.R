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
# accounts()
# 
# rsconnect::setAccountInfo(name=name_hablab,
#                           token=token_hablab,
#                           secret=secret_hablab)
# accounts()
# connectUser(account = "hablab",server = "shinyapps.io", quiet = FALSE)

# deployApp(account = "aleksandereiken") Y


