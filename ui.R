#Load libraries
library(shiny)
library(gwasrapidd)
library(stringr)
library(dplyr)
library(openxlsx)
library(DT)
library(shinyalert)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Search for traits and download data from GWAS and Ensembl databases"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("search_term","Words for searching for trait"),
      actionButton("apply_search", "Find traits"),
      actionButton("get_data", "Get GWAS data"),
      actionButton("get_data_ensembl", "Get Ensembl data"),
      actionButton("reset_search", "Clear search"),
      helpText("Type in search words and click 'Find traits'. First when all the traits you want the data for are visible, tick off the traits of interest to the right. Lastly click one of the 'Get data' buttons to fetch the data from the GWAS or Ensembl database. 'Download Data' buttons then appears, which lets you download the data as an xlsx file from the selected database(s)"),
      br(),
      br(),
      actionButton("related_traits", "Find related traits"),
      br(),      
      helpText("The button above firstly downloads all studies related to the GWAS traits selected to the right,
               then finds all traits associated with these studies and lastly returns the list of these traits to the tap 'Output: GWAS related traits'"),
      br(),
      br(),
      uiOutput("downloadData"),
      uiOutput("downloadDataEnsembl")
      # downloadButton("download_data", "Download Excel Data")
      ),
    mainPanel(
      tabsetPanel(tabPanel("Select traits",  
                           column(4,
                                  br(),
                                  "GWAS Database Traits", 
                                  br(),
                                  uiOutput("patient_tick_off")),
                           column(4,
                                  br(),
                                  "Ensembl Traits (Phenotype description)", 
                                  br(),
                                  uiOutput("ensembl_traits"))),
                  tabPanel("Output: GWAS related traits",
                           fluidRow(
                             column(12,
                                    br(),
                                    DTOutput("related_traits_table"))
                           )),
                  tabPanel("Output: GWAS associations",
                           br(),
                           selectInput("select_association", "Select association tap for displaying data",c("associations","loci","risk_alleles","genes","ensembl_ids","entrez_ids"),"associations"),
                           DTOutput("associations")),
                  tabPanel("Output: GWAS studies",
                           br(),
                           selectInput("select_studies", "Select studies tap for displaying data",c("studies","publications","genotyping_techs","platforms","ancestries","ancestral_groups","countries_of_origin"),"studies"),
                           DTOutput("studies")),
                  tabPanel("Output: Ensembl Data",
                           br(),
                           DTOutput("ensembl_data"))
      )
      # tableOutput("data_table"),
    )
  )
)
