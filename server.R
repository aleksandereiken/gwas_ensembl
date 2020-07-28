#Load libraries
library(shiny)
library(gwasrapidd)
library(stringr)
library(dplyr)
library(openxlsx)
library(shinyalert)
library(purrr)

server <- function(input, output) {
  
  v <- reactiveValues(words = "reset_dataframe")
  
  observeEvent(input$apply_search, {
    v$words <- input$search_term
  })
  
  observeEvent(input$reset_search, {
    v$words <- "reset_dataframe"
    Values <<- c()
  })
  
  #GWAS data
  
  output$patient_tick_off <- renderUI({
    add_rows_to_table_of_search_traits(v$words)
    number_of_traits <- length(all_returned_trait_names$traits)
    if(number_of_traits > 0) {
      lapply(1:number_of_traits, function(i) {
        checkboxInput(inputId = paste0("traits_", i), label = paste(all_returned_trait_names$traits[i]),
                      value = FALSE)
      })
    } else {return(NULL)}
  })
  
  observeEvent(input$get_data, {
    traits_to_search_for <- c()
    if(length(all_returned_trait_names$traits) != 0) {
      for(i in 1:length(all_returned_trait_names$traits)) {
        traits_to_search_for <- c(traits_to_search_for,input[[paste0("traits_", i)]])
      }
      withProgress(message = 'Getting data from GWAS', value = 0, {
          data_to_display <<- all_returned_trait_names[traits_to_search_for,"traits"]
          search_ids <<- paste(trait_conversion$efo_id[change_to_id(paste(data_to_display))])
          merge_data_from_ids(search_ids)
      })
    }
  })
  
  output$associations <- renderDT({
    req(input$get_data,cancelOutput = TRUE)
    req(associations,cancelOutput = TRUE)
    associations[[input$select_association]]
  }, 
  options = list(lengthChange = FALSE,pageLength = 15))
  
  output$studies <- renderDT({
    req(input$get_data,cancelOutput = TRUE)
    req(studies,cancelOutput = TRUE)
    studies[[input$select_studies]]
  }, 
  options = list(lengthChange = FALSE,pageLength = 15))
  
  #Ensembl data
  
  output$ensembl_traits <- renderUI({
    values <- k[str_detect(tolower(k),tolower(v$words))]
    Values <<- c(values,Values) 
    number_of_traits_ensembl <- length(Values)
    if(number_of_traits_ensembl > 0) {
      lapply(1:number_of_traits_ensembl, function(i) {
        checkboxInput(inputId = paste0("ensembl_traits_", i), label = paste(Values[i]),
                      value = FALSE)
      })
    } else {return(NULL)}
  })
  
  observeEvent(input$get_data_ensembl, {
    if(length(Values) != 0) {
      for(i in 1:length(Values)) {
        traits_to_search_for <- c(traits_to_search_for,input[[paste0("ensembl_traits_", i)]])
      }
      traits <- Values[traits_to_search_for]
      # DATA <<- traits
      withProgress(message = 'Getting data from Ensembl', value = 0, {
        for(i in 1:length(traits)) {
          if(i == 1) {
            incProgress(1/(length(traits)/2), detail = "getting data")
            DATA <- get_data_function(attributes = attributes_of_interest, values = traits[i])
          } else {
            Data <- get_data_function(attributes = attributes_of_interest, values = traits[i])
            DATA <- rbind(DATA, Data)
            incProgress(1/length(traits), detail = "getting data")
          }
        }
        ensembl_data_to_display <<- DATA
      })
    }
  })
  
  output$ensembl_data <- renderDT({
    req(input$get_data_ensembl,cancelOutput = TRUE)
    req(ensembl_data_to_display,cancelOutput = TRUE)
    ensembl_data_to_display
  }, 
  options = list(pageLength = 15,autoWidth = TRUE))
  
  #Find related GWAS traits
  
  observeEvent(input$related_traits, {
    traits_to_search_for <- c()
    if(length(all_returned_trait_names$traits) != 0) {
      for(i in 1:length(all_returned_trait_names$traits)) {
        traits_to_search_for <- c(traits_to_search_for,input[[paste0("traits_", i)]])
      }
      withProgress(message = 'Getting data from GWAS', value = 0, {
        data_to_display <<- all_returned_trait_names[traits_to_search_for,"traits"]
        incProgress(1/2, detail = "getting studies")
        studies <<- get_studies(efo_id = paste(trait_conversion$efo_id[change_to_id(paste(data_to_display))]))
        incProgress(1/2, detail = "getting traits")
        traits <<- get_traits(pubmed_id = paste(studies@publications$pubmed_id))
        output$related_traits_table <- renderDT({data.frame(related_traits = traits@traits$trait)}, 
                                                options = list(lengthChange = FALSE,pageLength = 15))
      })
    }
  })
  
  #Download Ensembl Data
  
  output$downloadDataEnsembl <- renderUI({
    req(input$get_data_ensembl, cancelOutput = TRUE)
    req(ensembl_data_to_display, cancelOutput = TRUE)
    downloadButton("download_data_ensembl", "Download Ensembl Excel Data")
  })
  
  output$download_data_ensembl <- downloadHandler(
    filename = function() {
      "Ensembl_data.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook("Ensembl_data")
      addWorksheet(wb, "data")
      writeData(wb,"data",ensembl_data_to_display)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  #Download GWAS data
  
  output$downloadData <- renderUI({
    req(input$get_data, data_to_display)
    req(associations, cancelOutput = TRUE)
    downloadButton("download_data", "Download GWAS Excel Data")
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "GWAS_data.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook("GWAS_data")
      addWorksheet(wb, "search_terms")
      addWorksheet(wb, "ass_associations")
      addWorksheet(wb, "ass_loci")
      addWorksheet(wb, "ass_risk_alleles")
      addWorksheet(wb, "ass_genes")
      addWorksheet(wb, "ass_ensembl_ids")
      addWorksheet(wb, "ass_entrez_ids")
      addWorksheet(wb, "stu_studies")
      addWorksheet(wb, "stu_genotyping_techs")
      addWorksheet(wb, "stu_platforms")
      addWorksheet(wb, "stu_ancestries")
      addWorksheet(wb, "stu_ancestral_groups")
      addWorksheet(wb, "stu_countries_of_origin")
      addWorksheet(wb, "stu_publications")
      
      writeData(wb,"search_terms",data_to_display)
      writeData(wb,"ass_associations",associations$associations)
      writeData(wb, "ass_loci",associations$loci)
      writeData(wb, "ass_risk_alleles",associations$risk_alleles)
      writeData(wb, "ass_genes",associations$genes)
      writeData(wb, "ass_ensembl_ids",associations$ensembl_ids)
      writeData(wb, "ass_entrez_ids",associations$entrez_ids)
      writeData(wb, "stu_studies",studies$studies)
      writeData(wb, "stu_genotyping_techs",studies$genotyping_techs)
      writeData(wb, "stu_platforms",studies$platforms)
      writeData(wb, "stu_ancestries",studies$ancestries)
      writeData(wb, "stu_ancestral_groups",studies$ancestral_groups)
      writeData(wb, "stu_countries_of_origin",studies$countries_of_origin)
      writeData(wb, "stu_publications",studies$publications)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
