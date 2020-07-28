#Load libraries
library(shiny)
library(gwasrapidd)
library(stringr)
library(dplyr)
library(openxlsx)
library(shinyalert)
library(biomaRt)
library(stringr)

#Ensembl functions
ensembl = useDataset("hsapiens_snp",mart= useMart("ENSEMBL_MART_SNP"))

#Get all phenotypes
k = keys(ensembl, keytype="phenotype_description")

#Search for keys for the phenotype_description

#Select columns to return
attributes = listAttributes(ensembl)
attributes_of_interest <- attributes$name[c(1,8,10:15,19:23,25:29)]

#Function to get data from ensembl
get_data_function <- function(attributes = attributes_of_interest,values) {
  getBM(attributes=attributes, 
        filters = "phenotype_description", 
        values = values, 
        mart = ensembl)
}

DATA <- data.frame()
traits_to_search_for <- c()
ensembl_data_to_display <- FALSE
Values <- c()
studies <- FALSE
associations <- FALSE
associations_list <- list(associations = data.frame(),loci = data.frame(),risk_alleles = data.frame(),genes = data.frame(),ensembl_ids = data.frame(),entrez_ids = data.frame())

#Get trait names
# all_traits <- get_traits()
# trait_names <- as.data.frame(all_traits@traits$trait)
# names(trait_names) <- "traits"
trait_names <- read.csv("trait_names.csv")
trait_names <- trait_names[,-1]
trait_names <- as.data.frame(trait_names)
names(trait_names) <- "traits"

trait_conversion <- read.csv("trait_conversion.csv")
trait_conversion <- trait_conversion[,-1]

change_to_id <- function(x) {
  indicies <- which(trait_conversion$trait %in% x)
  indicies <- as.numeric(unlist(str_split(indicies," ")))
  return(indicies)
}

change_to_traits <- function(x) {
  traits <- which(trait_conversion$efo_id %in% x)
  return(as.character(trait_conversion$trait)[traits])
}

#Create empty data frame for search function
all_returned_trait_names <- data.frame(traits = NULL)

#Functions
add_rows_to_table_of_search_traits <- function(x) {
  if(x == "reset_dataframe") {
    all_returned_trait_names <<- data.frame(traits = NULL)
  }
  else {
    all_returned_trait_names <<- trait_names %>%
      filter(str_detect(tolower(traits), tolower(x))) %>%
      rbind(all_returned_trait_names)
  }
}

merge_data_from_ids <- function(my_efo_ids) {
  incProgress(1/4, detail = "getting studies")
  names(my_efo_ids) <- my_efo_ids
  my_studies <- purrr::map(my_efo_ids, ~ get_studies(efo_id = .x))
  number_of_traits <- length(my_studies)
  number_of_data_frames_in_studies <- length(slotNames(my_studies[[1]]))
  cbind_study_data_frames <- function(STUDY,DATAFRAME_NUMBER_IN_STUDY) {
    if(nrow(slot(my_studies[[STUDY]],slotNames(my_studies[[STUDY]])[DATAFRAME_NUMBER_IN_STUDY])) != 0) {
      cbind(trait = change_to_traits(names(my_studies)[STUDY]),
            as.data.frame(
              slot(my_studies[[STUDY]],slotNames(my_studies[[STUDY]])[DATAFRAME_NUMBER_IN_STUDY])
            )
      )
    }
  }
  list_of_study_data <- list()
  for(j in 1:number_of_data_frames_in_studies) {
    #print(paste("j1=",j))
    list_of_study_data[[j]] <- data.frame()
    for(i in 1:number_of_traits) {
      #print(paste("i1=",i))
      list_of_study_data[[j]] <- rbind(list_of_study_data[[j]],cbind_study_data_frames(i,j))
    }
    names(list_of_study_data)[j] <- slotNames(my_studies[[i]])[j]
  }
  studies <<- list_of_study_data
  
  if(length(number_of_traits) > 0) { 
    for(i in 1:number_of_traits) { # i <- 2 h <- 1
      #print(paste("i=",i))
      study_ids <- my_studies[[i]]@studies$study_id
      studies_split <- split(study_ids, ceiling(seq_along(study_ids)/30)) 
      my_associations <- purrr::map(studies_split, ~ get_associations(study_id = .x))
      associations_collected <- list(associations = data.frame(),loci = data.frame(),risk_alleles = data.frame(),genes = data.frame(),ensembl_ids = data.frame(),entrez_ids = data.frame())
      for(j in 1:6) { #add empty data frame if there are 0 rows
        #print(paste("j=",j))
        associations_collected[[j]] <- do.call(rbind,lapply(my_associations, function(x) slot(x,slotNames(x)[j])))
      }
      associations_collected <- lapply(associations_collected,as.data.frame)
      for(h in 1:length(associations_collected)) { #rbind via names of list to ensure it is right below (associations_list[name])
        #print(paste("h=",h))
        if(i == 1) {
          if(nrow(associations_collected[[names(associations_collected)[h]]]) > 0) {associations_list[[names(associations_collected)[h]]] <- cbind(trait = change_to_traits(names(my_studies[i])),associations_collected[[names(associations_collected)[h]]])} #add empty data frame if there are 0 rows
        } else {
          if(nrow(associations_collected[[names(associations_collected)[h]]]) > 0) {associations_list[[names(associations_collected)[h]]] <- rbind(associations_list[[names(associations_collected)[h]]],cbind(trait = change_to_traits(names(my_studies[i])),associations_collected[[names(associations_collected)[h]]]))}
        }
      }
      incProgress(0.75/(number_of_traits), detail = "getting associations")
    }
  }
  associations <<- associations_list
  associations_list <<- list(associations = data.frame(),loci = data.frame(),risk_alleles = data.frame(),genes = data.frame(),ensembl_ids = data.frame(),entrez_ids = data.frame())
}
