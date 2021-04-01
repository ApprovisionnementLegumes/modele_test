
# Exemple of a simple demand/offer model

########################################
# Load the libraries
########################################

library(tidyverse)
library(readxl)
library(lubridate)

# Nouveau commentaire

########################################
# Variable definitions
########################################


# 1 - Model input
# These are the variables that should be coming from the user
input_demand <- 5000
input_product <- "salad"
input_localisation <- "Louvain-la-Neuve" # Might need to have a gps coordinates here
input_time <- "december"
input_production_mode <- "organic"


# 2 - Model variables
# these are the variables we would like to estimate with the model

production_locations <- NULL
production_total <- NULL
environmental_impact <- NULL
production_times <- NULL


# 3 - Data tables
# These are the tables loaded from the database
annual_production <- NULL
localities <- NULL
products <- NULL
production_modes <- NULL
impacts <- NULL
production_impacts <- NULL

# 4 - Other variables

months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")





########################################
# Load the database tables
########################################

# TODO -> THIS IS FROM AN EXCEL SHEET. SHOUDL BE FROM A REAL DATABASE

annual_production <- read_xlsx("data/sample_database.xlsx", sheet="annual_production")
localities <- read_xlsx("data/sample_database.xlsx", sheet="localities")
products <- read_xlsx("data/sample_database.xlsx", sheet="products")
production_modes <- read_xlsx("data/sample_database.xlsx", sheet="production_modes")
impacts <- read_xlsx("data/sample_database.xlsx", sheet="impacts")
production_impacts <- read_xlsx("data/sample_database.xlsx", sheet="production_impacts")



########################################
# Process the datatables
########################################

# Create a data table that is easier to use by merging the needed information
production_data <- merge(annual_production, products, by.x = "id_product", by.y = "id") %>% 
  mutate(name_product = name) %>% 
  select(-name)

# merge with the localities
production_data <- merge(production_data, localities, by.x = "id_province", by.y = "id") %>% 
  mutate(name_locality = name)%>% 
  select(-name)

# merge with the production modes
production_data <- merge(production_data, production_modes, by.x = "id_mode", by.y = "id") %>% 
  mutate(name_mode = name) %>% 
  select(-name)


# Get the monthly production
# ATTENTION HYPOTHESE -> LA PRODUCTION EST EQUIVALENTE SUR LES MOIS DE RECOLTE
production_data <- production_data %>% 
  mutate(monthly_quantity = quantity / (stop_harvest - start_harvest))



########################################
# Model functions
########################################

get_production <- function(product, time, locality, mode){
  
  time_num <- which(months == time) # the month of interest, in number
  
  # get the specifi production for the given product
  specific_production <- production_data %>% 
    filter(name_product == product)
  
  if(nrow(specific_production) == 0){
    print("Sorry no data for this product")
    return(NULL) # get out of the function
  }
  
  # Get the specific production for a given production mode
  specific_production <- specific_production %>% 
    filter(name_mode == mode)
  
  if(nrow(specific_production) == 0){
    print("Sorry no data for that mode of production in the database")
    return(NULL) # get out of the function
  }

  # Get the specific production for a place
  specific_production <- specific_production %>% 
    filter(name_locality == locality)
  
  if(nrow(specific_production) == 0){
    print("Sorry no data for that locality in the database")
    return(NULL) # get out of the function
  }
    
  
  # Get the specific production for a given time
  specific_production <- specific_production %>% 
    filter(start_harvest <= time_num & stop_harvest >= time_num)
  
  if(nrow(specific_production) == 0){
    print("Sorry no production for that date")
    return(0) # get out of the function
  }
  
  return(specific_production$monthly_quantity)
}


# Testing the error messages from the function
get_production(product = "potato", time = input_time, locality = "bw", mode = input_production_mode)

get_production(product = input_product, time = "march", locality = "bw", mode = input_production_mode)

get_production(product = input_product, time = input_time, locality = "bw", mode = "rationned")

get_production(product = input_product, time = input_time, locality = "bxl", mode = input_production_mode)


# TODO get_impact()




########################################
# RUN THEN MODEL
########################################

offer <- get_production(product = input_product, time = input_time, locality = "bw", mode = input_production_mode)

if(offer > input_demand){
  print(paste0("there is enough. Current offer is ",offer," while the demand is ",input_demand))
}else{
  print("there is not enough. ")
 # Should look for production elsewhere
}




########################################
# OUTPUT OF THE MODEL
########################################

# TODO reflechir aux output graphiques










