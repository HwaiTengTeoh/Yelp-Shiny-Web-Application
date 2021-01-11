################################
# Read JSON file to Data Frame #
################################

# Install packages
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("shinyWidgets")
install.packages("devtools")
devtools::install_github("dreamRs/shinyWidgets")
install.packages("shinyBS")
install.packages("leaflet")
install.packages("readr")
install.packages("lubridate")
install.packages("shinyjs")
install.packages("rintrojs")
install.packages("DT")
install.packages("plotly")
install.packages("reshape2")

# Load the package required to read JSON files.
library("jsonlite")
library("tibble")
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("leaflet")
library("shinyBS")
library("readr")
library("lubridate")
library("shinyjs")
library("rintrojs")
library("DT")
library("reshape2")

# Check directory
setwd("")
getwd()


#-------------------------------------#
#           Business data             #
#-------------------------------------#
# yelp_academic_dataset_business.json #
#-------------------------------------#

# Give the input file name to the function.
result <- stream_in(file("yelp_academic_dataset_business.json"))

# Flatten the nested list 
business=jsonlite::flatten(result)
str(business)


#----------------#
# Data Cleansing #
#----------------#


# Cleansed categories
cleansed_categories=readr::read_csv(file="clean_categories.csv",
                                    col_types=cols(.default = "c")
                                    )
str(cleansed_categories)
head(cleansed_categories)


# Cleansed cities name
cleansed_cities=readr::read_csv(file="clean_cities.csv",
                                col_types=cols(.default = "c")
                                 )
str(cleansed_cities)
head(cleansed_cities)

# Cleansed address text
replace_add= c("Ave"="Avenue",
               "Blvd"="Boulevard",
               "Bldg"="Building",
               "Crt"="Court",
               "Cres"="Crescent",
               "Dr"="Driver",
               "Pl"="Place",
               "Rd"="Road",
               "Sq"="Square",
               "Stn"="Station",
               "St"="Street",
               "Terr"="Terrace")



# Select useful attributes
# Only focus for Restaurant Business
# Cleaning the state and city
# Unnest categories
# Clean the responses of other attributes

restaurant = as_data_frame(business) %>% 
             # Only interested for Opening restaurant
             filter(is_open==1) %>%
   
             # Convert to upper case
             mutate(city=str_to_upper(str_trim(city))) %>%
   
             # Select Ontorio
             filter(state=="ON") %>% 
   
             # Get the cleansed cities name
             left_join(cleansed_cities,by="city") %>%
   
             # Remove NA city (to be excluded)
             filter(!is.na(city_cleansed)) %>%
            
             # Clean the address
             mutate(full_stop=".") %>%
             unite("address_cont",address,full_stop,sep=" ") %>%
             mutate(address=str_replace_all(address_cont,c("Ave "="Avenue",
                                                  "Blvd"="Boulevard",
                                                  "Bldg"="Building",
                                                  "Crt"="Court",
                                                  "Cres "="Crescent",
                                                  "Dr "="Driver",
                                                  "Pl "="Place",
                                                  "Rd "="Road",
                                                  "Sq "="Square",
                                                  "Stn"="Station",
                                                  "St "="Street",
                                                  "Terr "="Terrace",
                                                  " W "=" West",
                                                  " E "=" East",
                                                  " N "=" North",
                                                  " S "=" South")
                                        )) %>%
             # Select Restaurant only
             filter(str_detect(categories,"Restaurant")) %>%
   
             # Each category per row of obs
             separate_rows(categories,sep=",") %>%
             mutate(categories=str_trim(categories)) %>%
             #count(categories,sort=TRUE) %>%
            
             # Remove tag: Restaurant & Food & Restaurants
             # Remove tag: only include the categories from our cleansed_categories
             filter(categories != "Restaurant",categories != "Food",categories != "Restaurants") %>%
             filter(categories %in% cleansed_categories$categories) %>%
             
             # Select related columns
             select(name, business_id,address,
                    review_count,stars,categories,
                     longitude,latitude,city_cleansed,
                     attributes.WiFi,
                     attributes.GoodForKids,
                     attributes.RestaurantsTakeOut,
                     attributes.RestaurantsReservations,
                     attributes.Smoking,
                     attributes.GoodForMeal,
                     attributes.DietaryRestrictions
                    ) %>%
   
               # Cleaning and Process: attributes.DietaryRestrictions
               mutate(DietaryRestrictions=str_replace_all(attributes.DietaryRestrictions,"\\{|'|\\}","")) %>%
               mutate(DietaryRestrictions=ifelse(DietaryRestrictions=="None",NA,DietaryRestrictions)) %>%
               separate(DietaryRestrictions,c("dairy_free","gluten_free","vegan","kosher","halal","soy_free","vegetarian"),sep=",") %>%
               mutate(general=ifelse(
                                 (
                                   is.na(dairy_free) & is.na(gluten_free) &
                                   is.na(vegan)      & is.na(kosher)      &
                                   is.na(halal)      & is.na(soy_free)    &
                                   is.na(vegetarian)
                                 ) |
                                 (
                                  str_detect(dairy_free, c("False","None","NA")) & str_detect(gluten_free, c("False","None","NA")) &
                                  str_detect(vegan, c("False","None","NA")) & str_detect(kosher, c("False","None","NA"))           &
                                   str_detect(halal, c("False","None","NA")) & str_detect(soy_free, c("False","None","NA"))         &
                                   str_detect(vegetarian, c("False","None","NA"))
                                 ),"general: True","general: False")) %>%
              gather(key="DietaryRestrictions",value="DietaryRestrictions_TF",c("dairy_free","gluten_free","vegan","kosher","halal","soy_free","vegetarian","general")) %>%
              mutate(DietaryRestrictions_TF=as.logical(str_trim(sapply(strsplit(DietaryRestrictions_TF,':'),function(x)x[2])))) %>%
              filter(DietaryRestrictions_TF==TRUE) %>%
            
              # Cleaning and Process: attributes.WiFi
              mutate(WiFi=case_when(
                                     str_detect(attributes.WiFi,"free") ~ TRUE,
                                     str_detect(attributes.WiFi,"paid") ~ TRUE,
                                     TRUE ~ FALSE
                                     )) %>%
   
             # Cleaning and Process: attributes.GoodForKids
             mutate(GoodForKids=case_when(
                                           str_detect(attributes.GoodForKids,"True") ~ TRUE,
                                           TRUE ~ FALSE
                                           )) %>%
   
             # Cleaning and Process: attributes.RestaurantsTakeOut
             mutate(RestaurantsTakeOut=case_when(
                                                 str_detect(attributes.RestaurantsTakeOut,"True") ~ TRUE,
                                                 TRUE ~ FALSE
                                               )) %>%
   
             # Cleaning and Process: attributes.RestaurantsReservations
             mutate(RestaurantsReservations=case_when(
                                                       str_detect(attributes.RestaurantsReservations,"True") ~ TRUE,
                                                       TRUE ~ FALSE
                                                     )) %>%
   
             # Cleaning and Process: attributes.Smoking
             mutate(Smoking=case_when(
                                      str_detect(attributes.Smoking,"yes") ~ "Allow",
                                      str_detect(attributes.Smoking,"outdoor") ~ "Allow, but outdoor only",
                                      TRUE ~ "Restricted"
                                      )) %>%
   
             # Cleaning and Process: attributes.GoodForMeal
             mutate(GoodForMeal=str_replace_all(attributes.GoodForMeal,"\\{|'|\\}","")) %>%
             mutate(GoodForMeal=ifelse(GoodForMeal=="None",NA,GoodForMeal)) %>%
             separate(GoodForMeal,c("dessert","lastnight","lunch","dinner","brunch","breakfast"),sep=",") %>%
             mutate(general=ifelse(
                               (
                                 is.na(dessert) & is.na(lastnight) &
                                 is.na(lunch)   & is.na(dinner)    &
                                 is.na(brunch)  & is.na(breakfast)
                               ) |
                               (
                                 str_detect(dessert, c("False","None")) & str_detect(lastnight, c("False","None")) &
                                 str_detect(lunch, c("False","None")) & str_detect(dinner, c("False","None"))      &
                                 str_detect(brunch, c("False","None")) & str_detect(breakfast, c("False","None"))
                               ),"general: True","general: False")) %>%
            gather(key="MealType",value="MealType_TF",c("dessert","lastnight","lunch","dinner","brunch","breakfast","general")) %>%
            mutate(MealType_TF=as.logical(str_trim(sapply(strsplit(MealType_TF,':'),function(x)x[2])))) %>%
            filter(MealType_TF==TRUE) %>%
            
            # Remove unwanted columns and old attributes
            select(-contains("attributes"),-ends_with("_TF")) %>%
            rename(city=city_cleansed) %>%
            mutate(DietaryRestrictions=ifelse(DietaryRestrictions %in% c("vegan","vegetarian"),"vegetarian",DietaryRestrictions))



# Structure of cleansed data frame: restaurant
str(restaurant)
unique(restaurant$MealType)
unique(restaurant$gluten_free)
# First 6 observations
head(restaurant)

# Verify no missing values
sum(is.na(restaurant))

# Output to csv file
write.csv(restaurant,file="restaurant.csv")

# Output to rds file
saveRDS(restaurant, "restaurant.rds")

view(restaurant)

#-----------------------------------#
#           Review data             #
#-----------------------------------#
# yelp_academic_dataset_review.json #
#-----------------------------------#

# Data file is big
# R cant handle such big file
# We processed the data in Python using panda
# and write in a csv file
# which will be further process here
# Please refer to the "02 Manipulate review data and write to csv.ipynb" file


# Read the processed csv file for reviews data
review = readr::read_csv(file="reviews.csv",
                         col_types=cols(.default = "d", date = "c", business_id = "c", text="c")
)

# Conversion from string to date time format
review$date <- as.POSIXct(review$date)

# Rename 2nd column of review data frame from "stars" to "ratings"
colnames(review)[2] <- "ratings"

# Checking
str(review)
head(review)
nrow(review)
# Check the range of date: decided to take nearest few years? 
min(review$date) # 2007-12-15 00:38:57 +08 #
max(review$date) # 2019-12-13 15:40:43 +08 #

# Output to csv file
write.csv(review,file="review_clean.csv")

# Output to rds file
saveRDS(review, "review_clean.rds")


#---------------#
# Miscellaneous #
#---------------#
steps = readr::read_csv("help_info.csv")



