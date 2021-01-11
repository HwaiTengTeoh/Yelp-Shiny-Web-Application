
library(shiny)
library(shinydashboard)
library(tibble)
library(tidyverse)
library(devtools)
library(shinyWidgets)
library(shinyBS)
library(leaflet)
library(readr)
library(lubridate)
library(shinyjs)
library(rintrojs)
library(DT)
library(tm)
library(wordcloud)
library(SnowballC)
library(reshape2)


title <- tags$a(href='https://www.yelp.com/',
                tags$img(src="yelp2.png", height = '60', width = '120'),
                target="_blank",color="white")



# Read files
restaurant <- readRDS(file="restaurant.rds")
review <- readRDS(file="review_clean.rds")
steps <- readr::read_csv("help_info.csv")

# Define UI for application that draws a histogram
ui <- 
    dashboardPage(
        skin="red",
        title="Yelp",

        #--------#
        # Header #
        #--------#
        
        dashboardHeader(
            title = title,
            titleWidth = 300,
            
            
            dropdownMenu(
                type = "notifications", 
                headerText = strong("HELP"), 
                icon = icon("question"), 
                badgeStatus = NULL,
                notificationItem(
                    text = (steps$text[1]),
                    icon = icon("search-location")
                ),
                notificationItem(
                    text =  (steps$text[2]),
                    icon = icon("search-location")
                    
                ),
                notificationItem(
                    text = (steps$text[3]),
                    icon = icon("cutlery")
                ),
                notificationItem(
                    text = (steps$text[4]),
                    icon = icon("star")
                ),
                notificationItem(
                    text = (steps$text[5]),
                    icon = icon("thumbs-up")
                ),
                notificationItem(
                    text=(steps$text[6]),
                    icon = icon("exclamation")
                )
            )
            
        ),
        
        #---------#
        # Sidebar #
        #---------#
        dashboardSidebar(
            div(class="inlay",style="height:15px;width:100%;background-color: #ecf0f5;"),
            width=300,
            sidebarMenu(
                shinyBS::bsButton(
                    inputId="show", 
                    label="Confirm Selections",
                    icon=icon("play"),
                    style="danger",
                    size="large"),
                menuItem(
                    "Location",
                    icon=icon("search-location"),
                    selectizeInput(
                        inputId="cityInput",
                        label = "Where are you now?",
                        choices=sort(unique(restaurant$city)),
                        selected = "TORONTO",
                        multiple = FALSE
                    ),
                    textInput(
                        inputId="streetInput",
                        label = "Which street you are now?",
                        ""
                    ),
                    h6("Optional: You may input streets after selecting the city"),
                    h6("Please do not input the abbreviation of address.")
                ),
                br(),
                menuItem(
                    "Food Category",
                    icon=icon("cutlery"),
                    selectInput(
                        inputId="categoryInput",
                        label="What are you craving for?",
                        choices=sort(unique(restaurant$categories)),
                        selected = "Fast Food",
                        selectize = TRUE,
                        multiple = TRUE
                      ),
                    h6("Optional: You may choose more than one category")
                    
                ),
                br(),
                menuItem(
                    "Stars",
                    icon=icon("star"),
                    sliderInput(
                        inputId="starsInput",
                        label = "How many stars?",
                        min = 0,
                        max=max(unique(restaurant$stars)),
                        value=c(0,max(unique(restaurant$stars))),
                        step=0.5
                    ),
                    h6("Please move the slider")
                ),
                br(),
                menuItem(
                    "Services",
                    tabName="service",
                    icon=icon("thumbs-up"),
                    
                    menuItem(
                        "Smoking",
                        icon=icon("smoking"),
                        checkboxGroupInput(
                            inputId="smokeInput",
                            label="",
                            choices = unique(restaurant$Smoking),
                            selected = "Restricted"
                        )
                    ),
                    
                    menuItem(
                        "WiFi",
                        icon=icon("wifi"),
                        switchInput(inputId = "wifiInput", 
                                    value = FALSE,
                                    onLabel = "Yes",
                                    offLabel = "Any",
                                    offStatus = "danger",
                                    size="mini",
                                    width="100%"
                        )
                    ),
                    
                    menuItem(
                        "Good For Kids",
                        icon=icon("child"),
                        switchInput(inputId = "GoodForKidsInput", 
                                    value = FALSE,
                                    onLabel = "For Kids",
                                    offLabel = "Any",
                                    offStatus = "danger",
                                    size="mini",
                                    width="100%"
                        )
                    ),
                    
                    menuItem(
                        "Take Out",
                        icon=icon("cutlery"),
                        switchInput(inputId = "TakeOutInput", 
                                    value = FALSE,
                                    onLabel = "Yes",
                                    offLabel = "Any",
                                    offStatus = "danger",
                                    size="mini",
                                    width="100%"
                        )
                    ),
                    
                    menuItem(
                        "Reservation",
                        icon=icon("calendar-check"),
                        switchInput(inputId = "ReservationInput", 
                                    value = FALSE,
                                    onLabel = "Yes",
                                    offLabel = "Any",
                                    offStatus = "danger",
                                    size="mini",
                                    width="100%"
                        )
                    )
                    
                    
                ),
                br(),
                hr(),
                menuItem(
                    "Group Members",
                    icon=icon("users"),
                    h5("Teoh Hwai Teng (S2016411)"),
                    h5("Muhammad Syafiq Bin Abdullah Sani (S2021682)"),
                    h5("Chew Way Yan (S2023355)")
                ),
                br(),
                menuItem("Source code", icon = icon("file-code-o"), 
                         href = "https://github.com/HwaiTengTeoh/Yelp-Shiny-Web-Application"),
                br(),
                menuItem("Data source", icon = icon("database"), 
                         href = "https://www.yelp.com/dataset")
            )
        ),
        
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href ="custom.css")
            ),
            
            useShinyjs(),
            introjsUI(),
            
            
            #-----------#
            # Main Body #
            #-----------#
            fluidRow(
                column(
                    width=12,
                    bsButton("overview",
                             label = "Overview",
                             icon = icon("spinner", class = "spinner-box"),
                             style = "danger"),
                    bsButton("around_you", 
                             label = "Around you", 
                             icon = icon("user"), 
                             style = "danger"),
                    bsButton("preference", 
                             label = "Your preferences", 
                             icon = icon("spinner", class = "spinner-box"), 
                             style = "danger")
                )
            )
           ,
           br(),
           
           fluidRow(
               div(
                   id="overview_panel1",
                   valueBoxOutput("no_restaurant"),
                   valueBoxOutput("no_restaurant_city"),
                   valueBoxOutput("no_review")
               )
           ),
           br(),
           
           fluidRow(
               div(
                   id="overview_panel2",
                   column(
                       width=6,
                       h6("Note: Show frequency of the food categories"),
                       uiOutput("box_overview1")
                   ),
                   column(
                       width=6,
                       h6("Note: Popular restaurants are rated \nwith more than 3.5 stars"),
                       uiOutput("box_overview2")
                       
                   )
               )
            ),
           
           
           fluidRow(
               div(
                   id="overview_panel3",
                   column(
                       width=12,
                       uiOutput("box_overview3")
                   )
               )
           ),
           
           
           fluidRow(
               div(
                   id="overview_panel4",
                   column(
                       width=12,
                       uiOutput("box_overview4")
                   )
               )
           ),

            fluidRow(
                div(
                    id="around_you_panel1",
                    column(
                        width=12,
                        selectInput("review_year", label = "Select year of reviews", 
                                    choices = list("Since 6 years ago (2014)" = "2014",
                                                   "Since 5 years ago (2015)" = "2015",
                                                   "Since 4 years ago (2016)" = "2016",
                                                   "Since 3 years ago (2017)" = "2017",
                                                   "Since 2 years ago (2018)"="2018",
                                                   "Recent one year (2019)"="2019"
                                    ), selected = "2014"),
                        h6("Note: Yelp user ratings tab shows the average review ratings based on the selection of review year below."),
                        uiOutput("box_aroundyou1")
                    )
                ),
            
            ),
           
           
           fluidRow(
               div(
                   id="around_you_panel2",
                   column(
                       width=12,
                       uiOutput("box_aroundyou2")
                       
                   )
               ),
               
           ),
           
           fluidRow(
               div(
                   id="preference_panel1",
                   column(
                       width=6,
                       h6("Note: Wordclouds show the reviews from the Top restaurants shown in bar chart"),
                       selectInput("dietary_option", label = "Select dietary restriction (if any)", 
                                   choices = list("Vegetarian" = "vegetarian",
                                                  "Non-specialized" = "general",
                                                  "Gluten-free" = "gluten_free",
                                                  "Halal" = "halal"
                                                  ), selected = "general")
                       
                   ),
                   column(
                       width=6,
                       h6("Note: Wordclouds show the reviews from the Top restaurants shown in bar chart"),
                       selectInput("meal_option", label = "Select Meal option (if any)", 
                                   choices = list("General" = "general",
                                                  "Breakfast"="breakfast",
                                                  "Brunch" = "brunch",
                                                  "Lunch" = "lunch",
                                                  "Dessert"="dessert",
                                                  "Dinner" = "dinner",
                                                  "Last Night" = "lastnight"
                                   ), selected = "general")
                       
                   )
               ),
               
           ),
            
            fluidRow(
                div(
                    id="preference_panel2",
                    column(
                        width=6,
                        uiOutput("box_preference1")
                        
                    ),
                    column(
                        width=6,
                        uiOutput("box_preference2")
                        
                    )
                ),
                
            )
            
        )
    )