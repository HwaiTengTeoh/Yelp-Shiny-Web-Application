library(shiny)
library(shinydashboard)
library(tibble)
library(tidyverse)
library(devtools)
library(shinyWidgets)
library(shinyBS)
library(leaflet)
library(readr)
library(shinyjs)
library(lubridate)
library(rintrojs)
library(DT)
library(tm)
library(wordcloud)
library(SnowballC)
library(reshape2)



# Read files
restaurant <- readRDS(file="restaurant.rds")
review <- readRDS(file="review_clean.rds")
steps <- readr::read_csv("help_info.csv")

server= function(input, output, session) {
   
    
    #---------------#
    # Filtered sets #
    #---------------#
    
    filter_city = reactive({
        restaurant %>% filter(city==input$cityInput)
    })
    
    
    filter_main_set <- reactive({
        # confirm buttons needs to be pressed to initiate this code
        
        # isolate({
            if (!is.na(input$cityInput)) {
                restaurant %>%
                    filter(city %in% input$cityInput) %>%
                    filter(str_detect(str_to_upper(address),str_to_upper(input$streetInput))) %>%  
                    filter(categories %in% input$categoryInput) %>%
                    filter(stars <=input$starsInput[2],stars >=input$starsInput[1])
                    
                    }
                #}
            #)
    
    })
    
    filter_service <- eventReactive(input$show,{
            filter_main_set() %>% 
            filter(
                Smoking %in% if(!is.na(input$smokeInput)){
                    input$smokeInput
                } else{
                    c("Restricted","Allow","Allow, but outdoor only")
                },
                WiFi %in% if(input$wifiInput == TRUE){
                    input$wifiInput
                } else{
                    c(TRUE,FALSE)
                },
                GoodForKids %in% if(input$GoodForKidsInput == TRUE){
                    input$wifiInput
                } else{
                    c(TRUE,FALSE)
                },
                RestaurantsReservations %in% if(input$ReservationInput == TRUE){
                    input$ReservationInput
                } else{
                    c(TRUE,FALSE)
                },
                RestaurantsTakeOut %in% if(input$TakeOutInput == TRUE){
                    input$TakeOutInput
                } else{
                    c(TRUE,FALSE)
                }
            )     
        }
        
    )
    
    #show intro modal
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro.Rhtml"),
            easyClose = TRUE
        ))
    })
    
    ###############
    # Summary box #
    ###############
    
    output$no_restaurant <- renderValueBox({
        valueBox(
            value = length(unique(restaurant$business_id)), subtitle = "Restaurants in Ontorio",
            icon = icon("map-marker", lib = "glyphicon"), color = "blue"
        )
    })
    
    output$no_restaurant_city <- renderValueBox({
        valueBox(
            value =length(unique(filter_city()$business_id)), subtitle = paste0("Restaurants in ",input$cityInput),
            icon = icon("map-marker", lib = "glyphicon"), color = "blue"
        )
    })
    
    
    output$no_review <- renderValueBox({
        valueBox(
            value = nrow(review), subtitle = paste0("Number of reviews"),
            icon = icon("user", lib = "glyphicon"), color = "blue"
        )
    })
    
    
    # use action buttons as tab selectors
    update_all <- function(x) {
        updateSelectInput(session, "tab",
                          choices = c("", "Overview", "Around you","Your preferences"),
                          label = "",
                          selected = x
        )
    }
    
    observeEvent(input$overview, {
        update_all("Overview")
    })
    
    observeEvent(input$around_you, {
        update_all("Around you")
    })
    
    observeEvent(input$preference, {
        update_all("Your preferences")
    })
    
    
    
    # Dynamic render rules
    observeEvent("", {
        show("overview_panel1")
        show("overview_panel2")
        show("overview_panel3")
        show("overview_panel4")
        hide("around_you_panel1")
        hide("around_you_panel2")
        hide("preference_panel1")
        hide("preference_panel2")
    }, once = TRUE)
    
    
    observeEvent(input$overview, {
        show("overview_panel1")
        show("overview_panel2")
        show("overview_panel3")
        show("overview_panel4")
        hide("around_you_panell")
        hide("around_you_panel2")
        hide("preference_panel1")
        hide("preference_panel2")
    })
    
    
    observeEvent(input$around_you, {
        hide("overview_panel1")
        hide("overview_panel2")
        hide("overview_panel3")
        hide("overview_panel4")
        show("around_you_panel1")
        show("around_you_panel2")
        hide("preference_panel1")
        hide("preference_panel2")

    })
    
    observeEvent(input$preference, {
        hide("overview_panel1")
        hide("overview_panel2")
        hide("overview_panel3")
        hide("overview_panel4")
        hide("around_you_panel1")
        hide("around_you_panel2")
        show("preference_panel1")
        show("preference_panel2")

    })
    
    
    # show active button with color
    observeEvent(input$tab, {
        x <- input$tab
        updateButton(session, "overview", style = {
            if (x == "Overview") {
                paste0("warning")
            } else {
                paste0("success")
            }
        })
        
        updateButton(session, "around_you", style = {
            if (x == "Around you") {
                paste0("warning")
            } else {
                paste0("success")
            }
        })
        
    })
    
    
    ########
    # Tabs #
    ########
    
    output$box_overview1 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_overview1",
                title="Popular food categories",
                width = 12,
                tabPanel("State",
                         plotOutput("top10category_ontorio"),
                         icon=icon("location-arrow ")),
                tabPanel("City",
                         plotOutput("top10category_city"),
                         icon=icon("location-arrow "))
            )
        )
    })
    
    
    output$box_overview2 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_overview2",
                title="Popular Restaurants",
                width = 12,
                tabPanel("State",
                         plotOutput("top10restaurant_ontorio"),
                         icon=icon("location-arrow ")),
                tabPanel("City",
                         plotOutput("top10restaurant_city"),
                         icon=icon("location-arrow "))
            )
        )
    })
    
    
    output$box_overview3 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_overview3",
                title="Services",
                width = 12,
                tabPanel("State",
                         plotOutput("donut_summary"),
                         icon=icon("location-arrow ")),
                tabPanel("City",
                         plotOutput("donut_summary_city"),
                         icon=icon("location-arrow "))
            )
        )
    })
    
    output$box_overview4 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_overview4",
                title="Reviews in Ontorio",
                width = 12,
                tabPanel("By year",
                         plotOutput("review_time"),
                         icon=icon("location-arrow ")),
                tabPanel("By Month",
                         plotOutput("review_time_month"),
                         icon=icon("location-arrow "))
            )
        )
    })
    
    output$box_aroundyou1 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_aroundyou1",
                title="Restaurants Around you",
                width = 12,
                tabPanel("Location",
                         leafletOutput("map"),
                         icon=icon("map-marker")),
                tabPanel("Yelp users ratings",
                         leafletOutput("map_rating"),
                         icon=icon("star-o"))
            )
        )
    })
    
    output$box_aroundyou2 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_aroundyou2",
                title="",
                width = 12,
                tabPanel("List of Restaurants",
                         dataTableOutput("map_table"),
                         icon=icon("map-marker"))
            )
        )
    })
    
    output$box_preference1 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_preference1",
                title="",
                width = 12,
                tabPanel("Dietary Restrictions",
                         plotOutput("dietary_plot"),
                         icon=icon("ban")),
                tabPanel("Wordclouds (Reviews)",
                         plotOutput("dietary_plot_WC"),
                         icon=icon("ban"))
            )
        )
    })
    
    
    output$box_preference2 = renderUI({
        div(
            style = "position: relative",
            tabBox(
                id="box_preference2",
                title="",
                width = 12,
                tabPanel("Meal Types",
                         plotOutput("meal_plot"),
                         icon=icon("cutlery")),
                tabPanel("Wordclouds (Reviews)",
                         plotOutput("meal_plot_WC"),
                         icon=icon("cutlery"))
            )
        )
    })
    

    
    #########################################
    # Overview panel - Famous food category #
    #########################################
    
    
    top10category_ontorio = 
        #eventReactive(input$show,{
        reactive({
        restaurant %>% 
            group_by(categories) %>% 
            count() %>%  
            arrange(desc(n)) %>% 
            ungroup() %>%
            top_n(10) %>% 
            mutate(categories_ordered=fct_reorder(categories,n)) %>%
                ggplot(aes(x=categories_ordered,y=n)) + 
                geom_col(fill="#ED7D31",alpha=0.6,color="black") + 
                ggtitle("Top Food categories in Ontorio") +
                xlab("Food categories") +
                ylab("Number of restaurants") +
                coord_flip() +
                theme_bw() 
                    
    })
    
    
    top10category_city = 
        eventReactive(input$show,{
        #reactive({
        restaurant %>% 
            filter(city==input$cityInput) %>%
            group_by(categories) %>% 
            count() %>%  
            arrange(desc(n)) %>% 
            ungroup() %>%
            top_n(10) %>% 
            mutate(categories_ordered=fct_reorder(categories,n)) %>%
            ggplot(aes(x=categories_ordered,y=n)) + 
            geom_col(fill="#ED7D31",alpha=0.6,color="black") + 
            ggtitle(paste0("Top Food categories in ",input$cityInput)) +
            xlab("Food categories") +
            ylab("Number of restaurants") +
            coord_flip() +
            theme_bw() 
            
    })
       
    output$top10category_ontorio =  renderPlot({
        top10category_ontorio()
    })
    
    output$top10category_city =  renderPlot({
        top10category_city()
    })
    
    
    ######################################
    # Overview panel - Famous Restaurant #
    ######################################    
    
    top10restaurant_ontorio = 
        #eventReactive(input$show,{
        reactive({
            restaurant %>% 
                filter(review_count >= 100 & stars > 3.5 ) %>% 
                select(name,address,review_count,stars) %>%
                distinct(name,address,.keep_all=TRUE) %>%
                arrange(desc(review_count)) %>% 
                top_n(10,review_count) %>% 
                mutate(name_ordered=fct_reorder(name,review_count)) %>%
                ggplot(aes(x=name_ordered,y=review_count)) + 
                geom_col(fill="#DBAD03",alpha=0.6,color="black") + 
                coord_flip() +
                theme_bw() +
                ggtitle("Top Restaurants in Ontorio") +
                xlab("Restaurant") +
                ylab("Number of reviews")
        }
        )
    
    
    top10restaurant_city = 
        eventReactive(input$show,{
            #reactive({
            restaurant %>% 
                filter(city==input$cityInput) %>%
                filter(review_count >= 100 & stars > 3.5 ) %>% 
                select(name,address,review_count,stars) %>%
                distinct(name,address,.keep_all=TRUE) %>%
                arrange(desc(review_count)) %>% 
                top_n(10,review_count) %>% 
                mutate(name_ordered=fct_reorder(name,review_count)) %>%
                ggplot(aes(x=name_ordered,y=review_count)) + 
                geom_col(fill="#DBAD03",alpha=0.6,color="black") + 
                coord_flip() +
                theme_bw() +
                ggtitle(paste0("Top Restaurants in ",input$cityInput)) +
                xlab("Restaurant") +
                ylab("Number of reviews")
        }
        )
    
    
    output$top10restaurant_ontorio =  renderPlot({
        top10restaurant_ontorio()
    })
    
    output$top10restaurant_city =  renderPlot({
        top10restaurant_city()
    })
    
    
    
    ##########################################
    # Overview panel - Attributes proportion #
    ##########################################
    
    
    output$donut_summary =  renderPlot({
        count <- 
            restaurant %>% 
            select(business_id,WiFi,RestaurantsReservations,RestaurantsTakeOut,GoodForKids) %>% 
            distinct(business_id,.keep_all=TRUE) %>% 
            gather(key="service",value="category",-business_id) %>%
            group_by(service,category) %>%
            count() %>% ungroup(category) %>%
            mutate(prop=round(n/sum(n)*100)) %>%
            mutate(prop_text=ifelse(round(n/sum(n)*100)<3,"",paste0(round(n/sum(n)*100),"%"))) %>%
            group_by(service) %>% arrange(service,desc(category)) %>%
            mutate(cumsum=cumsum(prop),lab.ypos = cumsum(prop) - 0.5*prop)
        
        mycols <- c("#E20000","#6BA42C","#7F7F7F")
        ggplot(count, aes(x = 2, y = prop, fill = category)) +
            geom_bar(stat = "identity", color = "black",alpha=0.7) +
            coord_polar(theta = "y", start = 0)+
            geom_text(aes(y = lab.ypos, label = prop_text), color = "black")+
            scale_fill_manual(values = mycols) +
            theme_bw() +
            xlim(0.5, 2.5) +
            theme(legend.position="bottom") + 
            facet_wrap(~service,ncol=4)
    })
    
    donut_summary_city <- 
        #eventReactive(input$show,{
        reactive({
        count <- 
            restaurant %>% 
            filter(city==input$cityInput) %>%
            select(business_id,WiFi,RestaurantsReservations,RestaurantsTakeOut,GoodForKids) %>% 
            distinct(business_id,.keep_all=TRUE) %>% 
            gather(key="service",value="category",-business_id) %>%
            group_by(service,category) %>%
            count() %>% ungroup(category) %>%
            mutate(prop=round(n/sum(n)*100)) %>%
            mutate(prop_text=ifelse(round(n/sum(n)*100)<3,"",paste0(round(n/sum(n)*100),"%"))) %>%
            group_by(service) %>% arrange(service,desc(category)) %>%
            mutate(cumsum=cumsum(prop),lab.ypos = cumsum(prop) - 0.5*prop)
        
        mycols <- c("#E20000","#6BA42C","#7F7F7F")
        ggplot(count, aes(x = 2, y = prop, fill = category)) +
            geom_bar(stat = "identity", color = "black",alpha=0.7) +
            coord_polar(theta = "y", start = 0)+
            geom_text(aes(y = lab.ypos, label = prop_text), color = "black")+
            scale_fill_manual(values = mycols) +
            theme_bw() +
            xlim(0.5, 2.5) +
            theme(legend.position="bottom") + 
            facet_wrap(~service,ncol=4)
    })
    
    output$donut_summary_city =  renderPlot({
        donut_summary_city()
    })
    
    ########################################
    # Overview panel - Reviews time series #
    ########################################

    output$review_time = renderPlot({
        
        review %>% select(-text) %>% filter(year(date)>=2014) %>%
         mutate(date_y=year(date)) %>%
         group_by(date_y) %>% count() %>%
         ggplot(aes(x=date_y,y=n,color="black")) +
         geom_area(colour = "black", fill = "#DB6413", alpha = .3) +
         ylab("Number of reviews") +
         scale_x_discrete(name ="Year",
                          limits=c(2014,2015,2016,2017,2018,2019))+
         theme_bw() 
    })
    
    output$review_time_month = renderPlot({
        
        review %>% select(-text) %>% filter(year(date)>=2014) %>%
            mutate(date_y=year(date),date_m=month(date)) %>%
            group_by(date_y,date_m) %>% count() %>%
            ggplot(aes(x=date_m,y=n,color="black")) +
            geom_area(colour = "black", fill = "#C00000", alpha = .3) +
            ylab("Number of reviews") +
            scale_x_discrete(name ="Month", 
                             limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
            facet_wrap(~date_y)+
            theme_bw() 
        
    })
    
    
    
    
    ####################
    # Around_you panel #
    ####################
    
    # Map to show location #
    output$map = renderLeaflet(
        leaflet() %>%
            addTiles() %>%
            mapOptions(zoomToLimits ="always") %>%
            addMarkers(lat=filter_service()$latitude,
                       lng=filter_service()$longitude,
                       popup=paste("", filter_service()$name, "<br>",
                                   "Address:", filter_service()$address, "<br>",
                                   "Rated:",filter_service()$stars,"stars", "<br>")
            )
    )
    
    # Map to show distribution of review ratings #
    map2=eventReactive(input$show,{
        # Map to show Restaurant vs average ratings
        avg_rating =
            review %>% filter(year(date)>=input$review_year) %>%
            select(-text) %>%
            filter(business_id %in% filter_service()$business_id) %>%
            group_by(business_id) %>%
            summarize(average_rating=mean(ratings))
        
        
        binpal =
            colorBin(c("#FFD966","#F4B183","#C55A11","#843C0C"),
                     avg_rating$average_rating,
                     4,
                     pretty=FALSE)
        
        map_rating_data= filter_service() %>%
            left_join(avg_rating,by=c("business_id"))
        
        map_rating_data %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(lng=~longitude, lat=~latitude,
                             weight=0, radius=7, fillOpacity = 0.7,
                             color = ~binpal(average_rating),
                             popup = paste("",map_rating_data$name, "<br>",
                                           "Address:",map_rating_data$address, "<br>",
                                           "Rated:",map_rating_data$stars,"stars", "<br>",
                                           "Average review:",round(map_rating_data$average_rating,digits=2),"stars", "<br>"))%>%
            addLegend("bottomleft",
                      pal=binpal,
                      values=stars,
                      title = "Restaurants and their Average Ratings",
                      labels=~binpal(average_rating),
                      labFormat = labelFormat(prefix = ""),
                      opacity = 0.7)
        
    })
    
    output$map_rating = renderLeaflet({
        map2()
    })
    
    
    # Table to show the list of restaurant #
    output$map_table =renderDataTable({
        filter_service() %>% 
            select(name,address,review_count,stars) %>% 
            arrange(desc(stars)) %>% 
            distinct(name,address,review_count,stars)
    })
    
    
    ####################
    # Preference Panel #
    ####################
    
    top_Dietary = 
        eventReactive(input$show,{
        #reactive({    
            data= filter_main_set() %>% 
                  select(name,address,review_count,stars,DietaryRestrictions) %>%
                  distinct(name,address,DietaryRestrictions,.keep_all=TRUE) %>%
                  filter( DietaryRestrictions == input$dietary_option) %>%
                  arrange(desc(review_count)) %>% 
                  top_n(5,review_count) %>% 
                  mutate(name_ordered=fct_reorder(name,review_count)) 
            
            if(nrow(data)==0){
                 as.data.frame(cbind(x=c(0,100),y=c(0,100))) %>%
                     ggplot(aes(x=x,y=y)) +
                    ggplot2::annotate("text", x=50, y=50, label= "No restaurant shown") +
                     theme_bw() +
                     theme(axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.y = element_blank())
             } else if (nrow(data)!=0){
                 data %>%
                     ggplot(aes(x=name_ordered,y=review_count)) +
                     geom_col(fill="#ED7D31",alpha=0.6,color="black") +
                     coord_flip() +
                     theme_bw() +
                     ggtitle(paste0("Top Restaurants (",input$dietary_option,")\n fit your requirements")) +
                     xlab("Restaurant") +
                     ylab("Number of reviews")
             }

            
        }
        )  
    
    
     top_Dietary_WC =
         eventReactive(input$show,{
         #reactive({
             data= filter_main_set() %>%
                 select(name,address,business_id,review_count,stars,DietaryRestrictions) %>%
                 distinct(name,address,business_id,DietaryRestrictions,.keep_all=TRUE) %>%
                 filter( DietaryRestrictions == input$dietary_option) %>%
                 arrange(desc(review_count)) %>%
                 top_n(5,review_count) %>%
                 select(business_id)
    
             text_data = review %>%
                        filter(year(date)>=2014) %>%
                        filter(business_id %in% data$business_id) %>%
                        select(text)
    
             t <- data.frame(doc_id=row.names(text_data),text=text_data$text, stringsAsFactors = FALSE)
             corpus <- Corpus(DataframeSource(t))
             corpus <- tm_map(corpus, stripWhitespace)
             corpus <- tm_map(corpus, content_transformer(tolower))
             corpus <- tm_map(corpus, removeWords, stopwords("english"))
             corpus <- tm_map(corpus, stemDocument)
    
             if(nrow(data)==0){
                 as.data.frame(cbind(x=c(0,100),y=c(0,100))) %>%
                     ggplot(aes(x=x,y=y)) +
                     ggplot2::annotate("text", x=50, y=50, label= "No reviews shown") +
                     theme_bw() +
                     theme(axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.y = element_blank())
             } else if (nrow(data)!=0){
    
                 wordcloud(corpus, scale = c(3.5, 0.25), max.words = 50, min.freq = 5, random.order = ,
                           rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
    
             }
         }
         )

    top_MealType = 
        eventReactive(input$show,{
        #reactive({   
            data= filter_main_set() %>% 
                select(name,address,review_count,stars,MealType) %>%
                distinct(name,address,MealType,.keep_all=TRUE) %>%
                filter( MealType == input$meal_option) %>%
                group_by(name) %>%
                mutate(review_count=sum(review_count)) %>%
                ungroup() %>%
                distinct(name,.keep_all=TRUE) %>%
                arrange(desc(review_count)) %>% 
                top_n(5,review_count) %>% 
                mutate(name_ordered=fct_reorder(name,review_count)) 
            
            if(nrow(data)==0){
                as.data.frame(cbind(x=c(0,100),y=c(0,100))) %>%
                    ggplot(aes(x=x,y=y)) +
                    ggplot2::annotate("text", x=50, y=50, label= "No restaurant shown") +
                    theme_bw() +
                    theme(axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank())
            } else if (nrow(data)!=0){
                data %>%
                    ggplot(aes(x=name_ordered,y=review_count)) +
                    geom_col(fill="#DBAD03",alpha=0.6,color="black") +
                    coord_flip() +
                    theme_bw() +
                    ggtitle(paste0("Top Restaurants (best for ",input$dietary_option,")\n fit your requirements")) +
                    xlab("Restaurant") +
                    ylab("Number of reviews")
            }
            
            
        }
        )  
    
    top_MealType_WC =
        eventReactive(input$show,{
        #reactive({
            data= filter_main_set() %>%
                select(name,address,business_id,review_count,stars,MealType) %>%
                distinct(name,address,business_id,MealType,.keep_all=TRUE) %>%
                filter( MealType == input$meal_option) %>%
                arrange(desc(review_count)) %>%
                top_n(5,review_count) %>%
                select(business_id)
            
            text_data = review %>%
                filter(year(date)>=2014) %>%
                filter(business_id %in% data$business_id) %>%
                select(text)
            
            t <- data.frame(doc_id=row.names(text_data),text=text_data$text, stringsAsFactors = FALSE)
            corpus <- Corpus(DataframeSource(t))
            corpus <- tm_map(corpus, stripWhitespace)
            corpus <- tm_map(corpus, content_transformer(tolower))
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            corpus <- tm_map(corpus, stemDocument)
            
            if(nrow(data)==0){
                as.data.frame(cbind(x=c(0,100),y=c(0,100))) %>%
                    ggplot(aes(x=x,y=y)) +
                    ggplot2::annotate("text", x=50, y=50, label= "No reviews shown") +
                    theme_bw() +
                    theme(axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank())
            } else if (nrow(data)!=0){
                
                wordcloud(corpus, scale = c(3.5, 0.25), max.words = 50, min.freq = 5, random.order = ,
                          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
                
            }
        }
        )
    
    
    output$dietary_plot = renderPlot({
        top_Dietary()
    })
    
    output$dietary_plot_WC = renderPlot({
        top_Dietary_WC()
     })
    
    output$meal_plot = renderPlot({
        top_MealType()
    })
    
    output$meal_plot_WC = renderPlot({
        top_MealType_WC()
    })
    
    
}