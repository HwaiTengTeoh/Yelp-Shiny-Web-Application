# Yelp Eat: Travel around with the best authentic local taste

### Working Summary 
Everyone loves to travel. People travel for different reasons, mainly for work, family, or leisure. It helps to broaden one's horizon while maintaining inner balance. Traveling to a new place brings unfamiliarity with the culture, food, or even traditions of the place, especially food, as food is one of the main concerns and part of the joy throughout the journey. Hence, the motivation of bringing up "Yelp Eat" as an interactive web application hopes to connect people with recommended local eateries or even based on their preferred choices in Ontario with quick analysis by just clicking on fingers without much effort for researching from other sources.


### About "Yelp Eat"
The web application is named "Yelp Eat" which takes data as a service and provides quick and simple analysis for finding the best food and location that meet users' requirements. The data product serves as a platform to connect users with restaurant information based on their preferences by selecting choices. "Yelp Eat" provides quick and simple analysis to users, which help to save time from crawling different blogs and website for the search of the restaurant.

Here are the expectations that users can anticipate from "Yelp Eat":
- To have quick results of place and location of restaurants by navigating the side tab for selection of users' choices
- To have an overview of the restaurants information in Ontorio and different cities 
- To check the most popular food categories and popular restaurants at specified location 
- To visualize the trends of number of reviews received from past years 
- To identify the location of restaurants nearby with address input and check the average reviewer's ratings from different time range 
- To identify the top restaurants that matched the selected choices and special requirements such as dietary restrictions and type of meal for users. 
- To visualize the most common reviews from the top restaurants that matched the selected choices in simple Wordcloud 


### Data Sources
The data is obtained from Yelp.com and available to the public and is prepared for Yelp Data Challenge event launched by Yelp, which consists of the businesses, reviews, and user data for academic and research purposes. For development of the web application, this work is using only the business and reviews raw data.


### Data Pre-processing
Huge raw data files of business and review data are both available in JSON file format. We have worked using Python to import one of the data files and review data due to its huge file size before we continue the manipulation of data in R. As the data is originally in JSON format, we have spent lots of effort cleaning and flattening the data columns. Some data columns require further cleanings, such as city names, food categories, and other attributes that contain inconsistent category strings The cleaning process requires extra effort to produce clean dataset before the development of the R shiny web application. 


### Source Code
The data manipulation script, user interface, server script of the web application, and miscellaneous working files can be found in this repository.


### Showcase of Web Application (RPresenter)
More information is available in the well-documented R Presenters slide in RPubs:\
https://rpubs.com/javinchew/713020


### RShiny Web Application
Here's the link to access "Yelp Eat":\
https://hwaiteng-teoh.shinyapps.io/Yelp-Eat-Travel-around-with-best-authentic-local-taste/

