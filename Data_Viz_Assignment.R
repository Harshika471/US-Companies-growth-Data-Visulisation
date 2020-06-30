install.packages("rmarkdown")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("GoodmanKruskal") #categorical correlation
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("plotly")
install.packages("dplyr")
library(plotly)
library(GGally)
library(ggcorrplot)
library(GoodmanKruskal)
library(naniar)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rmarkdown)
us_companies <- read.csv("US_Companies-2018.csv",stringsAsFactors = TRUE,na.strings = c(""," ","NA"))
us_companies
head(us_companies)
dim(us_companies)
View(us_companies)
# Before we take a decision to clean the data we better inspect it
#Dimension of dataset
dim(us_companies)
# The Given Dataset contains 5013 Observations(Rows) and 23 variables aka fratures(Column)
#Explore the dataset 
colnames(us_companies)
#Note : Surprisingly, each columns seems to be appear in a meaningful way. 
#Lets Explore the structure of the dataset
str(us_companies)
summary(us_companies)
#We will have a closer look at the data, for that reason I will use the Glimpse() function from dplyr
glimpse(us_companies)
# Notes 
#1. After looking at the structure of the dataset we can see that some column values appears as a 
#int(interger), Now the task is to deal with these values to make sure if they play any significant
#Role in our dataset or not???
#Remove Redudent information/Column
#Before Removing any rows and column from a dataset just have a look what all rows and columns present
#in our dataset?
ncol(us_companies)
nrow(us_companies)
#Notes:
 #_ - ifmid is for metro area - it matches up with the column called _ - metrocode and 
#_ - ifiid matches up with _ - industry Hence we can remove the redudent column from the dataset
#Moreover, Id Column is also seems to be not useful. Hence Removing the column is best choice !

#Remove Id, ifmid and ifiid column from the dataset
us_companies <- subset(us_companies, select = -c(1,3,5,6,12,20,22,23))
dim(us_companies)
colnames(us_companies)
#Step2: Look for the Missing values in the dataset
anyNA(us_companies)
n_miss(us_companies)
# n_miss(us_companies)
#[1] 131
prop_miss(us_companies)
any_na(NaN)
n_miss(NaN)
any_na(Inf)
n_complete(us_companies)
# Missing values summary
miss_var_summary(us_companies)
#Missing case summary
miss_case_summary(us_companies)
miss_case_table(us_companies)
#Visulize missing values
vis_miss(us_companies,cluster = TRUE)
gg_miss_upset(us_companies)
#Observation : As per the gg_miss_upset plot, we can see that founded has the most missing values.
#There are 5 cases where latitude, langitude, yrs_on_list, previous_workers and founded have missing
#Values together
# DATA CLEANING -- Dealing with missing values:
miss_scan_count()
us_companies%>%
  miss_scan_count(search = list("N/A","N/a","NA"))

#Explore the starnge missing values
miss_scan_count(data = us_companies, search  = " ")

#Print the Top us_companies data
head(us_companies)
#arrange by revenue
us_companies%>%arrange(revenue)%>%vis_miss()
#arrange by growth
us_companies%>%arrange(growth)%>%vis_miss()
#arrange by industry
us_companies%>%arrange(industry)%>%vis_miss()

#Note : In case if we have more than 5% missing data we can use shadow matrix and nabular forms to 
#visulise and explore missing values. In my case, I have less tha 1% missing value so I will remove 
#the NA Values.

#Remove NA values
complete.cases(us_companies)
#if output = False (NA Values)
which(complete.cases(us_companies))
desc(us_companies)

#Correlation Matrix with categorical data
#Refer link: https://www.r-bloggers.com/to-eat-or-not-to-eat-thats-the-question-measuring-the-association-between-categorical-variables/
categorical_var <- c("url","city","company","website","state_l","state_s","industry","metrocode","metro")
company_frame <- subset(us_companies,select = categorical_var)
gkmatrix <- GKtauDataframe(company_frame)
plot(gkmatrix, corrColors = "blue")
us_companies <- subset(us_companies , select = -c(19))
View(us_companies)
str(us_companies)
sum(is.na(us_companies))
#Imputation of missing values using classifire techniuqe(KNN)
# install.packages("DMwR")
# library(DMwR)
# knnoutput <- knnImputation(us_companies[,!names(us_companies) %in% "industry"])
# anyNA(knnoutput)
#Removing missing values
companies_data <- na.omit(us_companies)
sum(is.na(companies_data))
str(companies_data)
#Check the levels of categorical data
nlevels(companies_data$url) #5000
nlevels(companies_data$city) #1379
nlevels(companies_data$company) #5000
nlevels(companies_data$state_l) #52
nlevels(companies_data$industry) #26
nlevels(companies_data$metro) #72

#Check for each variable unique value
sapply(companies_data, function(x) length(unique(x)))
#Test the relation between Industry and URL using chi square test
#Significance level = 0.5
install.packages('MASS')
library(MASS)
tbl = table(companies_data$industry,companies_data$state_l)
tbl
chisq.test(tbl)
corr <- cor.test(x=companies_data$revenue, y=companies_data$growth, method = 'spearman')
corr
#NOte :sperman not worke d. In order to find the reation between two numerical variables we can perform anova
#test as well.
install.packages("treemapify")
library(treemapify)
ggplot(plotdata, 
       aes(fill = industry, 
           area = n, 
           label = industry)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Industries with label") +
  theme(legend.position = "none")


options(scipen=999) # Turn off scientific notation
str(companies_data$revenue)
#plot with two variables to see industry average revenue
plot_revenue <- ggplot(companies_data, aes(industry, revenue)) +
  geom_bar(stat = "summary", fun.y = "mean" , color="blue",fill="steelblue")   + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  ylab('Average revenue')
plot_revenue
#Filter the industries as per growth
plot_growth <- ggplot(companies_data, aes(industry, growth)) +
  geom_bar(stat = "summary", fun.y = "mean",fill="steelblue")  + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  ylab('Average Growth')
plot_growth

#Filter data based on industry, growth , workers
industry_select <- companies_data[companies_data$industry %in% c("Security", "Media", "Health"), ]
#tally(industry_select)
library(dplyr)
security <- industry_select %>% 
  filter(industry %in% c("Security")) %>% 
  select(company,revenue,growth,workers)
security
#Average Revenue of companies under security Industry
library(scales)
ggplot(security, 
       aes(x = company,
           y = revenue)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  labs(title = "Mean revenue by company ", 
       subtitle = "Revenue of companies under security industry",
       x = "",
       y = "")

#if revenue is higher of a company must be growth should be good and no of workers should be more ?
library(ggplot2)
library(dplyr)
revenue_sec <- security %>% 
  filter(company %in% c("Allied Universal","Convergint Technologies")) %>% 
  select(growth,workers,revenue,company)
revenue_sec

gg <- ggplot(revenue_sec, 
       aes(x = growth, 
           y = workers, 
           color = company ) )+
  geom_point(alpha = .6) +
  labs(title = "Security industry companies highest reveue")
gg

#Filter Companies in the Media Industry to w.r.t to workers revenue and growth
library(ggplot2)
library(dplyr)
industry_media <- industry_select %>% 
  filter(industry %in% c("Media")) %>% 
  select(company,growth,workers,revenue)
industry_media
#Average Revenue of companies under Media Industry
library(scales)
mm <- ggplot(industry_media, 
       aes(x = company,
           y = revenue)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  labs(title = "Mean revenue by company ", 
       subtitle = "Revenue of companies under media industry",
       x = "",
       y = "")

mm

library(ggplot2)
library(dplyr)
revenue_media <- industry_media %>% 
  filter(company %in% c("Endeavor","N2 Publishing")) %>% 
  select(growth,workers,revenue,company)
revenue_media

mm1 <- ggplot(revenue_media, 
             aes(x = growth, 
                 y = workers, 
                 color = company ) )+
  geom_point(alpha = .6) +
  labs(title = "Security industry companies highest reveue")
mm1


# WHAT NEXT : AsI have seen these three Industry have the highest growth in 2018, lets see how was the growth in
#last 10 years
#Read the CSV File
previous_years_data <- read.csv("Previous_10years.csv",stringsAsFactors = TRUE,na.strings = c(""," ","NA"))
View(previous_years_data)
sum(is.na(previous_years_data))
data_store <- na.omit(previous_years_data)
data_store
dim(data_store)
sum(is.na(data_store))

#Plot the Industry data w.r.t revenue
options(scipen=999)
rev_2 <- ggplot(data_store, aes(industry, revenue)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  ylab('Average revenue')
rev_2


#Filter the industries as per growth
options(scipen=999)
grow_2 <- ggplot(data_store, aes(industry, growth)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  ylab('Average Growth')
grow_2

str(data_store)


subset_industry_growth <- data_store[data_store$industry %in% c("Energy","Consumer Products & Services"),]
View(subset_industry_growth)

library(rbokeh)

data_grow <- subset_industry_growth %>% 
  figure() %>% 
  ly_lines(x = year, y = workers, data= subset_industry_growth, color = industry)
data_grow


#NOTE: AS per the plot, we can say from 2010 to 2013 workers were more in both industries as compare to the others
# Filter the data in 2012 for both the industry plot revenue vs workers

 # create a treemap with tile labels


#Companies associated with the above industries with growth and revenue
companies_data$growth <- as.numeric(companies_data$growth)
companies_data$revenue <- as.numeric(companies_data$revenue)
is.numeric(companies_data$growth)
is.numeric(companies_data$revenue)
install.packages("trelliscopejs")
library(trelliscopejs)
company_data <- select(companies_data, growth,industry,revenue,workers)

ggplot(company_data,aes(growth,revenue))+
  geom_point()+
  facet_trelliscope(~ revenue + workers , nrow = 2,ncol = 3, as_plotly = TRUE)

#Time series analysis ---- growth with time ---dygraphs
install.packages("dygraphs")
library(dygraphs)
#fix date using pipes
founded_time <- companies_data %>%
  mutate(founded = as.Date(founded, format = "%m/%d/%y"))

annual_precip <- ggplot(companies_data, aes(x = founded, y = revenue)) +
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  labs(x = "Time",
       y = "Revenue",
       title = "my title")

annual_precip

#make the above plot interative with dygrahs
format(companies_data$revenue, scientific=FALSE)
install.packages("xts")
library(xts)
comany_timeSeries <- xts(x = companies_data$revenue,
                            order.by = as.POSIXct(companies_data$founded))

# create a basic interactive element
interact_time <- dygraph(comany_timeSeries)
interact_time

# create a basic interactive element
interact_time2 <- dygraph(comany_timeSeries) %>% dyRangeSelector()
interact_time2

summary(companies_data$revenue)
max(companies_data$revenue)


#Time Series Analysis (The goal here is to use previous year data and predict the growth in next year which is 2018)

install.packages("caret")
library(forecast)
library(caret)

trainIndex <- createDataPartition(data_store$industry, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

pre_Train <- data_store[ trainIndex,]
pre_Test  <- data_store[-trainIndex,]
nrow(data_store)
nrow(pre_Train)
nrow(pre_Test)

data_ts <- ts(pre_Train[, 6], start = c(2008, 1), end = c(2017, 12), frequency = 12)

#lines 2 to 4
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
naive_mod <- naive(data_ts, h = 12)
summary(naive_mod)

pre_Test$naive = 4018
mape(pre_Test$growth, pre_Test$naive) 

# MAPE ERROR: 5.2% 



#extract industry workers and founded year using rbokeh
library(lubridate)
industry_data <- companies_data %>% 
  filter(industry %in% c("Software","Business Products & Services","Advertising & Marketing"))
industry_data$founded <- lubridate::ymd(industry_data$founded,truncated = 2L)

fig_col <- figure(data = industry_data,ylim = (1:2000)) %>% 
  #ly_lines(x = industry_data$founded, y = workers, color = industry,legend = "Bottom") %>% 
  ly_points(industry_data, color = ramp, size = seq_len(n))
  ly_points(x = founded, y = workers, color = industry,hover = c(founded,workers))
fig_col

colnames(companies_data)


#Geo data
install.packages("(leaflet")
#devtools::install_github("rstudio/leaflet")
library(leaflet)
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

leaflet(data = companies_data[1:200,]) %>% addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~as.character(industry), label = ~as.character(industry))



#NOTE: Below Code is just for reference( My Efforts) not to display any data.....

install.packages("shiny")
library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "revenue", min(companies_data$revenue), max(companies_data$revenue),
                            value = range(companies_data$revenue), step = 0.1
                ),
                selectInput("industry", "industry",
                            rownames(subset(companies_data, industry %in% c("Software", "Media")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    companies_data[companies_data$revenue >= input$range[1] & companies_data$revenue <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$industry, companies_data$revenue)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(companies_data) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude=37.0902), ~max(latitude=95.7129))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("us.map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^revenue/10, weight = 1, color = "#777777"
                 #fillColor = ~pal(revenue), fillOpacity = 0.7, popup = ~paste(revenue)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("us.map", data = companies_data)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~revenue
      )
    }
  })
}

shinyApp(ui, server)

install.packages("ggmap")
library(ggmap)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
#Set your API Key
ggmap::register_google(key = "AIzaSyDknfzmzJBX6gXy5ma8ujDBM8ANtQXwKlg")

##1) Create a map with all of the crime locations plotted.

p <- ggmap(get_googlemap(center = c(companies_data$longitude = -104.99763, companies_data$latitude = 39.75191)))
p + geom_point(aes(x = companies_data$longitude, companies_data$latitude = Latitude,  colour = Initial.Type.Group), data = companies_data, size = 0.5) + 
  theme(legend.position="bottom")
p


