
library(ggplot2)
library(tidyverse)
library(stringr)
library(tidytext)
library(dplyr)
library(httr)
library(geojsonio)
library(sf)
library(leaflet)
library(shiny)
library(leaflet.extras)

# This R file contains code to create the app "Visualizing the '08 Housing Burst and its Consumption Implications", 
# which is available here: https://82ofzv-tai-nguyen.shinyapps.io/FinalProject/


#*******************************************************************************#

#setwd("/Users/tvnguyen/Desktop/MATH0216 Data Science/FinalProject")
store.data <- read_csv("math118_yr_store_sales.csv")
cty.retail.data <- read_csv("math118_yr_cty_sales.csv")
hp.data <- read_csv("math118_yr_hpi.csv")
cbsa.lng.lat <- read.delim("2021_Gaz_cbsa_national.txt")
counties <- geojson_read("gz_2010_us_050_00_20m.json", 
                         what = "sp")

## Manipulate aggregated retail data ##

store.data2 <- cty.retail.data %>%
    mutate(str_fips = as.character(County_Key)) %>%
    filter(State_Name != "Maryland*") %>%
    filter(State_Name != "Virginia*") %>%
    mutate(str_fips = str_pad(str_fips, width = 5, side = "left", pad = "0")) %>%
    mutate(STATE = str_sub(str_fips,1,2)) %>%
    mutate(COUNTY = str_sub(str_fips,3,5)) %>%
    left_join(cbsa.lng.lat, by = c("cbsa_code" = "GEOID"))

## SHINY APP ##

#*******************#
#  USER INTERFACE   #
#*******************#
ui <- navbarPage(titlePanel("Visualizing the '08 Housing Burst and its Consumption Implications"),
                 navlistPanel(
                #First tab
                tabPanel("Introduction", 
                          htmlOutput("intro")),
                #Second tab
                tabPanel("Store Distribution in the IRI Data Set",
                          leafletOutput(outputId = "store.map")),
                #Third tab
                tabPanel("Local Retail Sales and House Price Index",
                         htmlOutput("question1"),
                         selectInput(inputId = "statename",
                                    label = "Choose a State",
                                    choices = unique(store.data2$State_Name)), 
                        uiOutput(outputId = "cbsaname"),
                        sliderInput(inputId = "year",
                                   label = "Choose a year",
                                   min = 2004,
                                   max = 2011,
                                   value = 2008),
                        htmlOutput("ti.hp.map"),
                        leafletOutput(outputId = "hp.map"),
                        htmlOutput("ti.store.sales.map"),
                        leafletOutput(outputId = "store.sales.map")
                        ),
                #Fourth tab
                tabPanel("Percent Decline in Local Retail Sales and House Price over the 2008 Recession",
                         htmlOutput("question2"),
                         selectInput(inputId = "statename2",
                                     label = "Choose a State",
                                     choices = unique(store.data2$State_Name)), 
                         uiOutput(outputId = "cbsaname2"),
                         htmlOutput("ti.plot"),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                              plotOutput("plot"), 
                                              plotOutput("plot2"))),
                         htmlOutput("ti.hp.map2"),
                         leafletOutput(outputId = "hp.map2"),
                         htmlOutput("question3"),
                         htmlOutput("ti.store.sales.map2"),
                         leafletOutput(outputId = "store.sales.map2")
                         )
                 )     
)

#********#
# SERVER #
#********#

server <- function(input, output, session) {
    
    # FIRST TAB #
    output$intro <- renderUI ({
        HTML(paste( " ","During the 2008 Great Recession, all components of consumption expenditures dropped substantially, 
        and the leading explanation for these aggregate dynamics is the striking decline in housing net worth. In other words,
        the subtantial decline in house price over the course of the Great Recession may have driven grocery consumption in many local
        markets. In addition, there was significant variation in the initial house price level at the onset of the recession
        (2007) and in the magnitude of house price decline across different geographic units in the US between 2007-2011. Therefore, it is helpful to first
        understand the cross-sectional variation in housing prices and retail sales and second the change in house prices and 
        retail sales across time. I attempt to visualize both the geographical varition in house price levels and in 
        house price changes over the 2008 recession. There are three questions I want to pursue in this project:"," ",
        "1. Is there variation in house prices across geographic units in the U.S at the onset of the Great Recession?", " ",
        "2. Is there variation in the decline of house prices across geographic in the U.S over the recession?", " ",
        "3. And is there a correlation between the decline in local house price and local retail sales?", " ",
        "<B> Methodology </B>",
        "<B> I. Retail Data </B>",
        "The first data set that I use in this project is the IRI
        Marketing Data for years between 2004 and 2011. This data set is used as my primary measure of retail sales because
        it includes many chains and has a large geographic coverage. In fact, it contains data on price and quantity purchased for 
        products in 31 categories from stores in 47 retail chains in 50 geographic markets in the U.S. In total, the data set covers approximately
        7200 stores in over 2400 zip codes. The second tab of this app illustrates the distribution of these stores across the 
        country. I construct the measure of retail sales at the county-level by aggregating hundreds of millions of observations of scanner 
        data on transactions that took place in a county into annual average data. Tab 3 of this shiny app illustrates the total 
        retail sales in each county in each Core-based Statistical Area (city). Graph 1 in Tab 4 also tracks the development in log total 
        retail sales in a CBSA over time. In computing the percent decline in local retail sales in a country between 2008-2011, I take the log difference
        between the 2011 total retail sales and 2008 retail sales."," ",
        "<B> II. House Price Data </B>",
        "The primary data on house price is obtained from Zillow, Inc. The Zillow home value index is computed using Zillow’s “Zestimate”, an estimate of the 
        market value of a property, which is in turn computed using a propriety formula and different data sources. The data sources include property appraisals, 
        location, market condition, and transaction data for properties in the same geographic area. Properties are represented in terms of attributes, and the local value of 
        each attribute is computed from transaction data. The local value of each attribute is then used to calculate the total local value of a 
        property. The Zillow home value index is the median of the Zestimates for properties available within a given ZIP code. Having obtained raw 
        monthly house price data, I then compute yearly CBSA- or country- level averages."
        ," ",
        "<B> Structure of the App </B>",
        "The app is structured as follows:", 
        "Tab 2: Distribution of Stores in the IRI Data Sets",
        "Tab 3: Local (CBSA) House Price Index and Total Retail Sales across the U.S.",
        "Tab 4: The percent decline in Total Retail Sales 2008-2011 and the percent decline in House Price Index 2007-2011
        in local markets.", " ",
        "<B> Limitations </B>",
        "This visualization of variation in local house prices and total retail sales only demonstrates the correlation, if there is any,
        between the decline in house prices and retail sales at the local level. This is because local house prices and consumption are 
        simultaneously determined. For instance, an increase in house price, all things being equal, results in an increase in household wealth,
        which, theoretically, encourages consumption through direct wealth effects. However, an increase in local retail sales also leads to 
        increased local employment, for instance, which will have a positive effect on household wealth. In addition, there may be other confounding factors that are correlated with housing prices and local retail sales. For instance, 
        the industrial compostion of a CBSA affects the construction sector and hence house prices. At the same time, it affects wages and employment
        and hence retail sales."," ",
        "In order to measure the causal effect of changes in house prices on grocery consumption, one needs to come up with a solid econometric 
        identification strategy that isolate exogenous shocks to house prices that are not caused by local domestic macroeconomic conditions. For example,
        one possible extension is instrumenting the change in house prices with a Housing Supply Elasticity derived from the (un)availability
        of land in a CBSA. This method will isolate the component of the change in house price that is due to factors unrelated to macroeconomic
        conditions.", " ", " ", sep ="<br>"))
        
    })
    
    # SECOND TAB #
    
    #create a map of all stores in the data set
    output$store.map <- renderLeaflet({
        store.data1 <- store.data %>%
        group_by(IRI_KEY) %>%
        mutate(id = row_number()) %>%
        filter(id ==1)
    
    make.map.store <- function (data.file) {
        data.file %>%
            leaflet() %>%
            addTiles() %>%
            addMarkers(lng = ~Zip_lon,
                       lat = ~Zip_lat,
                       clusterOptions = markerClusterOptions())
    }
    
    store.map <- make.map.store(store.data1)
    })
    
    
    # THIRD TAB #
    output$question1 <- renderUI({
        HTML(paste(" ", "<B> 1. Is there variation in house prices across geographic units in the U.S at the onset of the Great Recession? </B>", " ",
                   "In answering this question, let's consider the cases of the San Francisco - Oakland - Hayward Metropolitan Area in CA and
                   the Kansas City Metropolitan Area in MO-KS. In 2007, the house price index in Jackson County, Kansas City was 128,795, while it was 909,170 in San Francisco
                   County, San Francisco. The primary reason for this disparity can certainly be explained by the high demand for housing in San 
                   Francisco. As more people come to San Francisco to work, the increased demand for housing will inevitably raise house prices. However,
                   another important factor that makes housing in San Francisco and other coastal cities so expensive is the lack of land. Whenever there
                   is an increase in housing demand, new housing cannot be built to match up with the increase in demand due to the topology of the city, which is
                   engulfed by the ocean. Therefore, the housing supply elasticity is so much lower in San Francisco than in Kansas city. This wedge in
                   demand and supply pushed up the house prices in metropolitan areas where land is scarce. In addition there are  differences in legislation and local market dynamics that contribute to this variation in 
                   housing prices.", " ",
                   "In sum, there is heterogeneity in housing demand, housing supply elasticity, and legislation across different geographic units, which together
                   may have resulted in variation in house prices in the U.S. at the onset of the Great Recession. Using the tool below, you can discover this
                   heterogeneity in house price index across the country. You can, for instance, pay attention to how the topopology of a city determines its 
                   house price.", " ", " ", sep = "<br>"))
    })
    
    # create a drop box 
    output$cbsaname <- renderUI({
        selectInput("inputId"="cbsaname", 
                    label = "Choose a CBSA (city)", 
                   choices = pull(unique(na.omit(store.data2[store.data2$State_Name==input$statename,
                                                             "cbsa_title"]))))
    })
    
    # map title
    output$ti.store.sales.map <- renderUI ({
        HTML("<B> Total Local (County) Retail Sales by Year. </B>")
    
    })
    
    #create a map for retail sales
    output$store.sales.map <- renderLeaflet({
            if (is.null(input$year) | is.null(input$cbsaname)) {
                return()
            } 
            counties.copy <- counties
            
            #retail data 
            store.data3 <- store.data2 %>%
                filter(cbsa_title == input$cbsaname) %>%
                filter(year == input$year)
            
            #join retail data with geographic data structures
            counties.copy@data <- left_join(counties.copy@data, store.data3, by = c("STATE", "COUNTY"))

            color.scheme <- colorBin(palette = "YlGnBu",
                                     domain = counties.copy@data$cty_yr_all_pch,
                                     bins = c(0, 5000000, 10000000, 15000000, 20000000, 25000000, 30000000, 35000000, 40000000, 45000000, 50000000, Inf),
                                     na.color="transparent")
            
            #markers
            mytext <- paste(
                "Local Store Sales ($): ", counties.copy@data$cty_yr_all_pch,"<br/>",
                "County: ", counties.copy@data$County_Name, "<br/>",
                "Metro: ", counties.copy@data$cbsa_title, "<br/>",
                "State: ", counties.copy@data$State_Name, "<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            #make a map
            counties.copy %>%
                leaflet() %>%
                addTiles() %>%
                addPolygons(stroke = TRUE,
                            color = "black",
                            weight = 0.5,
                            smoothFactor = 0.3,
                            fillColor = ~color.scheme(cty_yr_all_pch),
                            fillOpacity = 0.7,
                            label = mytext,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "13px",
                                direction = "auto"
                            )) %>%
                addLegend(pal = color.scheme,
                          values = counties.copy@data$cty_yr_all_pch,
                          title = "Local Store Sales") %>%
                setView(lat = mean(store.data3$INTPTLAT), lng = mean(store.data3$INTPTLONG), 8) %>%
                addFullscreenControl()
    })
    
    #create a map title
    output$ti.hp.map <- renderUI ({
        HTML("<B> Local (County) House Price Index by Year. </B>")
        
    })
    
    #create a map for house price index
    output$hp.map <- renderLeaflet({
        if (is.null(input$year) | is.null(input$cbsaname)) {
            return()
        }           
        counties.copy <- counties
        
        #retail data 
        store.data4 <- store.data2 %>%
            filter(cbsa_title == input$cbsaname) %>%
            filter(year == input$year)  %>%
            left_join(hp.data, by = c("County_Key", "year"))

        #join retail data with geographic data structures
        counties.copy@data <- left_join(counties.copy@data, store.data4, by = c("STATE", "COUNTY"))

        color.scheme2 <- colorBin(palette = "YlOrRd",
                                 domain = counties.copy@data$yr_index,
                                 bins = c(0, 150000, 300000, 450000, 600000, 750000, 900000, Inf),
                                 na.color="transparent")
        #markers
        mytext <- paste(
            "House Price Index: ", counties.copy@data$yr_index,"<br/>",
            "County: ", counties.copy@data$County_Name, "<br/>",
            "Metro: ", counties.copy@data$cbsa_title, "<br/>",
            "State: ", counties.copy@data$State_Name, "<br/>",
            sep="") %>%
            lapply(htmltools::HTML)
        
        #create the map for house price index
        counties.copy %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(stroke = TRUE,
                        color = "black",
                        weight = 0.5,
                        smoothFactor = 0.3,
                        fillColor = ~color.scheme2(yr_index),
                        fillOpacity = 0.7,
                        label = mytext,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "13px",
                            direction = "auto"
                        )) %>%
            addLegend(pal = color.scheme2,
                      values = counties.copy@data$yr_index,
                      title = "House Price Index") %>%
            setView(lat = mean(store.data4$INTPTLAT), lng = mean(store.data4$INTPTLONG), 8) %>%
            addFullscreenControl()

    })
    
    # FOURTH TAB #
    output$question2 <- renderUI({
        HTML(paste(" ", "<B> 2. Is there variation in the decline of house prices across geographic in the U.S over the recession? </B>", " ",
                   "In answering this question, let's consider the cases of Bloomington City in IL and
                   the Chicago-Naperville-Elgin Metropolitan Area in IL. Between 2007 and 2011, house price in Cook County in Chicago declined by 42.5%.
                   House prices in other surrounding counties also dropped precipitously by more than 30%. However, between the same years, house price
                   in Bloomington dropped by only 5%.", " ", 
                   "What, then, can explain this variation in the drop of house price across cities? Starting with the demand side, the burst
                   of the housing market put a halt to housing speculation, lowering demand for houses and house prices. Insofar as the housing bubble 
                   in Chicago was more severe than in Bloomnington, IL, house price in Chicago was more inflated in the first place and hence experienced
                   a sharper decline when the bubble burst. Differential unemployment may also explain the difference in
                   decline in house prices in different cities.", " ", 
                   "On the supply side, we can tie this back to the differential housing supply elasticity mentioned earlier. Chicago is
                   located right next to Lake Michigan, and therefore, there is a lack of land for new housing construction. As a consequence, the housing supply 
                   is so much more inelastic than that of Bloomington, making house price so much more volatile in Chicago. In good times, when demand for housing is high, 
                   house price surges at a much faster rate. However, in bad times, house price in Chicago might decline as substantially as it picks up.", " ",
                   "In sum, the heterogeneity in housing demand, housing supply elasticity, and legislation across different geographic units
                   again may have resulted in variation in the drop of house prices in the U.S. over the Great Recession. Using the tool below, you can discover this
                   heterogeneity in the decline in house price index across the country.", " ", " ", sep = "<br>"))
    })
    
    output$question3 <- renderUI({
        HTML(paste(" ", "<B> 3. Is there a correlation between the decline in local house price and local retail sales? </B>", " ",
                   "There seems to be some correlation between the decline in local house prices and local retail sales, which is evidenced across the country. 
                   On the west coast, Clark and Clackamas counties in Portland, OR experienced
                   the most dramatic declines in local house prices between 2007 and 2011 (39.4% and 32.7%). These counties also saw the most
                   precipitous declines in local retail sales between 2008-2011, 46.1% and 63.9% respectively.", " ",
                   "Coastal cities in the west are not the only metro areas in which this correlation between a decline in house price
                   and a drop in local retail sales emerged. Cities such as Salt Lake City in Utah
                   also saw a 24.8% decrease in house price and a 33.1% in local retail sales over the recession. In the midwest, Oakland
                   County in the Detroit-Warren-Dearborn Metropolitan Area witnessed an almost 42% drop in price. Retail sales here also
                   dropped by 26.1%. On the east coast, there was a 30% decline in house price and a whopping 81% decline in retail consumption 
                   in Kent County in Providence, RI.", " ",
                   "In sum, evidence of the correlation between the decline in house prices and the decline in retail sales is widespread from the West to East 
                   Coast. However, as mentioned in limitations, it is a nontrivial task to measure the causal link. Using the tool above, you can discover this
                   correlation between house price and grocery consumption in many cities across the country. ", " ", " ", sep = "<br>"))
    })
    
    #create a drop box
    output$cbsaname2 <- renderUI({
        selectInput("inputId"="cbsaname2", 
                    label = "Choose a CBSA (city)", 
                    choices = pull(unique(na.omit(store.data2[store.data2$State_Name==input$statename2, "cbsa_title"]))))
    })
    
    #create a map title
    output$ti.store.sales.map2 <- renderUI ({
        HTML("<B> Percent Decline in Local (County) Retail Sales between 2008-2011. </B>")

    })
    
    #create a map for the change in retail sales
    output$store.sales.map2 <- renderLeaflet({
        if (is.null(input$year) | is.null(input$cbsaname2)) {
            return()
        }  
        
        counties.copy <- counties
            
        #retail data 
        store.data5 <- store.data2 %>%
                group_by(County_Key) %>%
                mutate(id = row_number()) %>%
                filter(id ==1) %>%
                #filter(cbsa_title == "San Francisco-Oakland-Hayward, CA") %>%
                filter(cbsa_title == input$cbsaname2) %>%
                mutate(change_sales = change_sales*(-100))
            
        #join retail data with geographic data structures
        counties.copy@data <- left_join(counties.copy@data, store.data5, by = c("STATE", "COUNTY"))
            
        color.scheme3 <- colorBin(palette = "YlGnBu",
                                     domain = counties.copy@data$change_sales,
                                     bins = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 55, 60, Inf),
                                     na.color="transparent")
        
        #markers
        mytext <- paste(
            "Percent Decline in Local Store Sales 08-11 (%): ", counties.copy@data$change_sales,"<br/>",
            "County: ", counties.copy@data$County_Name, "<br/>",
            "Metro: ", counties.copy@data$cbsa_title, "<br/>",
            "State: ", counties.copy@data$State_Name, "<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
        #make the map
        counties.copy %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(stroke = TRUE,
                        color = "black",
                        weight = 0.5,
                        smoothFactor = 0.3,
                        fillColor = ~color.scheme3(change_sales),
                        fillOpacity = 0.7,
                        label = mytext,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "13px",
                            direction = "auto"
                        )) %>%
            addLegend(pal = color.scheme3,
                      values = counties.copy@data$change_sales,
                      title = paste("Percent Decline in", "<br/>",
                                    "Local Retail Sales 08-11")) %>%
                setView(lat = mean(store.data5$INTPTLAT), lng = mean(store.data5$INTPTLONG), 8) %>%
                addFullscreenControl()
        })
        
        #create a map title
        output$ti.hp.map2 <- renderUI ({
            HTML("<B> Percent Decline in Local (County) House Price Index between 2007-2011. </B>")

        })
        
        #create a map for the change in house price index
        output$hp.map2 <- renderLeaflet({
            if (is.null(input$year) | is.null(input$cbsaname2)) {
                return()
            }  
 
            counties.copy <- counties
            
            store.data6 <- store.data2 %>%
                group_by(County_Key) %>%
                mutate(id = row_number()) %>%
                filter(id ==1) %>%
                filter(cbsa_title == input$cbsaname2) %>%
                left_join(hp.data, by = c("County_Key", "year")) %>%
                mutate(change_hp = change_hp*(-100))
                
            
            counties.copy@data <- left_join(counties.copy@data, store.data6, by = c("STATE", "COUNTY"))
            
            color.scheme4 <- colorBin(palette = "YlOrRd",
                                      domain = counties.copy@data$change_hp,
                                      bins = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 55, 60, Inf),
                                      na.color="transparent")
            
            mytext <- paste(
                "Percent Decline in House Price Index 07-11(%): ", counties.copy@data$change_hp,"<br/>",
                "County: ", counties.copy@data$County_Name, "<br/>",
                "Metro: ", counties.copy@data$cbsa_title, "<br/>",
                "State: ", counties.copy@data$State_Name, "<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            counties.copy %>%
                leaflet() %>%
                addTiles() %>%
                addPolygons(stroke = TRUE,
                            color = "black",
                            weight = 0.5,
                            smoothFactor = 0.3,
                            fillColor = ~color.scheme4(change_hp),
                            fillOpacity = 0.7,
                            label = mytext,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "13px",
                                direction = "auto"
                            )) %>%
                addLegend(pal = color.scheme4,
                          values = counties.copy@data$change_hp,
                          title = paste("Percent Decline in", "<br/>",
                                        "House Price Index 07-11")) %>%
                setView(lat = mean(store.data6$INTPTLAT), lng = mean(store.data6$INTPTLONG), 8) %>%
                addFullscreenControl()
            
        })
        
        #create a graph title
        output$ti.plot <- renderUI ({
            HTML("<B> Change in Local (CBSA) Totral Retail Sales and House Price Index. </B> If no data is on display, there is
                 no data available for that CBSA.")

        })
        
        #create two plots for the changes in house price indices and retail sales
        output$plot <- renderPlot({
            if (is.null(input$year) | is.null(input$cbsaname2)) {
                return()
            } 
            
            store.data2 %>%
                filter(cbsa_title == input$cbsaname2) %>%
                left_join(hp.data, by = c("County_Key", "year")) %>%
                group_by(cbsa_code, year) %>%
                na.omit() %>%
                summarise(cbsa_yr_index = mean(yr_index), cbsa_tot_pch=sum(cty_yr_all_pch)) %>%
                mutate(log_cbsa_tot_pch = log(cbsa_tot_pch)) %>%
                ggplot() +
                geom_line(aes(x=year, y=log_cbsa_tot_pch), color = "purple", size = 2) +
                theme_bw() +
                ylab('CBSA Log Total Retail Sales') +
                xlab('Year')
        })
        
        output$plot2 <- renderPlot({
            if (is.null(input$year) | is.null(input$cbsaname2)) {
                return()
            } 
            
             store.data2 %>%
                filter(cbsa_title == input$cbsaname2) %>%
                left_join(hp.data, by = c("County_Key", "year")) %>%
                group_by(cbsa_code, year) %>%
                na.omit() %>%
                summarise(cbsa_yr_index = mean(yr_index), cbsa_tot_pch=sum(cty_yr_all_pch)) %>%
                mutate(log_cbsa_yr_index = log(cbsa_yr_index)) %>%
                ggplot() +
                geom_line(aes(x=year, y=log_cbsa_yr_index), color = "orange", size = 2) +
                theme_bw() +
                ylab('CBSA Log Average House Price Index') +
                xlab('Year')
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
