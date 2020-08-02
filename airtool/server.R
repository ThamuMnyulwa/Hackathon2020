# Author:  ----------------------------------------------------------------
# Description: server script-----------------------------------------
# Date: -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # NAVIGATION BUTTONS ------------------------------------------------------
  
  #basic button navigation
  observeEvent(input$visualisations.button, {
    updateTabItems(session, "sidebartabs", "visualisations")
  })
  
  observeEvent(input$maps.button, {
    updateTabItems(session, "sidebartabs", "maps")
  })
  
  observeEvent(input$predictions.button, {
    updateTabItems(session, "sidebartabs", "predictions")
  })
  
  # SELECT PAGE -------------------------------------------------------------
  
  # # new column for the popup label
  merchant_data_1 <- mutate(merchant_data, cntnt=paste0('<strong>Name: </strong>',Name,
                                          '<br><strong>Merchant Id:</strong> ', Merchant_Id,
                                          '<br><strong>Business Choice:</strong> ', Business_Choice,
                                          '<br><strong>Address:</strong> ',Address,
                                          '<br><strong>City:</strong> ',City,
                                          '<br><strong>Area Code:</strong> ',Area_Code,
                                          '<br><strong>Industry:</strong> ',Industry))
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(topo.colors(length(unique(merchant_data$Industry))), unique(merchant_data$Industry))
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(merchant_data_1) %>% 
      addProviderTiles("CartoDB") %>% 
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = merchant_data_1, lat =  ~Lat, lng =~Long, 
                       radius = 7, popup = ~as.character(cntnt), 
                       color = ~pal(Industry),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=merchant_data$Industry,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data
  output$merchant_datatable <-DT::renderDataTable(datatable(merchant_data))
  
  pal <- colorFactor(topo.colors(length(unique(merchant_data$Industry))), unique(merchant_data$Industry))
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(merchant_data_1) %>% 
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = merchant_data_1, lat =  ~Lat, lng =~Long, 
                       radius = 7, popup = ~as.character(cntnt), 
                       color = ~pal(Industry),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=merchant_data$Industry,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data
  output$merchant_datatable <-DT::renderDataTable(datatable(merchant_data))
  

  # ------------------------------------------------------------------------
  filtered_data <- eventReactive(input$filter_data, {
    data <- processed_data %>%
      subset((Merchant_Id %in% c(input$merchants)) &
               (Town %in% c(input$towns)) &
               (Avg_Income_3M %in% c(input$avg_income)) &
               (industry %in% c(input$industry)) &
               (Age_Band %in% c(input$age_band)) &
               (Gender_code %in% c(input$gender)) &
               (capitec_client %in% c(input$cap_client))
      ) %>%
      filter(
        between(date,
                input$transaction_date[1],
                input$transaction_date[2]) &
                (Amount >= input$range[1]) &
                (Amount <= input$range[2])
      )
  })
  
  output$ind_plots <- renderUI({
    tagList(
      stateSelectInput("industry_plots", "Select Industry for Plots", levels(filtered_data()$industry), levels(filtered_data()$industry_plots), FALSE)
    )
  })
  
  # Plots -------------------------------------------------------------------
  
  output$boxplot <- renderPlot({
    ggplot(data=filtered_data()) +
      aes(x = weekday, y = Amount, fill = industry) +
      geom_boxplot() +
      scale_fill_hue() +
      theme_minimal()
  })  
  
  output$stackedbar <- renderPlot({
    ggplot(data=filtered_data()) +
      aes(x = time_of_day, fill = industry, colour = industry) +
      geom_bar() +
      scale_fill_brewer(palette = "RdYlBu") +
      scale_color_brewer(palette = "RdYlBu") +
      labs(x = "Time of day (Hour)", y = "Transactions per hour", title = "Spending habits", subtitle = "Plot counting transactions in data set aganist the time of day") +
      theme_minimal()
  })  
  
  output$total_transactions <- renderPlot({
    ggplot(data= filtered_data() %>% filter(industry == input$industry_plots)) +
    aes(x = Tran_Date) +
    geom_histogram(bins = 30L, fill = "#0c4c8a") +
    labs(x = "Time", y = "Transactions", title = "Time plot of daily transactions frequency", subtitle = "Frequency of transactions") +
    theme_minimal()
  })
  
  output$daily_amount <- renderPlot({
    ggplot(data= filtered_data() %>% filter(industry == input$industry_plots)) +
    aes(x = Tran_Date, y = Amount) +
    geom_line(size = 1L, colour = "#0c4c8a") +
    labs(x = "Time (Daily)", y = "Amount (ZAR)", title = "Time plot of Daily ammount", subtitle = "Daily profit made over time") +
    theme_minimal()
  })
  
  output$gender_amount <- renderPlot({
    ggplot(data= filtered_data() %>% filter(industry == input$industry_plots)) +
      aes(x = Gender_code, weight = Amount) +
      geom_bar(fill = "#0c4c8a") +
      labs(x = "Gender", y = "Ammount (ZAR)", title = "Client Profit vs Gender", subtitle = "Aggregate Profit per gender over time period") +
      theme_minimal()
  })
  
  
  output$complete_industry_spending <- renderPlot({
    ggplot(data = processed_data) +
    aes(x = ordered(processed_data$weekday,
                    levels=c("Sun","Mon", "Tue", "Wed", "Thu","Fri", "Sat"))
        , fill = industry, colour = industry, weight = Amount) + 
    geom_bar() +
    scale_fill_hue() +
    scale_color_hue() +
    labs(x = "Time of day", y = "Number of Transactions", title = "Industry Spending ", subtitle = "Plot counting transactions in data set aganist the time of day") +
    theme_minimal() +
    facet_wrap(vars(industry))
  })
  
  output$complete_three_trans<- renderPlot({
    ggplot(processed_data) +
    aes(x = Avg_Income_3M, fill = industry, colour = industry, group = industry) +
    geom_bar() +
    scale_fill_viridis_d(option = "inferno") +
    scale_color_viridis_d(option = "inferno") +
    labs(x = "Average Income in 3 month period", y = "Frequency", title = "Frequency of transactions within each industry ", subtitle = "Complete data over the 3 month") +
    theme_minimal() +
    facet_wrap(vars(industry))
  })
  
  output$density_industry<- renderPlot({
    ggplot(processed_data %>% filter(Amount <= 1000)) +
    geom_density(aes(x=Amount, color = industry,fill=industry), alpha=0.8, show.legend = TRUE)+
    labs(x="Amount (ZAR)", y = "Density" ,subtitle = "Plot ") +
    scale_fill_brewer(palette = "RdYlBu") +
    scale_color_brewer(palette = "RdYlBu") +
    ggtitle("Density of amount between industry") +
    theme_minimal()
  })
  
  output$capitec_clients<- renderPlot({
    ggplot(processed_data) +
    aes(x = capitec_client, fill = industry, colour = industry, group = industry) +
    geom_bar() +
    scale_fill_brewer(palette = "RdBu") +
    scale_color_brewer(palette = "RdBu") +
    labs(x = "Capitec client transactions", y = "Number of transactions", title = "Capitec clients at Capitec clients", subtitle = "By location how many capitec clients go to capitec merchants over 3 month period") +
    theme_minimal() +
    facet_wrap(vars(industry))
})
  
  # create the leaflet map  
  output$merchant_leaf_map <- renderLeaflet({
    leaflet(merchant_data)%>%
      addProviderTiles(providers$OpenStreetMap)%>%
      addCircles(lng = merchant_data$Long,
                 lat = merchant_data$Lat,
                 popup = merchant_data$Industry) %>%
      addMarkers(data=competitor_data,lng = ~Long,
                 lat = ~Lat , icon=Comp_Icon ) %>%
      addMarkers(data=placeofinterest_data,lng = ~Long,
                 lat = ~Lat , icon=POI_icon) %>%
      addMarkers(data=transport_data,lng = ~Long,
                 lat = ~Lat , icon=Trans_Icon)
    
  })
  
  
  
}