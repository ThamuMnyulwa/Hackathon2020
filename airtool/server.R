# Author:  ----------------------------------------------------------------
# Description: server script-----------------------------------------
# Date: -------------------------------------------------------------------

shinyServer(function(input, output) {

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
  
  # # new column for the popup label
  # raw_data_1 <- mutate(raw_data, cntnt=paste0('<strong>Name: </strong>',Name,
  #                                                       '<br><strong>Merchant Id:</strong> ', Merchant_Id,
  #                                                       '<br><strong>Business Choice:</strong> ', Business_Choice,
  #                                                       '<br><strong>Address:</strong> ',Address,
  #                                                       '<br><strong>City:</strong> ',City,
  #                                                       '<br><strong>Area Code:</strong> ',Area_Code,
  #                                                       '<br><strong>Industry:</strong> ',Industry))
  
  # create a color paletter for category type in the data file
  
  #pal <- colorFactor(pal = c(palette = topo.colors(length(unique(merchant_data$Industry))), domain = unique(merchant_data$Industry)))
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
  
})