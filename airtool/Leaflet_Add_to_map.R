
library(rlist)
POI_icon <- readRDS("./data/POI_icon.rds")
Comp_Icon <- readRDS("./data/Comp_Icon.rds")
Trans_Icon <- readRDS("./data/Trans_Icon.rds")

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






