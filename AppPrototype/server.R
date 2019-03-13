library(shiny)
library(DT)
library(rgdal)
library(devtools)

#devtools::install_github('bhaskarvk/leaflet.extras')
#library(leaflet.extras)

# setwd("Z:/DDS/DDS30000 Research Capacity/Data Science/R_Shiny")

# on Linux/apple may need r-cran-xml, libcurl3-gnutls-dev
# e.g. sudo apt-get install r-cran-xml


# UPGRADE PLAN:
# https://github.com/seanangio/in_household
# various resources for improved plotting and interface.




# Preprocessing and data loading
#-------------------

# geo processing
RailData <- read.csv(file="./Data/ORR_WelshStationData.csv", header = TRUE, sep=",", fileEncoding="UTF-8-BOM")
RailData_geo <- RailData
coordinates(RailData_geo) <- ~OS.Grid.Easting+OS.Grid.Northing
proj4string(RailData_geo) <- CRS("+init=epsg:27700")
proj4string_map <- "+proj=utm +zone=30 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"#define proj4string to contain constants
RailData_geo <-  sp::spTransform(RailData_geo, CRS(proj4string_map))
#as.data.frame(RailData)
sputm <- SpatialPoints(RailData_geo, proj4string=CRS("+proj=utm +zone=30 +datum=WGS84"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))



# renaming cols:
names(RailData)[names(RailData)=="NLC"] <- "Station-ID"
names(RailData)[names(RailData)=="Station.Name"] <- "Station Name"
names(RailData)[names(RailData)=="Local.Authority"] <- "Local Authority"
names(RailData)[names(RailData)=="X1718.Entries...Exits"] <- "Attendance 2017-2018"
names(RailData)[names(RailData)=="X1617.Entries...Exits"] <- "Attendance 2016-2017"





# dropping columns we definitely don't need:
RailDataReduced <- subset(RailData, select=-c(TLC, OS.Grid.Easting, OS.Grid.Northing,  Region,Constituency, Entries...Exits_Full, Entries...Exits_Reduced, Entries...Exits_Season ))

#processing for each separate use case:

data_sort17 <- RailDataReduced[order(-RailDataReduced$"Attendance 2017-2018"),]

data_plot17 <- data_sort17[1:20,]
data_plot17$ID <- seq.int(nrow(data_plot17))
data_plot17 <- data_plot17[,c(ncol(data_plot17),1:(ncol(data_plot17)-1))]


data_sort16 <- RailDataReduced[order(-RailDataReduced$"Attendance 2016-2017"),]

data_plot16 <- data_sort16[1:20,]
data_plot16$ID <- seq.int(nrow(data_plot16))
data_plot16 <- data_plot16[,c(ncol(data_plot16),1:(ncol(data_plot16)-1))]


# Anything which depends on user input must go inside the function below:
#-------------------

server <- function(input, output) {
  
  
  # datatoplot <- reactive({
  # 
  #          if( input$Year == '2017-2018'){
  #            datatoplot <- data_plot17
  # 
  #          }
  #          else {
  #            datatoplot <- data_plot16
  # 
  #          }
  #   datatoplot
  # })
  
  
  output$mytable = DT::renderDataTable({
    
    if( input$Year == '2017-2018'){
      datatoplot <- data_plot17
      
    }
    else {
      datatoplot <- data_plot16
      
    }
    datatoplot
    
  }, rownames = FALSE)
  
  
  
  # This duplication will have to go in the final version!
  output$myleaflet = renderLeaflet({

    leaflet(options= leafletOptions(minZoom=7)) %>%
      addProviderTiles("CartoDB.PositronNoLabels", group = "Base") %>%
      addProviderTiles("OpenRailwayMap", group="Rail", options = providerTileOptions(opacity=0.5)) %>%
      hideGroup("Rail") %>%
      addCircles(data=spgeo, radius=15*(RailData_geo$X1718.Entries...Exits)^(1/4), col="red", popup= paste0("<strong> STATION:  " , RailData_geo$Station.Name, " </strong> <br><hr> <b> Local Authority: </b> <br>", RailData_geo$Local.Authority,"  <br> <b> Attendance in 2017/2018: </b> <br>",  RailData_geo$X1718.Entries...Exits)) %>%
      setView(lat=52.45521438, lng=-3.54943496, zoom=7) %>%
      setMaxBounds(-5.78028849,51.135868, -2.5482161836, 53.66259178) %>%
      addLayersControl(baseGroups = "Base",
                       overlayGroups = "Rail",
                       options = layersControlOptions(collapsed = FALSE))     
    #addSearchOSM() %>% 
    #addResetMapButton()
  
  
  
      #addSearchOSM() %>%
      # addReverseSearchOSM() %>%
      #addResetMapButton()
    
    
    # later investigate:
    # addCircleMarkers(data=spgeo, radius=(RailData_geo$X1718.Entries...Exits)^(1/3)/10, col="red", popup= paste0("<strong> STATION:  " , RailData_geo$Station.Name, " </strong> <br><hr> <b> Local Authority: </b> <br>", RailData_geo$Local.Authority,"  <br> <b> Attendance in 2017/2018: </b> <br>",  RailData_geo$X1718.Entries...Exits)) %>%
    
    
  })  
  
  
  
  #   if( input$Year == '2017-2018'){
  #     datatoplot <- data_plot17
  #     
  # 
  #   }
  #   else {
  #     datatoplot <- data_plot16
  #     
  #     
  #   }
  #   leaflet(width=500, height=500, options= leafletOptions(minZoom=7)) %>%
  #     addMarkers() %>%
  #     addProviderTiles("CartoDB.PositronNoLabels") %>%
  #     setView(lat=52.45521438, lng=-3.54943496, zoom=7) %>%
  #     setMaxBounds(-5.73028849,51.195868, -2.5982161836, 53.61259178) %>%
  #     addSearchOSM() %>%
  #     addReverseSearchOSM() %>%
  #     addResetMapButton()
  #   
  # })  
  
  
  
  
}