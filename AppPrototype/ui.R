library(shiny)
library(DT)
library(leaflet)


ui <- pageWithSidebar(
  headerPanel("Welsh Rail Dataset"),
  sidebarPanel( width=4, selectInput("Year", "Please Select Year:",  choices=c("2017-2018", "2016-2017")),  div(DT::dataTableOutput("mytable"), style = "font-size:60%")),
  mainPanel(leafletOutput("myleaflet",width="90%",height="700px")
            )
)
