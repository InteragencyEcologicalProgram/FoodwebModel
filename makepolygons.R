#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(sp)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(deltamapr)

delta = WW_Delta
delta = st_transform(delta, crs = "+proj=longlat +datum=WGS84")

ui <- fluidPage(
  
  textOutput("text"),leafletOutput("mymap"),
  downloadButton('downloadData', 'Download Shp'))

server<- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet("mymap") %>%
      #        addTiles() %>% 
      setView(lng = -121.363590, lat=38.668483,zoom=8) %>% 
      addPolygons(data=delta,weight=1,col = 'black')  %>% 
      #addPolygons(data = regs, group = "edit")%>%
      addDrawToolbar(targetGroup = "drawnPoly", 
                     rectangleOptions = F, 
                     polylineOptions = F, 
                     markerOptions = F, 
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleOptions=F,
                     polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE))) %>%
      
      addStyleEditor()
    
  })
  
  
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  #########
  #empty reactive spdf
  value<-reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
  
  #fix the polygon to start another
  
  observeEvent(input$mymap_draw_new_feature, {
    
    coor<-unlist(input$mymap_draw_new_feature$geometry$coordinates)
    
    Longitude<-coor[seq(1,length(coor), 2)] 
    
    Latitude<-coor[seq(2,length(coor), 2)]
    
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys<-Polygons(list(poly),    ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
    spPolys<-SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                    data=data.frame(notes=NA, row.names=
                                                                                      row.names(spPolys))))
    
    ###plot upon ending draw
    observeEvent(input$mymap_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('mymap') %>%  removeDrawToolbar(clearFeatures=TRUE) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% addPolygons(data=value$drawnPoly, popup="poly",   group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>% 
        
        addDrawToolbar(targetGroup = "drawnPoly", 
                       rectangleOptions = F, 
                       polylineOptions = F, 
                       markerOptions = F, 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                       circleOptions=F,
                       polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
      
    })
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
    
  })
  
  ########################
  ### edit polygons / delete polygons
  
  observeEvent(input$mymap_draw_edited_features, {
    
    f <- input$mymap_draw_edited_features
    
    coordy<-lapply(f$features, function(x){unlist(x$geometry$coordinates)})
    
    Longitudes<-lapply(coordy, function(coor) {coor[seq(1,length(coor), 2)] })
    
    Latitudes<-lapply(coordy, function(coor) { coor[seq(2,length(coor), 2)] })
    
    polys<-list()
    for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
      list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
    )}
    
    spPolys<-SpatialPolygons(polys)
    
    
    SPDF<-SpatialPolygonsDataFrame(spPolys, 
                                   data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
    value$drawnPoly<-rbind(value$drawnPoly, SPDF)
    
  })
  
  observeEvent(input$mymap_draw_deleted_features, { 
    
    f <- input$mymap_draw_deleted_features
    
    ids<-lapply(f$features, function(x){unlist(x$properties$layerId)})
    
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,]
    
  }) 
  
  
  
  #write the polys to .shp
  output$downloadData<-downloadHandler(
    
    filename = 'shpExport.zip',
    content = function(file) {
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
      
      proj4string(value$drawnPoly)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      writeOGR(value$drawnPoly, dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
      zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))
      file.copy("shpExport.zip", file)
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)