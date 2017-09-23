# THESE ARE THE PACKAGES NEEDED
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(devtools)
library(rgdal)
library(cartography)
library(ggplot2)
library(dummies)

MPApolygon <- readOGR("WDPA_June2017_IDN-shapefile/WDPA_June2017_IDN-shapefile-polygons.shp")
leaflet(MPApolygon) %>%
  addPolygons(color = "blue", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.2)
# IMPORTING THE DATA FROM THE CSV 
df_Benthic = read.csv("Kofiau.BenthicData.csv", quote="")
df_Fish1 = read.csv("Kofiau.FishData.csv", quote="")

#Clean up Benthic Data
df_Benthic <- na.omit(df_Benthic)

#Clean up Fish Data
df_Fish1$Abundance_SE[is.na(df_Fish1$Abundance_SE)] <- 0 
df_Fish1$Biomass_SE[is.na(df_Fish1$Biomass_SE)] <- 0 
df_Fish1 <- df_Fish1[(!(is.na(df_Fish1$Latitude)) | !(is.na(df_Fish1$Longitude))),]

#Explode the Fish Family column into multiple dummy columns so we can choose from them separately
df_Fish1$Fish_Family <- as.factor(df_Fish1$Fish_Family)
df_Fish <- dummy.data.frame(df_Fish1, names="Fish_Family", sep="_")

# UI CODE
ui = bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             ".leaflet .legend i{
             border-radius: 50%;
             border-style: solid; 
             border-width: 1px;
             border-color: black;
             width: 10px;
             height: 10px;
             margin-top: 2px;
             }
             "),
  dashboardPage(
    # HEADER
    dashboardHeader(title = "WWF Coral Mapping"),
    
    # SIDEBAR
    dashboardSidebar(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      sidebarMenu(
        id="sidebar",
        menuItem("Benthic Data", tabName="benthic", icon=icon("globe")),
        menuItem("Fish Data", tabName="fish", icon=icon("globe"),
                 menuItem("Abundance", tabName="abundance", icon=icon("globe")),
                 menuItem("Biomass", tabName="biomass", icon=icon("globe")))
      ),
      # CREATES SLIDER FOR THE YEARS
      conditionalPanel(
        condition="input.sidebar == 'benthic'",
        sliderInput("time","Year:",
                    min = min(df_Benthic$Year),
                    max = max(df_Benthic$Year),
                    value = max(df_Benthic$Year),
                    step = 1,
                    sep = ""
        ),
        # CREATES RADIO BUTTONS FOR THE TYPES OF MEASURE
        radioButtons("cover", "Input:",
                     c("Hard Coral" = "Percent.Coral",
                       "Soft Coral" = "Percent.SoftCoral",
                       "CCA" = "Percent.CCA",
                       "Bleached Coral" = "Percent.Bleached",
                       "Rubble" = "Percent.Rubble", 
                       "Other Algae" = "Percent.OthAlgae"),
                     selected = "Percent.Coral"
        )
      ),
      
      conditionalPanel(
        condition="(input.sidebar == 'abundance') | (input.sidebar == 'biomass')",
        sliderInput("fishTime","Year:",
                    min = min(df_Fish$Year),
                    max = max(df_Fish$Year),
                    value = max(df_Fish$Year),
                    step = 1,
                    sep = ""
        ),
        # CREATES RADIO BUTTONS FOR THE TYPES OF MEASURE
        radioButtons("fishFamily", "Input:",
                     c("Acanthuridae" = "Fish_Family_Acanthuridae",
                       "Scaridae" = "Fish_Family_Scaridae",
                       "Siganidae" = "Fish_Family_Siganidae",
                       "Haemulidae" = "Fish_Family_Haemulidae",
                       "Lutjanidae" = "Fish_Family_Lutjanidae",
                       "Serranidae" = "Fish_Family_Serranidae"
                       ),
                     selected = "Acanthuridae"
        )
      )
      # ,
      # checkboxGroupInput(inputId = "cols", label = "Pick Columns", choices = names(df_Benthic), selected = names(df_Benthic))
    ),
    dashboardBody(
      
      tabItems(
        tabItem(tabName="benthic", 
                fluidRow(
                  column( width = 8,
                          box(width = NULL,
                              leafletOutput("map")
                          )
                  ),
                  tabBox(width = 4,
                         height = 300,
                         tabPanel("Benthic Cover", plotOutput("pie")),
                         tabPanel("Benthic Change Over Time", plotOutput("ggplot"))
                  )
                ),
                fluidRow(
                  box(width = 12,
                      height = 300,
                      dataTableOutput("table")
                  )
                )      
        ),
        tabItem(tabName="abundance",
            fluidRow(
              column( width = 8,
                      box(width = NULL,
                          leafletOutput("fishAbundanceMap")
                      )
              ),
              tabBox(width = 4,
                     height = 300,
                     tabPanel("Fish Abundance", plotOutput("fishAbundance")),
                     tabPanel("Fish Abundance Over Time", plotOutput("fishAbundanceDT"))
              )
            ),
            fluidRow(
              box(width = 12,
                  height = 300,
                  dataTableOutput("fishAbundanceTable")
              )
            )         
       ),
       tabItem(tabName="biomass",
               fluidRow(
                 column( width = 8,
                         box(width = NULL,
                             leafletOutput("fishBiomassMap")
                         )
                 ),
                 tabBox(width = 4,
                        height = 300,
                        tabPanel("Fish Biomass", plotOutput("fishBiomass")),
                        tabPanel("Fish Biomass Over Time", plotOutput("fishBiomassDT"))
                 )
               ),
               fluidRow(
                 box(width = 12,
                     height = 300,
                     dataTableOutput("fishBiomassTable")
                 )
               )         
       )
       )
    )
  ))

server = function(input, output) {
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
  }
  
  filteredData = reactive({
    df_Benthic %>%
      select(MPA_Name, Site_ID, Site_Name, Year, Latitude, Longitude, Zone, one_of(input$cover)) %>%
      filter(Year == input$time)
  })

  filteredFishBiomass = reactive({
    df_Fish %>%
      select(MPA, Site_ID, Site_Name, Year, Latitude, Longitude, Zone, Biomass, Biomass_SE, one_of(input$fishFamily)) %>%
      filter(Year == input$fishTime)
  })
  
  filteredFishAbundance = reactive({
    df_Fish %>%
      select(MPA, Site_ID, Site_Name, Year, Latitude, Longitude, Zone, Abundance, Abundance_SE, one_of(input$fishFamily)) %>%
      filter(Year == input$fishTime)
  })
  
  size = function() {
    r = 0
    if(input$cover == "Percent.Coral") {r = ~Percent.Coral/2}
    else if (input$cover == "Percent.SoftCoral") {r = ~Percent.SoftCoral/2}
    else if (input$cover == "Percent.CCA") {r = ~Percent.CCA/2}
    else if (input$cover == "Percent.Bleached") {r = ~Percent.Bleached/2}
    else if (input$cover == "Percent.Rubble") {r = ~Percent.Rubble/2}
    else {r = ~Percent.OthAlgae/2}
    return(r)
  }
  
  df = function() {
    if(input$cover == "Percent.Coral") {se = "SE.Coral"}
    else if (input$cover == "Percent.SoftCoral") {se = "SE.SoftCoral"}
    else if (input$cover == "Percent.CCA") {se = "SE.CCA"}
    else if (input$cover == "Percent.Bleached") {se = "SE.Bleached"}
    else if (input$cover == "Percent.Rubble") {se = "SE.Rubble"}
    else {se = "SE.OthAlgae"}
    return(se)
  }
  
  zoomlvl = function() {
    z = 12
    if(input$map_zoom > 12) {z = input$map_zoom}
    else {z = 12}
    return(z)
  }
  
  # DETERMINES FILL COLOR OF MARKERS BASED ON THE ZONE TYPE (TAKE / NO TAKE / CONTROL)
  pal = colorFactor(palette = c("green", "blue", "red"), domain = df_Benthic$Zone)
  
  output$fishBiomassMap <- renderLeaflet({
    leaflet(data = df_Fish) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addPolygons(data = MPApolygon, color = "green", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.2) %>%
      addLegend("topright", pal = pal, values = ~Zone, title = "MPA Zone",
                opacity = 1) %>%
      addMiniMap("bottomleft", width = 150, height = 150, toggleDisplay = TRUE)  %>%
      addLayersControl(position=c("bottomleft"),
                       baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset Zoom",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      
      htmlwidgets::onRender("
                            function(el, x) {
                            var myMap = this;
                            myMap.on('baselayerchange',
                            function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                            })
                            }")  %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       #popup = content,
                       radius = ~log10(Biomass)*5,
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(df_Fish$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Fish))) %>% 
      
      addLegendCustom(colors=c("blue", "blue", "blue", "blue"), labels=c("5%", "10%", "25%", "50%"), sizes=c(5, 10, 25, 50))
  }) 
  
  output$fishAbundanceMap <- renderLeaflet({
    leaflet(data = df_Fish) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addPolygons(data = MPApolygon, color = "green", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.2) %>%
      addLegend("topright", pal = pal, values = ~Zone, title = "MPA Zone",
                opacity = 1) %>%
      addMiniMap("bottomleft", width = 150, height = 150, toggleDisplay = TRUE)  %>%
      addLayersControl(position=c("bottomleft"),
                       baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset Zoom",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      
      htmlwidgets::onRender("
                            function(el, x) {
                            var myMap = this;
                            myMap.on('baselayerchange',
                            function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                            })
                            }")  %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       #popup = content,
                       radius = ~log10(Abundance)*5,
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(df_Fish$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Fish))) %>% 
      
      addLegendCustom(colors=c("blue", "blue", "blue", "blue"), labels=c("5%", "10%", "25%", "50%"), sizes=c(5, 10, 25, 50))
  }) 
  
  output$map <- renderLeaflet({
    #mlayers = c("OpenStreetMap.Mapnik" = )
    leaflet(data = df_Benthic) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addPolygons(data = MPApolygon, color = "green", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.2) %>%
      addLegend("topright", pal = pal, values = ~Zone, title = "MPA Zone",
                opacity = 1) %>%
      addMiniMap("bottomleft", width = 150, height = 150, toggleDisplay = TRUE)  %>%
      addLayersControl(position=c("bottomleft"),
                       baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset Zoom",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      
      htmlwidgets::onRender("
                            function(el, x) {
                            var myMap = this;
                            myMap.on('baselayerchange',
                            function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                            })
                            }")  %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       #popup = content,
                       radius = ~Percent.Coral/2,
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(df_Benthic$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Benthic))) %>% 
      
      addLegendCustom(colors=c("blue", "blue", "blue", "blue"), labels=c("5%", "10%", "25%", "50%"), sizes=c(5, 10, 25, 50))
    })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      #clearControls() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       # label = ~htmlEscape(Site_Name),
                       # labelOptions = labelOptions(textOnly = TRUE),
                       # popup = paste(
                       #   "Site:", filteredData()$Site_Name , "<br/>",
                       #   "MPA:", filteredData()$MPA_Name , "<br/>",
                       #   "ID:", filteredData()$Site_ID , "<br/>",
                       #   "Location:", filteredData()$Longitude, ",",filteredData()$Latitude, "<br/>",
                       #   "Year:", filteredData()$Year, "<br/>",
                       #   "Zone:", filteredData()$Zone
                       # ),
                       radius = size(),
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(filteredData()$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Benthic))
      ) 
  })
  
  observe({
    leafletProxy("fishAbundanceMap", data = filteredFishAbundance()) %>%
      clearMarkers() %>%
      #clearControls() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius =~log10(Abundance)*5,
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(filteredFishAbundance()$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Fish))
      ) 
  })

  observe({
    leafletProxy("fishBiomassMap", data = filteredFishBiomass()) %>%
      clearMarkers() %>%
      #clearControls() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius =~log10(Biomass)*5,
                       stroke = FALSE,
                       #weight = 2,
                       fillColor = ~pal(filteredFishBiomass()$Zone),
                       fillOpacity = 0.1,
                       layerId = paste0("marker", 1:nrow(df_Fish))
      ) 
  })
  
  output$table = renderDataTable(
    filteredData(),
    rownames= FALSE,
    extensions = c("Scroller", "Buttons"), options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      buttons = I('colvis'),
      dom = "Bfrtip"
    )
  )
  DTproxy = dataTableProxy("table")
  
  output$pie = renderPlot({
    map_list = list()
    map_list$Percent.Coral = "Hard Coral"
    map_list$Percent.SoftCoral = "Soft Coral"
    map_list$Percent.CCA = "CCA"
    map_list$Percent.Bleached = "Bleached Coral"
    map_list$Percent.Rubble = "Rubble"
    map_list$Percent.OthAlgae = "Other Algae"
    
    t = df_Benthic[filteredData()$Year %in% df_Benthic$Year,]
    total = sum(t$Percent.Coral) + sum(t$Percent.SoftCoral) + sum(t$Percent.CCA) + sum(t$Percent.Bleached) + sum(t$Percent.Rubble) + sum(t$Percent.OthAlgae)
    slices = c((sum(t$Percent.Coral)/total)*100, (sum(t$Percent.SoftCoral)/total)*100, (sum(t$Percent.CCA)/total)*100, (sum(t$Percent.Bleached)/total)*100, (sum(t$Percent.Rubble)/total)*100, (sum(t$Percent.OthAlgae)/total)*100)
    lbls = c("Hard Coral", "Soft Coral", "CCA", "Bleached Coral", "Rubble", "Other Algae")
    #pie(slices, labels = lbls, main="Breakdown of Benthic Cover by Input")
    #barplot(slices, cex.names=0.7, names.arg=lbls)
    
    plot_df <- data.frame(labs = lbls, percents = slices)
    ggplot(data=plot_df, aes(x=labs, y=percents)) + geom_bar(stat="identity") + labs(y = "Percent (%)") + labs(x = "Benthic Cover")
    
  })
  output$ggplot = renderPlot({

    map_list = list()
    map_list$Percent.Coral = "Hard Coral"
    map_list$Percent.SoftCoral = "Soft Coral"
    map_list$Percent.CCA = "CCA"
    map_list$Percent.Bleached = "Bleached Coral"
    map_list$Percent.Rubble = "Rubble"
    map_list$Percent.OthAlgae = "Other Algae"
    
    t = df_Benthic
    avg_Inputs <- aggregate(t[[input$cover]], list(t$Year), mean)
    avg_SE_Inputs <- aggregate(t[[df()]], list(t$Year), mean)
    ndf <- merge(avg_Inputs, avg_SE_Inputs, by="Group.1")
    names(ndf) = c("Year", "Avg", "SE")
    ggplot(data = ndf, aes(x = ndf$Year, y=ndf$Avg)) + geom_bar(position="dodge", stat="identity") + geom_errorbar(aes(ymax=ndf$Avg + ndf$SE, ymin=ndf$Avg-ndf$SE), width=0.2, position="dodge") + labs(y = map_list[[ input$cover ]])
  })
  observe({
    if (!is.null(input$map_click)){
#       p = input$map_marker_click
#       p1 = filteredData()[(filteredData()$Longitude == p$lng & filteredData()$Latitude == p$lat),]
      
      leafletProxy("map") %>%
        removeMarker(layerId = "Selected")
      
      output$pie = renderPlot({
        t = df_Benthic[filteredData()$Year %in% df_Benthic$Year,]
        total = sum(t$Percent.Coral) + sum(t$Percent.SoftCoral) + sum(t$Percent.CCA) + sum(t$Percent.Bleached) + sum(t$Percent.Rubble) + sum(t$Percent.OthAlgae)
        slices = c((sum(t$Percent.Coral)/total)*100, (sum(t$Percent.SoftCoral)/total)*100, (sum(t$Percent.CCA)/total)*100, (sum(t$Percent.Bleached)/total)*100, (sum(t$Percent.Rubble)/total)*100, (sum(t$Percent.OthAlgae)/total)*100)
        #slices = c(sum(t$Percent.Coral), sum(t$Percent.SoftCoral), sum(t$Percent.CCA), sum(t$Percent.Bleached), sum(t$Percent.Rubble), sum(t$Percent.OthAlgae) )
        lbls = c("Hard Coral", "Soft Coral", "CCA", "Bleached Coral", "Rubble", "Other Algae")
        #pie(slices, labels = lbls)
        plot_df <- data.frame(labs = lbls, percents = slices)
        ggplot(data=plot_df, aes(x=labs, y=percents)) + geom_bar(stat="identity") + labs(y = "Percent (%)") + labs(x = "Benthic Cover")
      })
      output$ggplot = renderPlot({
        
        map_list = list()
        map_list$Percent.Coral = "Hard Coral"
        map_list$Percent.SoftCoral = "Soft Coral"
        map_list$Percent.CCA = "CCA"
        map_list$Percent.Bleached = "Bleached Coral"
        map_list$Percent.Rubble = "Rubble"
        map_list$Percent.OthAlgae = "Other Algae"
        

        t = df_Benthic
        avg_Inputs <- aggregate(t[[input$cover]], list(t$Year), mean)
        avg_SE_Inputs <- aggregate(t[[df()]], list(t$Year), mean)
        ndf <- merge(avg_Inputs, avg_SE_Inputs, by="Group.1")
        names(ndf) = c("Year", "Avg", "SE")
        ggplot(data = ndf, aes(x = ndf$Year, y=ndf$Avg)) + geom_bar(position="dodge", stat="identity") + geom_errorbar(aes(ymax=ndf$Avg + ndf$SE, ymin=ndf$Avg-ndf$SE), width=0.2, position="dodge") + labs(y = map_list[[ input$cover ]])
      })
      replaceData(DTproxy, filteredData(), resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  observeEvent(input$map_marker_click,{
    p = input$map_marker_click
    
    p1 = filteredData()[(filteredData()$Longitude == p$lng & filteredData()$Latitude == p$lat),]
    if (p$id != "Selected"){
      leafletProxy("map", data = p1) %>%
        setView(lng = p$lng, lat = p$lat, zoom = zoomlvl()) %>%
        addCircleMarkers(lng = p$lng, lat = p$lat,
                         #popup = content,
                         radius = size(),
                         #stroke = FALSE,
                         weight =  1,
                         fillColor = ~pal(p1$Zone),
                         color = "black",
                         fillOpacity = 1,
                         opacity = 1,
                         layerId="Selected"
        )
    }
    output$pie = renderPlot({
      if (nrow(p1) != 0) {
        t = df_Benthic[df_Benthic$Site_ID == p1$Site_ID & df_Benthic$Year == p1$Year,]
        total = sum(t$Percent.Coral) + sum(t$Percent.SoftCoral) + sum(t$Percent.CCA) + sum(t$Percent.Bleached) + sum(t$Percent.Rubble) + sum(t$Percent.OthAlgae)
        slices = c((sum(t$Percent.Coral)/total)*100, (sum(t$Percent.SoftCoral)/total)*100, (sum(t$Percent.CCA)/total)*100, (sum(t$Percent.Bleached)/total)*100, (sum(t$Percent.Rubble)/total)*100, (sum(t$Percent.OthAlgae)/total)*100)
        #slices = c(t$Percent.Coral, t$Percent.SoftCoral, t$Percent.CCA, t$Percent.Bleached, t$Percent.Rubble, t$Percent.OthAlgae )
        lbls = c("Hard Coral", "Soft Coral", "CCA", "Bleached Coral", "Rubble", "Other Algae")
        #pie(slices, labels = lbls)
        #print("success")
        
        plot_df <- data.frame(labs = lbls, percents = slices)
        ggplot(data=plot_df, aes(x=labs, y=percents)) + geom_bar(stat="identity") + labs(y = "Percent (%)") + labs(x = "Benthic Cover")
      }
    })
    output$ggplot = renderPlot({
      
      map_list = list()
      map_list$Percent.Coral = "Hard Coral"
      map_list$Percent.SoftCoral = "Soft Coral"
      map_list$Percent.CCA = "CCA"
      map_list$Percent.Bleached = "Bleached Coral"
      map_list$Percent.Rubble = "Rubble"
      map_list$Percent.OthAlgae = "Other Algae"
      
      t = df_Benthic[df_Benthic$Site_ID == p1$Site_ID,]
      avg_Inputs <- aggregate(t[[input$cover]], list(t$Year), mean)
      avg_SE_Inputs <- aggregate(t[[df()]], list(t$Year), mean)
      ndf <- merge(avg_Inputs, avg_SE_Inputs, by="Group.1")
      names(ndf) = c("Year", "Avg", "SE")
      ggplot(data = ndf, aes(x = ndf$Year, y=ndf$Avg)) + geom_bar(position="dodge", stat="identity") + geom_errorbar(aes(ymax=ndf$Avg + ndf$SE, ymin=ndf$Avg-ndf$SE), width=0.2, position="dodge") + labs(y = map_list[[ input$cover ]])
    })
    replaceData(DTproxy, p1, resetPaging = FALSE, rownames = FALSE)
    print(filteredData())
  })
  }
runApp(shinyApp(ui, server), launch.browser = TRUE)