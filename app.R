library(shiny)
# this Shiny App was designed using:
# Revolution R Open 3.2.1
# R version 3.2.1 (2015-06-18)
# Platform: x86_64-unknown-linux-gnu (64-bit)
# Running under: Ubuntu 14.04.3 LTS
# RevoUtilsMath_8.0.2
# shiny_0.12.2
# leaflet_1.0.0.9999 (
#     installed from github using:
#     options(unzip = "internal")
#     devtools::install_github("rstudio/leaflet")
#     )
library(leaflet)
library(DT)
library(dplyr)

if (!file.exists("R/v2.RData")) {
    stop("check your working directory is correct. Can't find .RData source files.
         You can https://github.com/mashoedoe/shiny_leaflet_choropleth")
    } else {
        load("R/v2.RData")
    }

ui <- navbarPage(
    title = "South African Demographics",
    tabPanel(
        tags$style(".floater { background-color: white;padding: 8px;
                           opacity: 0.7; border-radius: 6px;
                           box-shadow: 0 0 15px rgba(0,0,0,0.2);}",
                   ".sidebar {position: fixed; top: 50px; left: 0;
                    right: 350px; bottom: 0; overflow: hidden; padding: 0;
                    background-color: white;padding: 8px; opacity: 1;}",
                   ".outer {position: fixed; top: 50px; left: 350px;
                    right: 0; bottom: 0; overflow: hidden; padding: 0;}"
                   ),
        title = "Map",
        sidebarLayout(
            position="left", fluid = TRUE,
            sidebarPanel(
                class="sidebar",
                width=3,
                #absolutePanel(
                #    right=10,top=270,width=200,class="floater",
                #    draggable = TRUE,
                p(h4(strong('Select a level:')),
                  em('At high zoom',strong('Ward'), 'replaces',
                     strong('Province'),'as an option')),
                radioButtons( 
                    inputId='select_map_level',
                    label=NULL,
                    choices=c('Province','Municipality'),
                    selected='Municipality',
                    inline=F
                ),
                br(),
                #absolutePanel(
                #    right=10,top=20,width =260, class="floater",
                checkboxInput(
                    inputId = "enable_hover",
                    value = TRUE,
                    label = p(strong("Info display on hover."),
                              br(),
                              em("Users on mobile devices or those who prefer",
                                 br(),
                                 "popups should DESELECT this")
                    )
                ),
                conditionalPanel(
                    condition = "input.enable_hover == true",
                    h3("SA Population Density"),
                    uiOutput('hoverInfo')
                ),
                conditionalPanel(
                    condition = "input.enable_hover == false",
                    DT::dataTableOutput(outputId = "table_subset",
                                        width = '340px')
                )
            ),
            mainPanel(
                div(class="outer",
                    leafletOutput(outputId = "map1", width="100%", height="100%")
                )
            )
        ),
        
        absolutePanel( # Zoom level
            left=20,top=785,width =120,
            textOutput(outputId='message3',container=span,inline=T)
        ),
        absolutePanel( # Click coordinates
            left=140,top=785,width =280,
            textOutput(outputId='message1',container=span,inline=T)
        ),
        absolutePanel( # Municipality
            left=420,top=785,width =340,
            textOutput(outputId='message_slice2',container=span,inline=T)
        ),
        absolutePanel( # Province
            left=760,top=785,width =110,
            textOutput(outputId='message_slice1',container=span,inline=T)
        ),
        absolutePanel( # spd@data$ID)
            left=870,top=785,width =75,
            textOutput(outputId='message_shp_id',container=span,inline=T)
        ),
        absolutePanel( # gis$mouse_events
            left=945,top=785,width =20,
            textOutput(outputId='message_events',container=span,inline=T)
        )
    ),
    
    navbarMenu(title = "Data",
               tabPanel(title = "Provincial Data",
                        DT::dataTableOutput(outputId = "table_province")
               ),
               tabPanel("Municipal Data",
                        DT::dataTableOutput(outputId = "table_town")
               ),
               tabPanel("Ward Data",
                        DT::dataTableOutput(outputId = "table_ward")
               )
    )
)


server <- function(session, input, output) {
    # save selected map objects and geographic names as reactive values 
    gis <- reactiveValues(tj=town_tj) # chosen topojson object
    gis <- reactiveValues(shp=NULL) # chosen spatial polygon dataframe object
    gis <- reactiveValues(single=NULL) # chosen spatial polygon dataframe object
    gis <- reactiveValues(id=NULL) # Name of Geographical feature being hovered over
    gis <- reactiveValues(binpal=NULL) # color palette for choropleth being viewed
    gis <- reactiveValues(slice1=NULL) # Name of Province hovered over or clicked on
    gis <- reactiveValues(slice2=NULL) # Name of Municipality hovered over or clicked on
    gis <- reactiveValues(mouseover_id=NULL) # layerId of shape (polygon in this case) hovered over
    gis <- reactiveValues(mouse_events=0) # 1 or 0 - proxy for presence of topojson layer
#    gis <- reactiveValues(click=NULL) # 
     
     observe(if (input$enable_hover == FALSE & (!is.null(input$map1_shape_click$id))){
         label = "event0"
         gis$click <- input$map1_shape_click$id
     })
    
    observeEvent(input$map1_topojson_mouseover,label = "event1a", {
        gis$mouse_events <- 1
    })
    observeEvent(input$map1_topojson_mouseout,label = "event1b", {
        gis$mouse_events <- 0
    })
    # Choose a topojson oject that matches the administative layer being viewed 
    # by the user
    observe(if (input$select_map_level == 'Ward'){
        label="event2a"
        gis$tj <- town_tj
    })
    observe(if (input$select_map_level == 'Municipality'){
        label="event2b"
        gis$tj <- town_tj
    })
    observe(if (input$select_map_level == 'Province'){
        label="event2c"
        gis$tj <- province_tj
    })
    # Choose a spatial polygon dataframe oject that matches the administative 
    # layer being viewed by the user
    observe(if (input$enable_hover==TRUE &
                input$select_map_level == 'Ward' &  
                (!is.null(gis$mouseover_id))){ # | 
#                (!is.null(input$map1_topojson_mouseover))){
        label="event3a"
        gis$single <- subset(ward_tj_spd,
                          subset = ward_tj_spd@data$WARD == gis$mouseover_id)
    })
#     observe(if (input$enable_hover==FALSE &
#                 input$select_map_level == 'Ward' &
#                 (!is.null(input$map1_shape_click$id))){
#         label="event3b"
#         gis$single <- subset(ward_tj_spd,
#                              subset = ward_tj_spd@data$ID == input$map1_shape_click$id)
#     })
    observe(if (input$enable_hover==FALSE &
                input$select_map_level == 'Ward' &
                is.null(gis$mouseover_id) & is.null(input$map1_topojson_mouseover)){
        label="event3c"
        gis$shp <- subset(ward_tj_spd,
                          subset = ward_tj_spd@data$MUNICNAME == gis$slice2)
    })
    observe(if (input$enable_hover==FALSE &
                input$select_map_level == 'Ward' &
                ((!is.null(gis$mouseover_id)) | (!is.null(input$map1_topojson_mouseover)))){
        label="event3d"
        gis$shp <- subset(x = ward_tj_spd,
                          subset = ward_tj_spd@data$MUNICNAME == gis$slice2)
    })
    
    observe(if (input$select_map_level == 'Municipality'){# & input$enable_hover==FALSE){
        label="event3e"
        gis$shp <- town_tj_spd
    })
    observe(if (input$select_map_level == 'Municipality' &
#                input$enable_hover==TRUE & 
                (gis$mouse_events == 1)){
        label="event3f"
        gis$single <- subset(x = town_tj_spd,
                          subset = town_tj_spd@data$MUNICNAME == input$map1_topojson_mouseover$properties$MUNICNAME)
    })
#     observe(if (input$enable_hover==FALSE &
#                 input$select_map_level == 'Municipality'){
#         observeEvent(input$map1_shape_click,label="event3g", {
#             gis$single <- subset(town_tj_spd,
#                                  subset = town_tj_spd@data$ID == input$map1_shape_click$id)
#         })
#     })
    observe(if (input$select_map_level == 'Province'){#} & input$enable_hover==FALSE){
        label="event3h"
        gis$shp <- province_tj_spd
    })
    observe(if (input$select_map_level == 'Province' &
#                input$enable_hover==TRUE &
                (gis$mouse_events == 1)){
        label="event3i"
        gis$single <- subset(x = province_tj_spd,
                          subset = province_tj_spd@data$PROVINCE == input$map1_topojson_mouseover$properties$PROVINCE)
    })
#     observe(if (input$enable_hover==FALSE &
#                 input$select_map_level == 'Province'){
#         observeEvent(input$map1_shape_click, label="event3j", {
#             gis$single <- subset(province_tj_spd,
#                                  subset = province_tj_spd@data$ID == input$map1_shape_click$id)
#         })
#     })
#    observe(if (!is.null(gis$shp)){
#        gis$bbox_spd <- bbox(gis$shp)
#    })
    # identify the geographic geometry/shape being moused over
    observeEvent(input$map1_topojson_mouseover, label="event4a",{
        if (input$select_map_level == 'Ward'){
            gis$id <- input$map1_topojson_mouseover$properties$WARD
        }
    })
    observeEvent(input$map1_topojson_mouseover, label="event4b", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- input$map1_topojson_mouseover$properties$MUNICNAME
        }
    })
    observeEvent(input$map1_topojson_mouseover,label="event4c", {
        if (input$select_map_level == 'Province'){
            gis$id <- input$map1_topojson_mouseover$properties$PROVINCE
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event4d",{
        if (input$select_map_level == 'Ward'){
            gis$id <- gis$shp@data$WARD[gis$shp@data$ID == gis$mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event4e", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- gis$shp@data$MUNICNAME[gis$shp@data$ID == gis$mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event4f",{
        if (input$select_map_level == 'Province'){
            gis$id <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$sh_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseout, label="event4g",{
        gis$id <- NULL
    })
    # select the the color palette that matches the administative layer being viewed
    observe(if (input$select_map_level == 'Ward'){label="event11a"
    gis$binpal <- ward_binpal
    })
    observe(if (input$select_map_level == 'Municipality'){label="event11b"
    gis$binpal <- town_binpal
    })
    observe(if (input$select_map_level == 'Province'){label="event11c"
    gis$binpal <- province_binpal
    })
    # identify the Province being moused over or clicked on
    observeEvent(input$map1_topojson_mouseover, label="event12a",{
        gis$slice1 <- input$map1_topojson_mouseover$properties$PROVINCE
    })
    observeEvent(input$map1_shape_click, label="event12b",{
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == input$map1_shape_click$id]
    })
#    observeEvent(input$map1_shape_mouseover, label="event12c",{
#        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$mouseover_id]
#    })
    observe(if(!is.null(gis$mouseover_id) & input$enable_hover == FALSE){ 
        label="event12d"
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$mouseover_id]
    })
    observeEvent(input$map1_topojson_click, label="event12e", {
        gis$slice1 <- input$map1_topojson_click$properties$PROVINCE
    })
#    observeEvent(input$map1_shape_mouseout, label="event12f",{
#        gis$slice1 <- NULL
#    })
    # identify the Municipality being moused over or clicked on
    observeEvent(input$map1_topojson_mouseover, label="event13a",{
            gis$slice2 <- input$map1_topojson_mouseover$properties$MUNICNAME
    })
     observe(if((!is.null(gis$mouseover_id) & input$select_map_level == 'Municipality')){ 
         label="event13b"
#         gis$slice2 <- town_tj_spd@data$MUNICNAME[town_tj_spd@data$ID == gis$mouseover_id]
         gis$slice2 <- gis$shp@data$MUNICNAME[gis$shp@data$ID == gis$mouseover_id]
         
     })
#     observe(if((!is.null(input$map1_shape_click$id)) &
#                (input$select_map_level == 'Municipality'|input$select_map_level == 'Ward')){
#         label="event13c"
     observeEvent(input$map1_shape_click, label="event13c", {
         gis$slice2 <- gis$shp@data$MUNICNAME[gis$shp@data$ID == input$map1_shape_click$id]
     })
     observeEvent(input$map1_topojson_click, label="event13d", {
#         observe(if( input$select_map_level == 'Ward'|input$select_map_level == 'Province'){
             gis$slice2 <- input$map1_topojson_click$properties$MUNICNAME
#         })
    })
#    observeEvent(input$map1_topojson_mouseout, label="event15",{
#        gis$slice2 <- NULL
#    })
    # identify the layerId of the spatial polygon being moused over
    observeEvent(input$map1_shape_mouseover, label="event18a",{
        observe(if (input$enable_hover == FALSE){
            gis$mouseover_id <- input$map1_shape_mouseover$id
        })
    })
    observeEvent(input$map1_shape_mouseout, label="event18b",{
        gis$mouseover_id <- NULL
    })
    observeEvent(input$map1_topojson_mouseover, label="event18c",{
        observe(if (input$enable_hover == TRUE){
            gis$mouseover_id <- input$map1_topojson_mouseover$properties$WARD
          })
      })
#      observeEvent(input$map1_topojson_mouseout, label="event18d",{
#          gis$mouseover_id <- NULL
#      })
     
    
    
    # Create reactive values to store click coordinates, map boundary & zoom level
    v <- reactiveValues(msg1=NULL) # Click Latitude
    v <- reactiveValues(msg2=NULL) # Click Longitude
    v <- reactiveValues(msg3=NULL) # Zoom level
    v <- reactiveValues(lng1 =  10) # southwest longitude bound
    v <- reactiveValues(lat1 = -35) # southwest latitude bound
    v <- reactiveValues(lng2 =  43) # northeast longitude bound
    v <- reactiveValues(lat2 = -23) # northeast latitude bound
    
    # store click latitiude and longitude
    observeEvent(input$map1_click, {
        v$msg1 <- input$map1_click$lat
    })
    observeEvent(input$map1_topojson_click, {
        v$msg1 <- input$map1_topojson_click$lat
    })
    observeEvent(input$map1_shape_click,{
        v$msg1 <  input$map1_shape_click$lat
    })
    observeEvent(input$map1_click, {
        v$msg2 <- input$map1_click$lng
    })
    observeEvent(input$map1_topojson_click, {
        v$msg2 <- input$map1_topojson_click$lng
    })
    observeEvent(input$map1_shape_click,{
        v$msg2 <  input$map1_shape_click$lng
    })
    # store current map zoom level
    observeEvent(input$map1_zoom, label="zoom_message", {
        v$msg3 <- input$map1_zoom
    })
    # store current map bound coordinates
    observeEvent(input$map1_bounds, {
        v$lng1 <- input$map1_bounds[4]
    })
    observeEvent(input$map1_bounds, {
        v$lat1 <- input$map1_bounds[3]
    })
    observeEvent(input$map1_bounds, {
        v$lng2 <- input$map1_bounds[2]
    })
    observeEvent(input$map1_bounds, {
        v$lat2 <- input$map1_bounds[1]
    })
    
    # messages desplayed below map to help with debugging all mouse events
    output$message_slice1 <- renderText(gis$slice1)
    output$message_slice2 <- renderText(gis$slice2)
    output$message_shp_id <- renderText(gis$mouseover_id)
#    output$message_id <- renderText(gis$id)
    output$message_events <- renderText(gis$mouse_events)
    output$message1 <- renderText(if (!is.null(v$msg1)) {
        paste("Click coordinates: long",round(v$msg2,3),
              "lat",round(v$msg1,3))
    })# Click Coordinates
    output$message3 <- renderText(paste("Zoom level is",v$msg3)) # Zoom level
    output$message4 <- renderText(
        paste(
            "View Bounds: long",substr(paste(v$lng1),start=1,stop=6),
            "lat",substr(paste(v$lat1),start=1,stop=6),"(bottomleft/SW); ",
            "long",substr(paste(v$lng2),start=1,stop=6),
            "lat",substr(paste(v$lat2),start = 1, stop = 6),"(topright/NE)"
        )
    ) # Boundary Coordinates
    
    # update Radio Button options according to zoom level to add 'Ward' option 
    # at high zoom. This forces change to the 'Ward' administrative/map object 
    # level whether the map is zooming in or out while the zoom level is over 8.
    # If the map is zoomed in or out without ever touching a relevant map object
    # or the active administrative/map object level is 'Province' this action 
    # will NOT take effect until there is evidence that a Municipality is below 
    # the mouse/touch point. The map must to be on Municipal level (the default)
    # and the underlying Municipality must be detected (via mouseover or click)
    # before this action is activitated at the next zoom change which takes place
    # above zoom level 8. 
    observeEvent(input$map1_zoom, label="event20a",{
        if (v$msg3 > 8 & (!is.null(gis$slice2))) {
            label="event20b"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Municipality', 'Ward'),
                selected = 'Ward'
            )
        }
    })
    # remove 'Ward option' from Radio Buttons at low zoom. Changes to Municipal
    # administative/map object level if 'Ward' level objects were in operation as 
    # zoom crosses from above 8 to below 8..
    observeEvent(input$map1_zoom ,label="event20c", {
        if (v$msg3 <= 8 & input$select_map_level == 'Ward') {
            label="event20d"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality'),
                selected = 'Municipality'
            )
        }
    })
    
    # Render base map with legend and set view to South Africa
    observe({
        output$map1 <-renderLeaflet({
            leaflet() %>%
                setView(zoom=6,lng=26,lat=-29) %>%
                addTiles() %>%
                addLegend(
                    position = "bottomright", 
                    pal = town_binpal,
                    opacity = 1,
                    labFormat = labelFormat(big.mark = " "),
                    values = town_density
                ) 
        })
    })
#################################################################################
# leafletProxy objects to add and remove from map if hover checkbox is selected #
#################################################################################    
    # with hover selected, topojson is used to add to the empty base map:
    # 1. for objects with many elements, topojson loads much faster than spatial 
    #    polygon dataframe
    # 2. topojson does not have popup option taht spatial polygon dataframe has
    # 3. topojson geometry properties are easy to access on mouseover with 
    #    rstudio's leaflet bindings to leaflet-omnivore.js
    # For users who run this app locally and are have leaflet packages earlier 
    # than 1.0.0.9999 installed, 'clearTopojson' function can be replaced with 
    # assigning added Topojson objects  to groups and then clearing those groups.
    # clearShapes clears all polygons which is desirable in this configuration so
    # clearGroups is not required for specifying only some shapes
    # gis$tj stores the correct object based on the radio button status in the UI
    # so seperate proxies for Province, Municipality and Ward are not required.
    observe(if (input$enable_hover == TRUE & 
                input$select_map_level == 'Province'){
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1"
        )
        proxy %>%
            clearShapes() %>%
#            clearTopoJSON() %>%
            clearGroup('town') %>%
            #                clearGroup('gis_shp') %>%
            addTopoJSON(
                group = 'province', topojson = gis$tj,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5
            )
    })
    observe(if (input$enable_hover == TRUE & 
                input$select_map_level == 'Municipality'){
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1"
        )
        proxy %>%
            clearShapes() %>%
#            clearGroup('single') %>%
#            clearTopoJSON() %>%
            clearGroup('province') %>%
            clearGroup('town') %>%
            clearGroup('ward') %>%
            addTopoJSON(
                group = 'town', topojson = gis$tj,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5
            )
    })
    
    # when the user is at the ward level of detail, add the municipality's wards 
    # (as a Spatial Polygon Dataframe) on top of the municipal layer, tiling 
    # 1 municipality's worth of wards at a time to save memor and load time.
#    observeEvent(input$map1_topojson_mouseover,{
        observe(if(input$enable_hover == TRUE & input$select_map_level == 'Ward'){
            label="proxy_ward_event"
            proxy <- leafletProxy(
                "map1"
            )
            proxy %>%
                clearGroup('single') %>%
                clearGroup('ward') %>%
                addTopoJSON(
                    group = 'ward',
#                    topojson = town_wards[(town_tj_properties$ID[town_tj_properties$MUNICNAME == input$map1_topojson_mouseover$properties$MUNICNAME])+1],
                    topojson = town_wards[(town_tj_properties$ID[town_tj_properties$MUNICNAME == gis$slice2])+1],
                    stroke=T,dashArray=3,weight=2,color="white",
                    opacity=1,fill=T,smoothFactor = 0.5
                )
        })
#    })
    
    # add the outline of the single ward being hovered over 
    observe(if (gis$mouse_events == 1 & input$enable_hover == TRUE) {
        label="event22"
        proxy <- leafletProxy(
            "map1", data = gis$single
        )
        proxy %>%
            clearGroup('single') %>%
            #                clearGroup('gis_shp') %>%
            addPolygons(group = 'single',
                        stroke=T,weight=4,color="#555555",opacity=1,
                        smoothFactor=0,fill=F
            )
    })
    
    # remove the previous single ward/town/province outline hovered over when 
    # leaving (mouseout) the mapped area
    observeEvent(input$map1_topojson_mouseout,label="event24", {
        proxy <- leafletProxy(mapId = 'map1')
        proxy %>%
            clearGroup('single')
    })
    
#################################################################################
# leafletProxy objects to add & remove from map if hover checkbox is DEselected #
#################################################################################    
    
    observe(if (input$enable_hover==FALSE & input$select_map_level == 'Province') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
#                group = 'gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.7,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$PROVINCE,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    observe(if (input$enable_hover==FALSE & input$select_map_level == 'Municipality') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
#                group = 'gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.7,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$MUNICNAME,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    observe(if (input$enable_hover == FALSE & input$select_map_level == 'Ward') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1"
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
            addTopoJSON(
#               group = 'gis_tj',
                stroke=T,dashArray=3,weight=2,color="white", topojson = gis$tj,
                opacity=1,fill=T,smoothFactor = 0.5
            )
    })
    
    observe(if (input$enable_hover == FALSE & input$select_map_level == 'Ward'){
        label="event19b"
        proxy <- leafletProxy(
            "map1", data = gis$shp
        )
        proxy %>%
            clearShapes() %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
#                group='gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.5,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$MUNICNAME,
                                "<br />",
                                "<strong> Ward </strong>",
                                gis$shp@data$WARDNO,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    
    # create the output info to be dispalyed if 'hover' is selected
    output$hoverInfo <- renderUI({
        if (is.null(gis$mouseover_id) & gis$mouse_events == 0) {
            return(
                div(
                    paste("Hover over a", input$select_map_level)
                ))
        } else if (input$select_map_level == 'Ward'){
            return(
                div(
                    strong(
                        sub(
                            pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                            replacement="",
                            x=gis$single@data$MUNICNAME)),
                    br(),
                    "Ward",
                    gis$shp@data$WARDNO,
                    br(),
                    span(round(gis$single@data$DENSITY,1), HTML("people/km<sup>2</sup>"))
                )
            )
        } else if (input$select_map_level == 'Municipality'){
            return(
                div(
                    strong(sub(
                        pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                        replacement="",
                        x=gis$single@data$MUNICNAME)
                        ),
                    br(),
                    span(round(gis$single@data$DENSITY,1), HTML("people/km<sup>2</sup>"))
                )
            )
        } else if (input$select_map_level == 'Province'){
            return(
                div(
                    strong(gis$single@data$PROVINCE),
                    br(),
                    span(round(gis$single@data$DENSITY,1), HTML("people/km<sup>2</sup>"))
                )
            )
        }
    })
    observe(if (input$select_map_level == 'Ward' & (!is.null(gis$click))){
        output$table_subset <- DT::renderDataTable(
            expr = gis$shp@data[gis$shp@data$ID == input$map1_shape_click$id,c(4,5,12)] %>%
                mutate(
                    Municipality=sub(
                        pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                        replacement="",
                        x=MUNICNAME
                    ),
                    Density = round(DENSITY,1)
                ) %>%
                rename(Ward=WARDNO) %>%
                select(Municipality,Ward,Density),
            options = list(
                dom = "t",
                pageLength=-1
            )
        )
    })
    observe(if (input$select_map_level == 'Municipality'){# & (!is.null(gis$click))){
        output$table_subset <- DT::renderDataTable(
            extensions = 'Scroller',
            expr = ward_tj_properties[ward_tj_properties$MUNICNAME == gis$shp@data$MUNICNAME[gis$shp@data$ID == input$map1_shape_click$id],] %>%
                mutate(
                    Municipality=sub(
                        pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                        replacement="",
                        x=MUNICNAME
                    ),
                    Density = round(DENSITY,1)
                ) %>%
                rename(Ward=WARDNO) %>%
                select(Municipality,Ward,Density),
            options = list(
                deferRender = TRUE,
                dom = "ti",
                scrollY = 400,
                scrollCollapse = TRUE,
                pageLength=-1
            )
        )
    })
    observe(if (input$select_map_level == 'Province'){#& (!is.null(gis$click))){
        output$table_subset <- DT::renderDataTable(
            extensions = 'Scroller',
            expr = town_tj_properties[town_tj_properties$PROVINCE == gis$shp@data$PROVINCE[gis$shp@data$ID == input$map1_shape_click$id],] %>%
                mutate(
                    Municipality=sub(
                        pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                        replacement="",
                        x=MUNICNAME
                    ),
                    Density = round(DENSITY,1)
                ) %>%
                select(Municipality,Density),
            options = list(
                deferRender = TRUE,
                dom = "ti",
                scrollY = 400,
                scrollCollapse = TRUE,
                pageLength=-1
            )
        )
    })
    
#     observe(if (input$select_map_level == 'Ward'){
#         output$table_subset <- DT::renderDataTable(
#             expr = t(gis$shp@data), server=T,
#             options = list(pageLength = 15, dom = 'tip')
#         )
#     })
#     observe(if (input$select_map_level == 'Municipality'){
#         output$table_subset <- DT::renderDataTable(
#             expr = t(gis$shp@data[gis$shp@data$MUNICNAME == gis$id,]),#server=T
#             options = list(pageLength = 15, dom = 'tip')
#         )
#     })
#     observe(if (input$select_map_level == 'Province'){
#         output$table_subset <- DT::renderDataTable(
#             expr = t(gis$shp@data[gis$shp@data$PROVINCE == gis$id,]), #server=T
#             options = list(pageLength = 15, dom = 'tip')
#         )
#     })
    
    #the tables for the second tabPanel (which is a navbarMenu) 
    output$table_ward <- DT::renderDataTable(
        ward_tj_properties, server=T,
        options=list(pageLength = 50)
    )
    output$table_town <- DT::renderDataTable(
        town_tj_properties,
        server=T,options=list(pageLength=50)
    )
    output$table_province <- DT::renderDataTable(
        province_tj_properties,
        server=T
    )
}

shinyApp(ui,server)
