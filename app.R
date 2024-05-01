library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(purrr)
library(sf)
library(leaflet)

#-- Load data (can't use project working directory shortcut)
# Just a single layer to initially get the code working
# reproject to datum = WGS84
#Hab_lay <- st_transform(Hab_lay, crs = "+proj=longlat")
#-- Prep data
#HarvestYear_choices <- sort(unique(Hab_lay$HARVEST_YEAR[!is.na(Hab_lay$HARVEST_YEAR)]))


# We actually want to load multiple TSAs
# AOI's
#Hab_lay_options <- c("---","Lakes TSA","Kispiox TSA", "Bulkley TSA", "Morice TSA")
Hab_lay_options <- c("---","Kispiox TSA", "Bulkley TSA")
#Hab_lay_options <- c("Lakes TSA")

Hab_lay <- list(
  #Lakes <- readRDS("SkWERM_Lakes TSA_forSelection.rds"),
  Kispiox <- readRDS("data/SkWerm_Kispiox TSA_forSelection.rds"),
  Bulkley <- readRDS("data/SkWerm_Bulkley TSA_forSelection.rds")#,
  #Morice <- readRDS("SkWERM_Morice TSA_forSelection.rds")
)
# Define names for the list elements
#names(Hab_lay) <- Hab_lay_options[2:5]
names(Hab_lay) <- Hab_lay_options[2:3]

#-- Prep data -------------------------------------------------------------------------------------
#polygon type
#sec_beu <- 0 #only simple polygons
#minNumPolys_ByEcoType <- 100
#numSamples <- 75

#BEU-BEC of interest in 2023
BEU_BEC_ofInt <- c("SL_SBSdk", "WL_SBSdk", "WR_SBSdk", "SF_SBSmc2", "SL_SBSmc2", #(moose & bear)
                   "WL_SBSmc2", "WR_SBSmc2","FR_CWHws2","CS_ICHmc1", "CS_ICHmc2", #(moose & bear)
                   "RR_ICHmc1","RR_ICHmc2", "SF_ICHmc1","WL_ICHmc1", "WL_ICHmc2", #(moose & bear)
                   "SF_SBSwk3", #(moose)
                   "CW_CWHws2", "WL_CWHws2", "IS_ICHmc1", "WR_ICHmc2","EF_ESSFmc", #(bear)
                   "EF_ESSFmc", "ER_ESSFmc", "EF_ESSFmv1", "EF_ESSFmv3", "EW_ESSFmk", #(bear)
                   "EW_ESSFwv")

# harvests
# Extract all HARVEST_YEAR values from all spatial objects and combine into a single vector
#harvest_years <- unlist(lapply(Hab_lay, function(x) x$HARVEST_YR_ccb))

#HarvestYear_choices <- sort(unique(harvest_years[!is.na(harvest_years)]))

# Get min and max HARVEST_YEAR
harv_yr_min <- min(unlist(lapply(Hab_lay, function(x) min(x$HARVEST_YR_ccb, na.rm = TRUE))))
harv_yr_max <- round(max(unlist(lapply(Hab_lay, function(x) max(x$HARVEST_YR_ccb, na.rm = TRUE)))),0)


# Get min and max fire year
fire_yr_min <- min(unlist(lapply(Hab_lay, function(x) min(x$FIRE_YEAR, na.rm = TRUE))))
fire_yr_max <- round(max(unlist(lapply(Hab_lay, function(x) max(x$FIRE_YEAR, na.rm = TRUE)))),0)

#themes:
#HEX: #330066 (deep purple), #CCCC00 (yellowy-green), #669999 (teal-grey), #CCCCCC (grey), #000000 (black)
fsd_theme <- bslib::bs_theme(bg = "#e0e0e0", fg = "#000000",
                             primary = "#669999", secondary = "#330066",
                base_font = bslib::font_google("DM Sans"),
                heading_font = bslib::font_google("DM Serif Display"))

#road distances:
rd_dist_min <- min(unlist(lapply(Hab_lay, function(x) min(x$Road_dist))), na.rm = TRUE)
rd_dist_max <- round(max(unlist(lapply(Hab_lay, function(x) max(x$Road_dist))), na.rm = TRUE),0)

FN_bounds <- c("---","Gitanyow","WFN","Gitxan","OW","Cheslatta","LBN")
#-- App
ui <- fluidPage(
  theme = fsd_theme,
  br(),
  div(
    style = "background-color: #000000; color: #ffffff; padding: 10px; font-size: 36px;
    font-family: 'DM Serif Display', serif;",
    "Site Selection Tool: Validating SkWERM
    (Skeena Wildlife Ecological Resource Model)"
  ),
  #titlePanel("SkWERMevaluate Shiny App"),
  br(),
  p("Select from the various options to refine what kind and how many random samples
  to take from all the different forest polygons within a given area of interest that
    represents different Grizzly bear and moose habitat values"),
  helpText("Please provide feedback on what worked and what didn't in using this app
  to:"),
  br(),
  br(),
  # Options for filtering available polygons to select for sampling
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h2("5. How Many Polygons"),
      # minimum required number of polygons?
      p("After all the selections are done (left), (A) choose how many potential polygons
        an ecotype should have in order to randomly select polygons. And then (B) choose
        how many polygons of each remaining ecotype to randomly select (sample size). Once
        all this is done, hit the 'Apply Filter' button and your samples will be drawn "),
      br(),
      #h2("3. Minimum Polygons Required Per Ecotype"),
      sliderInput("min_polys", label = h4("A. Minimim number of polygons required in each ecotype"),
                   value = 50,
                   min = 0,
                   max = 100),
       helpText("In order to randomly select polygons, you can set the minimum sample size in
               each ecotype. Any ecotypes that do not meet this minimum, will be removed.
               Setting this value to 0 means that no ecotypes will be rejected just because
               they do not meet the minimum number of samples. Setting the value greater
               than the sample number is encouraged to ensure random selection of polygons
               is possible"),


      # select number of random samples
      br(),
      h4("B. Number of samples to randomly select from each ecotype"),
      helpText("How many samples of a given ecotype would you like to randomly select?"),
      numericInput("num_samps", label = "Number of samples",
                   value = 0),
      br(),
      br(),
      actionButton("btnApplyFilter", "Apply Filter"),
      textOutput("statusText"),
      br(),
      br(),
      p("Download your filtered site selection polygons."),

      # Download user filtered data
      downloadButton("downloadCSV", "Download filtered CSV"),
      downloadButton("downloadGPKG", "Download filtered GeoPackage")

    ),
  # Show a map of the area
  mainPanel(
    fluidRow(
      h2("1. Select Area of Interest"),
      br(),
      column(width = 6,
             selectInput("hablay",
                  label = h4("Select Timber Supply Area"),
                  choices = Hab_lay_options)
      ),
      p("OR"),
      column(width = 6,
             # select FN boundaries instead of TSA
             selectInput("FNbound",
                       label = h4("Select First Nations territorial boundary"),
                       choices = FN_bounds),
             #helpText("")
      ),
      column(width = 6,
      # User uploaded boundary data
      fileInput(inputId = "upload",
                label = "Upload spatial boundary (optional)",
                accept = c(".shp",".dbf",".sbn","sbx",".shx",".prj",
                           ".gpkg", ".kml"), multiple = TRUE),
      helpText("Upload spatial data. If uploading a shapefile, make sure to upload at least the
               .shp, .sbf, and .shx in order for it to work. .gpkg and .kml are also accepted.")
      )
    ),
    br(),
    h2("2. Habitat Scores to Sample"),
    br(),
    fluidRow(
    p("Decide what values of habitat to include in sampling. Selecting a value of 1 for either
     Grizzly bears or moose would mean you only want to visit sample the best habitats (1).
     Selecting a value of 5 would mean you would sample across the whole range from worst (5)
       to best (1) habitats"),
      column(width = 6,
             numericInput("min_bear", label = h4("Grizzly bear habitat scores"),
                          value = 1,
                          min = 1,
                          max = 5),

             checkboxGroupInput("InclHibernation",
                                label = "Include hibernation in best habitat score?",
                                choices = c("Yes")),
             helpText("Decide whether to include hibernation habitat scores in calculating
                 the best (lowest) habitat score for Grizzly bears."),

             conditionalPanel(
               condition = "input.InclHibernation.includes('Include hibernation')"
             )
      ),
      br(),
      column(width = 6,
             numericInput("min_moose", label = h4("Moose habitat scores"),
                          value = 1,
                          min = 1,
                          max = 5),
      )
    ),
    br(),
    h2("3. Optional Selection Features"),
    br(),
    fluidRow(
      p("Set the maximum distance potential sampling polygons can be
                      from any road."),
      column(width = 12,
             sliderInput("RoadDist",
                         label = h4("Distance to road (km)"),
                         min = rd_dist_min,
                         max = rd_dist_max, # make sure this is the maximum for all of the Hab_lay options
                         value = 1)
             )
    ),
    br(),
    br(),
    p("Decide whether sampled polygons can have a history of fire or harvest:"),
    fluidRow(
      column(width = 6,
             checkboxGroupInput("HarvestChoice",
                                label = h4("Include polygons with forest harvesting history"),
                                choices = c("Yes", "No"),
                                selected = c("No")),



             conditionalPanel(
               condition = "input.HarvestChoice.includes('Yes')",

               #helpText("Select the range of years harvesting."),

               sliderInput("HarvestYearRange",
                           label = "Select Range of Years",
                           min = harv_yr_min,
                           max = harv_yr_max,
                           value = c(1950,1980),
                           step = 1)
             )
             ),
      br(),
      column(width = 6,
             checkboxGroupInput("FireChoice",
                                label = h4("Include polygons that fall within historic fire
                                           boundaries?"),
                                choices = c("Yes","No"),
                                selected = c("No")),

             #helpText("If harvested, choose the range of years"),

             conditionalPanel(
               condition = "input.FireChoice.includes('Yes')",

               #helpText("Select the range of years where fires burned"),

               sliderInput("FireYearRange",
                           label = "Select Range of Years",
                           min = fire_yr_min,
                           max = fire_yr_max,
                           value = c(1950,1980),
                           step = 1)
             )
      )
    ),


    h2("4. BEU and BEC zone combinations"),
    p("Select which Broad Ecosystem Unit (BEU) and BEC zone combinations are of interest for
      sampling)"),
     # update the BEU-BEC combinations of interest
     br(),
    fluidRow(
      column(width = 6,
             textInput("BEU_BEC_text", label = h4("Add additional BEU/BEC combos of interest"),
                       value = ""),
             helpText("These must be valid BEU/BEC codes"),
     #column(width = 12,
      #      textInput("BEU_BEC_text", label = h4("Add additional BEU/BEC combos of interest"),
       #               value = ""),
        #    helpText("These must be valid BEU/BEC codes")
     #),
    br(),
     actionButton("btnRevealBEU", "Current BEU_BEC"),
     verbatimTextOutput("BEUvec"),
          ),
    br(),

      # simple or complex polygons?
     column(width = 6,
            checkboxGroupInput("simp_polys", label = "Include complex polygons?",
                               choices = c("No"),
                               selected = c("No")),
            helpText("The Broad Ecosystem Units can sometimes have multiple different
                     types associated with a single polygon. You can choose to sample only
                     simple polygons, which means only the polygons that have a single
                     BEU type (default), or you can include all polygons, no matter how
                     subdivided (unclick yes)"),
            br(),
     )
    ),
    br(),
    br(),

    h2("Results of Sample Selection"),
    br(),
    h4("Number samples selected by ecotype"),
    helpText("Number of potential (N) and selected (Samples) polygons by ecotype,
             [Blank until after selection complete]"),
    dataTableOutput("sum_tab"),

    br(),
    br(),
    h4("Map of your area of interest"),
    helpText("Polygons will display after filtering"),
    leafletOutput("map"),
    br(),
    verbatimTextOutput("selectedRange"),
    br(),
    h4("Filtered data based on your selections"),
    helpText("A preview of selected polygons,
             [Blank until after selection complete]"),
    dataTableOutput("table")

    #withSpinner(dataTableOutput("table"))

    )
  )
)




# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  thematic::thematic_shiny()

  # Reactive to load user-selected AOI spatial data
  selected_hablay <- reactive({

    if(input$hablay != "---"){
      req(input$hablay)  # Require selection of AOI
      #if a TSA is selected, use that
      hablay <- Hab_lay[[input$hablay]]

      # Reproject if necessary
      #if(st_crs(hablay)$proj4string != "+proj=longlat") {
       # hablay <- st_transform(hablay, crs = "+proj=longlat")
      #}

      return(hablay)

    }else if(input$FNbound != "---"){
      req(input$FNbound)
      #use selected FN boundary
      hablay <- lapply(Hab_lay, function(tsa_lay) {

        tsa_lay <- tsa_lay %>%
          filter(FN_bound == input$FNbound) # specify your condition here

      })
      # combine
      combined_hablay <- do.call(rbind, hablay)
      rownames(combined_hablay) <- NULL

      return(combined_hablay)

    }else{
      #showNotification("You must select one TSA boundary or one First Nations boundary",
       #                type = "error")

      return(NULL)
    }

  })

   # Reactive to load user-uploaded spatial data
  boundary <- reactive({
    if (!is.null(input$upload)) {
      shpdf <- input$upload

      # Name of the temporary directory where files are uploaded
      tempdirname <- dirname(shpdf$datapath[1])

      # Rename files
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }

      # Extract file extension
      file_ext <- tools::file_ext(shpdf$name[1])

      # Read the file based on the extension
      if (file_ext == "shp") {
        # Search for shapefile components
        shp_files <- grep("\\.shp$|\\.dbf$|\\.sbn$|\\.sbx$|\\.shx$", shpdf$name, value = TRUE)
        if (length(shp_files) != 5) {
          stop("Expected all shapefile components.")
        }
        boundary_data <- sf::st_read(file.path(tempdirname, shp_files[1]))
      } else {
        # For other formats, use the first file
        boundary_data <- sf::st_read(file.path(tempdirname, shpdf$name[1]))
      }

      return(boundary_data)
    }
    return(NULL)
  })

  # Filtered data based on user selection and boundary
  #filtered_data <- reactive({
   # req(selected_hablay())
    #filtered <- selected_hablay
    #})
      #reactive({
    #req(selected_hablay())  # Require selected_hablay to be available
    # filtered <- selected_hablay()
  filtered_data <- reactiveValues(data = NULL)

  observeEvent(input$btnApplyFilter, {

    #browser()
    #req(selected_hablay())
    if (is.null(selected_hablay()) || nrow(selected_hablay()) == 0) {
      showNotification("You must select one TSA boundary or one First Nations boundary", type = "error")
      #stop("Invalid selection: Please select either a TSA or a FN boundary.")
      return()  # Exit the observeEvent early
    }

    tryCatch({

      #showNotification("Please wait while these selections are processing",
       #                type = "message")
      req(selected_hablay())


      filtered <- selected_hablay()
      if (!is.null(boundary())) {
        filtered <- sf::st_intersection(filtered, boundary())
      }

      # Distance to road
      #browser()
      filtered <- subset(filtered, Road_dist <= input$RoadDist)

      # BEU_BEC of interest
      #BEU_BEC_ofInt_up <- c(BEU_BEC_ofInt, input$BEU_BEC_text)
      #filtered <- subset(filtered, BEU_BEC %in% BEU_BEC_ofInt_up)
      filtered <- subset(filtered, BEU_BEC %in% BEUvec_store$data)
      #simple or complex polygons
      if("No" %in% input$simp_polys){
        filtered <- subset(filtered, SDEC == 10)
      } else{
        filtered <- filtered
      }


      # Harvested or not harvested
      if ("Yes" %in% input$HarvestChoice && "No" %in% input$HarvestChoice) {
        if (length(input$HarvestYearRange) == 2) {
          filtered <- subset(filtered, (is.na(HARVEST_YR_ccb) | HARVEST_YR_ccb >= input$HarvestYearRange[1] &
                                          HARVEST_YR_ccb <= input$HarvestYearRange[2]))
        }
      } else if ("Yes" %in% input$HarvestChoice) {
        filtered <- subset(filtered, !is.na(HARVEST_YR_ccb) | HARVEST_YR_ccb >= input$HarvestYearRange[1] &
                             HARVEST_YR_ccb <= input$HarvestYearRange[2])
      } else if ("No" %in% input$HarvestChoice) {
        filtered <- subset(filtered, is.na(HARVEST_YR_ccb))
      }

      #Fire or no fire
      if ("Yes" %in% input$FireChoice && "No" %in% input$FireChoice) {
        if (length(input$FireYearRange) == 2) {
          filtered <- subset(filtered, (is.na(FIRE_YEAR) | FIRE_YEAR >= input$FireYearRange[1] &
                                          FIRE_YEAR <= input$FireYearRange[2]))
        }
      } else if ("Yes" %in% input$FireChoice) {
        filtered <- subset(filtered, !is.na(FIRE_YEAR) | FIRE_YEAR >= input$FireYearRange[1] &
                             FIRE_YEAR <= input$FireYearRange[2])
      } else if ("No" %in% input$FireChoice) {
        filtered <- subset(filtered, is.na(FIRE_YEAR))
      }

      # minimum habitat score
      if("Yes" %in% input$InclHibernation) {
        filtered <- subset(filtered, murar_hab_min_hib <= input$min_bear)
        filtered <- subset(filtered, malan_hab_min <= input$min_moose)
      }else{
        filtered <- subset(filtered, murar_hab_min <= input$min_bear)
        filtered <- subset(filtered, malan_hab_min <= input$min_moose)
      }

      if(input$min_polys == 0){
        if(input$num_samps == 0){
          showNotification("Please select more than 1 sample for each ecotype (#4)",
                           type = "warning")
        }else{
          # Sample the ecotypes:
        filtered <-  filtered %>%
          group_by(ECO_TYPE) %>%
          #mutate(N = n()) %>%
          mutate(N = min(input$num_samps, n(), na.rm = TRUE)) %>%
          group_split() %>%
          map_df(~ slice_sample(.x, n = min(.x$N, nrow(.x), na.rm = TRUE)))
        }

      }else{
        if(input$num_samps == 0){
          showNotification("Please select more than 1 sample for each ecotype (#4)",
                           type = "warning")
        }else{
        #showNotification("testing if/else behaviour",
         #                type = "message")
        filtered <- filtered %>%
          group_by(ECO_TYPE) %>%
          mutate(N = n()) %>%
          filter(N > input$min_polys) %>%
          mutate(sample_size = min(input$num_samps, n(), na.rm = TRUE)) %>%
          group_split() %>%
          map_df(~ slice_sample(.x, n = min(.x$sample_size, nrow(.x), na.rm = TRUE)))
        }

      }


      if(nrow(filtered) == 0){

        showNotification("No polygons are available for these selections",
                         type = "error")
      }else{


        showNotification("Your polygons should now display and your data available for download",
                         type = "message")

        output$table <- renderDataTable({
          #datatable(filtered_data())
          datatable(filtered, options = list(columnDefs = list(list(visible = TRUE,
                                                                    targets = c(1,2 ,4, 19,24)))))
        })

        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(data = filtered,
                      highlightOptions = highlightOptions(weight = 5,
                                                            color = "#666",
                                                            bringToFront = TRUE))


          summary_samples <- as_tibble(filtered) %>%
            #dplyr::select(ECO_TYPE) %>%
            dplyr::select(-geom) %>%
            group_by(ECO_TYPE,N) %>%
            summarise(samples = n())

          output$sum_tab <- renderDataTable({
             datatable(summary_samples)
          })

        filtered <- filtered %>%
          select(-c("N","sample_size"))
        filtered_data$data <- filtered
        #browser()
        return(filtered)

      }
    },error = function(e){
      showNotification("Error processing data:" + e$message, type = "error")
    })

  })

  BEUvec_store <- reactiveValues(data = NULL)

  observeEvent(input$btnRevealBEU,{
    #browser()
    if (is.null(BEUvec_store$data)) {
      BEUvec_store$data <- BEU_BEC_ofInt
    }else{

      if(nzchar(input$BEU_BEC_text)) {

    BEUvec_store$data <- c(BEUvec_store$data, input$BEU_BEC_text)
      }
    }

    output$BEUvec <- renderPrint({
      BEUvec_store$data
    })

  })

  # Filtered data table - changed to summary table
  #summary_samples <- reactive({
   # filtered <- filtered_data()

    #if(is.null(filtered)){
     # print("no polygons are available for these selections")
    #}

    #sum_tab <- filtered %>%
     # group_by(ECO_TYPE) %>%
      #dplyr::select(., ECO_TYPE, BEU_BEC,STAND,STRCT_mod,CROWN_ALL,DOM_TREE) %>%
      #summarise(samples = n())
  #  return(sum_tab)

  #})

  # Display map
  output$map <- renderLeaflet({

    # Map
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = -127.1743, lat = 54.7804, zoom = 6)


  })



  observeEvent(input$btnApplyFilter, function() {
      leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = filtered_data(),
                  highlightOptions = highlightOptions(weight = 5,
                                                      color = "#666",
                                                      bringToFront = TRUE))

    #output$sum_tab <- renderTable({
     # summary_samples()

   # })


    # Check if boundary data is not NULL
    #if (!is.null(boundary())) {
     # map <- map %>% addPolygons(data = boundary(), color = "red") # Add user-uploaded boundary to map
    #}
  })


  # Download data
  # CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("filtered", ".csv", sep = "")
    },
    content = function(file) {
      fil_no_geo <- st_drop_geometry(filtered_data$data)
      write.csv(fil_no_geo, file, row.names = FALSE)
    }
  )

  # GeoPackage
  output$downloadGPKG <- downloadHandler(
    filename = function() {
      paste("filtered", ".gpkg", sep = "")
    },
    content = function(file) {
      sf::st_write(filtered_data$data, file, driver = "GPKG")
    }
  )
}


# Create Shiny app ----
shinyApp(ui, server)


