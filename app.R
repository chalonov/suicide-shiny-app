library(shiny)
library(shinythemes)
library(shinycssloaders)
library(pacman)


source("./code/libraries.R", encoding = "UTF-8")
source("./code/plotting-maps.R")
source("./code/plotting-maps-cluster.R")


# Define UI for dataset viewer app ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("flatly"),
  
  # App title ----
  titlePanel(title=div(img(src="logo.png", height = 80, style = "float:center;"),align = "center"),
             windowTitle = "ISSAPP"),
  
  tags$head(tags$link(rel = "icon", href = "favicon.ico")), # favicon

  navbarPage(
    title = "ISSAPP",
    
    tabPanel('Inicio',
             
             fluidRow(
               column(8, align="center", offset = 2,
                      
                      includeMarkdown("./docs/home_info.md"),
                      
               ))),
    
    tabPanel('Acerca del proyecto',
             
             fluidRow(
               column(8, align="left", offset = 2,
                      
                      includeMarkdown("./docs/about.md"),
                      
               ))),

    tabPanel('Resultados', 
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(width = 3,
                 
                 radioButtons("inputScope", 
                              "Nivel:", 
                              choices = c("Departamental",
                                          "Municipal")),
                 
                 # Input: Select a dataset ----
                 selectInput("inputEvents",
                             "Seleccione una variable:",
                             choices = c("Suicidio" = "suicidio",
                                         "Intento suicida" = "intento",
                                         "Intoxicación" = "intox",
                                         "Depresión" = "depresion")),
                 
                 radioButtons("inputGroup",
                              "Población:", 
                              choices = c("Total", 
                                          "Mujeres",
                                          "Hombres")),
                 
                 selectInput("inputYear", 
                             "Seleccione un periodo:",
                             choices = c("2009-2018" = 9999,
                                         "2009" = 2009,
                                         "2010" = 2010,
                                         "2011" = 2011,
                                         "2012" = 2012,
                                         "2013" = 2013,
                                         "2014" = 2014,
                                         "2015" = 2015,
                                         "2016" = 2016,
                                         "2017" = 2017,
                                         "2018" = 2018)),
                 
                 #helpText("Periodo: 2009-2018"),
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Header + Mapa
                 #h3("Mapa"),
                 withSpinner(tmapOutput("mapPlot", height = 680),
                             type = 4, color = "#2C3E50", size = 1),

               )
               
             )),
    tabPanel('Clúster',
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(width = 3,
                            
                            # Input: Select a dataset ----
                            selectInput("inputCluster", "Seleccione un evento:",
                                        choices = c("Suicidio" = "suicidio",
                                                    "Intento suicida" = "intento",
                                                    "Intoxicación" = "intox")),
                          
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Header + Mapa
                 #h3("Mapa"),
                 withSpinner(tmapOutput("mapCluster", height = 680),
                             type = 4, color = "#2C3E50", size = 1),
                 
               )
               
             ))
    
  ),
  
  h5("Castro | Novoa - 2023", align = "center"),
  div(em("Powered by:"), 
      img(src = "esri.png", height = 28, style = "float:center;"),
      img(src = "shiny.png", height = 28, style = "float:center;"),
      img(src = "rstudio.png", height = 28, width = 80, style = "float:center;"),
      align = "center"),

)

# -------------------------------SERVER----------------------------------------
server <- function(input, output) {
  
  # Resultados ----------------------------------------------------------------

  output$mapPlot <- renderTmap(map_function(input$inputScope, 
                                            input$inputEvents,
                                            input$inputGroup,
                                            input$inputYear,
                                            input$inputEvents))
  
  # Cluster ----------------------------------------------------------------
  
  output$mapCluster <- renderTmap(map_function_cluster(input$inputCluster))
  
}

# Create Shiny app ----
shinyApp(ui, server)