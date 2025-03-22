#install.packages("shinyWidgets")
#install.packages("shinycssloaders")

library(shiny)
library(readr)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)


# Cargar la base de datos
Senotherapeutics <- read.csv("data/New_Senotherapeutics_Desmol.csv")

# Codificar imágenes a base64
logo_data <- base64enc::dataURI(file = "www/logo_INGER.jpg", mime = "image/jpeg") 
venn_data <- base64enc::dataURI(file = "www/Graphical_Abstract_Pic.jpg", mime = "image/jpeg") # Ejemplo si el nombre real tiene mayúsculas

# Definir la UI
ui <- navbarPage(
  title = div(
    img(src = logo_data, height = "50px", 
        style = "margin-right:10px; padding-bottom:5px;"),
    "Senotherapeutics ML Database"
  ),
  theme = shinytheme("flatly"),
  header = tags$head(
    tags$style(HTML("
      .navbar {font-size: 16px;}
      .dataTables_wrapper {font-size: 14px;}
      .main-header .logo {font-weight: bold; font-size: 20px;}
      .info-box {padding: 20px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 20px;}
      .venn-container {text-align: center; margin-top: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;}
      .instruction-box {background-color: #e9f5ff; padding: 15px; border-left: 4px solid #3498db; margin-bottom: 20px;}
      .custom-btn {background-color: #3498db !important; color: white !important; border: none;}
      .custom-btn:hover {background-color: #2980b9 !important;}
    "))
  ),
  
  tabPanel("Search Compound",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Compound Search", style = "color: #2c3e50; margin-bottom: 20px;"),
               searchInput(
                 inputId = "search",
                 label = "Enter compound name:",
                 placeholder = "e.g., Catechin",
                 btnSearch = icon("search"),
                 btnReset = icon("remove"),
                 width = "100%"
               ),
               tags$hr(),
               h5("Search Tips:", style = "color: #3498db;"),
               tags$ul(
                 tags$li("Use partial names for broader results"),
                 tags$li("Case-insensitive search"),
                 tags$li("Check spelling variations")
               )
             ),
             
             mainPanel(
               width = 9,
               div(class = "instruction-box",
                   h4(icon("info-circle"), "How to use:"),
                   HTML("Search for compounds using their PubChem database name. Results will show in the interactive table below.")
               ),
               div(class = "venn-container",
                   img(src = venn_data, height = "400px", 
                       style = "max-width: 80%; height: auto;")
               ),
               DTOutput("search_results") %>% 
                 withSpinner(type = 6, color = "#3498db")
             )
           )
  ),
  
  tabPanel("Full Database",
           fluidRow(
             column(12,
                    div(style = "margin: 10px;",
                        downloadButton("download_data", "Download Full Data", 
                                       class = "custom-btn"),
                        tags$hr(),
                        DTOutput("full_table") %>% 
                          withSpinner(type = 6, color = "#3498db")
                    )
             )
           )
  ),
  
  tabPanel("About",
           fluidRow(
             column(8, offset = 2,
                    div(class = "info-box",
                        h3(icon("flask"), "Project Overview", style = "color: #2c3e50;"),
                        tags$hr(),
                        HTML("<p style='font-size:16px; text-align: justify;'>
                             This project combines <strong>network pharmacology</strong> and <strong>machine learning</strong> 
                             to identify potential senotherapeutic compounds. Our methodology includes:
                             </p>
                             <ul>
                             <li>Analysis of 65,339 compounds interacting with senescence-related proteins</li>
                             <li>Machine learning classification using Random Forest, SVM, and KNN models</li>
                             <li>Cheminformatics analysis for pharmacokinetic property evaluation</li>
                             <li>Identification of 269 compounds with optimal drug-like properties</li>
                             <li>49 FDA-approved drugs flagged for potential repurposing</li>
                             </ul>
                             
                             <p style='font-size:16px; text-align: justify;'>
                             This computational approach accelerates drug discovery by prioritizing 
                             high-potential candidates for experimental validation, significantly 
                             reducing time and resource requirements.
                             </p>"),
                        tags$hr(),
                        h4("Key Features:", style = "color: #3498db;"),
                        fluidRow(
                          column(6,
                                 div(style = "padding:10px;",
                                     h5(icon("database"), "Data Resources"),
                                     tags$ul(
                                       tags$li("PubChem Compound Database"),
                                       tags$li("Protein Interaction Networks"),
                                       tags$li("FDA Drug Registry")
                                     )
                                 )
                          ),
                          column(6,
                                 div(style = "padding:10px;",
                                     h5(icon("chart-line"), "Analytics"),
                                     tags$ul(
                                       tags$li("Machine Learning Classification"),
                                       tags$li("Network Pharmacology Analysis"),
                                       tags$li("ADMET Property Prediction")
                                     )
                                 )
                          )
                        )
                    )
             )
           )
  )
)






# Definir el servidor
server <- function(input, output, session) {
  
  search_results <- reactive({
    req(input$search)
    Senotherapeutics %>%
      filter(grepl(toupper(input$search), toupper(Name)))
  })
  
  output$search_results <- renderDT({
    datatable(search_results(),
              options = list(
                pageLength = 5,
                autoWidth = TRUE,
                dom = 'ltipr',
                scrollX = TRUE
              ),
              rownames = FALSE,
              class = 'hover stripe') %>%
      formatStyle(columns = names(Senotherapeutics), 
                  fontSize = '14px')
  })
  
  output$full_table <- renderDT({
    datatable(Senotherapeutics,
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf'),
                scrollX = TRUE
              ),
              rownames = FALSE,
              class = 'hover stripe') %>%
      formatStyle(columns = names(Senotherapeutics), 
                  fontSize = '14px')
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Senotherapeutics_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Senotherapeutics, file, row.names = FALSE)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
