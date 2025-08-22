#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               ShinyApp HTA db 
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    14 May 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of structure of the Shiny app
# 
#--------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------#
####        Library & Source (script.R)                               ####  
#------------------------------------------------------------------------#
{
  
  rm(list = ls())
  
  # On Shiny - DEV server ....
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(shinydashboard)
  library(tidyverse)
  library(XML)
  library(rvest)
  library(httr)
  library(DT)
  library(readxl)
  library(graphics)
  library(readxlsb)
  library(writexl)
  library(reactable)
  library(htmltools)
  library(shinyTree)
  library(shinyfullscreen)
  library(shinyalert)
  library(shinymanager)
  library(scales)
  library(webshot2)
  library(chromote)

  source("functions.R")
  source("global_environment.R")
  source(paste0(folder_import,"/final_dataset.R"))
  source("global_environment.R")

}

#------------------------------------------------------------------------#
####        Creation of account                                       ####  
#------------------------------------------------------------------------#

credentials <- data.frame(
   user = c("charlenetournier","nicolasvirely","olfadoghri","marinesivignon","romainsupiot","rahmasellami","yosraboukhris","Putnam"), # mandatory
  password = c("cc11","nico02","olfa002","marine100","rom200","rahma03","yosra111",""), # mandatory
  # start = c("2019-04-15"), # optimal (all others)
  expire = rep(NA,8),
  admin = c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
  comment = "Simple and secure authentification mechanism for single Shiny applications.",
  stringsAsFactors = FALSE
)


#------------------------------------------------------------------------#
####        User interface for the Shiny app                          ####  
#------------------------------------------------------------------------#
{
  
  # Source files with UI code for each tab 
  source(file.path(paste0(folder_ui,"/editHAS_UI.R")),         local = TRUE)$value
  source(file.path(paste0(folder_ui,"/editReservesBIM_UI.R")), local = TRUE)$value
  source(file.path(paste0(folder_ui,"/editReservesCEM_UI.R")), local = TRUE)$value
  source(file.path(paste0(folder_ui,"/editTableAssoc_UI.R")),  local = TRUE)$value
  source(file.path(paste0(folder_ui,"/home_UI.R")),            local = TRUE)$value
  source(file.path(paste0(folder_ui,"/qc_UI.R")),              local = TRUE)$value
  source(file.path(paste0(folder_ui,"/qcCommView_UI.R")),      local = TRUE)$value
  source(file.path(paste0(folder_ui,"/view_UI.R")),            local = TRUE)$value
  
  # Define the UI structure 
  ui <-
    tagList(
      useShinyjs(), # Include shinyjs
      
      # authentication module
      auth_ui(
        id = "auth",
        tags_top = 
          tags$div(
            tags$img(
              # I would like to change this picture to a picture from my www/ folder
              src = "Putnam_Mark_Standard_Col.png", width = 200
            ),
            tags$p(strong("Guest Mode - "), "Username: Putnam - no password", style = "align:center" ),
          ),
        
        tags_bottom = tags$div(
          tags$p(
            "For any question, please contact ",
            tags$a(
              href = "mailto:charlene.tournier@putassoc.com?Subject=Shiny%20aManager",
              target = "_top", "administrator"
            )
          )
        ),
        background  = "url('Putnam_Zoom_Background_Dark3.jpg')", 
      ),
      
      actionButton("action_logout", "Log Out", icon = icon("user"),
                   style = "position: absolute; top: 5px; right: 5px; z-index:10000;"),
      
      navbarPage(id = "tabs", 
        
        # add PHS logo to navigation bar
        title = div(style = "position: relative;
                             top: -15px;
                             margin-left: 10px;
                             margin-top: 5px;",
                    tags$a(img(src = "Putnam_Mark_Standard_Col.png",
                               width = 120,
                               alt = "link to Putnam website"),
                           href = "https://www.putassoc.com/",
                           target = "_blank")
        ),
        
        # make navigation bar collapse on smaller screens
        windowTitle = "CEESP_application",
        collapsible = TRUE,
        
        header = tags$head(
          # sourcing css style sheet
          includeCSS("www/styles.css"),
          
          # include putnam icon in web browser
          HTML("<html lang='en'>"),
          tags$link(rel = "shortcut icon",
                    href = "PLetter_Col.png"),
        ),  
        
        # order of tabs
        homeTab,
        viewTab,
        # graphTab,
        navbarMenu("Edit", editHTA_Tab, editReservesCEM_Tab, editReservesBIM_Tab, editTableAssoc_Tab, icon = icon("pen-to-square")),
        navbarMenu("QC", qcTab, qcCommView_Tab, icon = icon("check-double")),
      )
    )

}

#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
{
  
  server <- function(input, output, session) {
    # authentication module
    res_auth <- callModule(
      module = auth_server,
      id = "auth",
      check_credentials = check_credentials(credentials)
    )
    
    observeEvent(input$action_logout, {
      session$reload()
    })
    
    observe({
      req(reactiveValuesToList(res_auth)$user)
      if(reactiveValuesToList(res_auth)$user %in% c("Putnam")){
        hideTab(inputId = "tabs", target = "Edit")
        hideTab(inputId = "tabs", target = "QC")
      }
      if(reactiveValuesToList(res_auth)$user %in% c("nicolasvirely","olfadoghri","marinesivignon","romainsupiot","rahmasellami","yosraboukhris")){
        hideTab(inputId = "tabs", target = "Edit")
        hideTab(inputId = "tabs", target = "QC")
        shiny::modalDialog(
            title = "Information",
            p("Edit and Quality Control tabs are currently updating."),
            size = "m",
            easyClose = FALSE,
            footer = shiny::actionButton(inputId = "ok", label = "Ok", class = "btn-info")
          ) %>% shiny::showModal()
      }
      
      # Source file to import all buttons
      source("create_btns.R", local = TRUE)$value 
      
      # Source files with server code for each tab
      df_form <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
      source(file.path(paste0(folder_server,"/editHAS_Server.R")),        local = TRUE)$value
      source(file.path(paste0(folder_server,"/editReservesBIM_Server.R")),local = TRUE)$value
      source(file.path(paste0(folder_server,"/editReservesCEM_Server.R")),local = TRUE)$value
      source(file.path(paste0(folder_server,"/editTableAssoc_Server.R")), local = TRUE)$value
      source(file.path(paste0(folder_server,"/qc_Server.R")),             local = TRUE)$value
      source(file.path(paste0(folder_server,"/qcCommView_Server.R")),     local = TRUE)$value
      source(file.path(paste0(folder_server,"/view_Server.R")),           local = TRUE)$value

    })
    
    
  }
  
}

#------------------------------------------------------------------------#
####        Run the application                                       ####  
#------------------------------------------------------------------------#

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
