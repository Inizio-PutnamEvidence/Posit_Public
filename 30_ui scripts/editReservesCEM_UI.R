#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for edit CEM Reservations tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Page for CEM Reservations
# 
#--------------------------------------------------------------------------------------------------------



editReservesCEM_Tab <-
  
  tabPanel("CEM reservations", 
           value = "ReservesCEM",
           
           tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),
           tags$head(tags$style(".datatables .display {margin-left: 0;}")),
           
           mainPanel(
             width = 12,
             h3("Edit - CEM reservations", 
                style = "padding: 10px; color: white; background-color: #451284; text-indent: 20px; border-radius: 5px;"),
             
             h3("CEM reservations"),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_reservesCEM", width = "100%"), 
                          style = "font-size: 98%; width: 98%"))
             ), 
             
             h3("Added / Edited Row"),
             fluidRow(
               column(12,
                      actionButton("recoverCEM", "Recover Saved Data", icon = icon("download"), class = "btn-success"),
                      div(DT::DTOutput(outputId = "dt_reservesCEM_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
             ),
             
             shiny::includeScript(paste0(folder_form,"/script.js"))
             ) 
           )



## END
