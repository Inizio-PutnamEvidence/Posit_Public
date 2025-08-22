#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for edit Efficiency Opinions tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Page for Efficiency Opinions
# 
#--------------------------------------------------------------------------------------------------------



editHTA_Tab <-
  
  tabPanel("Efficiency opinions", 
           value = "ExtractionHAS",
           
           tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),
           
           mainPanel(
             width = 12,
             h3("Edit - Efficiency opinions", 
                style = "padding: 10px; color: white; background-color: #451284; text-indent: 20px; border-radius: 5px;"),

             h3("Efficiency opinions"),
             fluidRow(  
               column(12, 
                      actionButton("add_row", "Add Row", icon = icon("plus"), class = "btn-success"), 
                      div(DT::DTOutput(outputId = "dt_table", width = "100%"), 
                          style = "font-size: 98%; width: 98%"))
             ), 
                    
             h3("Added / Edited Row"),
             strong(em("Avoid storing too many new lines / modified lines in this table, and send them to the QC when they are ready.")), 
             br(), 
             br(),
             fluidRow( 
               column(12,
                      actionButton("recover", "Recover Saved Data", icon = icon("download"), class = "btn-success"),
                      div(DT::DTOutput(outputId = "dt_table_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%")),
             ),
             
             shiny::includeScript(paste0(folder_form,"/script.js"))
             )
           ) 



## END
