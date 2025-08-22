#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for QC tab to see Comments from View Tab users
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  30 May 2024
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Page for QC
# 
#--------------------------------------------------------------------------------------------------------



qcCommView_Tab <-
  
  tabPanel("QC Comments View",
           value = "qc_commView",
           
           tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),
    
           mainPanel(
             width = 12,
             h3("Comments View: QC waiting", 
                style = "padding: 10px; color: white; background-color: #451284; text-indent: 20px; border-radius: 5px;"),
             
             strong(em("Here are the comments left by database users in the View tab.")),
             br(),
             
             h3("QC - Comments from View"),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_commView_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
               )
             )
           ) 



## END
