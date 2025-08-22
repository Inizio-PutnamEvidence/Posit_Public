#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for QC tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Page for QC
# 
#--------------------------------------------------------------------------------------------------------



qcTab <-
  
  tabPanel("QC Edited/New line",
           value = "qc_edit",
           
           tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),
    
           mainPanel(
             width = 12,
             h3("Edited/New line: QC waiting", 
                style = "padding: 10px; color: white; background-color: #451284; text-indent: 20px; border-radius: 5px;"),
             strong(em("The new lines added must be QC and validated in order to avoid having problems with the identifiers afterwards.")),
             br(),
             
             h3("QC - Efficiency opinions"),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_HAS_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
               ),
             
             h3("QC - CEM reservations"),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_resCEM_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
               ),     
             
             h3("QC - BIM reservations"),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_resBIM_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
               ),
             
             h3("QC - Combination products"),
             strong(em("You must validate the information from efficiency opinions before validating the associated lines from combination products.")),
             fluidRow(
               column(12,
                      div(DT::DTOutput(outputId = "dt_assoc_qc", width = "100%"),
                          style = "font-size: 98%; width: 98%"))
               )
             )
           ) 



## END
