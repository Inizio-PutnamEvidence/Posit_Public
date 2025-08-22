#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Edit Reserves BIM Form
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  09 August 2023
#   LAST MODIFIED DATE    15 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of a form
# 
#--------------------------------------------------------------------------------------------------------



modal_dialog_reservesBIM <- function(
    # Informations Géénérales
          nom_produit, date_val_CEESP, 
    # Reserves BIM
          BIM_DimHAS, autre_BIM_DimHAS, BIM_DimPutnam, autre_BIM_DimPutnam, BIM_Precision, BIM_TypeReserve, BIM_Remarque,
    # Edit
          edit ) {
  
  
  if (edit) {
    x <- "Submit Edits"
    title_form = paste("Edit Réserves BIM -", nom_produit, "(date de validation CEESP :", paste0(date_val_CEESP, ")"))
  } else {
    x <- "Add New Row"
    title_form <- "Add New Row"
  }
  
  shiny::modalDialog(
    title = title_form,
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #E2E2F2;  color:black}
    .tabbable > .nav > li > a[data-value='Réserves CEM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li > a[data-value='Réserves BIM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: #7949F0; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves CEM'] {background-color: #B502B1; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves BIM'] {background-color: #B502B1; color:white}
    ")),
    
    tabsetPanel(type = "tabs",
                
                {
                  tabPanel("Réserves BIM", 
                           selectInput(inputId = "BIM_DimHAS", label = "Dimension d'analyse par la HAS", width = "100%", choices = c(sort(unique(rv_resBIM$df$BIM_DimHAS)),"Autre (à préciser)"), selected = BIM_DimHAS),
                           conditionalPanel(condition = "input.BIM_DimHAS == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_BIM_DimHAS", label = em("Merci de préciser la dimension d'analyse par la HAS", style = "color:#00CA9F"), width = "100%", value = autre_BIM_DimHAS),
                           ),
                           selectInput(inputId = "BIM_DimPutnam", label = "Dimension d'analyse corrigée par Putnam", width = "100%", choices = c("-",sort(unique(rv_resBIM$df$BIM_DimPutnam)),"Autre (à préciser)"), selected = BIM_DimPutnam),
                           conditionalPanel(condition = "input.BIM_DimPutnam == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_BIM_DimPutnam", label = em("Merci de préciser la dimension d'analyse corrigée par Putnam", style = "color:#00CA9F"), width = "100%", value = autre_BIM_DimPutnam),
                           ),
                           textAreaInput(inputId = "BIM_Precision", label = HTML('<span style="color: black;">Intitulé de la réserve</span> <span style="color: red;">(A remplir obligatoirement)</span>'), width = "100%", value = BIM_Precision),
                           selectInput(inputId = "BIM_TypeReserve", label = "Type de réserve", width = "100%", choices = c(sort(unique(rv_resBIM$df$BIM_TypeReserve))), selected = BIM_TypeReserve),
                           textAreaInput(inputId = "BIM_Remarque", label = "Remarque", width = "100%", value = BIM_Remarque)
                    )
                },
    ),
    
    size = "l",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "dismiss_modal_reservesBIM", label = "Close", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "final_edit_reservesBIM", label = x, icon = shiny::icon("edit"), class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
  
}



## END
