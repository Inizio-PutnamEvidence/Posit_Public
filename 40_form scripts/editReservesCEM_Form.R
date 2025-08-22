#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Edit Reserves CEM Form
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  09 August 2023
#   LAST MODIFIED DATE    15 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO))
#   DESCRIPTION           Creation of a form
#  
#--------------------------------------------------------------------------------------------------------



modal_dialog_reservesCEM <- function(
    # Informations Générales
          nom_produit, date_val_CEESP, 
    # Reserves CEM
          CEM_DimHAS, autre_CEM_DimHAS, CEM_DimPutnam, autre_CEM_DimPutnam, CEM_Precision, CEM_TypeReserve, CEM_Remarque, 
    # Edit
          edit ) {
  
  
  if (edit) {
    x <- "Submit Edits"
    title_form = paste("Edit Réserves CEM -", nom_produit, "(date de validation CEESP :", paste0(date_val_CEESP, ")"))
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
                  tabPanel("Réserves CEM", 
                           selectInput(inputId = "CEM_DimHAS", label = "Dimension d'analyse par la HAS", width = "100%", choices = c(sort(unique(rv_resCEM$df$CEM_DimHAS)),"Autre (à préciser)"), selected = CEM_DimHAS),
                           conditionalPanel(condition = "input.CEM_DimHAS == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_CEM_DimHAS", label = em("Merci de préciser la dimension d'analyse par la HAS", style = "color:#00CA9F"), width = "100%", value = autre_CEM_DimHAS),
                           ),
                           selectInput(inputId = "CEM_DimPutnam", label = "Dimension d'analyse corrigée par Putnam", width = "100%", choices = c("-",sort(unique(rv_resCEM$df$CEM_DimPutnam)),"Autre (à préciser)"), selected = CEM_DimPutnam),
                           conditionalPanel(condition = "input.CEM_DimPutnam == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_CEM_DimPutnam", label = em("Merci de préciser la dimension d'analyse corrigée par Putnam", style = "color:#00CA9F"), width = "100%", value = autre_CEM_DimPutnam),
                           ),
                           textAreaInput(inputId = "CEM_Precision", label = HTML('<span style="color: black;">Intitulé de la réserve</span> <span style="color: red;">(A remplir obligatoirement)</span>'), width = "100%", value = CEM_Precision),
                           selectInput(inputId = "CEM_TypeReserve", label = "Type de réserve", width = "100%", choices = c(sort(unique(rv_resCEM$df$CEM_TypeReserve))), selected = CEM_TypeReserve),
                           textAreaInput(inputId = "CEM_Remarque", label = "Remarque", width = "100%", value = CEM_Remarque)
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
          shiny::actionButton(inputId = "dismiss_modal_reservesCEM", label = "Close", class = "btn-danger" ),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "final_edit_reservesCEM", label = x, icon = shiny::icon("edit"), class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
  
}



## END
