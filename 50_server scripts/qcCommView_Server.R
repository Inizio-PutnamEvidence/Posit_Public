#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for QC tab to see Comments from View Tab users
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  30 May 2024
#   LAST MODIFIED DATE    09 August 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           QC new form
# 
#--------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization - Data VIEW                                                                ####
#-------------------------------------------------------------------------------------------------------------------#

block_qc_commentView <- expression({
  df_commView_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_CommentsView.xlsx"))
  
  rv_commView_qc <- shiny::reactiveValues(
    df = df_commView_qc,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(df) + 1
  )
  
  output$dt_commView_qc <- DT::renderDT({
    df <- rv_commView_qc$df %>% select(Buttons, ID, NomProduit, Molecule, SubjectComm, CommentsView, NameUser, DateComment)
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Bouton", "ID", "Nom de Produit", "Molécule", "Sujet", "Commentaire", "Auteur", "Date du commentaire"),
      extensions = c("FixedHeader","FixedColumns","Buttons"),
      options = list(processing = FALSE,
                     searching = FALSE,
                     paging = FALSE,
                     scrollX = TRUE,
                     scrollY = '40vh',
                     scrollCollapse = TRUE,
                     autoWidth = TRUE,
                     fixedHeader = FALSE,
                     columnDefs = list(list(width = '150px', targets = c(which(names(df) %in% c("Bouton","Auteur")))-1),
                                       list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                       list(width = '500px', targets = c(which(names(df) %in% c("CommentsView")))-1),
                                       list(width = '100px', targets = 1:(ncol(df)-1)))
      ),
      filter = 'top'
    ) %>% formatStyle(columns = c("CommentsView"), backgroundColor = "lightblue")
  })
  
  proxy_commView_qc <- DT::dataTableProxy("dt_commView_qc")
  shiny::observe({
    DT::replaceData(proxy_commView_qc, rv_commView_qc$df, resetPaging = FALSE, rownames = FALSE)
  })
})

eval(block_qc_commentView)


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row - Data VIEW                                                                                 ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "deleteqcCommView"))
  rv_commView_qc$dt_row <- which(stringr::str_detect(rv_commView_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))

  shiny::modalDialog(
    title = "Suppression",
    "Voulez-vous supprimer ce commentaire ?",
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_deleteqcCommView", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_deleteqcCommView", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})


shiny::observeEvent(input$valid_deleteqcCommView, {
  rv_commView_qc$df <- rv_commView_qc$df[-rv_commView_qc$dt_row, ]
  write_xlsx(rv_commView_qc$df, paste0(getwd(),"/",folder_data,"/toQC_CommentsView.xlsx"))
  shiny::removeModal()
})


shiny::observeEvent(input$cancel_deleteqcCommView, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Validate row - Data VIEW                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "validateqcCommView"))
  rv_commView_qc$dt_row <- which(stringr::str_detect(rv_commView_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  if(rv_commView_qc$df$SubjectComm[rv_commView_qc$dt_row] == "Observation"){
    shiny::modalDialog(
      title = "Validation",
      "Voulez-vous ajouter un commentaire sur cette ligne ?",
      br(),
      if(baseHAS_shiny[as.numeric(rv_commView_qc$df$ID[rv_commView_qc$dt_row]),"CommentsQC"] != "-")({
        textAreaInput(inputId = "oldcommentsqcfromview", label = "Précédent(s) Commentaire(s) du reviewer :", placeholder = "OldComments", width = "100%",value = baseHAS_shiny[as.numeric(rv_commView_qc$df$ID[rv_commView_qc$dt_row]),"CommentsQC"])
      }),
      textAreaInput(inputId = "commentsqcfromview", label = "Nouveau Commentaire du reviewer :", placeholder = "Comments", width = "100%", value = ""),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-left",
            shiny::actionButton(inputId = "cancel_validqcCommView", label = "Cancel", class = "btn-danger"),
          ),
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "valid_validqcCommView", label = "Validate", class = "btn-info"),
          )
        )
      )
    ) %>% shiny::showModal()
    
  } 
  if(rv_commView_qc$df$SubjectComm[rv_commView_qc$dt_row] != "Observation"){
    shiny::modalDialog(
      title = "Validation",
      "Est-ce que ce commentaire a bien été pris en compte ?",
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-left",
            shiny::actionButton(inputId = "cancel_validqcCommView", label = "Cancel", class = "btn-danger"),
          ),
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "valid_validqcCommView", label = "Validate", class = "btn-info"),
          )
        )
      )
    ) %>% shiny::showModal()
    
  } 
})


shiny::observeEvent(input$valid_validqcCommView, {
  if(rv_commView_qc$df$SubjectComm[rv_commView_qc$dt_row] == "Observation"){
    df_val <- baseHAS_shiny[as.numeric(rv_commView_qc$df$ID[rv_commView_qc$dt_row]),]
    baseHAS_shiny[as.numeric(rv_commView_qc$df$ID[rv_commView_qc$dt_row]),]$CommentsQC <- ifelse(df_val$CommentsQC == "-",
                                                                                                 ifelse(input$commentsqcfromview %in% c("-",""),
                                                                                                        "-",
                                                                                                        paste0(reactiveValuesToList(res_auth)$user, ", le ", Sys.Date()," : ", input$commentsqcfromview)),
                                                                                                 paste0(input$oldcommentsqcfromview,ifelse(input$commentsqcfromview %in% c("-",""),"",
                                                                                                                                           paste0("\n", reactiveValuesToList(res_auth)$user, ", le ", Sys.Date()," : ", input$commentsqcfromview))))
  }
  rv_commView_qc$df <- rv_commView_qc$df[-rv_commView_qc$dt_row, ]
  write_xlsx(rv_commView_qc$df, paste0(getwd(),"/",folder_data,"/toQC_CommentsView.xlsx"))
  save(baseHAS_shiny, file = paste0(getwd(),"/",folder_import,"/baseHAS_shiny.RData"))
  
  #-----------------------------------------------> Base fusionView
  # Fusion HAS/CEM/BIM for Shiny - VIEW       
  fusion_to_show <- c((varlistHAS %>% filter(`Variable à inclure dans l'onglet de visualisation ?` != "Non"))$Variable, "CommentsQC")
  # NO ==> Need to have the variable "Buttons" to use the function "func_FormattingHAS" --> variable deleted via the "select"
  # NO ==> fusionView <- func_FormattingHAS(baseHAS_shiny %>% mutate(Buttons = "buttons")) %>% select(all_of(fusion_to_show)) 
  fusionView <- baseHAS_shiny %>%  mutate(
    # Date
    DateAMM              = func_DateFormatting(DateAMM),
    DateCOMP             = func_DateFormatting(DateCOMP),
    DateEligibiliteHAS   = func_DateFormatting(DateEligibiliteHAS),
    DateCT               = func_DateFormatting(DateCT),
    DateValidationCEESP  = func_DateFormatting(DateValidationCEESP),
    JO_Date1erJO         = func_DateFormatting(JO_Date1erJO),
    JO_DateJOpre         = func_DateFormatting(JO_DateJOpre),
    JO_Date1erJOpost     = func_DateFormatting(JO_Date1erJOpost)
  ) %>% select(all_of(fusion_to_show)) 
  # Join HAS/CEM/BIM
  fusionView <- fusionView %>% left_join(baseCEM_shiny %>% select(Index, starts_with("CEM")), by = "Index") %>% left_join(baseBIM_shiny %>% select(Index, starts_with("BIM")), by = "Index")
  # Formatting on the "missing" data
  fusionView[is.na(fusionView)] = "-"
  fusionView[is.null(fusionView)] = "-"
  fusionView[fusionView == ""] = "-"
  # Variables into factor
  fusionView <- as.data.frame(lapply(fusionView, as.factor))
  # Add buttons to view to post and read comments 
  fusionView <- fusionView %>% mutate(Buttons = ifelse(is.na(CommentsQC) | CommentsQC == "-", create_btns_view_without_qc_comm(as.numeric(ID)), create_btns_view_with_qc_comm(as.numeric(ID)))) %>% relocate(Buttons)
  
  # Save the new updated database
  save(fusionView, file = paste0(getwd(),"/",folder_import,"/fusionView.RData"))
  
  shiny::removeModal()
})


shiny::observeEvent(input$cancel_validqcCommView, {
  shiny::removeModal()
})



## END
