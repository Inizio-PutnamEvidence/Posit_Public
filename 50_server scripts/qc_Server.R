#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for QC tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  30 October 2023
#   LAST MODIFIED DATE    01 June 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           QC new form
# 
#--------------------------------------------------------------------------------------------------------



source(file.path(paste0(folder_form,"/editHAS_Form.R")), local = TRUE)$value


#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization - Data HAS                                                                 ####
#-------------------------------------------------------------------------------------------------------------------#

block_qc_has <- expression({
  df_HAS_to_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
  
  if(nrow(df_HAS_to_qc) > 0){
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      df_HAS_to_qc <- df_HAS_to_qc %>% mutate("Buttons" = create_btns_HAS_qc_with_right(1:nrow(df_HAS_to_qc)))
    }
    else {
      df_HAS_to_qc <- df_HAS_to_qc %>% mutate("Buttons" = create_btns_HAS_qc_without_right(1:nrow(df_HAS_to_qc)))
    }
  }
  
  rv_HAS_to_qc <- shiny::reactiveValues(
    df = df_HAS_to_qc,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(df) + 1
  )
  
  
  output$dt_HAS_qc <- DT::renderDT({
    df <- func_FormattingHAS(rv_HAS_to_qc$df) %>% relocate(Auteur, .after = Buttons)
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Buttons", "Auteur", "Statut",
                   varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_HAS_to_qc$df))]),
      extensions = c("FixedHeader","FixedColumns","Buttons"),
      options = list(processing = FALSE,
                     paging = FALSE,
                     scrollX = TRUE,
                     scrollY = '40vh',
                     scrollCollapse = TRUE,
                     autoWidth = TRUE,
                     fixedHeader = FALSE,
                     fixedColumns = list(leftColumns = 3),
                     columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index")))-1),
                                       list(width = '150px', targets = c(which(names(df) %in% c("Buttons","Auteur")))-1),
                                       list(width = '200px', targets = c(which(names(df) %in% c("ATC_Label","MotifReevaluation","HorizonTemporel","TypeModelePutnam")))-1),
                                       list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                       list(width = '500px', targets = c(which(names(df) %in% c("Indication","TypeModeleHAS","Posologie_Detail","Horizon")))-1),
                                       list(width = '100px', targets = 1:(ncol(df)-1)))
      ),
      filter = 'top'
    ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule"))$Variable), backgroundColor = "#FFF0CA")
  })
  
  proxy_HAS_to_qc <- DT::dataTableProxy("dt_HAS_qc")
  shiny::observe({
    DT::replaceData(proxy_HAS_to_qc, rv_HAS_to_qc$df, resetPaging = FALSE, rownames = FALSE)
  })
})

eval(block_qc_has)


#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization - Reserves CEM                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

block_qc_cem <- expression({
  df_resCEM_to_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
  
  if(nrow(df_resCEM_to_qc) > 0){
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      df_resCEM_to_qc <- df_resCEM_to_qc %>% mutate("Buttons" = create_btns_ResCEM_qc(1:nrow(df_resCEM_to_qc)))
    }
    else {
      df_resCEM_to_qc <- df_resCEM_to_qc %>% mutate("Buttons" = "")
    }
  }
  
  rv_resCEM_to_qc <- shiny::reactiveValues(
    df = df_resCEM_to_qc,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(df) + 1
  )
  
  output$dt_resCEM_qc <- DT::renderDT({
    df <- func_FormattingRes(rv_resCEM_to_qc$df %>% relocate(Auteur, .after = Buttons)) %>% arrange(ID, IDRes)
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Buttons", "Auteur", "Statut", "Numéro", "Numéro réserve",
                   as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resCEM_to_qc$df) & dico$Variable != "ID")])),
      extensions = c("FixedHeader","FixedColumns","Buttons"),
      options = list(processing = FALSE,
                     paging = FALSE,
                     scrollX = TRUE,
                     scrollY = '40vh',
                     scrollCollapse = TRUE,
                     autoWidth = TRUE, 
                     fixedHeader = FALSE,
                     fixedColumns = list(leftColumns = 3),
                     autoWidth = TRUE,
                     columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule","DateValidationCEESP","AireTheraPutnam","SourceAvisCEESP")))-1),
                                       list(width = '150px', targets = c(which(names(df) %in% c("Buttons","Auteur")))-1), 
                                       list(width = '200px', targets = c(which(names(df) %in% c("CEM_DimHAS","CEM_DimPutnam")))-1), 
                                       list(width = '50px',  targets = c(which(names(df) %in% c("ID","IDRes")))-1), 
                                       list(width = '500px', targets = c(which(names(df) %in% c("CEM_Precision")))-1),
                                       list(width = '100px', targets = 1:(ncol(df)-1)))
      )
    )
  })
  
  proxy_resCEM_to_qc <- DT::dataTableProxy("dt_resCEM_qc")
  shiny::observe({
    DT::replaceData(proxy_resCEM_to_qc, rv_resCEM_to_qc$df, resetPaging = FALSE, rownames = FALSE)
  })
})

eval(block_qc_cem)


#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization - Reserves BIM                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

block_qc_bim <- expression({
  df_resBIM_to_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
  
  if(nrow(df_resBIM_to_qc) > 0){
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      df_resBIM_to_qc <- df_resBIM_to_qc %>% mutate("Buttons" = create_btns_ResBIM_qc(1:nrow(df_resBIM_to_qc)))
    }
    else {
      df_resBIM_to_qc <- df_resBIM_to_qc %>% mutate("Buttons" = "")
    }
  }
  
  rv_resBIM_to_qc <- shiny::reactiveValues(
    df = df_resBIM_to_qc,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(df) + 1
  )
  
  output$dt_resBIM_qc <- DT::renderDT({
    df <- func_FormattingRes(rv_resBIM_to_qc$df %>% relocate(Auteur, .after = Buttons)) %>% arrange(ID, IDRes)
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Buttons", "Auteur", "Statut", "Numéro", "Numéro réserve", 
                   as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resBIM_to_qc$df) & dico$Variable != "ID")])),
      options = list(processing = FALSE,
                     paging = FALSE,
                     scrollX = TRUE,
                     scrollY = '40vh',
                     scrollCollapse = TRUE,
                     autoWidth = TRUE,
                     fixedHeader = FALSE,
                     fixedColumns = list(leftColumns = 3),
                     autoWidth = TRUE,
                     columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule","DateValidationCEESP","AireTheraPutnam","SourceAvisCEESP")))-1),
                                       list(width = '150px', targets = c(which(names(df) %in% c("Buttons","Auteur")))-1),
                                       list(width = '200px', targets = c(which(names(df) %in% c("BIM_DimHAS","BIM_DimPutnam")))-1),
                                       list(width = '50px',  targets = c(which(names(df) %in% c("ID","IDRes")))-1),
                                       list(width = '500px', targets = c(which(names(df) %in% c("BIM_Precision")))-1),
                                       list(width = '100px', targets = 1:(ncol(df)-1)))
      )
    )
  })
  
  proxy_resBIM_to_qc <- DT::dataTableProxy("dt_resBIM_qc")
  shiny::observe({
    DT::replaceData(proxy_resBIM_to_qc, rv_resBIM_to_qc$df, resetPaging = FALSE, rownames = FALSE)
  })
})

eval(block_qc_bim)


#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization - Table des associations                                                   ####
#-------------------------------------------------------------------------------------------------------------------#

block_qc_assoc <- expression({
  df_assoc_to_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
  
  if(nrow(df_assoc_to_qc) > 0){
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      df_assoc_to_qc <- df_assoc_to_qc %>% mutate("Buttons" = create_btns_assoc_qc(1:nrow(df_assoc_to_qc)))
    }
    else {
      df_assoc_to_qc <- df_assoc_to_qc %>% mutate("Buttons" = "")
    }
  }
  
  rv_assoc_to_qc <- shiny::reactiveValues(
    df = df_assoc_to_qc,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(df) + 1
  )
  
  output$dt_assoc_qc <- DT::renderDT({
    df <- func_FormattingAssoc(rv_assoc_to_qc$df %>% relocate(Auteur, .after = Buttons), "no_select")
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Buttons", "Auteur", "Statut",
                   varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc_to_qc$df) & varlistHAS$Variable %in% c("ID","NomProduit"))],
                   "Nom du produit Assoc.",
                   varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc_to_qc$df) & !varlistHAS$Variable %in% c("ID","NomProduit"))]),
      extensions = c("FixedHeader","FixedColumns","Buttons"),
      options = list(processing = FALSE,
                     paging = FALSE,
                     scrollX = TRUE,
                     scrollY = '40vh',
                     scrollCollapse = TRUE,
                     autoWidth = TRUE, 
                     fixedHeader = FALSE,
                     fixedColumns = list(leftColumns = 3),
                     autoWidth = TRUE,
                     columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("DateAMM","Demande","TypeProduit")))-1),
                                       list(width = '150px', targets = c(which(names(df) %in% c("Buttons","Auteur")))-1),
                                       list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                       list(width = '500px', targets = c(which(names(df) %in% c("Indication","Posologie_Detail","Horizon")))-1),
                                       list(width = '100px', targets = 1:(ncol(df)-1)))
      )
    ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule" & `Table des associations` == "Oui"))$Variable), backgroundColor = "#FFF0CA")
  })
  
  proxy_assoc_to_qc <- DT::dataTableProxy("dt_assoc_qc")
  shiny::observe({
    DT::replaceData(proxy_assoc_to_qc, rv_assoc_to_qc$df, resetPaging = FALSE, rownames = FALSE)
  })
})

eval(block_qc_assoc)


#-------------------------------------------------------------------------------------------------------------------#
####      View comment                                                                                           ####
#-------------------------------------------------------------------------------------------------------------------#

read_comment(name_pattern = "commentqcHAS", name_rv = rv_HAS_to_qc)


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row - Data HAS / Reserves CEM / Reserves BIM / Table des associations                           ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "deleteqcHAS"))
  rv_HAS_to_qc$dt_row <- which(stringr::str_detect(rv_HAS_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))

  shiny::modalDialog(
    title = "Suppression",
    "Voulez-vous rejeter cet ajout/cette modification ?",
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_deleteqcHAS", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_deleteqcHAS", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})


shiny::observeEvent(input$valid_deleteqcHAS, {
  IDdelete <- rv_HAS_to_qc$df$ID[rv_HAS_to_qc$dt_row]
  statut <- rv_HAS_to_qc$df$Statut[rv_HAS_to_qc$dt_row]
  if(statut == "Modification HAS"){
    line <- which(rv$df$ID == IDdelete)
    rv$df$Buttons[line] <- ifelse(is.na(rv$df$CommentsQC[line]) | rv$df$CommentsQC[line] == "-", create_btns_without_qc_comment(line), create_btns_with_qc_comment(line)) 
  }
  
  rv_HAS_to_qc$df <- rv_HAS_to_qc$df[-rv_HAS_to_qc$dt_row, ]
  write_xlsx(rv_HAS_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
  if(statut == "Nouvelle HAS"){
    
    if (IDdelete %in% as.numeric(c(rv_resCEM_to_qc$df$ID))){
      rv_resCEM_to_qc$df <- rv_resCEM_to_qc$df[-which(as.numeric(rv_resCEM_to_qc$df$ID) == IDdelete),]
    }
    if (IDdelete %in% as.numeric(c(rv_resBIM_to_qc$df$ID))){
      rv_resBIM_to_qc$df <- rv_resBIM_to_qc$df[-which(as.numeric(rv_resBIM_to_qc$df$ID) == IDdelete),]
    }
    
    rv_HAS_to_qc$df$ID[which(rv_HAS_to_qc$df$ID > IDdelete)] <- as.numeric(rv_HAS_to_qc$df$ID[which(as.numeric(rv_HAS_to_qc$df$ID) > IDdelete)]) -1
    rv_HAS_to_qc$df$Index <- paste0(paste0(rv_HAS_to_qc$df$ID,rv_HAS_to_qc$df$Molecule), as.numeric(rv_HAS_to_qc$df$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2)
    
    rv_resCEM_to_qc$df$ID[which(rv_resCEM_to_qc$df$ID > IDdelete)] <- as.numeric(rv_resCEM_to_qc$df$ID[which(as.numeric(rv_resCEM_to_qc$df$ID) > IDdelete)])-1
    rv_resCEM_to_qc$df$Index <- paste0(paste0(rv_resCEM_to_qc$df$ID,rv_resCEM_to_qc$df$Molecule), as.numeric(rv_resCEM_to_qc$df$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2)
    
    rv_resBIM_to_qc$df$ID[which(rv_resBIM_to_qc$df$ID > IDdelete)] <- as.numeric(rv_resBIM_to_qc$df$ID[which(as.numeric(rv_resBIM_to_qc$df$ID) > IDdelete)])-1
    rv_resBIM_to_qc$df$Index <- paste0(paste0(rv_resBIM_to_qc$df$ID,rv_resBIM_to_qc$df$Molecule),as.numeric(rv_resBIM_to_qc$df$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2)
  
    write_xlsx(rv_resCEM_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
    write_xlsx(rv_resBIM_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
  }
  write_xlsx(rv_HAS_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
  shiny::removeModal()
  
})


shiny::observeEvent(input$cancel_deleteqcHAS, {
  shiny::removeModal()
})


delete_row(name_pattern = "deleteqcCEM",   name_rv = rv_resCEM_to_qc, name_excel = "toQC_ReservesCEM", eval_block = block_qc_cem)
delete_row(name_pattern = "deleteqcBIM",   name_rv = rv_resBIM_to_qc, name_excel = "toQC_ReservesBIM", eval_block = block_qc_bim)
delete_row(name_pattern = "deleteqcAssoc", name_rv = rv_assoc_to_qc,  name_excel = "toQC_TableAssoc",  eval_block = block_qc_assoc)


#-------------------------------------------------------------------------------------------------------------------#
####      Edit line - Data HAS                                                                                   ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcHAS"))
  rv_HAS_to_qc$dt_row <- which(stringr::str_detect(rv_HAS_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_HAS_to_qc$df[rv_HAS_to_qc$dt_row, ]
  
  count_PopCible(1)
  count_Prix(1)
  count_PTC(1)
  count_PTCED(1)
  count_resCEM(1)
  count_resBIM(1)
  
  observe({
    shinyjs::disable("add_entry_PopCible")
    shinyjs::disable("add_entry_Prix")
    shinyjs::disable("add_entry_PTC")
    shinyjs::disable("add_entry_PTCED")
  })
  
  # Call modal_dialog with edit = TRUE & edit_module = FALSE
  {
    modal_dialog(
    # Informations Générales   
      ###########################################CENSORED######################
    # Evaluation HAS
    ###########################################CENSORED######################
    # Population cible  
    ###########################################CENSORED######################
    # Prix
    ###########################################CENSORED######################
    # Posologie
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Non Chronique
    ###########################################CENSORED######################
    # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
    ###########################################CENSORED######################
    # Posologie ---> Vaccin
          PVC_Nbinj = df$PVC_Nb,
    # Edit
          edit = TRUE,
    # Edit module
          edit_module = FALSE
  )}
  
  rv_HAS_to_qc$add_or_edit <- NULL
})


shiny::observeEvent(input$final_edit_qc, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcHAS") & is.null(rv_HAS_to_qc$add_or_edit))  
  if(input$sel_horizon_temp == "Vie entière"){ horizon_temp = ifelse(input$horizon_temp %in% c("sans précision","","-"), "Vie entière (sans précision)", paste0(input$sel_horizon_temp, " (", input$horizon_temp, ")")) }
  if(input$sel_horizon_temp != "Vie entière"){ horizon_temp = input$horizon_temp }

  edited_qc_row <- {dplyr::tibble(
    # Warning, these 3 variables are different between the section                     
    ###########################################CENSORED######################
    # Informations Générales
    ###########################################CENSORED######################
    # Evaluation HAS
    ###########################################CENSORED######################
    # Population cible
    ###########################################CENSORED######################
    # Prix
    ###########################################CENSORED######################
    # Posologie
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Non Chronique
    ###########################################CENSORED######################
    # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
    ###########################################CENSORED######################
    # Posologie ---> Vaccin
    ###########################################CENSORED######################
    # Délai
    ###########################################CENSORED######################
  )}
  
  # Function "mutate" --> creation of variables with formulas (via functions) + update of variables + formatting of missing data 
  ###########################################CENSORED######################
  write_xlsx(rv_HAS_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
  autosave_qcHAS()
})


shiny::observeEvent(input$final_edit_qc, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Edit line - Reserves CEM                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcCEM"))
  rv_resCEM_to_qc$dt_row <- which(stringr::str_detect(rv_resCEM_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resCEM_to_qc$df[rv_resCEM_to_qc$dt_row,]
  modal_dialog_reservesCEM(
    # Informations Générales
    ###########################################CENSORED######################
    # Reserves CEM
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resCEM_to_qc$add_or_edit <- NULL  
})


shiny::observeEvent(input$final_edit_reservesCEM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcCEM") & is.null(rv_resCEM_to_qc$add_or_edit))
  edited_form_row_resCEM <- dplyr::tibble(
      # Other variables
    ###########################################CENSORED######################
      # Display some variables from HAS table			
    ###########################################CENSORED######################
      # Reserves CEM 			
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  write_xlsx(rv_resCEM_to_qc$df , paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
  autosave_qcCEM()
})


shiny::observeEvent(input$final_edit_reservesCEM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Edit line - Reserves BIM                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcBIM"))
  rv_resBIM_to_qc$dt_row <- which(stringr::str_detect(rv_resBIM_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resBIM_to_qc$df[rv_resBIM_to_qc$dt_row, ]
  modal_dialog_reservesBIM(
    # Informations Générales
    ###########################################CENSORED######################
    # Reserves BIM
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resBIM_to_qc$add_or_edit <- NULL 
})


shiny::observeEvent(input$final_edit_reservesBIM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcBIM") & is.null(rv_resBIM_to_qc$add_or_edit))
  edited_form_row_resBIM <- dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table			
    ###########################################CENSORED######################
    # Reserves BIM 			
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  write_xlsx(rv_resBIM_to_qc$df , paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
  autosave_qcBIM()
})


shiny::observeEvent(input$final_edit_reservesBIM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Edit line - Table des associations                                                                     ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcAssoc"))
  rv_assoc_to_qc$dt_row <- which(stringr::str_detect(rv_assoc_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_assoc_to_qc$df[rv_assoc_to_qc$dt_row,]
  
  # Call modal_dialog_assoc with edit = TRUE
  {
    modal_dialog_assoc(
    # Informations Générales
      ###########################################CENSORED######################
    # Prix
    ###########################################CENSORED######################
    # Posologie
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Non Chronique
    ###########################################CENSORED######################
    # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
    ###########################################CENSORED######################
    # Posologie ---> Vaccin
          PVC_Nbinj = df$PVC_Nb,
    # Edit
          edit = TRUE
    )}
  
  rv_assoc_to_qc$add_or_edit <- NULL  
})


shiny::observeEvent(input$final_edit_assoc, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editqcAssoc") & is.null(rv_assoc_to_qc$add_or_edit))
  edited_form_row_assoc <- {dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table
    ###########################################CENSORED######################
    # Prix
    ###########################################CENSORED######################
    # Posologie
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Non Chronique
    ###########################################CENSORED######################
    # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
    ###########################################CENSORED######################
    # Posologie ---> Vaccin
          PVC_Nb = input$PVC_Nbinj
  )}
  
  # Function "mutate" --> creation of variables with formulas (via functions) + update of variables + formatting of missing data 
  ###########################################CENSORED######################
  write_xlsx(rv_assoc_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
  autosave_qcAssoc()
})


shiny::observeEvent(input$final_edit_assoc, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Validate QC - Data HAS                                                                                 ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "validateqcHAS"))
  rv_HAS_to_qc$dt_row <- which(stringr::str_detect(rv_HAS_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  
  shiny::modalDialog(
    title = "Validation du QC",
    "Voulez-vous valider le QC de cette ligne et l'intégrer à la base correspondante ?", br(),
    renderUI(HTML(paste(strong("Commentaire de l'auteur :", style = "color:#5343a3"), rv_HAS_to_qc$df$CommentsEdit[rv_HAS_to_qc$dt_row ], '<br/><br/>'))),
    if(rv_HAS_to_qc$df$CommentsQC[rv_HAS_to_qc$dt_row ] != "-")({
      textAreaInput(inputId = "oldcommentsqc", label = "Précédent(s) Commentaire(s) du reviewer :", placeholder = "OldComments", width = "100%",value = rv_HAS_to_qc$df$CommentsQC[rv_HAS_to_qc$dt_row ])
    }),
    textAreaInput(inputId = "commentsqc", label = "Nouveau Commentaire du reviewer :", placeholder = "Comments", width = "100%",value = ""),
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_confirmqcHAS", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_confirmqcHAS", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})

shiny::observeEvent(input$valid_confirmqcHAS, {
  df_val <- rv_HAS_to_qc$df[rv_HAS_to_qc$dt_row, ]
  df_val$CommentsQC <- ifelse(df_val$CommentsQC == "-",
                              ifelse(input$commentsqc %in% c("-",""),
                                     "-",
                                     paste0(reactiveValuesToList(res_auth)$user, ", le ", Sys.Date()," : ", input$commentsqc)),
                              paste0(input$oldcommentsqc,ifelse(input$commentsqc %in% c("-",""),"",
                                                                paste0("\n", reactiveValuesToList(res_auth)$user, ", le ", Sys.Date()," : ", input$commentsqc))))
  
  # ---------------------------------------------------------------> Download PDFs
  # Source CT (use the function 'download.file')
  if(str_sub(df_val$SourceAvisCT,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$SourceAvisCT[line]))
    }
    namepdf_source_ct <- paste0(folder_pdf,"/AvisCT/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateCT),"-"),"_avisCT.pdf")
    if(str_sub(df_val$SourceAvisCT,str_length(df_val$SourceAvisCT)-3,str_length(df_val$SourceAvisCT)) == ".pdf"){
      download.file(df_val$SourceAvisCT, paste0("www/",namepdf_source_ct), mode = "wb")
    } else {
      webshot(df_val$SourceAvisCT, paste0("www/",namepdf_source_ct))
    }
    df_val$SourceAvisCT <- namepdf_source_ct
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$SourceAvisCT != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$DateCT[line] != df_val$DateCT){
      file.rename(paste0(getwd(),"/www/",rv$df$SourceAvisCT[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/AvisCT/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateCT),"-"),"_avisCT.pdf"))
      df_val$SourceAvisCT <- paste0(folder_pdf,"/AvisCT/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateCT),"-"),"_avisCT.pdf")
    }
  }
  
  # Source CEESP (use the function 'download.file')
  if(str_sub(df_val$SourceAvisCEESP,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$SourceAvisCEESP[line]))
    }
    namepdf_source_avis_CEESP <- paste0(folder_pdf,"/AvisCEESP/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateValidationCEESP),"-"),"_avisCEESP.pdf")
    if(str_sub(df_val$SourceAvisCEESP,str_length(df_val$SourceAvisCEESP)-3,str_length(df_val$SourceAvisCEESP)) == ".pdf"){
      download.file(df_val$SourceAvisCEESP, paste0("www/",namepdf_source_avis_CEESP), mode = "wb")
    } else {
      webshot(df_val$SourceAvisCEESP, paste0("www/",namepdf_source_avis_CEESP))
    }
    df_val$SourceAvisCEESP <- namepdf_source_avis_CEESP
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$SourceAvisCEESP != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$DateValidationCEESP[line] != df_val$DateValidationCEESP){
      file.rename(paste0(getwd(),"/www/",rv$df$SourceAvisCEESP[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/AvisCEESP/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateValidationCEESP),"-"),"_avisCEESP.pdf"))
      df_val$SourceAvisCEESP <- paste0(folder_pdf,"/AvisCEESP/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateValidationCEESP),"-"),"_avisCEESP.pdf")
    }
  }
  
  # Source 1er JO (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_Source1erJO,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$JO_Source1erJO[line]))
    }
    namepdf_source_1erJO <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf")
    if(str_sub(df_val$JO_Source1erJO,str_length(df_val$JO_Source1erJO)-3,str_length(df_val$JO_Source1erJO)) == ".pdf"){
      download.file(df_val$JO_Source1erJO, paste0("www/",namepdf_source_1erJO), mode = "wb")
    } else {
      webshot(df_val$JO_Source1erJO, paste0("www/",namepdf_source_1erJO))
    }
    df_val$JO_Source1erJO <- namepdf_source_1erJO
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$JO_Source1erJO != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$JO_Date1erJO[line] != df_val$JO_Date1erJO){
      file.rename(paste0(getwd(),"/www/",rv$df$JO_Source1erJO[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf"))
      df_val$JO_Source1erJO <- paste0(folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf")
    }
  }
  
  # Source JO pre (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_SourceJOpre,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$JO_SourceJOpre[line]))
    }
    namepdf_source_JOpre <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf")
    if(str_sub(df_val$JO_SourceJOpre,str_length(df_val$JO_SourceJOpre)-3,str_length(df_val$JO_SourceJOpre)) == ".pdf"){
      download.file(df_val$JO_SourceJOpre, paste0("www/",namepdf_source_JOpre), mode = "wb")
    } else {
      webshot(df_val$JO_SourceJOpre, paste0("www/",namepdf_source_JOpre))
    }
    df_val$JO_SourceJOpre <- namepdf_source_JOpre
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$JO_SourceJOpre != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$JO_DateJOpre[line] != df_val$JO_DateJOpre){
      file.rename(paste0(getwd(),"/www/",rv$df$JO_SourceJOpre[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf"))
      df_val$JO_SourceJOpre <- paste0(folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf")
    }
  }
  
  # Source 1er JO post (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_Source1erJOpost,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$JO_Source1erJOpost[line]))
    }
    namepdf_source_1erJOpost <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf")
    if(str_sub(df_val$JO_Source1erJOpost,str_length(df_val$JO_Source1erJOpost)-3,str_length(df_val$JO_Source1erJOpost)) == ".pdf"){
      download.file(df_val$JO_Source1erJOpost, paste0("www/",namepdf_source_1erJOpost), mode = "wb")
    } else {
      webshot(df_val$JO_Source1erJOpost, paste0("www/",namepdf_source_1erJOpost))
    }
    df_val$JO_Source1erJOpost <- namepdf_source_1erJOpost
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$JO_Source1erJOpost != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$JO_Date1erJOpost[line] != df_val$JO_Date1erJOpost){
      file.rename(paste0(getwd(),"/www/",rv$df$JO_Source1erJOpost[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf"))
      df_val$JO_Source1erJOpost <- paste0(folder_pdf,"/JO/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf")
    }
  }
  
  # Source RCP (use the function 'download.file')
  if(str_sub(df_val$SourceRCP,1,4) == "http"){
    if(df_val$ID %in% rv$df$ID){
      line <- as.numeric(df_val$ID)
      file.remove(paste0(getwd(),"/www/",rv$df$SourceRCP[line]))
    }
    namepdf_source_rcp <- paste0(folder_pdf,"/RCP/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf")
    if(str_sub(df_val$SourceRCP,str_length(df_val$SourceRCP)-3,str_length(df_val$SourceRCP)) == ".pdf"){
      download.file(df_val$SourceRCP, paste0("www/",namepdf_source_rcp), mode = "wb")
    } else {
      webshot(df_val$SourceRCP, paste0("www/",namepdf_source_rcp))
    }
    df_val$SourceRCP <- namepdf_source_rcp
  } 
  else if(df_val$ID %in% rv$df$ID & df_val$SourceRCP != "-"){
    line <- as.numeric(df_val$ID)
    if(rv$df$NomProduit[line] != df_val$NomProduit | rv$df$DateAMM[line] != df_val$DateAMM){
      file.rename(paste0(getwd(),"/www/",rv$df$SourceRCP[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/RCP/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf"))
      df_val$SourceRCP <- paste0(folder_pdf,"/RCP/",sprintf("%03d", line),"_",str_replace_all(str_remove_all(df_val$NomProduit," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf")
    }
  }
  

  # ---------------------------------------------------------------> Save & Validate Data
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_HAS.xlsx"))) > 0){
    QC_HAS <- rbind(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_HAS.xlsx")), df_val %>% mutate(date = as.character(Sys.time())))
  }
  else {
    QC_HAS <- df_val %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(QC_HAS, paste0(getwd(),"/",folder_import_save,"/newline_HAS.xlsx"))
  
  # Validate Data
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/newline_HAS.xlsx"))) > 0){
    QC_HAS <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/newline_HAS.xlsx")), df_val)
  }
  else {
    QC_HAS <- df_val
  }
  write_xlsx(QC_HAS, paste0(getwd(),"/",folder_data,"/newline_HAS.xlsx"))
  rv_HAS_to_qc$df <- rv_HAS_to_qc$df[-rv_HAS_to_qc$dt_row, ]
  write_xlsx(rv_HAS_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
})

shiny::observeEvent(input$valid_confirmqcHAS, {
  shiny::removeModal()
})

shiny::observeEvent(input$cancel_confirmqcHAS, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Validate QC -  Reserves CEM                                                                            ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "validateqcCEM"))
  rv_resCEM_to_qc$dt_row <- which(stringr::str_detect(rv_resCEM_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  
  shiny::modalDialog(
    title = "Validation du QC",
    "Voulez-vous valider le QC de cette ligne et l'intégrer à la base correspondante ?", br(),
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_confirmqcCEM", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_confirmqcCEM", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})

shiny::observeEvent(input$valid_confirmqcCEM, {
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_ReservesCEM.xlsx"))) > 0){
    QC_CEM <- rbind(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_ReservesCEM.xlsx")), rv_resCEM_to_qc$df[rv_resCEM_to_qc$dt_row, ] %>% mutate(date = as.character(Sys.time())))
  }
  else {
    QC_CEM <- rv_resCEM_to_qc$df[rv_resCEM_to_qc$dt_row, ] %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(QC_CEM, paste0(getwd(),"/",folder_import_save,"/newline_ReservesCEM.xlsx"))
  
  # Validate Data
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/newline_ReservesCEM.xlsx"))) > 0){
    QC_CEM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/newline_ReservesCEM.xlsx")), rv_resCEM_to_qc$df[rv_resCEM_to_qc$dt_row, ])
  }
  else {
    QC_CEM <- rv_resCEM_to_qc$df[rv_resCEM_to_qc$dt_row, ]
  }
  write_xlsx(QC_CEM, paste0(getwd(),"/",folder_data,"/newline_ReservesCEM.xlsx"))
  rv_resCEM_to_qc$df <- rv_resCEM_to_qc$df[-rv_resCEM_to_qc$dt_row, ]
  write_xlsx(rv_resCEM_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
})

shiny::observeEvent(input$valid_confirmqcCEM, {
  shiny::removeModal()
})

shiny::observeEvent(input$cancel_confirmqcCEM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Validate QC - Reserves BIM                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "validateqcBIM"))
  rv_resBIM_to_qc$dt_row <- which(stringr::str_detect(rv_resBIM_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  
  shiny::modalDialog(
    title = "Validation du QC",
    "Voulez-vous valider le QC de cette ligne et l'intégrer à la base correspondante ?", br(),
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_confirmqcBIM", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_confirmqcBIM", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})

shiny::observeEvent(input$valid_confirmqcBIM, {
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_ReservesBIM.xlsx"))) > 0){
    QC_BIM <- rbind(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_ReservesBIM.xlsx")), rv_resBIM_to_qc$df[rv_resBIM_to_qc$dt_row, ] %>% mutate(date = as.character(Sys.time())))
  }
  else {
    QC_BIM <- rv_resBIM_to_qc$df[rv_resBIM_to_qc$dt_row, ] %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(QC_BIM, paste0(getwd(),"/",folder_import_save,"/newline_ReservesBIM.xlsx"))
  
  # Validate Data
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/newline_ReservesBIM.xlsx"))) > 0){
    QC_BIM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/newline_ReservesBIM.xlsx")), rv_resBIM_to_qc$df[rv_resBIM_to_qc$dt_row, ])
  }
  else {
    QC_BIM <- rv_resBIM_to_qc$df[rv_resBIM_to_qc$dt_row, ]
  }
  write_xlsx(QC_BIM, paste0(getwd(),"/",folder_data,"/newline_ReservesBIM.xlsx"))
  rv_resBIM_to_qc$df <- rv_resBIM_to_qc$df[-rv_resBIM_to_qc$dt_row, ]
  write_xlsx(rv_resBIM_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
})

shiny::observeEvent(input$valid_confirmqcBIM, {
  shiny::removeModal()
})

shiny::observeEvent(input$cancel_confirmqcBIM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Validate QC - Table des Assocs                                                                         ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "validateqcAssoc"))
  rv_assoc_to_qc$dt_row <- which(stringr::str_detect(rv_assoc_to_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  
  shiny::modalDialog(
    title = "Validation du QC",
    "Voulez-vous valider le QC de cette ligne et l'intégrer à la base correspondante ?", br(),
    size = "m",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "cancel_confirmqcAssoc", label = "Cancel", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = "valid_confirmqcAssoc", label = "Validate", class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
})

shiny::observeEvent(input$valid_confirmqcAssoc, {
  df_val <- rv_assoc_to_qc$df[rv_assoc_to_qc$dt_row, ]

  # ---------------------------------------------------------------> Download PDFs
  # Source 1er JO (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_Source1erJO,1,4) == "http"){
    if(df_val$ID %in% rv_assoc$df$ID){
      line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_Source1erJO, "_")[[1]][2])
      file.remove(paste0(getwd(),"/www/",rv_assoc$df$JO_Source1erJO[line]))
    }
    namepdf_source_1erJO <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf")
    if(str_sub(df_val$JO_Source1erJO,str_length(df_val$JO_Source1erJO)-3,str_length(df_val$JO_Source1erJO)) == ".pdf"){
      download.file(df_val$JO_Source1erJO, paste0("www/",namepdf_source_1erJO), mode = "wb")
    } else {
      webshot(df_val$JO_Source1erJO, paste0("www/",namepdf_source_1erJO))
    }
    df_val$JO_Source1erJO <- namepdf_source_1erJO
  }
  else if(df_val$ID %in% rv_assoc$df$ID & df_val$JO_Source1erJO != "-"){
    line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_Source1erJO, "_")[[1]][2])
    if(rv_assoc$df$NomProduitAssoc[line] != df_val$NomProduitAssoc | rv_assoc$df$JO_Date1erJO[line] != df_val$JO_Date1erJO){
      file.rename(paste0(getwd(),"/www/",rv_assoc$df$JO_Source1erJO[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf"))
      df_val$JO_Source1erJO <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJO),"-"),"_premierJO.pdf")
    }
  }
  
  # Source JO pre (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_SourceJOpre,1,4) == "http"){
    if(df_val$ID %in% rv_assoc$df$ID){
      line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_SourceJOpre, "_")[[1]][2])
      file.remove(paste0(getwd(),"/www/",rv_assoc$df$JO_SourceJOpre[line]))
    }
    namepdf_source_JOpre <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf")
    if(str_sub(df_val$JO_SourceJOpre,str_length(df_val$JO_SourceJOpre)-3,str_length(df_val$JO_SourceJOpre)) == ".pdf"){
      download.file(df_val$JO_SourceJOpre, paste0("www/",namepdf_source_JOpre), mode = "wb")
    } else {
      webshot(df_val$JO_SourceJOpre, paste0("www/",namepdf_source_JOpre))
    }
    df_val$JO_SourceJOpre <- namepdf_source_JOpre
  }  
  else if(df_val$ID %in% rv_assoc$df$ID & df_val$JO_SourceJOpre != "-"){
    line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_SourceJOpre, "_")[[1]][2])
    if(rv_assoc$df$NomProduitAssoc[line] != df_val$NomProduitAssoc | rv_assoc$df$JO_DateJOpre[line] != df_val$JO_DateJOpre){
      file.rename(paste0(getwd(),"/www/",rv_assoc$df$JO_SourceJOpre[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf"))
      df_val$JO_SourceJOpre <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_DateJOpre),"-"),"_JO_preEval.pdf")
    }
  }
  
  # Source 1er JO post (use the function 'webshot' instead of 'download.file' because the link doesn't open a pdf)
  if(str_sub(df_val$JO_Source1erJOpost,1,4) == "http"){
    if(df_val$ID %in% rv_assoc$df$ID){
      line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_Source1erJOpost, "_")[[1]][2])
      file.remove(paste0(getwd(),"/www/",rv_assoc$df$JO_Source1erJOpost[line]))
    }
    namepdf_source_1erJOpost <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf")
    if(str_sub(df_val$JO_Source1erJOpost,str_length(df_val$JO_Source1erJOpost)-3,str_length(df_val$JO_Source1erJOpost)) == ".pdf"){
      download.file(df_val$JO_Source1erJOpost, paste0("www/",namepdf_source_1erJOpost), mode = "wb")
    } else {
      webshot(df_val$JO_Source1erJOpost, paste0("www/",namepdf_source_1erJOpost))
    }
    df_val$JO_Source1erJOpost <- namepdf_source_1erJOpost
  }
  else if(df_val$ID %in% rv_assoc$df$ID & df_val$JO_Source1erJOpost != "-"){
    line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$JO_Source1erJOpost, "_")[[1]][2])
    if(rv_assoc$df$NomProduitAssoc[line] != df_val$NomProduitAssoc | rv_assoc$df$JO_Date1erJOpost[line] != df_val$JO_Date1erJOpost){
      file.rename(paste0(getwd(),"/www/",rv_assoc$df$JO_Source1erJOpost[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf"))
      df_val$JO_Source1erJOpost <- paste0(folder_pdf,"/JO/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$JO_Date1erJOpost),"-"),"_JO_postEval.pdf")
    }
  }
  
  # Source RCP (use the function 'download.file')
  if(str_sub(df_val$SourceRCP,1,4) == "http"){
    if(df_val$ID %in% rv_assoc$df$ID){
      line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$SourceRCP, "_")[[1]][2])
      file.remove(paste0(getwd(),"/www/",rv_assoc$df$SourceRCP[line]))
    }
    namepdf_source_rcp <- paste0(folder_pdf,"/RCP/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf")
    if(str_sub(df_val$SourceRCP,str_length(df_val$SourceRCP)-3,str_length(df_val$SourceRCP)) == ".pdf"){
      download.file(df_val$SourceRCP, paste0("www/",namepdf_source_rcp), mode = "wb")
    } else {
      webshot(df_val$SourceRCP, paste0("www/",namepdf_source_rcp))
    }
    df_val$SourceRCP <- namepdf_source_rcp
  }
  else if(df_val$ID %in% rv_assoc$df$ID & df_val$SourceRCP != "-"){
    line <- which(rv_assoc$df$NomProduitAssoc == str_split(df_val$SourceRCP, "_")[[1]][2])
    if(rv_assoc$df$NomProduitAssoc[line] != df_val$NomProduitAssoc | rv_assoc$df$DateAMM[line] != df_val$DateAMM){
      file.rename(paste0(getwd(),"/www/",rv_assoc$df$SourceRCP[line]),
                  paste0(getwd(),"/www/",folder_pdf,"/RCP/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf"))
      df_val$SourceRCP <- paste0(folder_pdf,"/RCP/",sprintf("%03d", as.numeric(df_val$ID)),"_",str_replace_all(str_remove_all(df_val$NomProduitAssoc," "),"/","-"),"_",str_remove_all(as.character(df_val$DateAMM),"-"),"_RCP.pdf")
    }
  }
  
  
  # ---------------------------------------------------------------> Save & Validate Data
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_TableAssoc.xlsx"))) > 0){
    QC_Assoc <- rbind(read_excel(paste0(getwd(),"/",folder_import_save,"/newline_TableAssoc.xlsx")), df_val %>% mutate(date = as.character(Sys.time())))
  }
  else {
    QC_Assoc <- df_val %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(QC_Assoc, paste0(getwd(),"/",folder_import_save,"/newline_TableAssoc.xlsx"))
  
  # Validate Data
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/newline_TableAssoc.xlsx"))) > 0){
    QC_Assoc <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/newline_TableAssoc.xlsx")), df_val)
  } 
  else {
    QC_Assoc <- df_val
  }
  write_xlsx(QC_Assoc, paste0(getwd(),"/",folder_data,"/newline_TableAssoc.xlsx"))
  rv_assoc_to_qc$df <- rv_assoc_to_qc$df[-rv_assoc_to_qc$dt_row, ]
  write_xlsx(rv_assoc_to_qc$df, paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
})

shiny::observeEvent(input$valid_confirmqcAssoc, {
  shiny::removeModal()
})

shiny::observeEvent(input$cancel_confirmqcAssoc, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Reload                                                                                                 ####
#-------------------------------------------------------------------------------------------------------------------#

source(paste0(folder_import,"/final_dataset.R"))
source("global_environment.R")



## END
