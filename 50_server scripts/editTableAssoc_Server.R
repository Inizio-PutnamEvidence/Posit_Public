#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for edit Combination Products tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  05 December 2023
#   LAST MODIFIED DATE    25 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of a form
#
#--------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization                                                                            ####
#-------------------------------------------------------------------------------------------------------------------#
{
  vals <- reactiveValues(data = NULL)

  source(file.path(paste0(folder_form,"/editTableAssoc_Form.R")), local = TRUE)$value
  
  
  # -------------------------------------------------> BLOCK ASSOC
  block_init_edit_assoc <- expression({
    
    rv_assoc <- shiny::reactiveValues(
      df = tableAssoc_buttons,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(tableAssoc_buttons) + 1
    )
    
    output$dt_tableassoc <- DT::renderDT({
      TableAssoc_work <- read_excel(paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
      QC_TableAssoc   <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
      ID_encours <- data.frame(ID = NA, NomProduitAssoc = NA) %>%
        mutate(ID = ifelse((nrow(TableAssoc_work) > 0 & nrow(QC_TableAssoc) > 0), c(TableAssoc_work$ID, QC_TableAssoc$ID),
                           ifelse(nrow(TableAssoc_work) > 0, TableAssoc_work$ID,
                                  ifelse(nrow(QC_TableAssoc) > 0, QC_TableAssoc$ID, NA))),
               NomProduitAssoc = ifelse((nrow(TableAssoc_work) > 0 & nrow(QC_TableAssoc) > 0), c(TableAssoc_work$NomProduitAssoc, QC_TableAssoc$NomProduitAssoc),
                                        ifelse(nrow(TableAssoc_work) > 0, TableAssoc_work$NomProduitAssoc,
                                               ifelse(nrow(QC_TableAssoc) > 0, QC_TableAssoc$NomProduitAssoc, NA))))
      
      df <- func_FormattingAssoc(rv_assoc$df, "with_select") %>% mutate(Buttons = ifelse(ID %in% as.character(as.numeric(ID_encours$ID)) & NomProduitAssoc %in% as.character(ID_encours$NomProduitAssoc), "", Buttons))
      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons",
                     varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc$df) & varlistHAS$Variable %in% c("ID","NomProduit"))],
                     "Nom du produit Assoc.",
                     varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc$df) & !varlistHAS$Variable %in% c("ID","NomProduit"))]),
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
                                         list(width = '50px',  targets = c(which(names(df) %in% c("Buttons","ID")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("Indication","Posologie_Detail","Horizon")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1)))
        ),
        filter = 'top'
      ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule" & `Table des associations` == "Oui"))$Variable), backgroundColor = "#EAD5FD")
    })
    
    proxy <- DT::dataTableProxy("dt_tableassoc")
    shiny::observe({
      DT::replaceData(proxy, rv_assoc$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> BLOCK QC ASSOC
  block_init_edit_qc_assoc <- expression({
    
    rv_assoc_qc <- shiny::reactiveValues(
      df = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(df) + 1
    )
    
    tableAssoc_empty <- reactiveValues(data = NULL)
    observe({
      shinyjs::delay(100, {
        shinyjs::click("recoverAssoc")
      })
    })
    
    # Hide the button
    observe({ shinyjs::hide("recoverAssoc") })
    
    shiny::observeEvent(input$recover, {
      req(reactiveValuesToList(res_auth)$user)
      tableAssoc_empty$data  <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
      if(nrow(tableAssoc_empty$data) == 0){
        tableAssoc_empty$data <- tableAssoc_empty2
      }
      rv_assoc_qc$df <- tableAssoc_empty$data
    })
    
    output$dt_tableassoc_qc <- DT::renderDT({
      df <- func_FormattingAssoc(rv_assoc_qc$df, "no_select")
      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", "Statut",
                     varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc_qc$df) & varlistHAS$Variable %in% c("ID","NomProduit"))],
                     "Nom du produit Assoc.",
                     varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_assoc_qc$df) & !varlistHAS$Variable %in% c("ID","NomProduit"))],
                     "Auteur"),
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
      ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule" & `Table des associations` == "Oui"))$Variable), backgroundColor = "#C1FFF1")
    })
    
    proxy_qc <- DT::dataTableProxy("dt_tableassoc_qc")
    shiny::observe({
      DT::replaceData(proxy_qc, rv_assoc_qc$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> CALL BLOCK 
  eval(block_init_edit_assoc)
  eval(block_init_edit_qc_assoc)
  
}  


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

delete_row(name_pattern = "deleteAssoc", name_rv = rv_assoc_qc, name_excel = "SaveFormTableAssoc", eval_block = block_init_edit_assoc)


#-------------------------------------------------------------------------------------------------------------------#
####      Copy row                                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

# Impossible action


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked, modal dialog shows current editable row filled out                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editAssoc"))
  rv_assoc$dt_row <- which(stringr::str_detect(rv_assoc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_assoc$df[rv_assoc$dt_row, ]
  
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
  
  rv_assoc$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_assoc, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editAssoc") & is.null(rv_assoc$add_or_edit))
  id_buttons <- ifelse(nrow(rv_assoc_qc$df) == 0, 1, max(as.numeric(str_extract(rv_assoc_qc$df$Buttons, "(\\d+)"))) +1)
  
  edited_row_assoc <- {dplyr::tibble(
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
          # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Non Chronique
    ###########################################CENSORED######################
    # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique  
    ###########################################CENSORED######################
    # Posologie ---> Vaccin
          PVC_Nb = input$PVC_Nbinj
  )}
  
  ###########################################CENSORED######################
  # to keep if HTA changes advise
  # rv_assoc$df$Buttons[rv_assoc$dt_row] <- ""
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
  autosave_editAssoc()
})


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked on temporary table, modal dialog shows current editable row filled out     ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformAssoc"))
  rv_assoc_qc$dt_row <- which(stringr::str_detect(rv_assoc_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_assoc_qc$df[rv_assoc_qc$dt_row, ]
  
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
  
  rv_assoc$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_assoc, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformAssoc") & is.null(rv_assoc_qc$add_or_edit))
  
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
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
  autosave_editAssoc()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Remove edit modal when close button is clicked or submit button                                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$dismiss_modal_assoc, {
  shiny::removeModal()
})


shiny::observeEvent(input$final_edit_assoc, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Send to QC                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "QCAssoc"))
  rv_assoc_qc$dt_row <- which(stringr::str_detect(rv_assoc_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  rv_assoc_qc$df$Buttons[rv_assoc_qc$dt_row ] <- ifelse(reactiveValuesToList(res_auth)$user %in% users_qc,
                                                        create_btns_assoc_qc(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx")))+1),
                                                        "")
  
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))) > 0){
    QC_Assoc <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx")), rv_assoc_qc$df[rv_assoc_qc$dt_row, ])
  }
  else {
    QC_Assoc <- rv_assoc_qc$df[rv_assoc_qc$dt_row, ]
  }
  
  write_xlsx(QC_Assoc, paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
  rv_assoc_qc$df <- setdiff(rv_assoc_qc$df, QC_Assoc)
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
  eval(block_qc_assoc)
})



## END
