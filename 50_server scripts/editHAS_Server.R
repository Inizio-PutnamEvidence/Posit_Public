#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server edit Efficiency Opinions tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    01 June 2025
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
  
  count_PopCible <- reactiveVal(0)
  count_Prix     <- reactiveVal(0)
  count_PTC      <- reactiveVal(0)
  count_PTCED    <- reactiveVal(0)
  count_resCEM   <- reactiveVal(0)
  count_resBIM   <- reactiveVal(0)
  
  source(file.path(paste0(folder_form,"/editHAS_Form.R")), local = TRUE)$value
  source(file.path(paste0(folder_form,"/add_item.R")), local = TRUE)$value
  source(file.path(paste0(folder_server,"/function_Server.R")), local = TRUE)$value

  
  # -------------------------------------------------> BLOCK HAS
  block_init_edit_has <- expression({
    
    rv <- shiny::reactiveValues(
      df = baseHAS_buttons,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(baseHAS_buttons) + 1
    )
    
    output$dt_table <- DT::renderDT({
      df <- func_FormattingHAS(rv$df) %>% mutate(Buttons = ifelse(ID %in% as.character(as.numeric(c(read_excel(paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))$ID, read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID))), "", Buttons))
      datatable(
        df, 
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv$df))]),
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
                                         list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                         list(width = '200px', targets = c(which(names(df) %in% c("Buttons","ATC_Label","HorizonTemporel","MotifReevaluation","TypeModelePutnam")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("Indication","TypeModeleHAS","Posologie_Detail","Horizon")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1))) 
        ),
        filter = 'top'
      ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule"))$Variable), backgroundColor = "#EAD5FD")  
      
    })
    
    proxy <- DT::dataTableProxy("dt_table")
    shiny::observe({
      DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> BLOCK QC HAS
  block_init_edit_qc_has <- expression({
    
    rv_qc <- shiny::reactiveValues(
      df = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(df) + 1
    )
    
    baseHAS_empty <- reactiveValues(data = NULL)
    observe({
      req(reactiveValuesToList(res_auth)$user)
      shinyjs::delay(100, {
        shinyjs::click("recover")
      })
    })
    
    # Hide the button
    observe({ shinyjs::hide("recover") })
    
    shiny::observeEvent(input$recover, {
      baseHAS_empty$data  <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
      if(nrow(baseHAS_empty$data) == 0){
        baseHAS_empty$data <- baseHAS_empty2
      }
      rv_qc$df <- baseHAS_empty$data
    })
    
    output$dt_table_qc <- DT::renderDT({
      df <- func_FormattingHAS(rv_qc$df)
      datatable(
        df, 
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons","Statut", varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(rv_qc$df))],"Auteur"),
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
                                         list(width = '200px', targets = c(which(names(df) %in% c("Buttons","ATC_Label","HorizonTemporel","MotifReevaluation","TypeModelePutnam")))-1),
                                         list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                         list(width = '150px', targets = c(which(names(df) %in% c("Auteur")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("Indication","TypeModeleHAS","Posologie_Detail","Horizon")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1))) 
        ),
      ) %>% formatStyle(columns = c((varlistHAS %>% filter(Formule == "Formule"))$Variable), backgroundColor = "#C1FFF1")
    })
    
    proxy_qc <- DT::dataTableProxy("dt_table_qc")
    shiny::observe({
      DT::replaceData(proxy_qc, rv_qc$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> CALL BLOCK 
  eval(block_init_edit_has)
  eval(block_init_edit_qc_has)
  
}  


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

delete_row(name_pattern = "deleteHAS", name_rv = rv_qc, name_excel = "SaveFormHAS", eval_block = block_init_edit_has)


#-------------------------------------------------------------------------------------------------------------------#
####      Copy row                                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

copy_row(name_pattern = "copyHAS", name_rv = rv_qc, name_create_btn = create_btns2)


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked but already in use                                                         ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  id_en_cours <- c(read_excel(paste0(getwd(),"/",folder_data,"/nicolasvirely/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/olfadoghri/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/marinesivignon/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/romainsupiot/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/rahmasellami/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/yosraboukhris/Form_in_progress.xlsx"))$dt_row,            
                   read_excel(paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))$ID,
                   read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID
                   )
  
  shiny::req((parse_number(input$current_id) %in% id_en_cours) & stringr::str_detect(input$current_id, pattern = "editHAS"))
  shiny::modalDialog(
    title = "Attention cette ligne est déjà en cours d'édition",
    p("Vous ne pouvez pas modifier cette ligne car elle est déjà en cours d'édition sur un autre compte."),
    size = "m",
    easyClose = FALSE,
    footer = shiny::actionButton(inputId = "ok", label = "Ok", class = "btn-info")
  ) %>% shiny::showModal()
})

shiny::observeEvent(input$ok, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked, modal dialog shows current editable row filled out                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  id_en_cours <- c(read_excel(paste0(getwd(),"/",folder_data,"/nicolasvirely/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/olfadoghri/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/marinesivignon/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/romainsupiot/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/rahmasellami/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/yosraboukhris/Form_in_progress.xlsx"))$dt_row,
                   read_excel(paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))$ID,
                   read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID
                   )
  
  shiny::req(!is.null(input$current_id) & !(parse_number(input$current_id) %in% id_en_cours) & stringr::str_detect(input$current_id, pattern = "editHAS"))
  rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv$df[rv$dt_row, ]

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
  
  df_form <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  df_form_inprogress <- df_form %>% mutate_if(is.logical, as.character) %>% add_row(type_form  = "edit1",
                                                                                    dt_row     = as.character(rv$dt_row),
                                                                                    current_id = as.character(input$current_id),
                                                                                    source_ct          = df$SourceAvisCT,
                                                                                    source_avis_CEESP  = df$SourceAvisCEESP,
                                                                                    JO_Source1erJO     = df$JO_Source1erJO, 
                                                                                    JO_SourceJOpre     = df$JO_SourceJOpre, 
                                                                                    JO_Source1erJOpost = df$JO_Source1erJOpost,
                                                                                    source_rcp         = df$SourceRCP)
  write_xlsx(df_form_inprogress, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  
  # Call modal_dialog with edit = edit_module = TRUE
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
          edit_module = TRUE
  )}
  
  rv$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit, {
  if(nrow(df_form)>0){
    shiny::req(!is.null(df_form$current_id) & stringr::str_detect(df_form$current_id, pattern = "editHAS") & is.null(rv$add_or_edit))
  }
  else{
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editHAS") & is.null(rv$add_or_edit))
  }  
  
  if(input$sel_horizon_temp == "Vie entière"){ horizon_temp = ifelse(input$horizon_temp %in% c("sans précision","","-"), "Vie entière (sans précision)", paste0(input$sel_horizon_temp, " (", input$horizon_temp, ")")) }
  if(input$sel_horizon_temp != "Vie entière"){ horizon_temp = input$horizon_temp }
  id_buttons <- ifelse(nrow(rv_qc$df) == 0, 1, max(as.numeric(str_extract(rv_qc$df$Buttons, "(\\d+)"))) +1)

  edited_row <- {dplyr::tibble(
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
  edited_row <- func_nettoyage_old_information_gen(func_nettoyage_old_information(edited_row, checkbox1erJO = input$afficher_1erJO, checkboxJOVigueur = input$afficher_JOvigueur, checkbox1erJOPostAvis = input$afficher_1erJOpostavis))
  edited_row <- func_mutate_edit(edited_row, "has")


  if(input$add_resCEM == 1){
    for(num_resCEM in 1:count_resCEM()){
      if(input[[paste0("CEM_Precision_",num_resCEM)]] != ""){
        id_buttons_resCEM <- ifelse(nrow(rv_resCEM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resCEM_qc$df$Buttons, "(\\d+)"))) +1)	
        new_row_resCEM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
          # Reserves BIM
          ###########################################CENSORED######################
        ) 
        ###########################################CENSORED######################
      }
    }
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
    autosave_editCEM()
  }

  
  if(input$add_resBIM == 1){
    for(num_resBIM in 1:count_resBIM()){
      if(input[[paste0("BIM_Precision_",num_resBIM)]] != ""){
        id_buttons_resBIM <- ifelse(nrow(rv_resBIM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resBIM_qc$df$Buttons, "(\\d+)"))) +1)
        new_row_resBIM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
           # Reserves BIM
          ###########################################CENSORED######################
        ) 
        ###########################################CENSORED######################
        }
    }
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
    autosave_editBIM()
  }
  
  
  edited_row <- edited_row %>% mutate_if(is.logical, as.character)
  rv_qc$df   <- rv_qc$df %>% mutate_if(is.logical, as.character)
  rv_qc$df   <- edited_row %>% dplyr::bind_rows(rv_qc$df)            
  
  
  shiny::removeModal()
  shiny::modalDialog(
    title = "Commentaires",
    radioButtons(inputId = "type_edit", 
                 label = "Sélectionner le but de cette modification", 
                 choices = c("1. Ajout d'une ou plusieurs nouvelle(s) réserve(s)", 
                             "2. Modification d'une ligne de la base HAS", 
                             "3. Modification d'une ligne de la base HAS et ajout d'une nouvelle réserve"), 
                 width = "100%", 
                 selected = "1. Ajout d'une ou plusieurs nouvelle(s) réserve(s)"),
    textAreaInput(inputId = "commentsEdit", label = NULL, placeholder = "Comments", width = "100%"),
    size = "m",
    easyClose = FALSE,
    footer = shiny::actionButton(inputId = "valid_comment_edit", label = "Validate", class = "btn-info")
  ) %>% shiny::showModal()
  
  # Deleting the current line 
  df_form <- df_form[-1,]
  write_xlsx(df_form, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
})


shiny::observeEvent(input$valid_comment_edit, {
  rv_qc$df[1,"CommentsEdit"] <- input$commentsEdit
  rv_qc$df[1,"Auteur"]  <- reactiveValuesToList(res_auth)$user
  if(input$type_edit == "1. Ajout d'une ou plusieurs nouvelle(s) réserve(s)"){
    rv_qc$df <- rv_qc$df[-1,]
  }
  # to keep if HTA changes advise
  # if(input$type_edit != "1. Ajout d'une ou plusieurs nouvelle(s) réserve(s)"){
  #   rv$df$Buttons[rv$dt_row] <- ""
  # }
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
  autosave_editHAS()
})


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked on temporary table, modal dialog shows current editable row filled out     ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformHAS"))
  rv_qc$dt_row <- which(stringr::str_detect(rv_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_qc$df[rv_qc$dt_row, ]

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
  
  df_form = read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  df_form_inprogress = df_form %>% mutate_if(is.logical, as.character) %>% add_row(type_form  = "edit2",
                                                                                   dt_row     = as.character(rv_qc$dt_row),
                                                                                   current_id = as.character(input$current_id),
                                                                                   source_ct          = df$SourceAvisCT,
                                                                                   source_avis_CEESP  = df$SourceAvisCEESP,
                                                                                   JO_Source1erJO     = df$JO_Source1erJO, 
                                                                                   JO_SourceJOpre     = df$JO_SourceJOpre, 
                                                                                   JO_Source1erJOpost = df$JO_Source1erJOpost,
                                                                                   source_rcp         = df$SourceRCP)
  write_xlsx(df_form_inprogress, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  
  # Call modal_dialog with edit = edit_module = TRUE
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
          edit_module = TRUE
  )}

  rv$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit, {
  if(nrow(df_form)>0){
    shiny::req(!is.null(df_form$current_id) & stringr::str_detect(df_form$current_id, pattern = "editformHAS") & is.null(rv$add_or_edit))
  }
  else{
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformHAS") & is.null(rv_qc$add_or_edit))
  }
  
  if(input$sel_horizon_temp == "Vie entière"){ horizon_temp = ifelse(input$horizon_temp %in% c("sans précision","","-"), "Vie entière (sans précision)", paste0(input$sel_horizon_temp, " (", input$horizon_temp, ")")) }
  if(input$sel_horizon_temp != "Vie entière"){ horizon_temp = input$horizon_temp }
  
  old_comment <- rv_qc$df$CommentsEdit[rv_qc$dt_row]

  edited_form_row <- {dplyr::tibble(
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
  edited_form_row <- func_nettoyage_old_information_gen(func_nettoyage_old_information(edited_form_row, checkbox1erJO = input$afficher_1erJO, checkboxJOVigueur = input$afficher_JOvigueur, checkbox1erJOPostAvis = input$afficher_1erJOpostavis))
  edited_form_row <- func_mutate_edit(edited_form_row, "has")
  
  
  if(input$add_resCEM == 1){
    for(num_resCEM in 1:count_resCEM()){
      if(input[[paste0("CEM_Precision_",num_resCEM)]] != ""){
        id_buttons_resCEM <- ifelse(nrow(rv_resCEM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resCEM_qc$df$Buttons, "(\\d+)"))) +1)		
        new_row_resCEM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
          # Reserves CEM
          ###########################################CENSORED######################
        ) 
        ###########################################CENSORED######################
      }
    }
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
    autosave_editCEM()
  }
  
  
  if(input$add_resBIM == 1){
    for(num_resBIM in 1:count_resBIM()){
      if(input[[paste0("BIM_Precision_",num_resBIM)]] != ""){
        id_buttons_resBIM <- ifelse(nrow(rv_resBIM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resBIM_qc$df$Buttons, "(\\d+)"))) +1)
        new_row_resBIM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
          # Reserves BIM
          ###########################################CENSORED######################
        ) 
        ###########################################CENSORED######################
      }
    }
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
    autosave_editBIM()
  }
  

  edited_form_row <- edited_form_row %>% mutate_if(is.logical, as.character)   
  rv_qc$df <- rv_qc$df %>% mutate_if(is.logical, as.character)
  rv_qc$df <- rv_qc$df[-rv_qc$dt_row, ]
  rv_qc$df <- edited_form_row %>% dplyr::bind_rows(rv_qc$df)
  
  
  shiny::removeModal()
  shiny::modalDialog(
    title = "Commentaires",
    textAreaInput(inputId = "commentsEdit", label = NULL, placeholder =  "Comments", width = "100%", value = old_comment),
    size = "m",
    easyClose = FALSE,
    footer = shiny::actionButton(inputId = "valid_comment", label = "Validate", class = "btn-info")
  ) %>% shiny::showModal()
  
  # Deleting the current line
  df_form <- df_form[-1,]
  write_xlsx(df_form, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
})


shiny::observeEvent(input$valid_comment, {
  rv_qc$df[1,"CommentsEdit"] <- input$commentsEdit
  rv_qc$df[1,"Auteur"]  <- reactiveValuesToList(res_auth)$user
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
  autosave_editHAS()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Adding value in table - initialization of form                                                         ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$add_row, {
  count_PopCible(1)
  count_Prix(1)
  count_PTC(1) 
  count_PTCED(1) 
  count_resCEM(1) 
  count_resBIM(1)
  
  df_form <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  df_form_inprogress <- df_form %>% mutate_if(is.logical, as.character) %>% add_row(type_form  = "newline",
                                                                                    dt_row     = "",
                                                                                    current_id = "NULL")
  write_xlsx(df_form_inprogress, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  
  # Call modal_dialog with edit = FALSE & edit_module = TRUE
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
          PVC_Nbinj = "",
    # Edit
          edit = FALSE,
    # Edit module
          edit_module = TRUE
  )}

  rv$add_or_edit <- 1
})


#-------------------------------------------------------------------------------------------------------------------#
####      Adding value in table                                                                                  ####
#-------------------------------------------------------------------------------------------------------------------#

rv_table_assoc <- reactiveValues(nb_products = 1, nb_products_tot = 1) 

shiny::observeEvent(input$final_edit, {
  shiny::req(rv$add_or_edit == 1)
  if(input$sel_horizon_temp == "Vie entière"){ horizon_temp = ifelse(input$horizon_temp %in% c("sans précision","","-"), "Vie entière (sans précision)", paste0(input$sel_horizon_temp, " (", input$horizon_temp, ")")) }
  if(input$sel_horizon_temp != "Vie entière"){ horizon_temp = input$horizon_temp }
  id_buttons <- ifelse(nrow(rv_qc$df) == 0, 1, max(as.numeric(str_extract(rv_qc$df$Buttons, "(\\d+)"))) +1)
  newID <- as.character(max(as.numeric(c(rv$df$ID,
                                         read_excel(paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))$ID,
                                         read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID
                                         ))) +1)
  
  table_assoc <- input$Asso_Several_Products

  add_row <- {dplyr::tibble(
    # Warning, these 3 variables are different between the section
    ###########################################CENSORED######################
    # Informations Générales
    ###########################################CENSORED######################
    # Evaluation HAS
    ###########################################CENSORED######################
    # Prix
    ###########################################CENSORED######################
    # Posologie
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
    ###########################################CENSORED######################
    # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
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
  add_row <- func_mutate_edit(add_row, "has")

  
  if(input$add_resCEM == 1){
    for(num_resCEM in 1:count_resCEM()){
      if(input[[paste0("CEM_Precision_",num_resCEM)]] != ""){
        id_buttons_resCEM <- ifelse(nrow(rv_resCEM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resCEM_qc$df$Buttons, "(\\d+)"))) +1)		
        new_row_resCEM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
          # Reserves CEM
          ###########################################CENSORED######################
        )
        ###########################################CENSORED######################
      }
    }
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
    autosave_editCEM()
  }

  
  if(input$add_resBIM == 1){
    for(num_resBIM in 1:count_resBIM()){
      if(input[[paste0("BIM_Precision_",num_resBIM)]] != ""){
        id_buttons_resBIM <- ifelse(nrow(rv_resBIM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resBIM_qc$df$Buttons, "(\\d+)"))) +1)
        new_row_resBIM <- dplyr::tibble(
          # Other variables
          ###########################################CENSORED######################
          # Display some variables from HAS table
          ###########################################CENSORED######################
          # Reserves BIM
          ###########################################CENSORED######################
        )
        ###########################################CENSORED######################
      }
    }
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
    autosave_editBIM()
  }
  
  
  add_row  <- add_row %>% mutate_if(is.logical, as.character)
  rv_qc$df <- rv_qc$df %>% mutate_if(is.logical, as.character) 
  rv_qc$df <- add_row %>% dplyr::bind_rows(rv_qc$df)
  rv_qc$keep_track_id <- rv_qc$keep_track_id + 1
  count_PTC(NULL)
  

  if(table_assoc == "Oui"){
    rv_table_assoc$nb_products <- 1
    rv_table_assoc$nb_products_tot <- input$Nb_Products_Asso
    count_Prix(1)
    count_PTC(1) 
    count_PTCED(1) 
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
              PVC_Nbinj = "",
        # Edit
        ###########################################CENSORED######################
      )}
    rv_assoc$add_or_edit <- 1
    rv_table_assoc$nb_products <- rv_table_assoc$nb_products + 1
  }
  
  
  if(table_assoc != "Oui"){
    shiny::modalDialog(
      title = "Commentaires",
      textAreaInput(inputId = "commentsEdit", label = NULL, placeholder = "Comments", width = "100%"),
      size = "m",
      easyClose = FALSE,
      footer = shiny::actionButton(inputId = "valid_comment", label = "Validate", class = "btn-info")
    ) %>% shiny::showModal()
  }
  
  # Deleting the current
  df_form <- df_form[-1,]
  write_xlsx(df_form, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
})


shiny::observeEvent(input[[paste0("final_edit_assoc_",(rv_table_assoc$nb_products)-1)]], {
  shiny::req(rv_assoc$add_or_edit == 1)
  id_buttons <- ifelse(nrow(rv_assoc_qc$df) == 0, 1, max(as.numeric(str_extract(rv_assoc_qc$df$Buttons, "(\\d+)"))) +1)

  add_row_assoc <- {dplyr::tibble(
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
    ###########################################CENSORED######################
  )}
  
  add_row_assoc <- func_mutate_edit(add_row_assoc, "assoc")
  add_row_assoc <- as.data.frame(add_row_assoc)
  add_row_assoc <- add_row_assoc %>% select(c("Buttons","Statut","ID","NomProduit","NomProduitAssoc","DateAMM","DateValidationCEESP","Demande","TypeProduit", varlistHAS$Variable[which(varlistHAS$`Table des associations` == "Oui")]))
  add_row_assoc <- add_row_assoc %>% mutate_if(is.logical, as.character)
  
  rv_assoc_qc$df <- rv_assoc_qc$df %>% mutate_if(is.logical, as.character)
  rv_assoc_qc$df <- add_row_assoc %>% dplyr::bind_rows(rv_assoc_qc$df)
  rv_assoc_qc$df[1,"Auteur"] <- reactiveValuesToList(res_auth)$user
  # to keep if HTA changes advise
  # rv_assoc$df$Buttons[rv_assoc$dt_row] <- ""
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
  write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
  autosave_editAssoc()
  
  
  if(rv_table_assoc$nb_products > rv_table_assoc$nb_products_tot)({
    shiny::modalDialog(
      title = "Commentaires",
      textAreaInput(inputId = "commentsEdit", label = NULL, placeholder = "Comments", width = "100%"),
      size = "m",
      easyClose = FALSE,
      footer = shiny::actionButton(inputId = "valid_comment", label = "Validate", class = "btn-info")
    ) %>% shiny::showModal()
  })
  
  
  if(rv_table_assoc$nb_products <= rv_table_assoc$nb_products_tot)({
    count_Prix(1)
    count_PTC(1)
    count_PTCED(1)
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
            PVC_Nbinj = "",
      # Edit
      ###########################################CENSORED######################
    )}
    rv_assoc$add_or_edit <- 1
    rv_table_assoc$nb_products <- rv_table_assoc$nb_products +1
  })
  
})


# Note: input$valid_comment already exists with the same information, and that's probably why it's causing problems in the EDIT, so I hide
# shiny::observeEvent(input$valid_comment, {
#   rv_qc$df[1,"CommentsEdit"] <- input$commentsEdit
#   rv_qc$df[1,"Auteur"]  <- reactiveValuesToList(res_auth)$user
#   write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))
#   write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
#   autosave_editHAS() # don't put because duplicates thelines
# })


#-------------------------------------------------------------------------------------------------------------------#
####      Save form in progress                                                                                  ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$tabsform, {
  {
    df_form_inprogress <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
    df_form_inprogress <- df_form_inprogress %>%
      mutate(
      # Other variables
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
      ###########################################CENSORED######################v
      # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
      ###########################################CENSORED######################
      # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
      ###########################################CENSORED######################
      # Posologie ---> Mono/Asso : Traitement Non Chronique
      ###########################################CENSORED######################
      # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
      ###########################################CENSORED######################
      # Posologie ---> Vaccin
            PVC_Nbinj = fct_input_vide(input$PVC_Nbinj)
      )
  }
  write_xlsx(df_form_inprogress, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
})


#-------------------------------------------------------------------------------------------------------------------#
####      Reopen form in progress                                                                                ####
#-------------------------------------------------------------------------------------------------------------------#

observe({
  req(reactiveValuesToList(res_auth)$user)
  df_form = read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  if(nrow(df_form) > 0){
    shinyjs::delay(100, {
      
      if(df_form$type_form != "newline"){
        if(df_form$type_form == "edit1"){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", df_form$current_id, "\\b")))
        }
        if(df_form$type_form == "edit2"){
          rv_qc$dt_row <- which(stringr::str_detect(rv_qc$df$Buttons, pattern = paste0("\\b", df_form$current_id, "\\b")))
        }
        rv$add_or_edit <- NULL
      }
      
      if(df_form$type_form == "newline"){
        rv$add_or_edit <- 1
      }
      
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
      
      # Call modal_dialog with edit = TRUE/FALSE & edit_module = TRUE
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
              PVC_Nbinj = df_form$PVC_Nbinj,
        # Edit
              edit = ifelse(df_form$type_form == "newline", FALSE, TRUE), 
        # Edit module
              edit_module = TRUE
        )}
      
    })
  }
  
})


#-------------------------------------------------------------------------------------------------------------------#
####      Remove edit modal when close button is clicked or submit button                                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$dismiss_modal, {
  # Deleting the current line
  df_form <- df_form[-1,]
  write_xlsx(df_form, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/Form_in_progress.xlsx"))
  shiny::removeModal()
})


shiny::observeEvent(input$valid_comment, {
  shiny::removeModal()
})


shiny::observeEvent(input$valid_comment_edit, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Read comment button                                                                                    ####
#-------------------------------------------------------------------------------------------------------------------#

read_comment(name_pattern = "commentHAS", name_rv = rv_qc)
read_comment_HAS(name_pattern = "commentQCView", name_rv = rv)


#-------------------------------------------------------------------------------------------------------------------#
####      Send to QC                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

send_qc(name_pattern = "QCHAS", name_rv = rv_qc)


shiny::observeEvent(input$valid_qc, {
  if(nrow(filter(rv_resCEM_qc$df, ID == rv_qc$df$ID[rv_qc$dt_row])) > 0){
    ResCEM_to_QC <- filter(rv_resCEM_qc$df, ID == rv_qc$df$ID[rv_qc$dt_row])
    rv_resCEM_qc$df <- setdiff(rv_resCEM_qc$df, ResCEM_to_QC)
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      ResCEM_to_QC$Buttons <- create_btns_ResCEM_qc((nrow(rv_resCEM_to_qc$df)+1):(nrow(rv_resCEM_to_qc$df)+nrow(ResCEM_to_QC)))
    }
    else{
      ResCEM_to_QC$Buttons <- ""
    }
  }
  if(nrow(filter(rv_resBIM_qc$df, ID == rv_qc$df$ID[rv_qc$dt_row])) > 0){
    ResBIM_to_QC <- filter(rv_resBIM_qc$df, ID == rv_qc$df$ID[rv_qc$dt_row]) 
    rv_resBIM_qc$df <- setdiff(rv_resBIM_qc$df, ResBIM_to_QC)
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      ResBIM_to_QC$Buttons <- create_btns_ResBIM_qc((nrow(rv_resBIM_to_qc$df)+1):(nrow(rv_resBIM_to_qc$df)+nrow(ResBIM_to_QC)))
    }
    else{
      ResBIM_to_QC$Buttons <- ""
    }
  }
  if(nrow(filter(rv_assoc_qc$df,  ID == rv_qc$df$ID[rv_qc$dt_row])) > 0){
    Assoc_to_QC  <- filter(rv_assoc_qc$df,  ID == rv_qc$df$ID[rv_qc$dt_row])
    rv_assoc_qc$df  <- setdiff(rv_assoc_qc$df,  Assoc_to_QC)
    if(reactiveValuesToList(res_auth)$user %in% users_qc){
      Assoc_to_QC$Buttons <- create_btns_assoc_qc((nrow(rv_assoc_to_qc$df)+1):(nrow(rv_assoc_to_qc$df)+nrow(Assoc_to_QC)))
    }
    else{
      Assoc_to_QC$Buttons <- ""
    }
  }

  #----------------------------------------------------------------------------------> Focus - New HAS
  if(rv_qc$df$Statut[rv_qc$dt_row] == "Nouvelle HAS"){
    newID <- as.character(max(as.numeric(c(rv$df$ID, read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID)))+1)
    rv_qc$df$ID[rv_qc$dt_row] <- newID
    rv_qc$df$Index[rv_qc$dt_row] <- paste0(paste0(newID, rv_qc$df$Molecule[rv_qc$dt_row], as.numeric(rv_qc$df$DateValidationCEESP[rv_qc$dt_row])+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2))
    
    if(exists("ResCEM_to_QC")){
      ResCEM_to_QC$ID <- newID
      ResCEM_to_QC$Index <- paste0(paste0(newID, ResCEM_to_QC$Molecule, as.numeric(ResCEM_to_QC$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2))
    }
    if(exists("ResBIM_to_QC")){
      ResBIM_to_QC$ID <- newID
      ResBIM_to_QC$Index <- paste0(paste0(newID, ResBIM_to_QC$Molecule, as.numeric(ResBIM_to_QC$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2))
    }
    if(exists("Assoc_to_QC")){
      Assoc_to_QC$ID <- newID
      # no variable INDEX here
    }
  }

  #----------------------------------------------------------------------------------> Buttons
  rv_qc$df$Buttons[rv_qc$dt_row ] <- ifelse(reactiveValuesToList(res_auth)$user %in% users_qc,
                                            create_btns_HAS_qc_with_right(nrow(rv_HAS_to_qc$df)+1),
                                            create_btns_HAS_qc_without_right(nrow(rv_HAS_to_qc$df)+1))

  #----------------------------------------------------------------------------------> Base HAS
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))) > 0){
    QC_HAS <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx")), rv_qc$df[rv_qc$dt_row, ])
  }
  else {
    QC_HAS <- rv_qc$df[rv_qc$dt_row, ]
  }
  rv_HAS_to_qc$df <- rbind(rv_HAS_to_qc$df, rv_qc$df[rv_qc$dt_row, ])
  write_xlsx(QC_HAS, paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))
  rv_qc$df <- rv_qc$df[-rv_qc$dt_row, ]
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))
  write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
  
  #----------------------------------------------------------------------------------> Reserves CEM
  if(exists("ResCEM_to_QC")){
    if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))) > 0){
      QC_ResCEM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx")), ResCEM_to_QC)
    }
    else {
      QC_ResCEM <- ResCEM_to_QC
    }
    rv_resCEM_to_qc$df <- rbind(rv_resCEM_to_qc$df, ResCEM_to_QC)
    write_xlsx(QC_ResCEM, paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
    write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
  }
  
  #----------------------------------------------------------------------------------> Reserves BIM
  if(exists("ResBIM_to_QC")){
    if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))) > 0){
      QC_ResBIM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx")), ResBIM_to_QC)
    }
    else {
      QC_ResBIM <- ResBIM_to_QC
    }
    rv_resBIM_to_qc$df <- rbind(rv_resBIM_to_qc$df, ResBIM_to_QC)
    write_xlsx(QC_ResBIM, paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
    write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
  }
  
  #----------------------------------------------------------------------------------> Table des associations
  if(exists("Assoc_to_QC")){
    if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))) > 0){
      QC_Assoc <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx")), Assoc_to_QC)
    }
    else {
      QC_Assoc <- Assoc_to_QC
    }
    rv_assoc_to_qc$df <- rbind(rv_assoc_to_qc$df, Assoc_to_QC)
    write_xlsx(QC_Assoc, paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))
    write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormTableAssoc_commun.xlsx"))
    write_xlsx(rv_assoc_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormTableAssoc.xlsx"))
  }
  
  shiny::removeModal()
})



## END
