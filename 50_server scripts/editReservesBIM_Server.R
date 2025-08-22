#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for edit BIM Reservations tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
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
  vals <- reactiveValues(data = data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==NULL)
  
  source(file.path(paste0(folder_form,"/editReservesBIM_Form.R")), local = TRUE)$value
  
  
  # -------------------------------------------------> BLOCK BIM
  block_init_edit_bim <- expression({
    
    rv_resBIM <- shiny::reactiveValues(
      df = reservesBIM_buttons,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(reservesBIM_buttons) + 1
    )

    output$dt_reservesBIM <- DT::renderDT({
      ReservesBIM_work <- read_excel(paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
      QC_ReservesBIM   <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
      ID_encours <- data.frame(ID = NA, IDRes = NA) %>%
        mutate(ID = ifelse((nrow(ReservesBIM_work) > 0 & nrow(QC_ReservesBIM) > 0), c(ReservesBIM_work$ID, QC_ReservesBIM$ID),
                           ifelse(nrow(ReservesBIM_work) > 0, ReservesBIM_work$ID,
                                  ifelse(nrow(QC_ReservesBIM) > 0, QC_ReservesBIM$ID, NA))),
               IDRes = ifelse((nrow(ReservesBIM_work) > 0 & nrow(QC_ReservesBIM) > 0), c(ReservesBIM_work$IDRes, QC_ReservesBIM$IDRes),
                              ifelse(nrow(ReservesBIM_work) > 0, ReservesBIM_work$IDRes,
                                     ifelse(nrow(QC_ReservesBIM) > 0, QC_ReservesBIM$IDRes, NA))))
      
      df <- func_FormattingRes(rv_resBIM$df) %>% mutate(Buttons = ifelse(ID %in% as.character(as.numeric(ID_encours$ID)) & IDRes %in% as.character(as.numeric(ID_encours$IDRes)), "", Buttons))
      datatable(
        df, 
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", "Numéro", "Numéro réserve",
                    as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resBIM$df) & dico$Variable != "ID")])),
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
                       columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule")))-1),
                                         list(width = '50px',  targets = c(which(names(df) %in% c("Buttons","ID","IDRes")))-1),
                                         list(width = '200px', targets = c(which(names(df) %in% c("BIM_DimHAS","BIM_DimPutnam")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("BIM_Precision")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1)))
        ),
        filter = 'top'   
        )
    })
    
    proxy <- DT::dataTableProxy("dt_reservesBIM")
    shiny::observe({
      DT::replaceData(proxy, rv_resBIM$df, resetPaging = FALSE, rownames = FALSE)
    })
  
  })
  
  
  # -------------------------------------------------> BLOCK QC BIM
  block_init_edit_qc_bim <- expression({
    
    rv_resBIM_qc <- shiny::reactiveValues(
      df = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(df) + 1
    )
    
    reservesBIM_empty <- reactiveValues(data = NULL)
    observe({
      req(reactiveValuesToList(res_auth)$user)
      shinyjs::delay(100, {
        shinyjs::click("recoverBIM")
      })
    })
    
    # Hide the button
    observe({ shinyjs::hide("recoverBIM") })
    
    shiny::observeEvent(input$recover, {
      reservesBIM_empty$data  <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
      if(nrow(reservesBIM_empty$data) == 0){
        reservesBIM_empty$data <- reservesBIM_empty2
      }
      rv_resBIM_qc$df <- reservesBIM_empty$data
    })

    output$dt_reservesBIM_qc <- DT::renderDT({
      df <- func_FormattingRes(rv_resBIM_qc$df)
      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", "Statut", "Numéro", "Numéro réserve", 
                     as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resBIM_qc$df) & dico$Variable != "ID")]),
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
                       columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule")))-1),
                                         list(width = '150px', targets = c(which(names(df) %in% c("Buttons","Auteur")))-1),
                                         list(width = '50px',  targets = c(which(names(df) %in% c("ID","IDRes")))-1),
                                         list(width = '200px', targets = c(which(names(df) %in% c("BIM_DimHAS","BIM_DimPutnam")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("BIM_Precision")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1)))
        )
      )
    })
  
    proxy_qc <- DT::dataTableProxy("dt_reservesBIM_qc")
    shiny::observe({
      DT::replaceData(proxy_qc, rv_resBIM_qc$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> CALL BLOCK 
  eval(block_init_edit_bim)
  eval(block_init_edit_qc_bim)

}  


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

delete_row(name_pattern = "deleteBIM", name_rv = rv_resBIM_qc, name_excel = "SaveFormReservesBIM", eval_block = block_init_edit_bim)


#-------------------------------------------------------------------------------------------------------------------#
####      Copy row                                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

# Impossible action


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked, modal dialog shows current editable row filled out                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editBIM"))
  rv_resBIM$dt_row <- which(stringr::str_detect(rv_resBIM$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resBIM$df[rv_resBIM$dt_row, ]
  modal_dialog_reservesBIM(
    # Informations Générales
    ###########################################CENSORED######################
    # Reserves BIM 
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resBIM$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_reservesBIM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editBIM") & is.null(rv_resBIM$add_or_edit))
  id_buttons <- ifelse(nrow(rv_resBIM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resBIM_qc$df$Buttons, "(\\d+)"))) +1)
  edited_row_resBIM <- dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table
    ###########################################CENSORED######################
    # Reserves BIM
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  # to keep if HTA changes advise 
  # rv_resBIM$df$Buttons[rv_resBIM$dt_row] <- ""
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
  autosave_editBIM()
})


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked on temporary table, modal dialog shows current editable row filled out     ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformBIM"))
  rv_resBIM_qc$dt_row <- which(stringr::str_detect(rv_resBIM_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resBIM_qc$df[rv_resBIM_qc$dt_row, ]
  modal_dialog_reservesBIM(
    # Informations Générales
    ###########################################CENSORED######################
    # Reserves BIM 
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resBIM$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_reservesBIM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformBIM") & is.null(rv_resBIM_qc$add_or_edit))
  edited_form_row_resBIM <- dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table
    ###########################################CENSORED######################
    # Reserves BIM
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
  autosave_editBIM()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Remove edit modal when close button is clicked or submit button                                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$dismiss_modal_reservesBIM, {
  shiny::removeModal()
})


shiny::observeEvent(input$final_edit_reservesBIM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Send to QC                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "QCBIM"))
  rv_resBIM_qc$dt_row <- which(stringr::str_detect(rv_resBIM_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  rv_resBIM_qc$df$Buttons[rv_resBIM_qc$dt_row ] <- ifelse(reactiveValuesToList(res_auth)$user %in% users_qc,
                                                          create_btns_ResBIM_qc(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx")))+1),
                                                          "")
  
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))) > 0){
    QC_ResBIM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx")), rv_resBIM_qc$df[rv_resBIM_qc$dt_row, ])
  }
  else {
    QC_ResBIM <- rv_resBIM_qc$df[rv_resBIM_qc$dt_row, ]
  }
  
  write_xlsx(QC_ResBIM, paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))
  rv_resBIM_qc$df <- setdiff(rv_resBIM_qc$df, QC_ResBIM)
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesBIM_commun.xlsx"))
  write_xlsx(rv_resBIM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesBIM.xlsx"))
  eval(block_qc_bim)
})



## END
