#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for edit CEM Reservations tab
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
  vals <- reactiveValues(data = NULL)

  source(file.path(paste0(folder_form,"/editReservesCEM_Form.R")), local = TRUE)$value

  
  # -------------------------------------------------> BLOCK CEM
  block_init_edit_cem <- expression({
    
    rv_resCEM <- shiny::reactiveValues(
      df = reservesCEM_buttons,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(reservesCEM_buttons) + 1
    )
  
    output$dt_reservesCEM <- DT::renderDT({
      ReservesCEM_work <- read_excel(paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
      QC_ReservesCEM   <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
      ID_encours <- data.frame(ID = NA, IDRes = NA) %>%
        mutate(ID = ifelse((nrow(ReservesCEM_work) > 0 & nrow(QC_ReservesCEM) > 0), c(ReservesCEM_work$ID, QC_ReservesCEM$ID),
                           ifelse(nrow(ReservesCEM_work) > 0, ReservesCEM_work$ID,
                                  ifelse(nrow(QC_ReservesCEM) > 0, QC_ReservesCEM$ID, NA))),
               IDRes = ifelse((nrow(ReservesCEM_work) > 0 & nrow(QC_ReservesCEM) > 0), c(ReservesCEM_work$IDRes, QC_ReservesCEM$IDRes),
                              ifelse(nrow(ReservesCEM_work) > 0, ReservesCEM_work$IDRes,
                                     ifelse(nrow(QC_ReservesCEM) > 0, QC_ReservesCEM$IDRes, NA))))

      df <- func_FormattingRes(rv_resCEM$df) %>% mutate(Buttons = ifelse(ID %in% as.character(as.numeric(ID_encours$ID)) & IDRes %in% as.character(as.numeric(ID_encours$IDRes)), "", Buttons))
      datatable(
        df, 
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", "Numéro", "Numéro réserve",
                     as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resCEM$df) & dico$Variable != "ID")])),
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
                                         list(width = '200px', targets = c(which(names(df) %in% c("CEM_DimHAS","CEM_DimPutnam")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("CEM_Precision")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1)))
        ),
        filter = 'top'
      )
    })
    
    proxy <- DT::dataTableProxy("dt_reservesCEM")
    shiny::observe({
      DT::replaceData(proxy, rv_resCEM$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> BLOCK QC CEM
  block_init_edit_qc_cem <- expression({
    
    rv_resCEM_qc <- shiny::reactiveValues(
      df = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(df) + 1
    )
    
    reservesCEM_empty <- reactiveValues(data = NULL)
    observe({
      req(reactiveValuesToList(res_auth)$user)
      shinyjs::delay(100, {
        shinyjs::click("recoverCEM")
      })
    })
    
    # Hide the button
    observe({ shinyjs::hide("recoverCEM") })
    
    
    shiny::observeEvent(input$recover, {
      reservesCEM_empty$data  <- read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
      if(nrow(reservesCEM_empty$data) == 0){
        reservesCEM_empty$data <- reservesCEM_empty2
      }
      rv_resCEM_qc$df <- reservesCEM_empty$data
    })
    
    output$dt_reservesCEM_qc <- DT::renderDT({
      df <- func_FormattingRes(rv_resCEM_qc$df)
      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Buttons", "Statut", "Numéro", "Numéro réserve", 
                     as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resCEM_qc$df) & dico$Variable != "ID")]),
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
                                         list(width = '200px', targets = c(which(names(df) %in% c("CEM_DimHAS","CEM_DimPutnam")))-1),
                                         list(width = '500px', targets = c(which(names(df) %in% c("CEM_Precision")))-1),
                                         list(width = '100px', targets = 1:(ncol(df)-1)))
        )
      )
    })
    
    proxy_qc <- DT::dataTableProxy("dt_reservesCEM_qc")
    shiny::observe({
      DT::replaceData(proxy_qc, rv_resCEM_qc$df, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
  
  
  # -------------------------------------------------> CALL BLOCK 
  eval(block_init_edit_cem)
  eval(block_init_edit_qc_cem)
  
}  


#-------------------------------------------------------------------------------------------------------------------#
####      Delete row                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

delete_row(name_pattern = "deleteCEM", name_rv = rv_resCEM_qc, name_excel = "SaveFormReservesCEM", eval_block = block_init_edit_cem)


#-------------------------------------------------------------------------------------------------------------------#
####      Copy row                                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

# Impossible action


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked, modal dialog shows current editable row filled out                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editCEM"))
  rv_resCEM$dt_row <- which(stringr::str_detect(rv_resCEM$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resCEM$df[rv_resCEM$dt_row, ]
  modal_dialog_reservesCEM(
    # Informations Générales
          ###########################################CENSORED######################
    # Reserves CEM 
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resCEM$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_reservesCEM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editCEM") & is.null(rv_resCEM$add_or_edit))
  id_buttons <- ifelse(nrow(rv_resCEM_qc$df) == 0, 1, max(as.numeric(str_extract(rv_resCEM_qc$df$Buttons, "(\\d+)"))) +1)
  edited_row_resCEM <- dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table
    ###########################################CENSORED######################
    # Reserves CEM
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  # to keep if HTA changes advise
  #  rv_resCEM$df$Buttons[rv_resCEM$dt_row] <- ""
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
  autosave_editCEM()
})


#-------------------------------------------------------------------------------------------------------------------#
####      When edit button is clicked on temporary table, modal dialog shows current editable row filled out     ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformCEM"))
  rv_resCEM_qc$dt_row <- which(stringr::str_detect(rv_resCEM_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  df <- rv_resCEM_qc$df[rv_resCEM_qc$dt_row, ]
  modal_dialog_reservesCEM(
    # Informations Générales
    ###########################################CENSORED######################
    # Reserves CEM
    ###########################################CENSORED######################
    # Edit
          edit                = TRUE
  )
  rv_resCEM$add_or_edit <- NULL
})


#-------------------------------------------------------------------------------------------------------------------#
####      When final edit button is clicked, table will be changed                                               ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$final_edit_reservesCEM, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "editformCEM") & is.null(rv_resCEM_qc$add_or_edit))
  edited_form_row_resCEM <- dplyr::tibble(
    # Other variables
    ###########################################CENSORED######################
    # Display some variables from HAS table
    ###########################################CENSORED######################
    # Reserves CEM 			
    ###########################################CENSORED######################
  )
  ###########################################CENSORED######################
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
  autosave_editCEM()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Remove edit modal when close button is clicked or submit button                                        ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$dismiss_modal_reservesCEM, {
  shiny::removeModal()
})


shiny::observeEvent(input$final_edit_reservesCEM, {
  shiny::removeModal()
})


#-------------------------------------------------------------------------------------------------------------------#
####      Send to QC                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

shiny::observeEvent(input$current_id, {
  shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "QCCEM"))
  rv_resCEM_qc$dt_row <- which(stringr::str_detect(rv_resCEM_qc$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
  rv_resCEM_qc$df$Buttons[rv_resCEM_qc$dt_row ] <- ifelse(reactiveValuesToList(res_auth)$user %in% users_qc,
                                                          create_btns_ResCEM_qc(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx")))+1),
                                                          "")
  
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))) > 0){
    QC_ResCEM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx")), rv_resCEM_qc$df[rv_resCEM_qc$dt_row, ])
  }
  else {
    QC_ResCEM <- rv_resCEM_qc$df[rv_resCEM_qc$dt_row, ]
  }
  
  write_xlsx(QC_ResCEM, paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))
  rv_resCEM_qc$df <- setdiff(rv_resCEM_qc$df, QC_ResCEM)
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormReservesCEM_commun.xlsx"))
  write_xlsx(rv_resCEM_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormReservesCEM.xlsx"))
  eval(block_qc_cem)
})



## END
