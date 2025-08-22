#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Function for Server
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    16 January 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Functions 
# 
#--------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------#
####      Delete row                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

delete_row = function(name_pattern, name_rv, name_excel, eval_block){
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = name_pattern))
    name_rv$dt_row <- which(stringr::str_detect(name_rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    if(str_sub(name_excel,1,4) == "Save"){
      shiny::modalDialog(
        title = "Suppression",
        "Voulez-vous vraiment supprimer cette modification ?",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          div(
            class = "row",
            div(
              class = "col-md-6 text-left",
              shiny::actionButton(inputId = paste0("cancel_", name_pattern), label = "Cancel", class = "btn-danger"),
            ),
            div(
              class = "col-md-6 text-right",
              shiny::actionButton(inputId = paste0("valid_", name_pattern), label = "Validate", class = "btn-info"),
            )
          )
        )
      ) %>% shiny::showModal()
    }
    else {
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
              shiny::actionButton(inputId = paste0("cancel_", name_pattern), label = "Cancel", class = "btn-danger"),
            ),
            div(
              class = "col-md-6 text-right",
              shiny::actionButton(inputId = paste0("valid_", name_pattern), label = "Validate", class = "btn-info"),
            )
          )
        )
      ) %>% shiny::showModal()
    }

  })
  
  shiny::observeEvent(input[[paste0("cancel_", name_pattern)]], {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input[[paste0("valid_", name_pattern)]], {
    
    
    if (str_sub(name_excel,1,4) == "Save"){
      
      # Add the updated buttons ---> we are in the case where we want to hide/unhide the existing row
      if (name_pattern == "deleteHAS"){
        ID <- rv_qc$df$ID[rv_qc$dt_row]
        if (rv_qc$df$Statut[rv_qc$dt_row] == "Modification HAS"){
          line <- which(rv$df$ID == ID)
          rv$df$Buttons[line] <- ifelse(is.na(rv$df$CommentsQC[line]) | rv$df$CommentsQC[line] == "-", create_btns_without_qc_comment(line), create_btns_with_qc_comment(line)) 
        }
      }
      if (name_pattern == "deleteBIM"){
        ID <- rv_resBIM_qc$df$ID[rv_resBIM_qc$dt_row]
        IDRes <- rv_resBIM_qc$df$IDRes[rv_resBIM_qc$dt_row]
        if (ID %in% rv_resBIM$df$ID & IDRes %in% rv_resBIM$df$IDRes){
          line <- which(rv_resBIM$df$ID == ID & rv_resBIM$df$ID == ID)
          rv_resBIM$df$Buttons[line] <- create_btns_resBIM(line)
        }
      }
      if (name_pattern == "deleteCEM"){
        ID <- rv_resCEM_qc$df$ID[rv_resCEM_qc$dt_row]
        IDRes <- rv_resCEM_qc$df$IDRes[rv_resCEM_qc$dt_row]
        if (ID %in% rv_resCEM$df$ID & IDRes %in% rv_resCEM$df$IDRes){
          line <- which(rv_resCEM$df$ID == ID & rv_resCEM$df$ID == ID)
          rv_resCEM$df$Buttons[line] <- create_btns_resCEM(line)
        }
      }
      
      name_rv$df <- name_rv$df[-name_rv$dt_row, ]
      write_xlsx(name_rv$df, paste0(getwd(),"/",folder_data,"/",name_excel,"_commun",".xlsx"))
      write_xlsx(name_rv$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/",name_excel,".xlsx"))

    }
    
    else {
      
        name_rv$df <- name_rv$df[-name_rv$dt_row, ]
        write_xlsx(name_rv$df, paste0(getwd(),"/",folder_data,"/",name_excel,".xlsx"))
      
    }

    # eval(eval_block)
    shiny::removeModal()
    
  })
 
}


#-------------------------------------------------------------------------------------------------------------------#
####      Copy row                                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#

copy_row <- function(name_pattern, name_rv, name_create_btn){
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = name_pattern))
    name_rv$dt_row <- which(stringr::str_detect(name_rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    new_row    <- name_rv$df[name_rv$dt_row, ]
    id_buttons <- ifelse(nrow(name_rv$df) == 0, 1, max(as.numeric(str_extract(name_rv$df$Buttons, "(\\d+)"))) +1)
    new_row$Buttons <- name_create_btn(id_buttons)
    new_row$Statut <- "Nouvelle HAS"
    new_row$ID <- as.character(max(as.numeric(c(rv$df$ID,
                                                read_excel(paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))$ID,
                                                read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))$ID)))+1)
    oldIndex <- strsplit(new_row$Index[1], "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)[[1]]
    new_row$Index <- paste0(paste0(new_row$ID,oldIndex[2]), oldIndex[3])
    name_rv$df <- new_row %>% dplyr::bind_rows(name_rv$df)
    
    write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/SaveFormHAS_commun.xlsx"))
    write_xlsx(rv_qc$df, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/SaveFormHAS.xlsx"))
    
  })
  
}


#-------------------------------------------------------------------------------------------------------------------#
####      Read comment button                                                                                    ####
#-------------------------------------------------------------------------------------------------------------------#

read_comment <- function(name_pattern, name_rv){
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = name_pattern))
    name_rv$dt_row <- which(stringr::str_detect(name_rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    ui_elements <- lapply(1:length(str_split(name_rv$df$CommentsQC[name_rv$dt_row ], "\n")[[1]]), function(i) {
      tagList(
        str_split(name_rv$df$CommentsQC[name_rv$dt_row ], "\n")[[1]][i],
        br()
      )
    })
    
    shiny::modalDialog(
      title = "Commentaires",
      strong("Commentaires du reviewer lors du QC"), br(),
      do.call(tagList, ui_elements),
      br(),
      strong("Commentaires de l'auteur"), br(),
      name_rv$df$CommentsEdit[name_rv$dt_row ], br(),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "close_comment", label = "Close", class = "btn-danger"),
          )
        )
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$close_comment, {
    shiny::removeModal()
  })

}


read_comment_HAS <- function(name_pattern, name_rv){
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = name_pattern))
    name_rv$dt_row <- which(stringr::str_detect(name_rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    ui_elements <- lapply(1:length(str_split(name_rv$df$CommentsQC[name_rv$dt_row ], "\n")[[1]]), function(i) {
      tagList(
        str_split(name_rv$df$CommentsQC[name_rv$dt_row ], "\n")[[1]][i],
        br()
      )
    })
    
    shiny::modalDialog(
      title = "Commentaires",
      strong("Commentaires du reviewer lors du QC"), br(),
      do.call(tagList, ui_elements),
      br(),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "close_commentQCView", label = "Close", class = "btn-danger"),
          )
        )
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$close_commentQCView, {
    shiny::removeModal()
  })
  
}


#-------------------------------------------------------------------------------------------------------------------#
####      Send to QC                                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#

send_qc <- function(name_pattern, name_rv){
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = name_pattern))
    name_rv$dt_row <- which(stringr::str_detect(name_rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    shiny::modalDialog(
      title = "Envoi au QC",
      "Validez pour envoyer au QC cette ligne de la base HAS ainsi que les lignes de la table des associations et les réserves CEM/BIM associées.",
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-left",
            shiny::actionButton(inputId = "cancel_qc", label = "Cancel", class = "btn-danger"),
          ),
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "valid_qc", label = "Validate", class = "btn-info"),
          )
        )
      )
    ) %>% shiny::showModal()
  
  })
}


#-------------------------------------------------------------------------------------------------------------------#
####      AutoSave (EDIT) for HAS/CEM/BIM/ASSOC                                                                  ####
#-------------------------------------------------------------------------------------------------------------------#

autosave_editHAS <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormHAS.xlsx"))) > 0){
    autosaveHAS <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormHAS.xlsx")), rv_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveHAS <- rv_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveHAS, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormHAS.xlsx"))
}


autosave_editCEM <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesCEM.xlsx"))) > 0){
    autosaveCEM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesCEM.xlsx")), rv_resCEM_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveCEM <- rv_resCEM_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveCEM, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesCEM.xlsx"))	
}


autosave_editBIM <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesBIM.xlsx"))) > 0){
    autosaveBIM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesBIM.xlsx")), rv_resBIM_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveBIM <- rv_resBIM_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveBIM, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormReservesBIM.xlsx"))
}


autosave_editAssoc <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormTableAssoc.xlsx"))) > 0){
    autosaveAssoc <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormTableAssoc.xlsx")), rv_assoc_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveAssoc <- rv_assoc_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveAssoc, paste0(getwd(),"/",folder_data,"/",reactiveValuesToList(res_auth)$user,"/AutoSaveFormTableAssoc.xlsx"))
}


#-------------------------------------------------------------------------------------------------------------------#
####      AutoSave (QC) for HAS/CEM/BIM/ASSOC                                                                  ####
#-------------------------------------------------------------------------------------------------------------------#

autosave_qcHAS <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCHAS.xlsx"))) > 0){
    autosaveHAS <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCHAS.xlsx")), rv_HAS_to_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveHAS <- rv_HAS_to_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveHAS, paste0(getwd(),"/",folder_data,"/AutoSaveQCHAS.xlsx"))
}


autosave_qcCEM <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesCEM.xlsx"))) > 0){
    autosaveCEM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesCEM.xlsx")), rv_resCEM_to_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveCEM <- rv_resCEM_to_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveCEM, paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesCEM.xlsx"))	
}


autosave_qcBIM <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesBIM.xlsx"))) > 0){
    autosaveBIM <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesBIM.xlsx")), rv_resBIM_to_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveBIM <- rv_resBIM_to_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveBIM, paste0(getwd(),"/",folder_data,"/AutoSaveQCReservesBIM.xlsx"))
}


autosave_qcAssoc <- function(){
  # Save Data --> all rows are saved including the date
  if(nrow(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCTableAssoc.xlsx"))) > 0){
    autosaveAssoc <- rbind(read_excel(paste0(getwd(),"/",folder_data,"/AutoSaveQCTableAssoc.xlsx")), rv_assoc_to_qc$df %>% mutate(date = as.character(Sys.time())))
  }
  else {
    autosaveAssoc <- rv_assoc_to_qc$df %>% mutate(date = as.character(Sys.time())) 
  }
  write_xlsx(autosaveAssoc, paste0(getwd(),"/",folder_data,"/AutoSaveQCTableAssoc.xlsx"))
}



## END
