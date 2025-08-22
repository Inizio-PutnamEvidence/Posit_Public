#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               create_btns
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    09 August 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of buttons for EDIT/QC/VIEW
# 
#--------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------#
####      Buttons into EDIT module - HAS                              ####
#------------------------------------------------------------------------#
{
  
  # ------------------------------------------------------> The COMMENT button is only displayed if a comment noted by the reviewer is present 
  create_btns_with_qc_comment <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                    <button class="btn btn-default action-button btn-info action_button" id="editHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                    <button class="btn btn-default action-button btn-default action_button" id="commentQCView_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-comment"></i></button>
                       </div>'
                       ))
  }
  create_btns_without_qc_comment <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                    <button class="btn btn-default action-button btn-info action_button" id="editHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                       </div>'
                       ))
  }
  baseHAS_buttons <- baseHAS_shiny %>% mutate(Buttons = ifelse(is.na(CommentsQC) | CommentsQC == "-", create_btns_without_qc_comment(as.numeric(ID)), create_btns_with_qc_comment(as.numeric(ID)))) %>% relocate(Buttons)
  
  
  # ------------------------------------------------------> All EDIT buttons are present (edit/copy/comment/sendToQC)
  create_btns2 <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editformHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="deleteHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>
                   <button class="btn btn-default action-button btn-primary action_button" id="copyHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-copy"></i></button>
                   <button class="btn btn-default action-button btn-default action_button" id="commentHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-comment"></i></button>
                   <button class="btn btn-default action-button btn-warning action_button" id="QCHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-arrow-up-from-bracket"></i></button>
                       </div>'
                       ))
  }
  baseHAS_empty2 <- baseHAS_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns2(1:nrow(baseHAS_shiny)))) %>% mutate(Statut = NA, Auteur = NA)
  baseHAS_empty2 <- baseHAS_empty2[-c(1:nrow(baseHAS_shiny)),c(ncol(baseHAS_empty2), 1:(ncol(baseHAS_empty2)-1))]
  
}


#------------------------------------------------------------------------#
####        Buttons into EDIT module - Reserves CEM                   ####  
#------------------------------------------------------------------------#
{
  
  # ------------------------------------------------------> The COMMENT button is displayed
  create_btns_resCEM <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                       </div>'
                       ))
  }
  reservesCEM_buttons <- baseCEM_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_resCEM(1:nrow(baseCEM_shiny))))
  reservesCEM_buttons <- reservesCEM_buttons[,c(ncol(reservesCEM_buttons),1:(ncol(reservesCEM_buttons)-3), (ncol(reservesCEM_buttons)-1), (ncol(reservesCEM_buttons)-2))]
  
  
  
  # ------------------------------------------------------> All EDIT buttons are present (edit/delete/sendToQC)
  create_btns_resCEM2 <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editformCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="deleteCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>
                   <button class="btn btn-default action-button btn-warning action_button" id="QCCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-arrow-up-from-bracket"></i></button>
                       </div>'
                       ))
  }
  reservesCEM_empty2 <- baseCEM_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_resCEM2(1:nrow(baseCEM_shiny))))
  reservesCEM_empty2 <- reservesCEM_empty2[-c(1:nrow(baseCEM_shiny)),c(ncol(reservesCEM_empty2), 1:(ncol(reservesCEM_empty2)-3), (ncol(reservesCEM_empty2)-1), (ncol(reservesCEM_empty2)-2))]
  
}


#------------------------------------------------------------------------#
####        Buttons into EDIT module - Reserves BIM                   ####  
#------------------------------------------------------------------------#
{
  
  # ------------------------------------------------------> The COMMENT button is displayed
  create_btns_resBIM <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                       </div>'
                       ))
  }
  reservesBIM_buttons <- baseBIM_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_resBIM(1:nrow(baseBIM_shiny))))
  reservesBIM_buttons <- reservesBIM_buttons[,c(ncol(reservesBIM_buttons),1:(ncol(reservesBIM_buttons)-3), (ncol(reservesBIM_buttons)-1), (ncol(reservesBIM_buttons)-2))]
  
  
  
  # ------------------------------------------------------> All EDIT buttons are present (edit/delete/sendToQC)
  create_btns_resBIM2 <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editformBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="deleteBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>
                   <button class="btn btn-default action-button btn-warning action_button" id="QCBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-arrow-up-from-bracket"></i></button>
                       </div>'
                       ))
  }
  reservesBIM_empty2 <- baseBIM_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_resBIM2(1:nrow(baseBIM_shiny))))
  reservesBIM_empty2 <- reservesBIM_empty2[-c(1:nrow(baseBIM_shiny)),c(ncol(reservesBIM_empty2), 1:(ncol(reservesBIM_empty2)-3), (ncol(reservesBIM_empty2)-1), (ncol(reservesBIM_empty2)-2))]
  
}


#------------------------------------------------------------------------#
####        Buttons into EDIT module - Table des associations         ####
#------------------------------------------------------------------------#
{
  
  # ------------------------------------------------------> The COMMENT button is displayed
  create_btns_assoc <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                       </div>'
                       ))
  }
  tableAssoc_buttons <- baseAssoc_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_assoc(1:nrow(baseAssoc_shiny))))
  tableAssoc_buttons <- tableAssoc_buttons[,c(ncol(tableAssoc_buttons),1:(ncol(tableAssoc_buttons)-3), (ncol(tableAssoc_buttons)-1), (ncol(tableAssoc_buttons)-2))]
  
  
  
  # ------------------------------------------------------> All EDIT buttons are present (edit/delete/sendToQC)
  create_btns_assoc2 <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="editformAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="deleteAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>,
                   <button class="btn btn-default action-button btn-warning action_button" id="QCAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-arrow-up-from-bracket"></i></button>
                       </div>'
                       ))
  }
  
  tableAssoc_empty2 <- baseAssoc_shiny %>% dplyr::bind_cols(tibble("Buttons" = create_btns_assoc2(1:nrow(baseAssoc_shiny))))
  tableAssoc_empty2 <- tableAssoc_empty2[-c(1:nrow(baseAssoc_shiny)),c(ncol(tableAssoc_empty2), 1:(ncol(tableAssoc_empty2)-3), (ncol(tableAssoc_empty2)-1), (ncol(tableAssoc_empty2)-2))]
  
}

#------------------------------------------------------------------------#
####      Buttons into QC module (Databases & Comment View)           ####
#------------------------------------------------------------------------#
{ 
  
  create_btns_HAS_qc_with_right <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                     <button class="btn btn-default action-button btn-info action_button" id="editqcHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                         <button class="btn btn-default action-button btn-danger action_button" id="deleteqcHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-x"></i></button>
                     <button class="btn btn-default action-button btn-success action_button" id="validateqcHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-check-double"></i></button>
                     <button class="btn btn-default action-button btn-default action_button" id="commentqcHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-comment"></i></button>
                         </div>'
                       ))
  }
  
  create_btns_HAS_qc_without_right <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                     <button class="btn btn-default action-button btn-default action_button" id="commentqcHAS_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-comment"></i></button>
                         </div>'
                       ))
  }
  
  create_btns_ResCEM_qc <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                     <button class="btn btn-default action-button btn-info action_button" id="editqcCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                         <button class="btn btn-default action-button btn-danger action_button" id="deleteqcCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-x"></i></button>
                     <button class="btn btn-default action-button btn-success action_button" id="validateqcCEM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-check-double"></i></button>
                         </div>'
                       ))
  }
  
  create_btns_ResBIM_qc <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                     <button class="btn btn-default action-button btn-info action_button" id="editqcBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                         <button class="btn btn-default action-button btn-danger action_button" id="deleteqcBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-x"></i></button>
                     <button class="btn btn-default action-button btn-success action_button" id="validateqcBIM_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-check-double"></i></button>
                     </div>'
                       ))
  }
  
  create_btns_assoc_qc <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                     <button class="btn btn-default action-button btn-info action_button" id="editqcAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                         <button class="btn btn-default action-button btn-danger action_button" id="deleteqcAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-x"></i></button>
                     <button class="btn btn-default action-button btn-success action_button" id="validateqcAssoc_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-check-double"></i></button>
                         </div>'
                       ))
  }
  
  create_btns_comm_view_qc <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                      <button class="btn btn-default action-button btn-danger action_button" id="deleteqcCommView_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-x"></i></button>
                      <button class="btn btn-default action-button btn-success action_button" id="validateqcCommView_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-check"></i></button>
                         </div>'
                       ))
  }
  
}


#------------------------------------------------------------------------#
####      Buttons into VIEW module (call in final_database.R)         ####
#------------------------------------------------------------------------#
{
  
  create_btns_view_with_qc_comm <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                    <button class="btn btn-default action-button btn-info action_button" id="writecommentView_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-pen"></i></button>
                    <button class="btn btn-default action-button btn-default action_button" id="readcommentQC_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-comment"></i></button>
                       </div>'
                       ))
  }
  
  create_btns_view_without_qc_comm <- function(x) {
    x %>%
      purrr::map_chr(~
                       paste0(
                         '<div class = "btn-group">
                    <button class="btn btn-default action-button btn-info action_button" id="writecommentView_',
                         .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-pen"></i></button>
                       </div>'
                       ))
  }
  
}



## END
