#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Global environment
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  22 December 2023
#   LAST MODIFIED DATE    14 May 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Global environment
# 
#--------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------#
####      Global environment                                          ####  
#------------------------------------------------------------------------#
{
  
  folder_doc     = "00_document" 
  folder_data    = "10_data" 
  folder_import  = "20_import_db"
  folder_ui      = "30_ui scripts" 
  folder_form    = "40_form scripts" 
  folder_server  = "50_server scripts" 
  folder_www     = "www"
  folder_pdf     = "pdf"
  
  folder_import_init = "20_import_db/00_import_dataset"
  folder_import_save = "20_import_db/10_save"

  # source(paste0(folder_import_init,"/import_dataset.R"))

  load(paste0(getwd(),"/",folder_import,"/baseHAS_shiny.RData"))
  load(paste0(getwd(),"/",folder_import,"/baseCEM_shiny.RData"))
  load(paste0(getwd(),"/",folder_import,"/baseBIM_shiny.RData"))
  load(paste0(getwd(),"/",folder_import,"/baseAssoc_shiny.RData"))
  load(paste0(getwd(),"/",folder_import,"/fusionView.RData"))
  load(paste0(getwd(),"/",folder_import,"/HASView.RData"))
  
  load(paste0(getwd(),"/",folder_import,"/varlistHAS.RData"))
  load(paste0(getwd(),"/",folder_import,"/ATC_Class_Label.RData"))
  load(paste0(getwd(),"/",folder_import,"/dico.RData"))
  load(paste0(getwd(),"/",folder_import,"/treelist.RData"))

  
  users_qc <- c("charlenetournier","nicolasvirely","olfadoghri")
  users_edit <- c(users_qc,"marinesivignon","romainsupiot","rahmasellami","yosraboukhris")
  
  list_func_grandeur  <- c("p","n","μ","m","k","M","G")
  list_grandeurunite  <- c("-",list_func_grandeur)
  list_unitedosefixe  <- c("-","g","L","u","inj","Bq")
  list_unitedosepoids <- c("-","g/kg","L/kg","u/kg","inj/kg","Bq/kg","c/kg","vg/kg")
  list_unitedosesurf  <- c("-","g/m2","L/m2","u/m2","inj/m2","Bq/m2")
  
  
  # Deletion of rows identified as current_id == "NULL"
  for (i in 1:length(users_edit)){
    test = read_excel(paste0(getwd(),"/",folder_data,"/",users_edit[i],"/Form_in_progress.xlsx"))
    test = test %>% filter(current_id != "NULL")
    write_xlsx(test, paste0(getwd(),"/",folder_data,"/",users_edit[i],"/Form_in_progress.xlsx"))
  }
  
}



## END
