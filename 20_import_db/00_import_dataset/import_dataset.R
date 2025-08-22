#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Import databases (Extraction HTA, Réserves CEM, Réserves BIM)
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  14 August 2023
#   LAST MODIFIED DATE    27 August 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) ((MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Importation of databases & creation of variables
#   UPDATE                23-Feb-2024 - add TypeProduit in baseAssoc_shiny        
#                         27-Aug-2024 - db 'dico' --> replace mutate(Titre = "HAS") by mutate(Titre = "Avis Efficience") --> idem for CEM/BIM by Réserves CEM/Réserves BIM
#                         
#--------------------------------------------------------------------------------------------------------



#                /\           Note
#               /  \            - DATE (https://stats.oarc.ucla.edu/r/faq/how-does-r-handle-date-values/)
#              /    \             To have the same date conversion as in EXCEL, we must add the days in 01/01/1970 (Reference R) and 01/01/1900 (Reference Excel) and add +2 because there is an error in the Excel conversion
#             /      \        
#            /  !!!   \                               
#           /          \
#          /            \
#         /_ _ _ _ _ _ _ \
  


#------------------------------------------------------------------------#
####  Importation of datasets                                         ####  
#------------------------------------------------------------------------#
{
 
  # Databases
  baseHAS <- as.data.frame(read_excel(path = paste0(folder_doc,"/Putnam PHMR_Base avis efficience_VF_forR_20230901.xlsx"), sheet = "Extraction HAS", range = cell_rows(c(4,204)), col_names = TRUE))[,-1]
  baseCEM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Putnam PHMR_Base avis efficience_VF_forR_20230901.xlsx"), sheet = "Tableau réserves CEM", range = cell_rows(c(2,1761)), col_names = TRUE))[,-1]
  baseBIM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Putnam PHMR_Base avis efficience_VF_forR_20230901.xlsx"), sheet = "Tableau réserves BIM", range = cell_rows(c(2,342)), col_names = TRUE))
  
  # List of variables
  varlistHAS <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "VarlistHAS", col_names = TRUE))
  varlistCEM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "VarlistCEM", col_names = TRUE))
  varlistBIM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "VarlistBIM", col_names = TRUE))
  
  # Table of correspondence between the variables necessary to the application and the Excel file built by HTA team
  correspHAS <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "CorrespHAS", range = cell_rows(c(1,NA)), col_names = TRUE))
  correspCEM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "CorrespCEM", range = cell_rows(c(1,NA)), col_names = TRUE))
  correspBIM <- as.data.frame(read_excel(path = paste0(folder_doc,"/Variable_liste.xlsx"), sheet = "CorrespBIM", range = cell_rows(c(1,NA)), col_names = TRUE))
  
  ###########################################CENSORED######################
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  Table des réserves - BIM                                        #### 
#------------------------------------------------------------------------#
{

  ###########################################CENSORED######################
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  Table - HAS                                                     #### 
#------------------------------------------------------------------------#
{
  
  ###########################################CENSORED######################
  
}
  
#------------------------------------------------------------------------#
####  HAS - Informations Générales                                    ####  
#------------------------------------------------------------------------#
{

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Update regardless "correction"              #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  HAS - Evaluation HAS                                            ####  
#------------------------------------------------------------------------#
{

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Formules                                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  HAS - Population cible                                          ####  
#------------------------------------------------------------------------#
{

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  HAS - Prix                                                      ####  
#------------------------------------------------------------------------#
{

  ###########################################CENSORED######################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #     Correction on modalities                    #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  HAS - Posologie                                                 ####  
#------------------------------------------------------------------------#
{

  ###########################################CENSORED######################
  
}

#------------------------------------------------------------------------#
####  HAS - Posologie --> focus on conditionnement/cout               ####  
#------------------------------------------------------------------------#
{

  ###########################################CENSORED######################a
 }
 
}

#------------------------------------------------------------------------#
####  HAS - Délai                                                     #### 
#------------------------------------------------------------------------#
{
  
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  Table HAS/CEM/BIM/TableAssoc for Shiny - EDIT/ADD               #### 
#------------------------------------------------------------------------#
{

  ###########################################CENSORED######################

}

#------------------------------------------------------------------------#
####  Fusion HAS/CEM/BIM for Shiny - VIEW                             ####  
#------------------------------------------------------------------------#
{
 
  ###########################################CENSORED######################
}

#------------------------------------------------------------------------#
####  Exportation of all databases for Shiny                          #### 
#------------------------------------------------------------------------#
{
  ###########################################CENSORED######################
}



## END
