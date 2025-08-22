#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Functions
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  14 August 2023
#   LAST MODIFIED DATE    16 April 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of functions
# 
#--------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------#
####      General Functions                                           ####  
#------------------------------------------------------------------------#
{
  
  func_select_other <- function(input, other_input){
    response <- ifelse(substr(input,1,5) == "Autre", other_input, input)
    return(response)
  }
  
  func_Index <- function(df){
    Index <- paste0(df$ID,
                    df$Molecule,
                    as.numeric(df$DateValidationCEESP)+abs(as.numeric(as.Date("01011900", format = "%d%m%Y")))+2)
    return(Index)
  }
  
  func_Date <- function(var){
    var <- suppressWarnings(as.Date(as.numeric(var)-2, origin = "1900-01-01"))
    return(var)
  }
  
  # Using this function to display DATE by replacing "2000-01-01" by NA during the view of database 
  func_DateFormatting <- function(date){
    date <- format(as.Date(ifelse(date == "2000-01-01", NA, as.Date(date)), origin = "1970-01-01"), format = "%d-%m-%Y")
    return(date)
  }
  
  func_SourceHyperlink <- function(var){
    var <- ifelse(var %in% c("-","Voir table des associations"), var , sapply(var, function(x) toString(tags$a(href = x, "Source", target = "_blank", style = "font-style: italic;"))))
    return(var)
  }
  
  func_NbFormatting = function(var, suffixe, decimal){
    var <- suppressWarnings(ifelse(is.na(as.numeric(var)), var, label_dollar(prefix = "", suffix = suffixe, accuracy = decimal, big.mark = " ")(as.numeric(var))))
    return(var)
  }
  
  func_ChrFormatting <- function(var){
    var <- str_replace_all(var, "\\*10", "\\x10")
    new_var <- ifelse(is.na(str_extract(str_extract(var, "\\^\\d+"), "\\d+")),
                      gsub("\\^.*","",var), 
                      str_replace_all(string = str_replace_all(var, "10\\^", "10<sup>"), pattern = "<sup>(\\d+)", replacement = paste0("<sup>\\1", "</sup>")))
    return(new_var)
  }
  
  func_nettoyage_old_information <- function(row,checkbox1erJO,checkboxJOVigueur,checkbox1erJOPostAvis){
    
  # -----------------------------------> Prix
  {
    if(checkbox1erJO == FALSE){
      row <- row %>%
        mutate(
          ###########################################CENSORED######################
        )
    }
    if(row$Demande == 'Inscription' | checkboxJOVigueur == FALSE){
      row <- row %>%
        mutate(
          ###########################################CENSORED######################
        )
    }
    if(checkbox1erJOPostAvis == FALSE){
      row <- row %>%
        mutate(
          ###########################################CENSORED######################
        )
    }
    if(row$ATU_Presence == "Voir table des associations"){
      row <- row %>% mutate(JO_QualifPrix = "-")
    }
    if(row$JO_QualifPrix == "-" | row$ATU_Presence == "Voir table des associations"){
      row <- row %>%
        mutate(
          ###########################################CENSORED######################
        )
    }


    if(row$JO_QualifPrix != "-"){
      if(row$ATU_Presence == "Non"){
        row <- row %>%
          mutate(
            ###########################################CENSORED######################
          )
      }
      ###########################################CENSORED######################
    
  } # END Prix
   
  
  # -----------------------------------> Posologie
  {
    if(row$TypeProduit == 'Vaccin'){
      row <- row %>%
        mutate(
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
            # PVC_Nb = "-"
        )
    }
  
    if(row$TypeProduit == 'Dispositif Médical'){
      row <- row %>%
        mutate(
          ###########################################CENSORED######################
        # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
        ###########################################CENSORED######################
        # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
        ###########################################CENSORED######################
        # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
        ###########################################CENSORED######################
        # Posologie ---> Mono/Asso : Traitement Non Chronique
        ###########################################CENSORED######################
        # Posologie ---> Vaccin
            PVC_Nb = "-"
        )
        # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
            if(row$UtilisationDM == 'Oui'){ row <- row %>% mutate(PDMC_Nb = "-", PDMC_Duree = "-") }
            if(row$UtilisationDM == 'Non'){ row <- row %>% mutate(PDMNC_Nb = "-")                  }
    }

    if(!(row$TypeProduit %in% c('Vaccin','Dispositif Médical'))){
          if(row$TraitementChronique == "Non"){
            row <- row %>%
              mutate(
                  EscaladeDose = "-", 
              # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
              ###########################################CENSORED######################
              # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
              ###########################################CENSORED######################
              # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
              ###########################################CENSORED######################
              # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
              ###########################################CENSORED######################
              # Posologie ---> Vaccin
                  PVC_Nb = "-"
              )
              # Posologie ---> Mono/Asso : Traitement Non Chronique  
                  # "PTNC_TypeDose = "-"
                  if (row$PTNC_TypeDose == "One Shot") { row <- row %>% mutate(PTNC_CateAge = "-", PTNC_Age = "-", PTNC_DosePrise = "-", PTNC_NbPriseCycle = "-", PTNC_NbCycle = "-") }
                  if (row$PTNC_TypeDose != "One Shot") { row <- row %>% mutate(PTNC_NbPriseCycleOneShot = "-")                                                                                             }
          }
          if(row$TraitementChronique == "Oui"){
            row <- row %>%
              mutate(
              # Posologie ---> Mono/Asso : Traitement Non Chronique
                ###########################################CENSORED######################
              # Posologie ---> Dispositifs Médicaux (DM) : Traitement Non Chronique & Chronique
              ###########################################CENSORED######################
              # Posologie ---> Vaccin
                 PVC_Nb = "-"
             )
             if (row$EscaladeDose == "Oui") {
               row <- row %>%
                 mutate(
                 # Posologie ---> Mono/Asso : Traitement Chronique - Pas d'escalade de Dose
                   ###########################################CENSORED######################
                 )
              }
             if (row$EscaladeDose == "Non") {
               row <- row %>%
                 mutate(
                 # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'attaque
                   ###########################################CENSORED######################
                 # Posologie ---> Mono/Asso : Traitement Chronique - Escalade de Dose - Phase d'entretien
                 ###########################################CENSORED######################
                 )
            }
      } 
    }
  
  } # End posologie 
    
    return(row)
  }
  
  func_nettoyage_old_information_gen <- function(row){
  # -----------------------------------> Informations générales
    if(row$StatutMedOrphelin == "Non"){
      row <- row %>% mutate(DateCOMP = as.Date("2000-01-01"))
    }
    return(row)
  }
  
  fct_input_vide <- function(variable){
    ifelse(test = !(exists("variable")) || is.null(variable) || is.na(variable),
           yes  = "-",
           no   = variable)
  }
  
  func_table_assoc <- function(table_assoc, answer){
    if(table_assoc == "Oui"){
      answer <- "Voir table des associations"
    }
    return(answer)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Informations générales                                ####  
#------------------------------------------------------------------------#
{
  
  func_ATC_Lab <- function(ATC){
    ATC_Label <- ATC_Class_Label[which(ATC_Class_Label$ATC_code == str_sub(ATC,1,5)),"Libellé français"]
    ATC_Label <- ifelse(identical(ATC_Label,character(0)), "", ATC_Label)
    return(ATC_Label)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Evaluation HAS                                        ####  
#------------------------------------------------------------------------#
{
  
  func_RDCR_Aconsiderer = function(RDCR_ConclusionCEESP, RDCR_QalyCclMinRef, RDCR_QalyHorsCclMin, RDCR_QalyMin, RDCR_UniteCcl){
    var <- 
      case_when( RDCR_ConclusionCEESP == "Invalide"                                                                                                      ~ "Invalide",
                 RDCR_QalyCclMinRef %in% c("","-") &  RDCR_QalyHorsCclMin %in% c("","-") &  RDCR_QalyMin %in% c("","-") &  RDCR_UniteCcl %in% c("","-")  ~ "Donnée indisponible",
                 RDCR_QalyCclMinRef %in% c("","-") &  RDCR_QalyHorsCclMin %in% c("","-") &  RDCR_QalyMin %in% c("","-") & !RDCR_UniteCcl %in% c("","-")  ~ RDCR_UniteCcl,
                 RDCR_QalyCclMinRef %in% c("","-") &  RDCR_QalyHorsCclMin %in% c("","-") & !RDCR_QalyMin %in% c("","-")                                  ~ RDCR_QalyMin,
                 RDCR_QalyCclMinRef %in% c("","-") & !RDCR_QalyHorsCclMin %in% c("","-")                                                                 ~ RDCR_QalyHorsCclMin,
                !RDCR_QalyCclMinRef %in% c("","-")                                                                                                       ~ RDCR_QalyCclMinRef)
    return(var)
  }
  
  func_CT_ASMR_ObtRev <- function(CT_ASMR_Rev,CT_ASMR_Obt){
    var <- 
      case_when(CT_ASMR_Rev == "Projet d'avis - demande retirée"                ~ "-",
                CT_ASMR_Rev == "Donnée manquante" | CT_ASMR_Obt == "Sans objet" ~ "-",
                CT_ASMR_Rev == CT_ASMR_Obt                                      ~ "Equivalente",
                CT_ASMR_Rev != CT_ASMR_Obt                                      ~ "Différente")
    return(var)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Population cible                                      ####  
#------------------------------------------------------------------------#
{
  
  func_infoDateEvalCEESPPrev <- function(db){
    
    # Extraction of temporary database
    db_tmp <- db %>% arrange(Molecule, DateValidationCEESP, DateCT) %>% select(Index, Molecule, DateValidationCEESP, DateCT)
    db_tmp$order <- suppressWarnings(with(db_tmp, ave(as.numeric(Molecule), Molecule, FUN=seq_along)))
    
    # Creation of new variable = CT_DateEvalCEESPPrev
    tmp <- db_tmp %>% filter(order==1) %>% mutate(CT_DateEvalCEESPPrev = as.Date(NA)) %>% select(Index, CT_DateEvalCEESPPrev)
    
    # Focus only data when order>=1  
    db_tmp1 <- db_tmp %>% filter(order>1)
    
    # Loop on molecule/order
    for (name_molecule in unique(db_tmp1$Molecule)) {
      max_order <- db_tmp %>% filter(Molecule == name_molecule) %>% group_by(Molecule) %>% summarise(max = max(order)) %>% select(max) %>% unlist()
      for (num in 2:max_order) {
        tmp_ <- 
          suppressWarnings(
            left_join(db_tmp %>% filter(Molecule == name_molecule & order == num),
                      db_tmp %>% filter(Molecule == name_molecule & order<num) %>% select(Molecule, DateValidationCEESP, DateCT) %>% rename(DateValidationCEESPprevious = DateValidationCEESP, DateCTprevious = DateCT),
                      by = "Molecule") %>%
              filter(DateValidationCEESPprevious < DateValidationCEESP & (DateCTprevious < DateCT | is.na(DateCT))) %>%
              group_by(Index) %>%
              summarise(CT_DateEvalCEESPPrev = max(DateCTprevious))
          )
        tmp <- rbind(tmp, tmp_)
      }
    }
    db <- db %>% left_join(tmp, by = "Index")
    
    # Now, I only keep for the form some information necessary
    tmp <- db %>% arrange(Molecule, DateValidationCEESP, DateCT) %>% select(Index, Molecule, DateValidationCEESP, DateCT, CT_DateEvalCEESPPrev)
    tmp$order <- suppressWarnings(with(tmp, ave(as.numeric(Molecule), Molecule, FUN=seq_along)))
    tmp$order_char <- ifelse(tmp$order<10, as.character(paste0("0",tmp$order)), as.character(tmp$order))
    tmp$CT_concat_info <- paste( paste (tmp$Molecule, paste0("(",tmp$order_char,")")),
                                 tmp$DateValidationCEESP,
                                 tmp$DateCT,
                                 tmp$CT_DateEvalCEESPPrev,
                                 sep = " // ")
    tmp$CT_concat_info <- str_replace(tmp$CT_concat_info, "// NA", "// -")
    db <- db %>% left_join(tmp %>% select(Index, CT_concat_info) , by = "Index")
    
    # Return
    return(db)
  }
  
  
  func_PopPrevalente <- function(db){
    
    # Extraction of temporary database
    db_tmp <- rbind(db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCibleSpe, CT_Precision) %>% rename(cost = CT_PopCibleSpe, precision = CT_Precision),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible1, CT_Precision1) %>% rename(cost = CT_PopCible1, precision = CT_Precision1),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible2, CT_Precision2) %>% rename(cost = CT_PopCible2, precision = CT_Precision2),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible3, CT_Precision3) %>% rename(cost = CT_PopCible3, precision = CT_Precision3),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible4, CT_Precision4) %>% rename(cost = CT_PopCible4, precision = CT_Precision4))
    
    # Taking into account in this calculation the cases where for the same INDEX, I have the same PRECISSION several times, then I order
    db_tmp <- db_tmp %>% filter(cost != "-" & precision == "prévalente/totale") %>% 
                         group_by(Index, NomProduit, DateValidationCEESP, Demande, precision) %>% 
                         summarise(cost = as.character(sum(as.numeric(cost)))) %>% 
                         ungroup() %>% 
                         arrange(NomProduit, DateValidationCEESP) 
    db_tmp$order <- suppressWarnings(with(db_tmp, ave(as.numeric(NomProduit), NomProduit, FUN=seq_along)))
    
    # Creation of the temporary variable "cost_prev"
    tmp <- db_tmp %>% filter(order==1) %>% mutate(cost_prev = as.numeric(NA)) %>% select(Index, cost_prev)
    db_tmp1 <- db_tmp %>% filter(order>1)
    
    # Loop for each NomProduit with order>1
    for (name_produit in unique(db_tmp1$NomProduit)) {
      max_order <- db_tmp %>% filter(NomProduit == name_produit) %>% group_by(NomProduit) %>% summarise(max = max(order)) %>% select(max) %>% unlist()
      for (num in 2:max_order) {
        tmp_ <- 
          left_join(db_tmp %>% filter(NomProduit == name_produit & order == num) %>% select (-cost),
                    db_tmp %>% filter(NomProduit == name_produit & order<=num) %>% select(NomProduit, DateValidationCEESP, cost) %>% rename(DateValidationCEESPprevious = DateValidationCEESP),
                    by = "NomProduit") %>%
          filter(DateValidationCEESPprevious < DateValidationCEESP | is.na(DateValidationCEESPprevious)) %>%
          group_by(Index) %>%
          summarise(cost_prev = sum(as.numeric(cost)))
        tmp <- rbind(tmp, tmp_)
      }
    }

    # Join with db_tmp & creation of "PCP_Total" and "PCP_VariationAvis_n" variables
    db_tmp <- db_tmp %>% left_join(tmp, by = "Index")
    db_tmp <- db_tmp %>% mutate(PCP_Total = ifelse(is.na(as.numeric(cost_prev)), cost, as.character(as.numeric(cost_prev) + as.numeric(cost)) ))
    db_tmp <- db_tmp %>% mutate(PCP_VariationAvis_n = case_when(is.na(as.numeric(cost_prev)) & Demande == "Inscription" ~ "Non concerné", 
                                                                is.na(as.numeric(cost_prev)) & Demande != "Inscription" ~ "-", 
                                                                TRUE ~ as.character(((as.numeric(PCP_Total) / as.numeric(cost_prev))- 1) * 100) ))

    # Join with db & update of variables
    db <- db %>% left_join(db_tmp %>% select(Index, PCP_Total, PCP_VariationAvis_n), by = "Index")
    db <- db %>% mutate(PCP_VariationAvis_n = case_when(!is.na(PCP_VariationAvis_n) ~ PCP_VariationAvis_n,
                                                        is.na(PCP_VariationAvis_n) & (CT_Precision == "incidente/an" | CT_Precision1 == "incidente/an" | CT_Precision2 == "incidente/an" | CT_Precision3 == "incidente/an" | CT_Precision4 == "incidente/an") ~ "Non concerné",
                                                        TRUE  ~ "-" ))
    # Return
    return(db)
  }
  
  
  func_PopIncidente <- function(db){
    
    # Extraction of temporary database
    db_tmp <- rbind(db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCibleSpe, CT_Precision) %>% rename(cost = CT_PopCibleSpe, precision = CT_Precision),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible1, CT_Precision1) %>% rename(cost = CT_PopCible1, precision = CT_Precision1),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible2, CT_Precision2) %>% rename(cost = CT_PopCible2, precision = CT_Precision2),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible3, CT_Precision3) %>% rename(cost = CT_PopCible3, precision = CT_Precision3),
                    db %>% select(Index, NomProduit, DateValidationCEESP, Demande, CT_PopCible4, CT_Precision4) %>% rename(cost = CT_PopCible4, precision = CT_Precision4))
    
    # Taking into account in this calculation the cases where for the same INDEX, I have the same PRECISSION several times, then I order
    db_tmp <- db_tmp %>% filter(cost != "-" & precision == "incidente/an") %>% 
                         group_by(Index, NomProduit, DateValidationCEESP, Demande, precision) %>% 
                         summarise(cost = as.character(sum(as.numeric(cost)))) %>% 
                         ungroup() %>% 
                         arrange(NomProduit, DateValidationCEESP) 
    db_tmp$order <- suppressWarnings(with(db_tmp, ave(as.numeric(NomProduit), NomProduit, FUN=seq_along)))
    
    # Creation of the temporary variable "cost_prev"
    tmp <- db_tmp %>% filter(order==1) %>% mutate(cost_prev = as.numeric(NA)) %>% select(Index, cost_prev)
    db_tmp1 <- db_tmp %>% filter(order>1)
    
    # Loop for each NomProduit with order>1
    for (name_produit in unique(db_tmp1$NomProduit)) {
      max_order <- db_tmp %>% filter(NomProduit == name_produit) %>% group_by(NomProduit) %>% summarise(max = max(order)) %>% select(max) %>% unlist()
      for (num in 2:max_order) {
        tmp_ <- 
          left_join(db_tmp %>% filter(NomProduit == name_produit & order == num) %>% select (-cost),
                    db_tmp %>% filter(NomProduit == name_produit & order<=num) %>% select(NomProduit, DateValidationCEESP, cost) %>% rename(DateValidationCEESPprevious = DateValidationCEESP),
                    by = "NomProduit") %>%
          filter(DateValidationCEESPprevious < DateValidationCEESP | is.na(DateValidationCEESPprevious)) %>%
          group_by(Index) %>%
          summarise(cost_prev = sum(as.numeric(cost)))
        tmp <- rbind(tmp, tmp_)
      }
    }
    
    # Join with db_tmp & creation of "PCI_Total" and "PCI_VariationAvis_n" variables
    db_tmp <- db_tmp %>% left_join(tmp, by = "Index")
    db_tmp <- db_tmp %>% mutate(PCI_Total = ifelse(is.na(as.numeric(cost_prev)), cost, as.character(as.numeric(cost_prev) + as.numeric(cost)) ))
    db_tmp <- db_tmp %>% mutate(PCI_VariationAvis_n = case_when(is.na(as.numeric(cost_prev)) & Demande == "Inscription" ~ "Non concerné", 
                                                                is.na(as.numeric(cost_prev)) & Demande != "Inscription" ~ "-", 
                                                                TRUE ~ as.character(((as.numeric(PCI_Total) / as.numeric(cost_prev))- 1) * 100) ))
    
    # Join with db & update of variables
    db <- db %>% left_join(db_tmp %>% select(Index, PCI_Total, PCI_VariationAvis_n), by = "Index")
    db <- db %>% mutate(PCI_VariationAvis_n = case_when(!is.na(PCI_VariationAvis_n) ~ PCI_VariationAvis_n,
                                                        is.na(PCI_VariationAvis_n) & (CT_Precision == "prévalente/totale" | CT_Precision1 == "prévalente/totale" | CT_Precision2 == "prévalente/totale" | CT_Precision3 == "prévalente/totale" | CT_Precision4 == "prévalente/totale") ~ "Non concerné",
                                                        TRUE  ~ "-" ))
    # Return
    return(db)
  }
  
  
  func_pop_cible_mean <- function(var1, var2){
    var <- suppressWarnings(as.character(mean(c(as.numeric(var1), as.numeric(var2)), na.rm = TRUE)))
    var <- ifelse(var=="NaN", "-", var)
    return(var)
  }
  
  
  func_pop_cible_mean2 <- function(var1, var2){
    var <-
      suppressWarnings(
        case_when(
          var2 == "-"                                         ~ var1,
          !is.na(as.numeric(var1)) & !is.na(as.numeric(var2)) ~ as.character((as.numeric(var1) + as.numeric(var2))/2),
          TRUE                                                ~ "-")
      )
    return(var)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Prix: JO & ATU                                        ####  
#------------------------------------------------------------------------#
{
  
  func_UCD_Nb <- function(JO_QualifPrix, UCD_Flacon, UCD_Nb){
    var <- ifelse(JO_QualifPrix == "UCD" & UCD_Flacon %in% c("Oui","Non"), "1", UCD_Nb)
    return(var)
  }
  
  func_UCD_Quantite <- function(UCD_Flacon, UCD_Dose, UCD_Volume, UCD_Quantite){
    var <- ifelse(UCD_Flacon == "Oui", as.character(as.numeric(UCD_Dose)*as.numeric(UCD_Volume)), UCD_Quantite)
    return(var)
  }
    
  func_ATU_IndemMaxPresentation <- function(ATU_Presence, Demande, ATU_IndemMaxUCD, UCD_Nb, JO_PrixVigueurJO){
    var <-
      suppressWarnings(
        case_when(
          ATU_Presence == "Oui" &                            ATU_IndemMaxUCD != "-" ~ as.character(as.numeric(ATU_IndemMaxUCD)*as.numeric(UCD_Nb)),
          ATU_Presence == "Oui" & Demande != "Inscription" & ATU_IndemMaxUCD == "-" ~ JO_PrixVigueurJO,
          TRUE                                                                      ~ "-")
      )
    return(var)
  }
  
  func_PC_InfoPrix <- function(ATU_Presence, Demande, ATU_IndemMaxPresentation1, ATU_IndemMaxPresentation2, ATU_IndemMaxPresentation3, ATU_IndemMaxPresentation4, ATU_IndemMaxPresentation5, ATU_IndemMaxPresentation6, ATU_IndemMaxPresentation7, ATU_IndemMaxPresentation8, JO_Date1erJO, JO_DateJOpre){
    var <-
      case_when(
        Demande == "Inscription" & ATU_Presence == "Oui" &
          (ATU_IndemMaxPresentation1 != "-" | ATU_IndemMaxPresentation2 != "-" | ATU_IndemMaxPresentation3 != "-" | ATU_IndemMaxPresentation4 != "-" |
             ATU_IndemMaxPresentation5 != "-" | ATU_IndemMaxPresentation6 != "-" | ATU_IndemMaxPresentation7 != "-" | ATU_IndemMaxPresentation8 != "-" )   ~ "Prix ATU",
        Demande == "Inscription" & ATU_Presence %in% c("Oui","Non") &
          ATU_IndemMaxPresentation1 == "-" & ATU_IndemMaxPresentation2 == "-" & ATU_IndemMaxPresentation3 == "-" & ATU_IndemMaxPresentation4 == "-" &
          ATU_IndemMaxPresentation5 == "-" & ATU_IndemMaxPresentation6 == "-" & ATU_IndemMaxPresentation7 == "-" & ATU_IndemMaxPresentation8 == "-" &
          JO_Date1erJO != "2000-01-01"                                                                                                                   ~ as.character(JO_Date1erJO),
        Demande != "Inscription" & ATU_Presence == "Non"            & JO_Date1erJO != "2000-01-01" & JO_DateJOpre == "2000-01-01"                        ~ as.character(JO_Date1erJO),
        Demande != "Inscription" & ATU_Presence %in% c("Oui","Non") & JO_Date1erJO != "2000-01-01" & JO_DateJOpre != "2000-01-01"                        ~ as.character(JO_DateJOpre),
        Demande != "Inscription" & ATU_Presence == "Oui"            & JO_Date1erJO != "2000-01-01" & JO_DateJOpre == "2000-01-01"                        ~ "Prix ATU",
        TRUE                                                                                                                                             ~ "-")
    return(var)
  }
  
  func_PC_Qualif = function(PC_InfoPrix, JO_Date1erJO, JO_DateJOpre, JO_QualifPrix){
    var <-
      case_when(
        PC_InfoPrix == "Prix ATU"                 ~ "ATU",
        PC_InfoPrix == as.character(JO_Date1erJO) ~ JO_QualifPrix,
        PC_InfoPrix == as.character(JO_DateJOpre) ~ JO_QualifPrix,  # now, I only have 1 variable on QUALIF, so I replace "JO_QualifPrixJOpre" by "JO_QualifPrix"
        TRUE                                      ~ "-")
    return(var)
  }
  
  func_PC <- function(PC_InfoPrix,ATU_IndemMaxUCD,ATU_IndemMaxPresentation,JO_Date1erJO,JO_Prix1erJO,JO_DateJOpre,JO_PrixVigueurJO){
    var <-
      case_when(
        PC_InfoPrix == "Prix ATU" & ATU_IndemMaxUCD != "-" ~ ATU_IndemMaxPresentation,
        PC_InfoPrix == as.character(JO_Date1erJO)          ~ JO_Prix1erJO,
        PC_InfoPrix == as.character(JO_DateJOpre)          ~ JO_PrixVigueurJO,
        TRUE                                               ~ "-")
    return(var)
  }
  
  func_UCD_PrixContenance <- function(PC_Qualif, PC, UCD_Quantite, UCD_Nb){
    var <-
      suppressWarnings(
        case_when(
          PC_Qualif != "UCD" & PC != "-" & UCD_Quantite != "-" & UCD_Nb != "-" ~ as.character(as.numeric(PC)/as.numeric(UCD_Quantite)/as.numeric(UCD_Nb)),
          PC_Qualif == "UCD"                                                   ~ PC,
          TRUE                                                                 ~ "-")
      )
    return(var)
  }
  
  func_ATU1erJO <- function(JO_QualifPrix, JO_Prix1erJO, ATU_IndemMaxUCD, ATU_IndemMaxPresentation){
    var <-
      suppressWarnings(
        case_when(
           JO_QualifPrix %in% c("UCD","PLV") & as.numeric(JO_Prix1erJO)>=0 & as.numeric(ATU_IndemMaxUCD)>0 ~ as.character((as.numeric(JO_Prix1erJO)-as.numeric(ATU_IndemMaxUCD)) / as.numeric(ATU_IndemMaxUCD) * 100),
          !JO_QualifPrix %in% c("UCD","PLV") & as.numeric(JO_Prix1erJO)>=0 & as.numeric(ATU_IndemMaxUCD)>0 ~ as.character((as.numeric(JO_Prix1erJO)-as.numeric(ATU_IndemMaxPresentation)) / as.numeric(ATU_IndemMaxPresentation) * 100),
          TRUE                                                                                             ~ "-")
      )
    return(var)
  }
  
  func_JOprepost <- function(JO_PrixPostJO, JO_PrixVigueurJO){
    var <- 
      suppressWarnings(
        case_when(
          as.numeric(JO_PrixPostJO)>=0 & as.numeric(JO_PrixVigueurJO)>0 ~ as.character((as.numeric(JO_PrixPostJO)-as.numeric(JO_PrixVigueurJO)) / as.numeric(JO_PrixVigueurJO) * 100),
          TRUE                                                          ~ "-")
      )
    return(var)
  }
  
}  

#------------------------------------------------------------------------#
####      HAS - Posologie                                             ####  
#------------------------------------------------------------------------#
{ 
  
  func_PTCED_DureeEntretien <- function(df){
    df <- suppressWarnings(
      df %>% 
        rowwise() %>%
        mutate(PTCED_DureeEntretien = as.character(365.25 - sum((as.numeric(PTCED1_NbCycleAttaque) * as.numeric(PTCED1_DureeCycleAttaque)), (as.numeric(PTCED2_NbCycleAttaque) * as.numeric(PTCED2_DureeCycleAttaque)), 
                                                                (as.numeric(PTCED3_NbCycleAttaque) * as.numeric(PTCED3_DureeCycleAttaque)), (as.numeric(PTCED4_NbCycleAttaque) * as.numeric(PTCED4_DureeCycleAttaque)), 
                                                                 na.rm = TRUE)))
    )
    return(df)
  }
  
  func_PTCED_NbCycleEntretien <- function(df){
    PTCED_NbCycleEntretien <- suppressWarnings(as.numeric(df$PTCED_DureeEntretien)/as.numeric(df$PTCED_DureeCycleEntretien))
    PTCED_NbCycleEntretien <- ifelse(is.na(PTCED_NbCycleEntretien), "-", as.character(PTCED_NbCycleEntretien))
    return(PTCED_NbCycleEntretien)
  }
  
  func_PTCED_NbAdmEntretien <- function(df){
    PTCED_NbAdmEntretien <- suppressWarnings(as.numeric(df$PTCED_NbCycleEntretien)*as.numeric(df$PTCED_NbAdmCycleEntretien))
    PTCED_NbAdmEntretien <- ifelse(is.na(PTCED_NbAdmEntretien), "-", as.character(PTCED_NbAdmEntretien))
    return(PTCED_NbAdmEntretien)
  }
  
  func_extract_grandeur <- function(dose){
    all_unite <- str_split(dose, " ")[[1]][2]
    grandeur <- ifelse(test = !exists("dose") || is.null(dose) || is.na(is.numeric(dose)) || is.na((dose)) || grepl("-",dose),
                       yes  = " ",
                       no   = ifelse(test = str_sub(all_unite,1,1) %in% list_func_grandeur,
                                     yes  = str_sub(all_unite,1,1),
                                     no   = " "))
    return(grandeur)
  }
  
  func_extract_unite <- function(dose){
    all_unite <- str_split(dose, " ")[[1]][2]
    unite <-  ifelse(test = !exists("dose") || is.null(dose) || is.na(is.numeric(dose)) || is.na((dose)) || grepl("-",dose),
                     yes  = " ",
                     no   = ifelse(test = str_sub(all_unite,1,1) %in% list_func_grandeur,
                                   yes  = str_sub(all_unite,2,str_length(all_unite)),
                                   no   = str_sub(all_unite,1,str_length(all_unite)))
                     ) 
    return(unite)
  }
  
  func_unite_dose <- function(dose, grand_unit, unit){
    grand_unit <- ifelse(grand_unit == "-"," ", grand_unit)
    unit       <- ifelse(unit == "-"," ", unit)
    dose <- ifelse(test = !(exists("dose")) || is.null(dose) || is.na(is.numeric(dose)) || is.na((dose)) || grepl("-",dose), 
                   yes  = "-", 
                   no   = ifelse(test = grand_unit == " ", 
                                 yes  = paste(dose, unit), 
                                 no   = paste(dose, paste0(grand_unit,unit))))
    return(dose)
  }
  
  func_add_unit_dose <- function(dose, unite = "mg"){
    dose_unit <- suppressWarnings(ifelse(is.na(as.numeric(dose)), "-", paste(dose, unite, sep = " ")))
    return(dose_unit)
  }
  
  create_multiple_variable <- function(df, group, variable_name, iteration, value){
    for(i in iteration){
      df[,paste(paste0(group,i),variable_name, sep="_")] <- value
      i=i+1
    }
    return(df)
  }
  
  func_cateage <- function(TypeDose, CateAge){
    var <- ifelse(TypeDose %in% c("-","Dose Fixe","One Shot") || is.null(TypeDose), "-", CateAge)
    return(var)
  }
  
  func_age <- function(TypeDose, CateAge, Age){
    CateAge <- func_cateage(TypeDose, CateAge)
    var <- ifelse(CateAge %in% c("-","Adulte") || is.null(CateAge), "-",Age)
    return(var)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Posologie --> focus on conditionnement/Cout           ####  
#------------------------------------------------------------------------#
{ 
  
  func_Min_UCDPrixContenance <- function(db){
    var <-
      suppressWarnings(
        apply(db[, c("UCD_PrixContenance1","UCD_PrixContenance2","UCD_PrixContenance3","UCD_PrixContenance4","UCD_PrixContenance5","UCD_PrixContenance6","UCD_PrixContenance7","UCD_PrixContenance8")],
              1,
              function(x) ifelse(all(is.na(as.numeric(x))), NA, min(as.numeric(x), na.rm=T)))
      )
    var = as.character(ifelse(is.na(var), "-", var))
    return(var)
  }
  
  func_Min_Cond <- function(minimum, UCD_PrixContenance1, UCD_PrixContenance2, UCD_PrixContenance3, UCD_PrixContenance4, UCD_PrixContenance5, UCD_PrixContenance6, UCD_PrixContenance7, UCD_PrixContenance8, JO_InfoPrix1, JO_InfoPrix2, JO_InfoPrix3, JO_InfoPrix4, JO_InfoPrix5, JO_InfoPrix6, JO_InfoPrix7, JO_InfoPrix8){
    var <-
      suppressWarnings(
        case_when(###########################################CENSORED######################)
      )
    return(var)
  }
  
  func_Min_CondDose <- function(minimum, UCD_PrixContenance1, UCD_PrixContenance2, UCD_PrixContenance3, UCD_PrixContenance4, UCD_PrixContenance5, UCD_PrixContenance6, UCD_PrixContenance7, UCD_PrixContenance8, UCD_Quantite1, UCD_Quantite2, UCD_Quantite3, UCD_Quantite4, UCD_Quantite5, UCD_Quantite6, UCD_Quantite7, UCD_Quantite8, UCD_Nb1, UCD_Nb2, UCD_Nb3, UCD_Nb4, UCD_Nb5, UCD_Nb6, UCD_Nb7, UCD_Nb8){
    var <-
      suppressWarnings(
        case_when(###########################################CENSORED######################)
      )
    return(var)
  }
  
  func_Min_CondPrix <- function(minimum, UCD_PrixContenance1, UCD_PrixContenance2, UCD_PrixContenance3, UCD_PrixContenance4, UCD_PrixContenance5, UCD_PrixContenance6, UCD_PrixContenance7, UCD_PrixContenance8, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8){
    var <-
      suppressWarnings(
        case_when(###########################################CENSORED######################)
      )
    return(var)
  }
  
  # Calculation of DosePoids/DoseSurfaceCorporelle
  func_CoefDose <- function(TypeDose, CateAge, Age){
    var <- 
      suppressWarnings(
        case_when(###########################################CENSORED######################                                                                        ~ "-")
      )
    return(var)
  }
  
  # I need to have variable DOSE without the measure unit (ex. 100 mg --> 100) --> using directly "str_split" in functions such as "func_PTC_NbCond" poses concerns 
  func_dose = function(var){
    var <- str_split(var, " ", simplify = TRUE)[,1]
    return(var)
  }
  
  func_PTC_NbCond <- function(coef, DosePrise, NbPrise, NbAdmCycle, DureeCycle, CondDose){
    var <- suppressWarnings((as.numeric(coef) * as.numeric(DosePrise) * as.numeric(NbPrise)) * (as.numeric(NbAdmCycle)/as.numeric(DureeCycle)) / as.numeric(CondDose))
    var <- as.character(ifelse(is.na(var), "-", as.character(var)))
    return(var)
  }
  
  func_PTNC_NbCond <- function(coef, DosePrise, NbPriseCycle, NbCycle, CondDose, TypeDose, NbPriseCycleOneShot){
    var <- suppressWarnings(
              ifelse(TypeDose == "One Shot",
                     as.numeric(NbPriseCycleOneShot), 
                     (as.numeric(coef) * as.numeric(DosePrise) * as.numeric(NbPriseCycle) * as.numeric(NbCycle)) / as.numeric(CondDose) ))
    var <- as.character(ifelse(is.na(var), "-", as.character(var)))
    return(var)
  }
  
  func_PTCED_EntretienCond <- function(DosePriseEntretien, NbPriseEntretien, DureeEntretien){
    var <- suppressWarnings(as.numeric(DosePriseEntretien) * as.numeric(NbPriseEntretien) * as.numeric(DureeEntretien))
    var <- ifelse(is.na(var), 0, var)
    return(var)
  }
  
  func_PTCED_AttaqueCond <- function(DosePrise, NbPrise, NbAdmCycleAttaque, NbCycleAttaque){
    var <- suppressWarnings(as.numeric(DosePrise) * as.numeric(NbPrise) * as.numeric(NbAdmCycleAttaque) * as.numeric(NbCycleAttaque))
    var <- ifelse(is.na(var), 0, var)
  }
  
  func_PTCED_Cond <- function(DosePriseEntretien, NbPriseEntretien, DureeEntretien,
                              PTCED1_DosePrise, PTCED1_NbPrise, PTCED1_NbAdmCycleAttaque, PTCED1_NbCycleAttaque,
                              PTCED2_DosePrise, PTCED2_NbPrise, PTCED2_NbAdmCycleAttaque, PTCED2_NbCycleAttaque,
                              PTCED3_DosePrise, PTCED3_NbPrise, PTCED3_NbAdmCycleAttaque, PTCED3_NbCycleAttaque,
                              PTCED4_DosePrise, PTCED4_NbPrise, PTCED4_NbAdmCycleAttaque, PTCED4_NbCycleAttaque,
                              CondDose){
    # Calculation of each "Escalade de dose"
    entretien <- func_PTCED_EntretienCond(DosePriseEntretien, NbPriseEntretien, DureeEntretien)
    attaque1  <- func_PTCED_AttaqueCond(PTCED1_DosePrise, PTCED1_NbPrise, PTCED1_NbAdmCycleAttaque, PTCED1_NbCycleAttaque)
    attaque2  <- func_PTCED_AttaqueCond(PTCED2_DosePrise, PTCED2_NbPrise, PTCED2_NbAdmCycleAttaque, PTCED2_NbCycleAttaque)
    attaque3  <- func_PTCED_AttaqueCond(PTCED3_DosePrise, PTCED3_NbPrise, PTCED3_NbAdmCycleAttaque, PTCED3_NbCycleAttaque)
    attaque4  <- func_PTCED_AttaqueCond(PTCED4_DosePrise, PTCED4_NbPrise, PTCED4_NbAdmCycleAttaque, PTCED4_NbCycleAttaque)
    # All "Escalade de dose" 
    var <- suppressWarnings((entretien + attaque1 + attaque2 + attaque3 + attaque4) / 365.25 / as.numeric(CondDose))
    var <- ifelse(var == 0, NA, var)
    var <- ifelse(is.na(var), "-", as.character(var))
    var <- as.character(var)
    return(var)
  }
  
  func_CoutCond <- function(CondPrix, NbCond){
    var <- suppressWarnings(as.character(as.numeric(CondPrix) * as.numeric(NbCond)))
    var <- as.character(ifelse(is.na(var), "-", var))
    return(var)
  }
  
}

#------------------------------------------------------------------------#
####      HAS - Delai                                                 ####  
#------------------------------------------------------------------------#
{
  
  func_delai <-function(date1, date2){
    d1   <- as.Date(ifelse(date1 == "2000-01-01", NA, as.Date(date1)), origin = "1970-01-01") 
    d2   <- as.Date(ifelse(date2 == "2000-01-01", NA, as.Date(date2)), origin = "1970-01-01") 
    date <- ifelse(is.na(d1-d2), "-", as.character(d1-d2))
    return(date)
  }
  
}

#------------------------------------------------------------------------#
####      editHAS_Server.R / editQC_Server.R                          ####  
#------------------------------------------------------------------------#
{
  
  func_mutate_edit <- function(df, param){
    
    # Creation of variables with formulas (via functions)
    if(param == "assoc"){
      missing_var = varlistHAS$Variable[which(!varlistHAS$Variable %in% colnames(df) & varlistHAS$`Table des associations` == "Oui")]
    }
    if(param == "has"){
      missing_var = varlistHAS$Variable[which(!(varlistHAS$Variable %in% colnames(df)))]
    }
    
    
    # Characterization of some variables "missing"
    for (i in 1:length(missing_var)){
      if (missing_var[i] %in% c("Efficience_ReserveMineureNb","Efficience_ReserveImportanteNb","Efficience_ReserveMajeureNb","Efficience_ReserveMajeureplus1","Efficience_ReserveTotalNb",
                                "AIB_ReserveMineureNb","AIB_ReserveImportanteNb","AIB_ReserveMajeureNb","AIB_ReserveMajeureplus1","AIB_ReserveTotalNb")){
        df[, missing_var[i]] <- "A valider après le QC"
      }
      else if (missing_var[i] %in% c("PCP_Total","PCP_VariationAvis_n","PCI_Total","PCI_VariationAvis_n")){
        df[, missing_var[i]] <- "A valider après le QC"
      }
      else {
        df[, missing_var[i]] <- "-"
      }
    }
    
    
    # Update of variables
    df <- {df %>%
        mutate(###########################################CENSORED######################
        )}
    
    
    # Posologie - Escalade de dose
    ###########################################CENSORED######################
    
    
    # Prix/Conditionnement --> doesn't work in a mutate statment ie df = df %>% mutate (..)
    ###########################################CENSORED######################
    
    # Formatting on the "missing" data
    ###########################################CENSORED######################
    
    # Return
    return(df)
  }
  
}

#------------------------------------------------------------------------#
####      Formatting                                                  ####  
#------------------------------------------------------------------------#
{
  
  func_FormattingHAS = function(df){
    df <- {df %>% 
        mutate(
          # Source
          ###########################################CENSORED######################
          # Date
          ###########################################CENSORED######################
          # Euro - 2 decimal
          ###########################################CENSORED######################
          # Percentage - 2 decimal 
          ###########################################CENSORED######################
           # Number - 0 decimal 
          ###########################################CENSORED######################
          # Number - 2 decimal  
          ###########################################CENSORED######################
          # Jours - 0 decimal  
          ###########################################CENSORED######################
          # Character - formatage puissance
          ###########################################CENSORED######################
        )} 
    
    # "select" aims to order the database
    if("Auteur" %in% names(df)){
      df <- df %>% select(c("Buttons","Statut",varlistHAS$Variable,"Auteur"))
    }
    else{
      df <- df %>% select(c("Buttons",varlistHAS$Variable))
    }
    
    return (df)
  }
  
  func_FormattingRes = function(df){
    df <- df %>% mutate(SourceAvisCEESP     = func_SourceHyperlink(SourceAvisCEESP),
                        DateValidationCEESP = func_DateFormatting(DateValidationCEESP)) 
    return (df)
  }
  
  func_FormattingAssoc = function(df, param){
    df <- {df %>% 
        mutate(
          # Source
          ###########################################CENSORED######################
          # Date
          ###########################################CENSORED######################
          # Euro - 2 decimal
          ###########################################CENSORED######################
          # Percentage - 2 decimal 
          ###########################################CENSORED######################
          # Number - 2 decimal  
          ###########################################CENSORED######################
        )} 
    
    # "select" aims to order the database
    if (param == "with_select") {
      df <- df %>% select(c("Buttons","ID","NomProduit","NomProduitAssoc","DateAMM","DateValidationCEESP","Demande","TypeProduit", varlistHAS$Variable[which(varlistHAS$`Table des associations` == "Oui")]))
    }
    if (param == "with_select_qc") {
      df <- df %>% select(c("Buttons","Statut","ID","NomProduit","NomProduitAssoc","DateAMM","DateValidationCEESP","Demande","TypeProduit", varlistHAS$Variable[which(varlistHAS$`Table des associations` == "Oui")]))
    }
    if (param == "no_select") {
      df <- df 
    }
    
    return (df)
  }
  
}



## END
