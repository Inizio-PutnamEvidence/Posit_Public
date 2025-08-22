#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Add Item
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 July 2023
#   LAST MODIFIED DATE    15 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of ITEM
#  
#--------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for: Avis CT - Population cible                                                            ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_PopCible <- function(count_PopCible, value_CT_PopCible1, value_CT_PopCible2, value_CT_PopCibleComm, value_CT_Precision, value_CT_Source) {
  conditionalPanel(condition = "input.ct_popciblespe1 != '' ",
                  div(                                 
                    id = paste0("div_", count_PopCible),
                    div(
                      h3(paste("Avis n-", count_PopCible)),
                      strong(paste("Population cible de l'avis n-", count_PopCible), br()),
                      em("Dans le cas où 2 valeurs sont nécessaires, remplir les 2 valeurs (la moyenne des 2 valeurs sera calculée et conservée pour la suite).", br(), 
                         "Sinon, remplir uniquement la valeur 1.", br(), style = "color:#00CA9F"),
                      column(3, textInput(inputId = paste0("ct_popcible_", count_PopCible, "1"), label = "Valeur 1", width = "100%", value = value_CT_PopCible1)),
                      column(3, textInput(inputId = paste0("ct_popcible_", count_PopCible, "2"), label = "Valeur 2", width = "100%", value = value_CT_PopCible2)),
                      column(6, textInput(inputId = paste0("ct_popcible_comm_", count_PopCible), label = "Commentaires", width = "100%", value = value_CT_PopCibleComm))  
                    ),
                    conditionalPanel(condition = paste0("input.ct_popcible_", count_PopCible, "1 != ''"),
                                     radioButtons(inputId = paste0("ct_precision_", count_PopCible), label = paste("Précision de l'avis n-",count_PopCible), choices = c("incidente/an","prévalente/totale","-"), width = "100%", selected = value_CT_Precision),
                                     textInput(inputId = paste0("ct_source_", count_PopCible), label = paste("Source de l'avis n-",count_PopCible), width = "100%", value = value_CT_Source)
                    )
                  )
 )
}


#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for: Prix - JO & ATU/Accès précoce                                                         ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_Prix <- function(count_Prix, value_JO_InfoPrix, value_JO_Prix1erJO, value_JO_PrixVigueurJO, value_JO_PrixPostJO, value_UCD_Flacon, value_UCD_Dose, value_UCD_Volume, value_UCD_Quantite, value_UCD_Nb, value_ATU_IndemMaxUCD) {
  div(
    id = paste0("div_", count_Prix),
    br(),
    h3(paste0("Prix de l'UCD/conditionnement ", count_Prix, sep = " ")),
    em("Si traitement = thérapie 'One Shot', merci de ne pas compléter les informations concernant l'UCD/conditionnement et la quantité de principe actif.", style = "color:#00CA9F"),
    div(
      strong("Journal Official - prix (€)", br()),
      em("Information sur l'UCD/conditionnement",br()),
      textInput(inputId = paste0("JO_InfoPrix_", count_Prix), label = NULL, placeholder = "information sur l'UCD/conditionnement", width = "100%", value = value_JO_InfoPrix),
      conditionalPanel(condition = "input.ATU_presence == 'Oui'",
                       div(
                         em("ATU/Accès précoce - prix (€)", br()),
                         em("Merci d'indiquer le prix sans unité, sans espace, et avec un point comme séparateur si besoin (ex. 13456.78).", br()),
                         textInput(inputId = paste0("ATU_IndemMaxUCD_", count_Prix), label = NULL, placeholder = "indemnité max/UCD", width = "100%", value = value_ATU_IndemMaxUCD),
                       )
      ),
      conditionalPanel(condition = "input.afficher_1erJO == 1",
                       em("Prix du 1er JO (€)",br()),
                       em("Merci d'indiquer le prix sans unité, sans espace, et avec un point comme séparateur si besoin (ex. 13456.78).", br()),
                       textInput(inputId = paste0("JO_Prix1erJO_", count_Prix), label = NULL, placeholder = "prix du 1er JO (€)", width = "100%", value = value_JO_Prix1erJO),
      ),
      conditionalPanel(condition = "input.demande != 'Inscription' && input.afficher_JOvigueur == 1",
                       em("Prix du JO pré-évaluation (€)",br()),
                       em("Merci d'indiquer le prix sans unité, sans espace, et avec un point comme séparateur si besoin (ex. 13456.78).", br()),
                       textInput(inputId = paste0("JO_PrixVigueurJO_", count_Prix), label = NULL, placeholder = "prix du JO pré-évaluation (€)", width = "100%", value = value_JO_PrixVigueurJO),
      ),
      conditionalPanel(condition = "input.afficher_1erJOpostavis == 1",
                       em("Prix 1er JO post-évaluation (€)",br()),
                       em("Merci d'indiquer le prix sans unité, sans espace, et avec un point comme séparateur si besoin (ex. 13456.78).", br()),
                       textInput(inputId = paste0("JO_PrixPostJO_", count_Prix), label = NULL, placeholder = "prix 1er JO post-évaluation (€)", width = "100%", value = value_JO_PrixPostJO)
      )
    ),
    radioButtons(inputId = paste0("UCD_Flacon_", count_Prix), label = "Dosage de principe actif dépendant du volume ?", width = "100%", choices = c("Oui","Non","-"), selected = value_UCD_Flacon),
    div(
      strong("Dosage et Nombre d'UCD", br()),
      em("Merci di'ndiquer les valeurs ci-dessous sans unité, sans espace, et avec un point comme séparateur si besoin (ex. 13456.78).", br()),
      conditionalPanel(condition = paste0("input.UCD_Flacon_", count_Prix," == 'Oui'"),
                       textInput(inputId = paste0("UCD_Dose_", count_Prix), label = NULL, placeholder = "dosage du principe actif", width = "100%", value = value_UCD_Dose),
                       textInput(inputId = paste0("UCD_Volume_", count_Prix), label = NULL, placeholder = "volume de l'UCD", width = "100%", value = value_UCD_Volume)
      ),
      conditionalPanel(condition = paste0("input.UCD_Flacon_", count_Prix," != 'Oui'"),
                       textInput(inputId = paste0("UCD_Quantite_", count_Prix), label = NULL, placeholder = "quantité du principe actif", width = "100%", value = value_UCD_Quantite),
      ),
      conditionalPanel(condition = "input.JO_QualifPrix != 'UCD'",   # if JO_QualifPrix == UCD' then nb=1 ==> I don't program
                       textInput(inputId = paste0("UCD_Nb_", count_Prix), label = NULL, placeholder = "nombre", width = "100%", value = value_UCD_Nb)
      )
    
  ))
}


#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for: Posologie traitement chronique (PTC)                                                  ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_PTC <- function(count_PTC, value_PTC_typedose, value_PTC_cate_age, value_PTC_age, value_PTC_dose, value_PTC_grandunite, value_PTC_unite, value_PTC_nb, value_PTC_nbadm, value_PTC_duree) {
  div(
    id = paste0("div_", count_PTC),
    h3(paste0("Posologie ", count_PTC, sep = " ")),
    selectInput(inputId = paste0("PTC_Type_", count_PTC), label = "Type de posologie", choices = c("-","Dose Fixe","Dose Poids","Dose Surface Corporelle"), selected = value_PTC_typedose, width = "100%"),
    conditionalPanel(condition = paste0("input.PTC_Type_", count_PTC,"== 'Dose Poids' || input.PTC_Type_", count_PTC,"== 'Dose Surface Corporelle'"),
                     selectInput(inputId = paste0("PTC_cate_age_", count_PTC), label = "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrisson"), selected = value_PTC_cate_age, width = "100%"),
                     conditionalPanel(condition = paste0("input.PTC_cate_age_", count_PTC, "!= 'Adulte'"),
                                      div(
                                        strong("Age (années)", br()),
                                        em("Considérer la moyenne ou l'âge moyen pour des intervalles d'âge. Exemple - si la population est âgée de 6-11 ans, sélectionner 9 ans dans la liste déroulante."),
                                        conditionalPanel(condition =  paste0("input.PTC_cate_age_", count_PTC,"== 'Adolescent'"),
                                                         selectInput(inputId = paste0("PTC_age_ado_", count_PTC), label = NULL, choices = c(12:17,"Inconnu"), selected = value_PTC_age, width = "100%")
                                        ),
                                        conditionalPanel(condition =  paste0("input.PTC_cate_age_", count_PTC,"== 'Enfant'"),
                                                         selectInput(inputId = paste0("PTC_age_enf_", count_PTC), label = NULL, choices = c(3:11,"Inconnu"), selected = value_PTC_age, width = "100%")
                                        ),
                                        conditionalPanel(condition =  paste0("input.PTC_cate_age_", count_PTC,"== 'Nourrisson'"),
                                                         selectInput(inputId = paste0("PTC_age_nour_", count_PTC), label = NULL, choices = c(0:2,"Inconnu"), selected = value_PTC_age, width = "100%")
                                        )
                                    )
                     )
    ),
    numericInput(inputId = paste0("PTC_DosePrise_", count_PTC), label = "Dose par unité de prise", width = "100%", value = value_PTC_dose),
    div(
      strong("Grandeur d'unité", br()),
      em("p = Pico; n = Nano; μ = Micro; m = Mili; k = Kilo; M = Mega; G = Giga"),
      selectInput(inputId = paste0("PTC_GrandeurUniteDose_", count_PTC), label = NULL, choices = list_grandeurunite, selected = value_PTC_grandunite, width = "100%")
    ),
    conditionalPanel(condition = paste0("input.PTC_Type_", count_PTC,"== 'Dose Fixe'"),
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                       selectInput(inputId = paste0("PTC_UniteDoseFixe_", count_PTC), label = NULL, choices = list_unitedosefixe, selected = value_PTC_unite, width = "100%")
                     )
    ),
    conditionalPanel(condition = paste0("input.PTC_Type_", count_PTC,"== 'Dose Poids'"),
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel; c = Cellule; vg = Vector genomes"),  
                       selectInput(inputId = paste0("PTC_UniteDosePoids_", count_PTC), label = NULL, choices = list_unitedosepoids, selected = value_PTC_unite, width = "100%")
                     )
    ),
    conditionalPanel(condition = paste0("input.PTC_Type_", count_PTC,"== 'Dose Surface Corporelle'"),
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),  
                       selectInput(inputId = paste0("PTC_UniteDoseSurf_", count_PTC), label = NULL, choices = list_unitedosesurf, selected = value_PTC_unite, width = "100%")
                     )
    ),
    textInput(inputId = paste0("PTC_NbPrise_", count_PTC), label = "Nombre d'unité prise", width = "100%", value = value_PTC_nb), 
    textInput(inputId = paste0("PTC_NbAdmCycle_", count_PTC), label = "Nombre d'administration par cycle", width = "100%", value = value_PTC_nbadm), 
    textInput(inputId = paste0("PTC_DureeCycle_", count_PTC), label = "Durée d'un cycle (jours)", width = "100%", value = value_PTC_duree) 
  )
}


#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for PTCED = Posologie traitement chronique - Escalade de dose                              ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_PTCED <- function(count_PTCED, value_PTCED_dose, value_PTCED_grandunite, value_PTCED_unite, value_PTCED_nbdose, value_PTCED_nbadm, value_PTCED_duree, value_PTCED_nbcycle) {
  div(
    id = paste0("div_", count_PTCED),
    h4(paste0("Phase d'attaque ", count_PTCED, sep = " ")),
    numericInput(inputId = paste0("PTCED_DosePrise_", count_PTCED), label = "Dose par unité de prise de la phase d'attaque", width = "100%", value = value_PTCED_dose),
    div(
      strong("Grandeur d'unité", br()),
      em("p = Pico; n = Nano; μ = Micro; m = Mili; k = Kilo; M = Mega; G = Giga"),
      selectInput(inputId = paste0("PTCED_GrandeurUniteDose_", count_PTCED), label = NULL, choices = list_grandeurunite, selected = value_PTCED_grandunite, width = "100%")
    ),
    conditionalPanel(condition = "input.PTCED_TypeDoseAttaque == 'Dose Fixe'",
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"), 
                       selectInput(inputId = paste0("PTCED_UniteDoseFixe_", count_PTCED), label = NULL, choices = list_unitedosefixe, selected = value_PTCED_unite, width = "100%")
                     )
    ),
    conditionalPanel(condition = "input.PTCED_TypeDoseAttaque == 'Dose Poids'",
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel; c = Cellule; vg = Vector genomes"),
                       selectInput(inputId = paste0("PTCED_UniteDosePoids_", count_PTCED), label = NULL, choices = list_unitedosepoids, selected = value_PTCED_unite, width = "100%")
                     )
    ),
    conditionalPanel(condition = "input.PTCED_TypeDoseAttaque == 'Dose Surface Corporelle'",
                     div(
                       strong("Unité", br()),
                       em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                       selectInput(inputId = paste0("PTCED_UniteDoseSurf_", count_PTCED), label = NULL, choices = list_unitedosesurf, selected = value_PTCED_unite, width = "100%")
                     )
    ),
    textInput(inputId = paste0("PTCED_NbPrise_", count_PTCED), label = "Nombre d'unité prise de la phase d'attaque", width = "100%", value = value_PTCED_nbdose),
    textInput(inputId = paste0("PTCED_NbAdmCycleAttaque_", count_PTCED), label = "Nombre d'administration par cycle de la phase d'attaque", width = "100%", value = value_PTCED_nbadm),
    textInput(inputId = paste0("PTCED_DureeCycleAttaque_", count_PTCED), label = "Durée d'un cycle de la phase d'attaque (jours)", width = "100%", value = value_PTCED_duree),
    textInput(inputId = paste0("PTCED_NbCycleAttaque_", count_PTCED), label = "Nombre de cycle de la phase d'attaque", width = "100%", value = value_PTCED_nbcycle)
  )
}


#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for: Reserves CEM                                                                          ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_resCEM <- function(count_resCEM) {
  div(
    id = paste0("div_", count_resCEM),
    h4(paste0("Nouvelle Réserve CEM ", count_resCEM, sep = " ")),
    selectInput(inputId = paste0("CEM_DimHAS_",count_resCEM), label = "Dimension d'analyse par la HAS", width = "100%", choices = c(unique(sort(reservesCEM_buttons$CEM_DimHAS)),"Autre (à préciser)"), selected = NULL),
    conditionalPanel(condition = paste0("input.CEM_DimHAS_",count_resCEM, "== 'Autre (à préciser)'"),
                     textInput(inputId = paste0("autre_CEM_DimHAS_",count_resCEM), label = em("Merci de préciser la dimension d'analyse par la HAS", style = "color:#00CA9F"), width = "100%", value = ""),
    ),
    selectInput(inputId = paste0("CEM_DimPutnam_",count_resCEM), label = "Dimension d'analyse corrigée par Putnam", width = "100%", choices = c("-",unique(sort(reservesCEM_buttons$CEM_DimPutnam)),"Autre (à préciser)"), selected = NULL),
    conditionalPanel(condition = paste0("input.CEM_DimPutnam_",count_resCEM, "== 'Autre (à préciser)'"),
                     textInput(inputId = paste0("autre_CEM_DimPutnam_",count_resCEM), label = em("Merci de préciser la dimension d'analyse corrigée par Putnam", style = "color:#00CA9F"), width = "100%", value = ""),
    ),
    textAreaInput(inputId = paste0("CEM_Precision_",count_resCEM), label = HTML('<span style="color: black;">Intitulé de la réserve</span> <span style="color: red;">(A remplir obligatoirement)</span>'), width = "100%", value = ""),
    selectInput(inputId = paste0("CEM_TypeReserve_",count_resCEM), label = "Type de réserve", width = "100%", choices = c(unique(sort(reservesCEM_buttons$CEM_TypeReserve))), selected = NULL),
    textAreaInput(inputId = paste0("CEM_Remarque_",count_resCEM), label = "Remarque", width = "100%", value = ""),
  )
}


#-------------------------------------------------------------------------------------------------------------------#
####      Add an item for: Reserves BIM                                                                          ####
#-------------------------------------------------------------------------------------------------------------------#

add_item_resBIM <- function(count_resBIM) {
  div(
    id = paste0("div_", count_resBIM),
    h4(paste0("Nouvelle Réserve BIM ", count_resBIM, sep = " ")),
    selectInput(inputId = paste0("BIM_DimHAS_",count_resBIM), label = "Dimension d'analyse par la HAS", width = "100%", choices = c(unique(sort(reservesBIM_buttons$BIM_DimHAS)),"Autre (à préciser)"), selected = NULL),
    conditionalPanel(condition = paste0("input.BIM_DimHAS_",count_resBIM, "== 'Autre (à préciser)'"),
                     textInput(inputId = paste0("autre_BIM_DimHAS_",count_resBIM), label = em("Merci de préciser la dimension d'analyse par la HAS", style = "color:#00CA9F"), width = "100%", value = ""),
    ),
    selectInput(inputId = paste0("BIM_DimPutnam_",count_resBIM), label = "Dimension d'analyse corrigée par Putnam", width = "100%", choices = c("-",unique(sort(reservesBIM_buttons$BIM_DimPutnam)),"Autre (à préciser)"), selected = NULL),
    conditionalPanel(condition = paste0("input.BIM_DimPutnam_",count_resBIM, "== 'Autre (à préciser)'"),
                     textInput(inputId = paste0("autre_BIM_DimPutnam_",count_resBIM), label = em("Merci de préciser la dimension d'analyse corrigée par Putnam", style = "color:#00CA9F"), width = "100%", value = ""),
    ),
    textAreaInput(inputId = paste0("BIM_Precision_",count_resBIM), label = HTML('<span style="color: black;">Intitulé de la réserve</span> <span style="color: red;">(A remplir obligatoirement)</span>'), width = "100%", value = ""),
    selectInput(inputId = paste0("BIM_TypeReserve_",count_resBIM), label = "Type de réserve", width = "100%", choices = c(unique(sort(reservesBIM_buttons$BIM_TypeReserve))), selected = NULL),
    textAreaInput(inputId = paste0("BIM_Remarque_",count_resBIM), label = "Remarque", width = "100%", value = ""),
  )
}



## END
