#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for home tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 October 2024
#   LAST MODIFIED DATE    14 April 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Introduction page
# 
#--------------------------------------------------------------------------------------------------------



homeTab <- tabPanel(
    div(div(class = "fa fa-home", role = "navigation"), "Home"),
    value = "home",

    tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),

    mainPanel(width = 12,
              tagList(
                h3("Welcome to CEESPlorer application !!!", 
                   style = "padding: 10px; color: white; background-color: #451284; text-indent: 20px; border-radius: 5px;"),
                
                # Row 1 for intro  
                fluidRow(
                  style = "border-bottom: 4px double #451284; text-indent: 20px;",
                  p("Note:", 
                    style = "font-weight: bold; font-style: italic; text-decoration:underline; text-underline-offset: auto; font-size: 95%"),
                  p("Welcome to CEESPlorer application - collaboration between RWE & Biostatistics and HTA Operations teams.",
                    style = "font-size: 95%"), 
                ),
                
                br(),
                
                # Row 2 for methodology
                fluidRow(
                  column(10,
                         h3("About CEESPlorer application", style = "color:black; font-weight: 600"),
                         p("The CEESPlorer application allows users to explore the efficiency opinions, as well as the BIM & CEM methodological reservations (MRs)."),
                         p("The tool is a collaboration between the RWE & Biostatistics and HTA Operations teams of the ",
                           tags$a(href="https://www.putassoc.com", "Putnam", target = "_blank", class = "externallink"), 
                           "company"),
                         
                         
                         h3("About databases", style = "color:black; font-weight: 600"),
                         strong(tags$u("Executive summary of the CEESP database:")),
                         p(style="text-align: justify;",
                           "In France, since 2013, pharmaceutical and medical device companies have been required to submit an economic analysis to",
                           strong("French National Authority for Health"),
                           "(HAS) for presumed innovative products according to submission criteria updated in 2022.",
                           tags$sup("(1)"),
                           "The",
                           strong("Economic Evaluation and Public Health Committee"),
                           "(CEESP) evaluates the efficiency dossier and publishes economic opinions (EOs).",
                           tags$sup("(2)")),
                         p(style="text-align: justify;",
                           "In this context, the CEESP database developed by the HTA operations team and adapted to R-Shiny closely with the RWE & Biostatistics teams,
                               compiles all the outcomes of interest from EOs published by the CEESP since 2014, such as submission type, therapeutic area,
                               incremental cost-effectiveness ratio (ICER), methodological reservation (MRs) and CEESP conclusion.
                               Based on the selection of these EOs, this database is supplemented by relevant information from opinions published by Transparency Commission (CT)
                               - such as Clinical benefit (SMR) and Clinical added value (ASMR), as well as price data from the Official Journal of the French Republic (JORF) and
                               posology data from the Summary of product characteristics (SmPC)."),
                         p(style="text-align: justify;",
                           "This database is thus an internal tool, enabling to various analyses of EOs, either in-house or for clients (data analysis, monitoring of CEESP
                               publications related to a therapeutic area for a current project or for business development), to ensure that our teams are up to date on the
                               methodological issues raised by the CEESP (so as to guarantee the high quality of the dossiers we deliver to our clients and to publish literature
                               (4 posters for ISPOR Europe 2023 were designed from the database this year)."),
                         p(style="text-align: justify;","To date, the CEESP database contains information collected from over 200 EOs, and it is regularly updated."),
                         
                         p(strong(tags$u("References")),
                           br(),
                           tags$sup("(1)"),
                           "Décision n°2022.0212/DC/SED/SEM du 23 juin 2022 du collège de la Haute Autorité de santé relative à l’impact significatif sur
                                     les dépenses de l’assurance maladie déclenchant l’évaluation médico-économique des produits de santé revendiquant une ASMR ou
                                     une ASA de niveaux I, II ou III. Paris: HAS; 2022. Available at:",
                           tags$a(href=" https://www.has-sante.fr/upload/docs/application/pdf/2022-08/decision_n2022.0212_dc_sed_sem_du_23_juin_2022_du_college_de_la_haute_autorite_de_sante_relative_a_limpact_significatif_sur_.pdf",
                                  "HAS", target = "_blank", class = "externallink"),
                           br(),
                           tags$sup("(2)"),
                           "Haute Authorité de Santé. Avis économiques rendus par la Commission d’évaluation économique et de santé publique (CEESP)
                                     [Economic opinions issued by the CEESP]. Available at:",
                           tags$a(href="https://www.has-sante.fr/jcms/p_3149875/fr/avis-economiques-rendus-par-la-commission-d-evaluation-economique-et-de-sante-publique-ceesp",
                                  "CEESP", target = "_blank", class = "externallink"),
                         ),
                         
                         
                         h3("Contact us", style = "color:black; font-weight: 600"),
                         p("If you have any questions or comments relating to the data or the use of the application, don't hesitate to contact us."),
                         p(strong(tags$u("Teams members")),
                           tags$li(strong("RWE & Biostatistics:"), "Charlène Tournier (project manager)"),
                           tags$li(strong("HTA Operations:"), "Nicolas Virely (project manager), Olfa Doghri, Marine Sivignon")
                         ),
                         img(src = "2025Team.png", width = 800),
                         offset = 1
                  )
                )
              )
    )
  )



## END
