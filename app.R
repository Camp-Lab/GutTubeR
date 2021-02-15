#### GutTubeR ####
#_______________________####

# Load Libraries ####

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(scatterplot3d)
library(plotly)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(shinyBS)
library(gridExtra)
library(DT)
library(egg)
library(grid)
library(cowplot)
library(shinymanager)
library(scales)
library(Matrix)

# define some credentials
credentials <- data.frame(
    user = c('user'),
    password = c('password'), 
    stringsAsFactors = FALSE
)


source('data_descriptions.R')

#_______________________####

# UI Elements ####
#_______________________####
# 1: Dashboard elements ####

# |— Header ####
header <- dashboardHeader(title = 'GutTubeR')

# |— Sidebar ####
sidebar <- dashboardSidebar(
    conditionalPanel(condition = "input.data=='Res_tHIO_fetal_and_adult_duodenum_epi_integrated_with_CSS_new_2.rds'",
                     sidebarMenu(
                         menuItem('Atlas', tabName = 'atlas', icon = icon('atlas'), startExpanded = TRUE,
                                  menuSubItem('Data', tabName = 'data_select', icon = icon('folder-open')),
                                  menuSubItem('UMAP', tabName = 'atlas_2d', icon = icon('map')),
                                  menuSubItem('UMAP-3D', tabName = 'atlas_3d', icon = icon('map'))),
                         menuItem('Interactions', tabName = 'interactions', icon = icon('project-diagram'),
                                  menuSubItem('Browser', tabName = 'browser_interactions', icon = icon('search')),
                                  menuSubItem('Inspector', tabName = 'inspect_interactions', icon = icon('search-plus'))),
                         menuItem('How to', tabName = 'how_to', icon = icon('mortar-board')),
                         menuItem('About', tabName = 'about', icon = icon('question')))),
    conditionalPanel(condition = "input.data!='Res_tHIO_fetal_and_adult_duodenum_epi_integrated_with_CSS_new_2.rds'",
                     sidebarMenu(
                         menuItem('Atlas', tabName = 'atlas', icon = icon('atlas'),
                                  menuSubItem('Data', tabName = 'data_select', icon = icon('folder-open')),
                                  menuSubItem('UMAP', tabName = 'atlas_2d', icon = icon('map'))),
                         
                         menuItem('Interactions', tabName = 'interactions', icon = icon('project-diagram'),
                                  menuSubItem('Browser', tabName = 'browser_interactions', icon = icon('search')),
                                  menuSubItem('Inspector', tabName = 'inspect_interactions', icon = icon('search-plus'))),
                         menuItem('How to', tabName = 'how_to', icon = icon('mortar-board'), selected = TRUE),
                         menuItem('About', tabName = 'about', icon = icon('question'))))
)

#_______________________####
# 2: Tab Items ####

## |— Select Data ####
data_select <- tabItem(tabName = 'data_select',
                       fluidRow(
                           column(width = 12,
                                  box(title = 'Data', width = 12, 
                                      fluidRow(
                                          column(width = 4,
                                                 selectInput(
                                                     'data', 'Select dataset:',
                                                     choices = datasets %>% deframe() %>% as.list()),
                                                 actionButton('select','Load')
                                                 ),
                                          column(width = 7,
                                                 tags$b('Description'),
                                                 br(),
                                                 uiOutput('description'),
                                                 textOutput('metadata'))
                                      ),hr(),
                                      fluidRow(
                                          column(width = 2),
                                          column(width = 8,
                                                 plotOutput('preview', height = '65vh'))
                                          )
                                  )
                           )
                       )
)


## |— Atlas 2D ####
atlas_2d <- tabItem(tabName = 'atlas_2d',
                    fluidRow(
                        column(width = 12,
                               box(title = textOutput('dataset'), width = 12, ## |—|— UMAPs  ####
                                   fluidRow(
                                       column(width = 2,
                                              materialSwitch(
                                                  inputId = "sub1",
                                                  label = textOutput('label_plot1', inline = 1), 
                                                  value = TRUE,
                                                  right = TRUE,
                                                  status = "primary"
                                              )),
                                       column(width = 2,
                                              materialSwitch(
                                                  inputId = "sub2",
                                                  label = textOutput('label_plot2', inline = 1), 
                                                  value = FALSE,
                                                  right = TRUE,
                                                  status = "primary"
                                              )),
                                       column(width = 2,
                                              materialSwitch(
                                                  inputId = "sub3",
                                                  label = textOutput('label_plot3', inline = 1), 
                                                  value = FALSE,
                                                  right = TRUE,
                                                  status = "primary"
                                              )),
                                       column(width = 2,
                                              materialSwitch(
                                                  inputId = "sub4",
                                                  label = textOutput('label_plot4', inline = 1), 
                                                  value = FALSE,
                                                  right = TRUE,
                                                  status = "primary"
                                              )),
                                       uiOutput('sampling_ui'),
                                       column(width = 1, 
                                              actionButton('show_settings_umap', 'Settings', icon = icon('gear'),
                                                           style = "margin-top: -8px;")),
                                       column(width = 1,
                                              downloadButton('download_umap','Download',
                                                             style = "margin-top: -8px;"))
                                   ),
                                   fluidRow(
                                       column(width = 1, uiOutput('select_window')),
                                       column(width = 2, uiOutput('meta_select')),
                                       uiOutput('select_gene'),
                                       column(width = 1, 
                                              actionButton('add_setting',' Add',
                                                           icon = icon('plus'),
                                                           style = "margin-top: 25px;")),
                                       column(width = 1, 
                                              actionButton('refresh_umap',' Update',
                                                           icon = icon('refresh'),
                                                           style = "margin-top: 25px;")),
                                       column(width = 1, 
                                              numericInput('nRow','Rows:',
                                                           min = 1, max = 2,
                                                           step = 1, value = 1)),
                                       column(width = 1, actionButton('show_distributions','Statistics',
                                                                    icon = icon('chart-bar'),
                                                                    style = "margin-top: 25px;"))
                                       
                                   ),
                                   hr(),
                                   ## |—|— Settings  ####
                                   bsModal('settings_umap','Settings','show_settings_umap', 
                                           size = 'large',
                                           tags$b('Plotting Options'),
                                           fluidRow(
                                               column(width = 3,
                                                      selectInput('colorscale',
                                                                  'Colorscale:',
                                                                  choices = list(Blues = 'blues',
                                                                                 Viridis = 'D',
                                                                                 Magma = 'A',
                                                                                 Inferno = 'B',
                                                                                 Plasma = 'C'
                                                                  ))),
                                               column(width = 2,
                                                      numericInput('point_size','Point size:',
                                                                   value = 0.25,
                                                                   min = 0.25,
                                                                   max = 5,
                                                                   step = 0.25)),
                                               column(width = 3, 
                                                      numericInput('sampling_rate', 
                                                                   'Sampling Rate:',
                                                                   value = 0.5,
                                                                   min = 0.1,
                                                                   max = 1,
                                                                   step = 0.1))
                                           ),
                                           hr(),
                                           tags$b('Download Options'),
                                           fluidRow(
                                               column(width = 2,
                                                      selectInput('file_format','Format:', choices = c('png','jpeg','pdf'))),
                                               column(width = 2,
                                                      selectInput('unit','Unit:', choices = c('cm','in','mm'))),
                                               column(width = 2,
                                                      numericInput('resolution',
                                                                   'Dpi:',
                                                                   value = 300,
                                                                   min = 100,
                                                                   max = 600,
                                                                   step = 100)),
                                           ),
                                           fluidRow(
                                               column(width = 2,
                                                      numericInput('width',
                                                                   'Width:',
                                                                   value = 40,
                                                                   min = 1,
                                                                   max = 50,
                                                                   step = .5)),
                                               column(width = 2,
                                                      numericInput('height',
                                                                   'Height:',
                                                                   value = 20,
                                                                   min = 1,
                                                                   max = 50,
                                                                   step = .5))
                                           )),
                                   tags$head(tags$style("#distributions_umap .modal-dialog {width: 85%;}")),
                                   ## |—|— Statistics  ####
                                   bsModal('distributions_umap','Distributions', 'show_distributions',size = 'large',
                                           fluidRow(
                                               column(width = 3,
                                                      radioGroupButtons('display_mode', choices = list(Accompany = 'accompany',
                                                                                                       Comparison = 'compare'))),
                                               column(width = 3,
                                                      checkboxInput('gene_clip','Clip gene expression histogram', value = TRUE),
                                                      checkboxInput('add_combination', 'Combine expression and grouping', value = TRUE)),
                                               column(width = 1, actionButton('refresh_distributions','Update',icon = icon('refresh'))),
                                               column(width = 1, 
                                                      downloadButton('download_dist','Download'))
                                           ),
                                           fluidRow(
                                               uiOutput('stats_ui') 
                                           ),
                                           hr(),
                                           fluidRow(plotOutput('distributions_plot', height = '65vh'))),
                                   plotOutput('umap_2d', height = "65vh"))
                        )
                        
                    )
)

# |— Atlas 3D ####
atlas_3d <- tabItem(tabName = 'atlas_3d',
                    fluidPage(
                        fluidRow(
                            column(width = 2,
                                   selectInput('meta_3d',
                                               'Metadata:',
                                               choices = list(`Cell type` = 'cell_type',
                                                               Gene = 'gene',
                                                               Age = 'age'))),
                            uiOutput('gene_3d'),
                            column(width = 1,
                                   actionButton('refresh_umap_3d',
                                                'Update',
                                                icon = icon('refresh'),
                                                style = "margin-top: 25px;"))
                        ),
                        plotlyOutput('umap_3d', height = '80vh'))
)

# |— Interactions ####
browser_interactions <- tabItem(tabName = 'browser_interactions',
                                fluidPage(
                                    fluidRow(
                                        column(width = 2,
                                               selectInput('organ_select',
                                                           'Select Organ:',
                                                           choices = list(`All Organs` = 'all_organs',
                                                                          `Small Intestine` = "SI",
                                                                          Colon = 'Colon',
                                                                          Stomach = 'Stomach',
                                                                          Esophagus = 'Esophagus',
                                                                          Lung = 'Lung'))), 
                                        column(width = 3,
                                               selectInput('direction_select',
                                                           'Direction:',
                                                           choices = list(`Mesenchyme to epithelium` = 'mes2epi',
                                                                          `Epithelium to mesenchyme` = 'epi2mes'))),
                                        column(width = 1, 
                                               actionButton('update_heatmap',' Update',
                                                            icon = icon('refresh'),
                                                            style = "margin-top: 25px;")),
                                        column(width = 4, tableOutput('click')),
                                        column(width = 1,
                                               actionButton('add_interaction', ' Add',
                                                            icon = icon('plus'),
                                                            style = "margin-top: 25px;",
                                                            align = 'left')),
                                        column(width = 1, actionButton('setting_interaction','Settings',
                                                                       icon = icon('gear'),
                                                                       style = "margin-top: 25px;",
                                                                       align = 'left'))
                                    ),
                                    # |—|— Settings ####
                                    bsModal('setting_modal_interactions','Settings','setting_interaction',size = 'large',
                                            fluidRow(
                                                column(width = 3,
                                                       checkboxInput('scale_matrix','Scale colors unidirectional', value = FALSE),
                                                       radioGroupButtons('scale_by_row',
                                                                         'Scale color by:',
                                                                         choices = list(Row = 'row', 
                                                                                        Column = 'col'),
                                                                         selected = 'row')),
                                                column(width = 4,
                                                       uiOutput('heatmap_genes')
                                                       ))
                                            ),
                                            
                                    
                                    plotlyOutput('heatmap', width = '100%', height = '75vh')
                                )
)    


# |— Inspector ####
inspect_interactions <- tabItem(tabName = 'inspect_interactions',
                                fluidRow(
                                    column(width = 12,
                                           tabBox(title = 'Inspector', width = 12, 
                                                  ## |—|— Editor  ####
                                                  tabPanel(title = 'Editor', icon = icon('edit'),
                                                           DTOutput('interactions_list'),
                                                           tags$b("Add gene pairs manually:"),
                                                           selectizeInput('user_receptor','Receptor:', choices = NULL),
                                                           selectizeInput('user_ligand','Ligand:',choices = NULL),
                                                           actionButton('add_user_pair','Add gene pair', icon = icon('plus'))),
                                                  ## |—|— Plotting  ####
                                                  tabPanel(title = 'Inspect', icon = icon('map'),
                                                           br(),
                                                           fluidRow(column(width = 2,
                                                                           materialSwitch(
                                                                               inputId = "ins1",
                                                                               label = textOutput('label_ins1',inline = 1), 
                                                                               value = TRUE,
                                                                               right = TRUE,
                                                                               status = "primary"
                                                                           )),
                                                                    column(width = 2,
                                                                           materialSwitch(
                                                                               inputId = "ins2",
                                                                               label = textOutput('label_ins2',inline = 1), 
                                                                               value = FALSE,
                                                                               right = TRUE,
                                                                               status = "primary"
                                                                           )),
                                                                    column(width = 2,
                                                                           materialSwitch(
                                                                               inputId = "ins3",
                                                                               label = textOutput('label_ins3',inline = 1), 
                                                                               value = FALSE,
                                                                               right = TRUE,
                                                                               status = "primary"
                                                                           )),
                                                                    column(width = 2,
                                                                           materialSwitch(
                                                                               inputId = "ins4",
                                                                               label = textOutput('label_ins4',inline = 1), 
                                                                               value = FALSE,
                                                                               right = TRUE,
                                                                               status = "primary"
                                                                           )),
                                                                    column(width = 1, tags$style(HTML('#sampling_inspector {margin-top: -28px}')), 
                                                                           radioGroupButtons('sampling_inspector','',
                                                                                             selected = FALSE, 
                                                                                             choiceNames = list(icon('running'), icon('hiking')),
                                                                                             choiceValues = list(TRUE,FALSE),
                                                                                             justified = TRUE)),
                                                                    column(width = 1, 
                                                                           actionButton('show_settings_inspector', 'Settings', icon = icon('gear'),
                                                                                        style = "margin-top: -8px;")),
                                                                    column(width = 1,
                                                                           downloadButton('download_inspector','Download',
                                                                                          style = "margin-top: -8px;"))),
                                                           fluidRow(
                                                               uiOutput("select_inspector"),
                                                               column(width = 2,
                                                                      selectInput('inspector_data','Atlas:', choices = datasets %>%
                                                                                      deframe() %>% as.list(), 
                                                                                  selected = 'Res_fetal_age_and_cell_type_selected_mes_all_organ_combined.rds')),
                                                               column(width = 2,
                                                                      uiOutput('meta_inspect')),
                                                               
                                                               column(width = 1,
                                                                      actionButton('add_inspector','Add',
                                                                                   icon = icon('plus'),
                                                                                   style = "margin-top: 25px;")),
                                                               column(width = 1, 
                                                                      actionButton('update_inspector','Update',
                                                                                   icon = icon('refresh'),
                                                                                   style = "margin-top: 25px;")),
                                                               column(width = 1, 
                                                                      numericInput('nrow_inspect', 'Rows:',
                                                                                   value = 1, min = 1, max = 2, step = 1)),
                                                               column(width = 1,
                                                                      actionButton('show_distributions_inspector',
                                                                                   'Statistics',
                                                                                   icon = icon('chart-bar'),
                                                                                   style = "margin-top: 25px;"))
                                                           ),
                                                           hr(),
                                                           ## |—|—|— Settings  ####
                                                           bsModal('settings_inspector','Settings','show_settings_inspector',
                                                                   size = 'large',
                                                                   tags$b('Plotting Options'),
                                                                   fluidRow(
                                                                       column(width = 3,
                                                                              selectInput('colorscale_ins',
                                                                                          'Colorscale:',
                                                                                          choices = list(Blues = 'blues',
                                                                                                         Viridis = 'D',
                                                                                                         Magma = 'A',
                                                                                                         Inferno = 'B',
                                                                                                         Plasma = 'C')
                                                                              )),
                                                                       column(width = 2,
                                                                              numericInput('point_size_ins','Point size:',
                                                                                           value = 0.25,
                                                                                           min = 0.25,
                                                                                           max = 5,
                                                                                           step = 0.25)),
                                                                       column(width = 3, 
                                                                              numericInput('sampling_rate_inspector', 
                                                                                           'Sampling Rate:',
                                                                                           value = 0.5, min = 0.1, max = 1, step = 0.1)),
                                                                       column(width = 3, 
                                                                              selectInput('bivariate_scale','Bivariate scale:',
                                                                                          choices = list(Viridis = 'D',
                                                                                                         Magma = 'A',
                                                                                                         Inferno = 'B',
                                                                                                         Plasma = 'C')))
                                                                   ),
                                                                   
                                                                   hr(),
                                                                   tags$b('Download Options'),
                                                                   fluidRow(
                                                                       column(width = 2,
                                                                              selectInput('file_format_ins','Format:',
                                                                                          choices = c('png','jpeg','pdf'))),
                                                                       column(width = 2,
                                                                              selectInput('unit_ins','Unit:',
                                                                                          choices = c('cm','in','mm'))),
                                                                       column(width = 2,
                                                                              numericInput('resolution_ins','Dpi:',
                                                                                           value = 300,
                                                                                           min = 100,
                                                                                           max = 600,
                                                                                           step = 100))
                                                                       
                                                                   ),
                                                                   fluidRow(
                                                                       column(width = 2, 
                                                                              numericInput('width_ins','Width:',
                                                                                           value = 40,
                                                                                           min = 1,
                                                                                           max = 50,
                                                                                           step = .5)),
                                                                       column(width = 2, 
                                                                              numericInput('height_ins','Height:',
                                                                                           value = 20,
                                                                                           min = 1,
                                                                                           max = 50,
                                                                                           step = .5))
                                                                   )
                                                           ),
                                                           ## |—|—|— Statistics  ####
                                                           tags$head(tags$style("#distributions_inspector .modal-dialog {width: 85%;}")),
                                                           bsModal('distributions_inspector','Distributions','show_distributions_inspector', size = 'large',
                                                                   fluidRow(
                                                                       column(width = 3,
                                                                              checkboxInput('gene_clip_ins','Clip gene expression histograms',
                                                                                            value = TRUE)),
                                                                       column(width = 1,
                                                                              actionButton('refresh_distribution_ins','Update', icon = icon("refresh"))),
                                                                       column(width = 1, 
                                                                              downloadButton("download_dist_ins","Download"))
                                                                   ),
                                                                   fluidRow(uiOutput('stats_ui_ins'),uiOutput('stats_ui_filter')),
                                                                   hr(),
                                                                   fluidRow(uiOutput('distributions_plots')#,
                                                                       #column(width=11,plotOutput('distributions_plot_ins',height = '65vh'))
                                                                       )
                                                           ),
                                                           fluidRow(
                                                               uiOutput('inspector_plotting')
                                                           ))
                                                  
                                           )
                                    )
                                    
                                )
)


# |— How To ####
how_to <- tabItem(tabName = 'how_to',
                  fluidPage(
                    tabBox(title = 'How To', width = 12, 
                           tabPanel(title = 'General', icon = icon('mortar-board'),
                                    fluidRow(
                                      column(width = 1),
                                      column(width = 10,
                                             h3('Welcome to GutTubeR!'),
                                             p('Here you will find instructions on how to use 
                                                this Shiny-app and how it accompanies the paper \"Charting human development using a multi-endodermal organ cell atlas and organoid technologies\".
                                                In general, GutTubeR consists of two main sections: 
                                                The Atlas section (',icon('atlas'),') allows you to browse selected datasets from the paper by creating multiple
                                                UMAPs with various meta annotations and customizable settings as well as gaining deeper insights into
                                                the data by observing and comparing distributions of gene expression and annotations. Within the Interactions section (',icon('project-diagram'),')
                                                you can browse (',icon('search'),') signalling interactions between mesenchyme and epithelial across all organs available
                                                from our datasets in an interactive fashion. Ongoing from your selection you can inspect interesting ligand-receptor
                                                pairs further on bivariate colored UMAPs accompanied by their respective distributions. All plotting options available
                                                in the Atlas section (',icon('atlas'),') also apply for the Inspector section (',icon('search-plus'),'). In order to learn how to use GutTuber in more detail, 
                                                please proceed by consulting the Atlas or Interactions tabs above.',
                                               style = "text-align: justify"),
                                             hr(),
                                             img(src = 'guttuber_example.png', align = "center", width = '100%'),
                                             hr()))
                            
                                     ),
                           tabPanel(title = 'Atlas', icon = icon('atlas'),
                                    fluidRow(
                                      column(width = 1),
                                      column(width = 10,
                                             br(),
                                             h4(icon('folder-open'), ' Data'),
                                             p('In this menu you can select a dataset that you want to analyze within the UMAP menu (',icon('map'),').
                                                Once you have chosen a dataset, make sure you click \"Load\" in order to make
                                                it available in the UMAP menu. After you loaded a dataset, a brief description
                                                of the dataset with regard to its experimental details as well as a preview of the UMAP embedding is shown. You are now set 
                                                up to explorer the selected dataset with GutTubeR.', style = "text-align: justify"),
                                             br(),
                                             img(src = 'guttuber_data_2.png', align = "center", width = '100%'),
                                             br(),
                                             br(),
                                             h4(icon('map'), ' UMAP'),
                                             p('In this menu you can create UMAPs of the selected dataset in a dynamic fashion. First, select how many plots you want to create
                                               by activating up to four plots with the switches at the top. After having done so, you can select the meta annotations for the corresponding
                                               UMAPs from a drop-down menu. Note, if you want to annotate gene expression, just type and search for your gene of interest in the  
                                               selection field. You can also filter grouping annotations by selecting multiple groups from the filter selection menu. After having chosen a annotation click the Add button (',icon('plus'),').
                                               Proceed like this for all activated plots. Once you set all the annotations you want to plot, click the Update button (',icon('refresh'),') and your UMAPs will be rendered.
                                               Note, that when clicking the Add button (',icon('plus'),') the corresponding plot will be created 
                                               with the current settings and it will overwrite existing annotations for that plot.
                                               Adjusting settings (',icon('gear'),') later on will just affect newly added plots. This allows you to select for example different color scales for gene 
                                               expression on the same UMAP or load and compare other datasets to already created UMAPs from different datasets. You can download the plots you created by clicking the Download button
                                               (',icon('download'),') with the set download parameters.',
                                               style = "text-align: justify"),
                                             br(),
                                             img(src = 'guttuber_plots_ui.png', align = "center", width = '100%'),
                                             img(src = 'guttuber_plot.png', align = "center", width = '100%'),
                                             br(),br(),
                                             h4(icon('running'), ' Performance', icon('hiking')),
                                             p('Some of the available datasets are rather huge which leads to increased plotting time. When you encounter such behavior, you can activate the 
                                               performance mode (',icon('running'),'). When activated the selected dataset is sampled before creating the UMAP, which reduces overplotting and plotting time.
                                               The applied sampling rate can be adjusted in the settings (',icon('gear'),'). By default the performance mode is activated (',icon('running'),') when the selected dataset exceeds 150,000 cells otherwise 
                                               it is deactivated (',icon('hiking'),').',
                                               style = "text-align: justify"),
                                             br(),
                                             h4(icon('chart-bar'), ' Statistics'),
                                             p('The statistics pop-up window allows you to plot underlying distributions of your created UMAPS. You can either accompany a selected UMAP with a distribution 
                                               for its selected annotation or compare two UMAPs and their corresponding distributions. Note, that when you select a UMAPs with a discrete mapping and 
                                               a gene expression mapping, a combined violine plot will be created showing the gene expression grouped by the selected discrete meta annotation.
                                               In many cases, it makes sense to filter the gene expression values for not expressed genes. You can toggle this with the clip gene expression histogram option. 
                                               You can download the displayed distribution plots by clicking the Download button (',icon('download'),'). Here the same download parameters apply as for the UMAP section (',icon('map'),')',
                                               style = "text-align: justify"),
                                             br(),
                                             img(src = 'guttuber_stats_1.png', align = "center", width = '100%'),
                                             br(),br(),
                                             h4(icon('gear'), ' Settings'),
                                             p('In the settings menu you can specify options for plotting and downloading. The chosen settings apply for the main UMAP plotting functionality (',icon('map'),')
                                               as well as for the statistics menu (',icon('chart-bar'),'). By now you can choose a color scale for continous colormapping (blues and viridis color palettes) of gene expression, set the point size and set the sampling rate (0.1 - 1) which is 
                                               applied when the performance mode (',icon('running'),') is activated. Apart from this you can specify aspect ratio, unit, resolution and format parameters for downloading (',icon('download'),') the created UMAPs and distribution plots
                                               as an image file.',
                                               style = "text-align: justify"),
                                             br(),
                                             img(src = 'guttuber_settings_1.png', align = "center", width = '100%'),
                                             br(),br(),hr()
                                             )
                                    )),
                           tabPanel(title = 'Interactions', icon = icon('project-diagram'),
                                    fluidRow(
                                      column(width = 1),
                                      column(width = 10,
                                             br(),
                                             h4(icon('search'), ' Browser'),
                                             p('This sections allows you to browse the results of CellPhoneDB analysis for signalling between mesenchyme and epithelial across all organs available
                                                from our datasets with an interactive interaction heatmap. In order to start select a dataset and the signalling direction and click the Update button 
                                               (',icon('refresh'),'). Please be patient when loading the interaction heatmap for the All Organs dataset. Once the heatmap is loaded, 
                                               you can start hovering over it in order to highlight specific ligand-receptor pairs and their related pathway/celltype/organ information. When you found 
                                               a interesting pair, click on it in the heatmap and click Add (',icon('plus'),') in order to save this interaction and investigate it further in the Inspector section (',icon('search-plus'),').
                                               Note that you can navigate to specific areas of the heatmap by drawing a rectangular. This will zoom into the selected area. Double-clicking will show the entire heatmap again.
                                               It is also possible to filter the interaction pairs for genes of your interest in the Settings (',icon('gear'),') as well as setting the center of the colorscaling 
                                               to either rowwise or columnwise. Changed settings will apply as soon as you click Update (',icon('refresh'),') again. You can download the displayed heatmap by clicking on download button (',icon('camera'),')
                                               in the upper right corner of the interactive heatmap.',
                                               style = "text-align: justify"),
                                             br(),
                                             img(src = 'interactions_zoom.png', align = "center", width = '100%'),
                                             br(), br(),
                                             img(src = 'interactions_settings_2.png', align = "center", width = '100%'),
                                             br(),
                                             br(),
                                             img(src = 'interactions_heatmap_filter.png', align = "center", width = '100%'),
                                             br(),
                                             br(),
                                             h4(icon('search-plus'), ' Inspector'),
                                             p('This section consists of two subsections: The Editor (',icon('edit'),') that allows you to view and edit all selected interactions
                                               from the Browser section (',icon('search'),') as well as add receptor-ligand pairs manually. And Inspect (',icon('map'), ') which enables you to map ligand-receptor pairs by bivariate color mapping to UMAPs 
                                               of either all organs or specific ones. The plotting interface has the same functionalities as already described for the Atlas section (',icon('atlas'), '). Meaning that you can plot up to four different 
                                               UMAP embeddings of different datasets with annotations for gene expression for either ligand or receptor, age, organ, tissue and cell type. In addition it is possible to select annotations for 
                                               ligand-receptor expression which utilizes bivariate coloring and mapping. 
                                               Within the statistics menu (',icon('chart-bar'), ') you can view the underlying distributions of UMAPs created with the dynamic plotting interface.',
                                               style = "text-align: justify"),
                                             br(),
                                             img(src = 'inspector_editor.png', align = "center", width = '100%'),
                                             br(),br(),
                                             img(src = 'inspector_plots.png', align = "center", width = '100%'),
                                             br(),br(),
                                             img(src = 'inspector_settings_2.png', align = "center", width = '100%'),
                                             br(),br(),
                                             p('In the Statistics pop-up window (',icon('chart-bar'),') you can 
                                               accompany the UMAP of selected ligand-receptor interactions or other selected annotations with their corresponding distributions grouped by either organ, tissue or cell type. When a grouping is selected, 
                                               the corresponding annotated UMAP is plotted additionally. Note, that having to many groups can create faceted histograms that are not feasible. When you encounter this, please filter the 
                                               displayed groups from the selection menu. The filtering also applies to the added UMAP with the grouping annotation. In general, 
                                               the same setting options (',icon('gear'),') apply for the Inspector interface (',icon('search-plus'), ') as for the Atlas section (',icon('atlas'), ') 
                                               with regards to setting colorscales, point size, performance mode, sampling rate and download parameters.',
                                               style = "text-align: justify"),
                                             br(),br(),
                                             img(src = 'inspector_stats_1.png', align = "center", width = '100%'),
                                             br(),br(),hr()
                                             )
                                    ))
                           )
                    
                  )
)


# |— About ####
about <- tabItem(tabName = 'about',
                 fluidPage(
                     box(title = 'About', width = 12,
                         fluidRow(
                             column(width = 1),
                             column(width = 10,
                                    h3('GutTubeR a Shiny Databrowser'),
                                    h4('Charting human development using a multi-endodermal organ cell atlas and organoid technologies'),
                                    p('Organs are composed of diverse cell types, which traverse transient states during organogenesis.
                                      To interrogate this diversity during human development, we generate a single-cell transcriptome atlas from multiple developing
                                      endodermal organs of the respiratory and gastrointestinal tract. We illuminate cell states, transcription factors, and organ-specific
                                      epithelial stem cell and mesenchyme interactions across lineages. We implement the atlas as a high-dimensional search space
                                      to benchmark pluripotent stem cell-derived human intestinal organoids (HIOs) in multiple culture conditions.
                                      We show that HIOs recapitulate reference cell states, and use HIOs to reconstruct the molecular dynamics of
                                      the intestinal epithelium and mesenchyme emergence. We show that the mesenchyme-derived niche cue NRG1 enhances
                                      intestinal stem cell maturation in vitro and that the homeobox transcription factor CDX2 is required for regionalization
                                      of intestinal epithelium and mesenchyme in humans. Collectively, this work combines cell atlases and organoid technologies
                                      to understand how human organ development is orchestrated.',
                                      style = "text-align: justify"),
                                    h4('Contact'),
                                    h5(tags$a(href = "https://www.graycamplab.org/", "Camp Lab", target = "_blank")),
                                    p("Institute of Molecular and Clinical Ophthalmology Basel"),
                                    p('University of Basel'),
                                    p("Mittlere Strasse 91, CH-4031 Basel, Switzerland"),
                                    br(),
                                    h5(tags$a(href = "https://bsse.ethz.ch/qdb", "Quantitative Developmental Biology Lab", target = "_blank")),
                                    p('Department of Biosystems Science and Engineering'),
                                    p('ETH Zürich'),
                                    p('Mattenstrasse 26, 4058 Basel, Switzerland'),
                                    br(),
                                    h5(tags$a(href = "http://www.jasonspencelab.com/", "Spence Lab", target = "_blank")),
                                    p('Department of Cell and Developmental Biology'),
                                    p('University of Michigan Medical School'),
                                    p('109 Zina Pitcher Place, Ann Arbor, Michigan 48109, USA'),
                                    br(),
                                    br(),
                                    p('Developed by: Christoph Harmel '),
                                    p(tags$a(href = "mailto:christoph.harmel@iob.ch","christoph.harmel@iob.ch"))
                                    
                                    
                                    )
                         ))
                     ))
#_______________________####
# 3: Body ####
body <- dashboardBody(tabItems(data_select,atlas_2d, atlas_3d, browser_interactions, inspect_interactions, how_to, about))

#_______________________####
# 4: Setup UI ####
ui <- dashboardPage(header, sidebar, body)

set_labels(
    language = "en",
    "Please authenticate" = "GutTubeR"
)
# Wrap your UI with secure_app
ui <- secure_app(ui,
                 theme = shinythemes::shinytheme("lumen"),
                 background  = "linear-gradient(rgba(21, 140, 186, 1),
                       rgba(255, 255, 255, 255))")
#_______________________####

# Server ####
server <- function(input, output, session) {
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    #_______________________####
    # 1: Load functions ####
    source('util_functions.R') 
    #_______________________####    
    # 2:  Atlas  ####
    
    ### |— Render UI ####
    
    
    ### |—|— Data description ####
    
    output$description <- renderUI({
        descriptions[[datasets %>% filter(file == input$data) %>% pull(name)]]
    })
    
    ### |—|— Window selection ####
    windows <- reactive({
        c(1,2,3,4)[c(input$sub1,input$sub2,input$sub3,input$sub4)]
    })
    
    output$select_window <- renderUI(
        list(selectInput('window','Plot:',choices = windows()))
    )
    
    ### |—|— Meta selection ####
    output$meta_select <- renderUI(
        if (input$data == 'Res_fetal_atlas_new_2.rds') {
            list(selectInput('annotation','Metadata:',
                             choices = list('Gene' = 'gene',
                                           'Age' = 'age_week',
                                           'Organ' = 'organ',
                                           'Tissue' = 'tissue',
                                           'Major Cell Type' = 'major_cell_type',
                                           'Cell Type' = 'cell_type'
                                           ),
                             selected = NULL))
        } else {
            list(selectInput('annotation','Metadata:',
                             choices = list('Gene' = 'gene',
                                           'Age' = 'age_week',
                                           'Organ' = 'organ',
                                           'Tissue' = 'tissue',
                                           'Cell Type' = 'cell_type'),
                             selected = NULL))
        }
    )
    
    observeEvent(input$annotation,{
        output$select_gene <- renderUI({
            if (is.null(input$annotation)) {
                list()
            }
            if (input$annotation == 'gene') {
                list(column(width = 2, selectizeInput('gene_selected',
                                                    'Select gene:',
                                                    choices = NULL)))
            } else {
                list(column(width = 2, selectizeInput('highlight_meta','Filter:',
                                                    choices = data_loaded[[input$data]]$meta.data %>%
                                                        distinct_(str_to_sentence(input$annotation)) %>%
                                                        pull(), 
                                                    multiple = TRUE)))
            }
        }) 
    })   
    
    observeEvent(input$annotation,{
        updateSelectizeInput(session = session,
                             inputId = 'gene_selected',
                             choices = data_loaded[[input$data]]$normed.expr %>%
                                 row.names() %>% sort.genes(),
                             server = TRUE)
    })
    
    output$sampling_ui <- renderUI({
        if (isTruthy(data_loaded[[input$data]]$meta.data %>% nrow() > 100000)) {
            list(
                column(width = 1, tags$style(HTML('#sampling {margin-top: -28px}')), 
                       radioGroupButtons('sampling','',
                                         selected = TRUE,
                                         choiceNames = list(icon('running'), icon('hiking')),
                                         choiceValues = list(TRUE,FALSE),
                                         justified = TRUE))
            )
        } else {
            list(
                column(width = 1, tags$style(HTML('#sampling {margin-top: -28px}')), 
                       radioGroupButtons('sampling','',
                                         selected = FALSE,
                                         choiceNames = list(icon('running'), icon('hiking')),
                                         choiceValues = list(TRUE,FALSE),
                                         justified = TRUE))
            )
        }   
    })    
    
    
    observeEvent(input$meta_3d,{
        output$gene_3d <- renderUI({
            if (is.null(input$meta_3d)) {
                list()
            }
            if (input$meta_3d == 'gene') {
                list(column(width = 2, selectizeInput('gene_selected_3d',
                                                    'Select gene:',
                                                    choices = NULL)))
            } else {
                list()
            }
        }) 
    }) 
    
    # render meta data selection for 3d umap 
    observeEvent(input$meta_3d,{
        updateSelectizeInput(session = session,
                             inputId = 'gene_selected_3d',
                             choices = data_loaded[[input$data]]$normed.expr %>% row.names() %>% sort.genes(),
                             server = TRUE)
    })
    
    # |—|— Plot labeling ####
    observe({
        plot_labels <- reactiveValuesToList(plots_meta) %>% map(label_plot)
        if (!is_empty(plot_labels)) {
            output$label_plot1 <- renderText(paste('1',plot_labels[['plot_1']]))
            output$label_plot2 <- renderText(paste('2',plot_labels[['plot_2']]))
            output$label_plot3 <- renderText(paste('3',plot_labels[['plot_3']]))
            output$label_plot4 <- renderText(paste('4',plot_labels[['plot_4']]))
        } else {
            output$label_plot1 <- renderText('1')
            output$label_plot2 <- renderText('2')
            output$label_plot3 <- renderText('3')
            output$label_plot4 <- renderText('4')
        }
        
        
    })
    
    # |—|— Distributions ####
    # render ui for the statistics modal for accompany and comparision mode
    output$stats_ui <- renderUI({
        if (input$display_mode == 'accompany') {
            list(
                column(width = 1, 
                       selectInput('window_dist','Plot:',choices = windows()))
                )
        } else {
            list(
                column(width = 1, 
                       selectInput('window_dist','Plot:',choices = windows())),
                column(width = 1, 
                       selectInput('window_dist_2','Plot:',choices = windows()))
                )
        }
    })    
    
    # |—  Read in data ####
    # set up reactive for datasets 
    data_loaded <- reactiveValues()
    
    # update loaded datasets 
    observeEvent(input$select,{
        withProgress(message = 'Reading data...', value = 1, expr = {
        # get names of loaded datasets
        loaded <- names(data_loaded %>% reactiveValuesToList())
        # if empty, load selected dataset
        if (is_empty(loaded)) {
            data_loaded[[input$data]] <- load_data(input$data)
        # if not empty check whether dataset was loaded before    
        } else {
            # if not loaded before, load the dataset
            if (!(input$data %in% loaded)) {
              data_loaded[[input$data]] <- load_data(input$data) 
              # if loaded before check whether it has been deleted
            } else {
                status <- data_loaded %>% reactiveValuesToList() %>% map(is.null)
                # if it was deleted reload the dataset
                if (status[[input$data]] == TRUE) {
                    data_loaded[[input$data]] <- load_data(input$data)  
                }
            }
        }
        # delete datasets that are not needed anymore
        status <- data_loaded %>% reactiveValuesToList() %>% map(is.null) %>% unlist()
        loaded <- names(status)[!status]
        needed <- c(input$data,data_ins[['needed']])
        remove <- loaded[!(loaded %in% needed)]
        if (!is_empty(remove)) {
            for (i in remove) {
                data_loaded[[i]] <- NULL
            }
        }
        })
    })
    
    
    
    
    # render dataset descriptions
    output$metadata <- renderText(
        if (is.null(data_loaded[[input$data]])) {
            'Load a dataset in order to view a brief summary and see a preview of its UMAP embedding.'
        } else {
            summarize_dataset(data_loaded[[input$data]])
        }
        
    )
    
    # render umap preview
    observeEvent(input$select,{
        output$preview <- renderPlot({
            if (data_loaded[[input$data]] %>% is.null()) {
                list()
            } else {
                plot_umap_preview(data_loaded[[input$data]]$meta.data)
            }
           
        })
    })
    
    
    # handle the rendering of the 3D umap menuitem 
    observeEvent(input$data, {
        output$menu <- renderMenu({
            if (input$data == 'Res_tHIO_fetal_and_adult_duodenum_epi_integrated_with_CSS_new_2.rds') {
                menuSubItem('UMAP-3D', tabName = 'atlas_3d', icon = icon('layer-group'))
            } else {list()}
            
        }) 
    }) 
    
   # create box title for umap menuItem 
    output$dataset <- renderText({
        if (!is.null(input$data)) {
            paste0(datasets %>% filter(file == input$data) %>% pull(name))
        } else {
            paste0('UMAP')
        }
        
    })
    
    
    # |— 2D UMAP ####
    # |—|— Reactives ####
    # set up reactive values for plot meta info and objects
    plots_meta <- reactiveValues()
    
    umaps <- reactiveValues()
    
    # update meta info and plot objects upon adding a setting
    observeEvent(input$add_setting, {
        plots_meta[[paste0('plot_',input$window)]] <- list(annotation = input$annotation,
                                                           gene = input$gene_selected,
                                                           colorscale = input$colorscale)
        withProgress(message = 'Adding plot...', value = 1, expr = {
            if (input$annotation != 'gene') {
                umaps[[paste0('plot_',input$window)]] <- plot_umap(sample_dataset(data_loaded[[input$data]],
                                                                                  performance = input$sampling,
                                                                                  sampling_rate = input$sampling_rate,
                                                                                  gene = NULL),
                                                                   mapping = input$annotation,
                                                                   point.size = input$point_size, 
                                                                   filter = input$highlight_meta)
            } else {
                umaps[[paste0('plot_',input$window)]] <- plot_umap(sample_dataset(data_loaded[[input$data]],
                                                                                  performance = input$sampling,
                                                                                  sampling_rate = input$sampling_rate,
                                                                                  gene = input$gene_selected),
                                                                   mapping = input$annotation,
                                                                   legend.title = input$gene_selected,
                                                                   color_pal = input$colorscale,
                                                                   point.size = input$point_size,
                                                                   filter = NULL)
            }
        })
    })
    
    # |—|—  Plotting ####
    umap_temp <- eventReactive(input$refresh_umap,{
        withProgress(message = 'Updating plots...', value = 1, expr = {
        grid.draw(do.call('ggarrange',
                          c(prepare_umaps(umaps, paste('plot_',windows(), sep = '')),
                            list(nrow = input$nRow))))  
        })
    })
    
    observeEvent(input$refresh_umap, {
        withProgress(message = 'Rendering plots...', value = 1, expr = {
        output$umap_2d <- renderPlot({
            umap_temp()
        })
        })
    })
    
   
    
    # |—|—|— Download ####
    output$download_umap <- downloadHandler(
        filename = function(){paste("umap_",Sys.Date(),'.', input$file_format, sep = '')},
        content = function(file){
            if (input$file_format == 'pdf') {
                ggsave(file, plot = grid.draw(do.call('ggarrange',
                                                      c(prepare_umaps(umaps, windows()),
                                                        list(nrow = input$nRow)))),
                       device = input$file_format,
                       unit = input$unit,
                       width = input$width,
                       height = input$height,
                       dpi = input$resolution,
                       onefile = FALSE
                )
            } else {
                ggsave(file, plot = grid.draw(do.call('ggarrange',
                                                      c(prepare_umaps(umaps, windows()),
                                                        list(nrow = input$nRow)))),
                       device = input$file_format,
                       unit = input$unit,
                       width = input$width,
                       height = input$height,
                       dpi = input$resolution
                )
            }
            
        }
    )
    
    # |— Statistics #### 
    distributions <- eventReactive(input$refresh_distributions,{
        # create histogram for selected input
        withProgress(message = 'Updating plots...', value = 1, expr = {
        df <- umaps[[paste0('plot_',input$window_dist)]]$data 
        meta <- plots_meta[[paste0('plot_',input$window_dist)]]
        if (meta$annotation == 'gene') {
            hist <- df %>% plot_distributions(type = 'histogram',
                                              mapping = 'gene',
                                              gene = meta$gene,
                                              gene.clip = input$gene_clip,
                                              palette = meta$colorscale)
        } else {
            hist <- df %>% plot_distributions(type = 'barplot',
                                              group = meta$annotation %>% str_to_sentence())
        }
        
        # multi grid plot with umap and histograms
        p1 <- ggarrange(umaps[[paste0('plot_',input$window_dist)]],
                        hist,
                        nrow = 1)
        if (input$display_mode == 'accompany') {
            return(p1)
        } else {
            df <- umaps[[paste0('plot_',input$window_dist_2)]]$data 
            meta <- plots_meta[[paste0('plot_',input$window_dist_2)]]
            if (meta$annotation == 'gene') {
                hist_2 <- df %>% plot_distributions(type = 'histogram',
                                                    mapping = 'gene',
                                                    gene = meta$gene,
                                                    gene.clip = input$gene_clip,
                                                    palette = meta$colorscale)
            } else {
                hist_2 <- df %>% plot_distributions(type = 'barplot',
                                                    group = meta$annotation %>% str_to_sentence())
            }
            meta_1 <- plots_meta[[paste0('plot_',input$window_dist)]]$annotation
            meta_2 <- plots_meta[[paste0('plot_',input$window_dist_2)]]$annotation
            p2 <- ggarrange(umaps[[paste0('plot_',input$window_dist)]],
                            hist,
                            umaps[[paste0('plot_',input$window_dist_2)]],
                            hist_2, 
                            ncol = 2)
            if ((meta_1 == 'gene' & meta_2 == 'gene') | (meta_1 != 'gene' & meta_2 != 'gene')) {
                return(p2)
            } else {
                if (input$add_combination == FALSE) {
                    return(p2)
                }
                if (meta_1 == 'gene') {
                    gene_plot <- input$window_dist
                    group_plot <- input$window_dist_2
                } else {
                    gene_plot <- input$window_dist_2
                    group_plot <- input$window_dist
                }
                df <- umaps[[paste0('plot_',gene_plot)]]$data %>%
                    select(cells, gene) %>%
                    left_join(umaps[[paste0('plot_',group_plot)]]$data, by = 'cells')
                meta_gene <- plots_meta[[paste0('plot_',gene_plot)]]
                meta_group <- plots_meta[[paste0('plot_',group_plot)]]
                hist_3 <- df %>% plot_distributions(type = 'violine',
                                                    group = meta_group$annotation %>% str_to_sentence(),
                                                    gene.clip = input$gene_clip,
                                                    gene = meta_gene$gene,
                                                    mapping = 'gene')
                p2 <- ggarrange(umaps[[paste0('plot_',input$window_dist)]],
                                hist,
                                hist_3,
                                umaps[[paste0('plot_',input$window_dist_2)]],
                                hist_2, 
                                ncol = 3)
            }
            return(p2)
        }
        })
    })
    
    # |—|— Plotting ####
    output$distributions_plot <- renderPlot({
        withProgress(message = 'Rendering plots...', value = 1, expr = {
        distributions()
        })
    })
    
    # |—|—|— Download ####
    output$download_dist <-  downloadHandler(
        filename = function(){paste("distributions_",Sys.Date(),'.',input$file_format, sep = '')},
        content = function(file){
            if (input$file_format == 'pdf') {
                ggsave(file, plot = distributions(),
                       device = input$file_format,
                       unit = input$unit,
                       width = input$width,
                       height = input$height,
                       dpi = input$resolution,
                       onefile = FALSE)
            } else {
                ggsave(file, plot = distributions(),
                       device = input$file_format,
                       unit = input$unit,
                       width = input$width,
                       height = input$height,
                       dpi = input$resolution)
            }
           
        }
    )
    
    
    # |— 3D UMAP ####
    observeEvent(input$refresh_umap_3d, {
        if (input$data == 'Res_tHIO_fetal_and_adult_duodenum_epi_integrated_with_CSS_new_2.rds') {
            output$umap_3d <- renderPlotly({
                withProgress(message = 'Updating 3D-plot...', value = 1, expr = {
                    plot_umap_3d(data_loaded[[input$data]], meta = input$meta_3d, gene = input$gene_selected_3d) 
                })
            })
        } 
    })
    
    #_______________________####
    # 3: Interactions ####
    # |—  Read in data ####
    # select interaction matrix
    selected.mat <- reactive({
        if (input$organ_select == 'all_organs') {
            if (input$direction_select == 'mes2epi') {
                readRDS("Res_all_organ_combined_mes2epi_cellphonedb_mean_mat.rds") 
            } else {
                readRDS("Res_all_organ_combined_epi2mes_cellphonedb_mean_mat.rds")
            } 
        } else {
            rds <- readRDS("Res_mean_mat_by_organ.rds")
            rds[[input$organ_select]][[input$direction_select]]
        } 
    })
    # select corresponding color matrix
    combined.col.mat <- reactive({
        if (input$organ_select == 'all_organs') {
            readRDS("Res_all_organ_cell_type_pair_cols.rds")
        } else {
            rds <- readRDS("Res_cell_type_pair_colors_by_organ.rds")
            rds[[input$organ_select]][[input$direction_select]]
        }
    })
    
    
    ## |— Prepare data ####
    # prepare tree and heatmap from interaction matrix
    res <- reactive({
        prepareTreeAndHeatmapInput(expr.mat = selected.mat(),
                                   cor.method = "spearman", hc.method = "ward.D2", 
                                   norm.method = "scale", 
                                   genes.to.highlight = NULL,
                                   column.reorder = FALSE,
                                   row.reorder = TRUE)})
    # create plotly object from heatmap input
    p <- eventReactive(input$update_heatmap,{
        if (input$scale_matrix == FALSE) {
            input.norm <- isolate(res()$heatmap_input)
        } else {
            if (input$scale_by_row == 'row') {
                input.norm <- isolate(res()$heatmap_input %>% scale_by_row())
            } else {
                input.norm <- isolate(res()$heatmap_input %>% scale_by_col())
            }
        }
        if (isTruthy(input$apply_filter == TRUE)) {
            input.norm <- input.norm %>% filter_heatmap(genes = input$filter_genes)
        }
        ct.hc <- reactive(res()$hc_col)
        if (input$organ_select == 'all_organs') {
            selected.col.mat <- reactive(combined.col.mat()[colnames(input.norm),])
            p1 <- heatmaply(normalize(input.norm), 
                            colors = brewer.pal(11,"RdBu") %>% rev(),
                            Colv = as.dendrogram(ct.hc()),
                            Rowv = NULL,
                            ColSideColors = selected.col.mat()[,c("Organ_col","Source_ct_col","Target_ct_col")],
                            showticklabels = c(FALSE,input$apply_filter),
                            key.title = NULL,
                            ylab = 'Receptor-Ligand',
                            xlab = 'Celltypes|Organ', 
                            main = '',
                            hide_colorbar = 1, 
                            source = 'p',
                            dend_hoverinfo = FALSE,
                            margins = c(50,50,50,50)
            )
            p1$x$layout$annotations[[1]]$text <- ''
            p1$x$layout$yaxis2$ticktext <- c('Organ','Source','Target')
            remove_labels(p1) %>% layout(
                xaxis = list(titlefont = list(size = 15)), 
                yaxis3 = list(titlefont = list(size = 15)),
                yaxis2 = list(tickfont = list(size = 15)))
           
        } else {
            col.mat <- combined.col.mat()
            rownames(col.mat) <- col.mat[,1]
            p1 <- heatmaply(normalize(input.norm), 
                            colors = brewer.pal(11,"RdBu") %>% rev(),
                            Colv = as.dendrogram(ct.hc()),
                            Rowv = NULL,
                            ColSideColors = col.mat[,c(2,3)],
                            showticklabels = c(FALSE,input$apply_filter),
                            key.title = NULL,
                            ylab = 'Receptor-Ligand',
                            xlab = 'Celltypes',
                            main = '',
                            hide_colorbar = 1, 
                            source = 'p',
                            dend_hoverinfo = FALSE,
                            margins = c(50,50,50,50)
            )
            p1$x$layout$annotations[[1]]$text <- ''
            p1$x$layout$yaxis2$ticktext <- c('Source','Target')
            remove_labels(p1) %>% layout(
                xaxis = list(titlefont = list(size = 15)), 
                yaxis3 = list(titlefont = list(size = 15)),
                yaxis2 = list(tickfont = list(size = 15)))  
        }
    })
    
    ## |— Plotly Heatmap ####
    observeEvent(input$update_heatmap,{
        output$heatmap <- renderPlotly({
            withProgress(message = 'Updating Heatmap...', value = 1, expr = {
                p() %>% event_register('plotly_click')
            }) 
        })
    })
    
    
    ## |—|— Plotly Event Data ####
    # extract data from plotly click event and relink to interaction dataset
    df_clicked <- reactive({
        clicked <- event_data('plotly_click')
        if (is.null(clicked)) {
            tibble(Interaction = c('Ligand','Receptor'),
                   Gene = c(' ',' '),
                   `Cell type` = c(' ',' '),
                   Organ = c(' ',' '))
        } else {
            list <- list(receptor_ligand = p()$x$layout$yaxis3$ticktext[clicked %>% pull(y)],
                         tissue = p()$x$layout$xaxis$ticktext[clicked %>% pull(x)]) 
            row_info <- list[[1]]
            column_info <- list[[2]] %>% str_split('\\|') %>% unlist()
            df <- row_info %>%
                str_split('_') %>%
                unlist() %>% as_tibble() %>%
                separate(value, c('interaction','gene'), sep = ':') %>%
                mutate(`cell type` = column_info[c(1,2)], 
                       organ = ifelse(input$organ_select == 'all_organs', column_info[3], input$organ_select),
                       interaction = ifelse(interaction == 'L', 'Ligand', 'Receptor'))
            colnames(df) <- str_to_sentence(colnames(df))
            df
        }
    })
    
    
    # |—|— Render Interaction ####
    output$heatmap_genes <- renderUI({
        list(
            checkboxInput('apply_filter','Filter Genes', value = FALSE),
            selectizeInput('filter_genes','Select Genes:',
                           choices = distinct_genes(res()$heatmap_input), multiple = TRUE)
        )
    })
    
    
    
    
    
    output$click <- renderTable({
        df_clicked() 
    })
    
    
    # |—|— Store Interactions ####
    # initialize heatmap browser interactions reactive
    selected_interactions <- reactiveValues(`0` = tibble(Interaction = c('Ligand','Receptor'),
                                                         Gene = c(' ',' '),
                                                         `Cell type` = c(' ',' '),
                                                         Organ = c(' ',' '),
                                                         Source = c('Browser','Browser')))
    # initialize user interactions reactive 
    user_interactions <- reactiveValues(`0` = tibble(Interaction = c('Ligand','Receptor'),
                                                   Gene = c(' ',' '),
                                                   `Cell type` = c(' ',' '),
                                                   Organ = c(' ',' '),
                                                   Source = c('User','User')))
    
    # update reactive for manual user selection of interaction pair
    observeEvent(input$add_user_pair, {
        user_interactions[[paste0('interaction_',input$add_user_pair)]] <- tibble(Interaction = c('Ligand','Receptor'),
                                                                                  Gene = c(input$user_ligand, input$user_receptor),
                                                                                  `Cell type` = c(NA,NA),
                                                                                  Organ = c(NA,NA),
                                                                                  Source = c('User','User'))
    })
    
    
    # update reactive from heatmap selection 
    observeEvent(input$add_interaction, {
        selected_interactions[[paste0('interaction_',input$add_interaction)]] <- df_clicked() %>% mutate(Source = 'Browser')
    })
    
    
    # merge manual selected interactions and the ones that come from the heatmap
    df_interactions <- reactive({
        manual <- bind_rows(reactiveValuesToList(user_interactions), .id = 'id') %>%
            mutate(id = as.numeric(gsub('interaction_','',id))) %>%
            rename(ID = id)
        browser <- bind_rows(reactiveValuesToList(selected_interactions), .id = 'id') %>%
            mutate(id = as.numeric(gsub('interaction_','',id))) %>%
            rename(ID = id)
        return(bind_rows(browser,manual) %>% filter(Gene != ' ') %>%
                   mutate(ID = group_indices(.,Source,ID)) %>% arrange(Source,ID))
    })
    
    # render selected interactions table 
    output$interactions_list <- renderDT({
        df_interactions() 
    })
    #_______________________####
    ## 4: Inspector ####
    
    ### |— Render UI ####
    
    # |—|— Window selection ####
    inspectors <- reactive({
        c(1,2,3,4)[c(input$ins1,input$ins2,input$ins3,input$ins4)]
    })
    
    # |—|— Meta selection ####
    interaction_list <- reactive({
        df_interactions() %>% select(ID, Interaction, Gene, Source) %>%
            pivot_wider(names_from = Interaction, values_from = Gene) %>%
            unite(pair, c("Ligand","Receptor"),sep = '-') %>% select(-Source) %>%
            mutate(id_new = group_indices(.,pair)) %>% distinct(pair, id_new, .keep_all = 1) %>% arrange(pair) %>%
            select(pair, ID) %>% deframe() %>% as.list()
    })
    
    
    # ui for selecting inspector, interaction pair and dataset
    output$select_inspector <- renderUI({
        list(column(width = 1, selectInput('inspector','Plot:',choices = inspectors())),
             column(width = 2, selectInput('pair_select', 'Interaction:', choices = interaction_list()))
             
        )
    })
    
    
    # selective metadata selcetion interface based on selected dataset
    output$meta_inspect <- renderUI({
        if (isTruthy(input$inspector_data == 'Res_fetal_atlas_new_2.rds')) {
            list(selectInput('annotation_inspect','Metadata:',
                             choices = list('Ligand-Receptor' = 'bivariate',
                                           'Ligand' = 'ligand',
                                           'Receptor' = 'receptor',
                                           'Age' = 'age_week',
                                           'Organ' = 'organ',
                                           'Tissue' = 'tissue',
                                           'Major Cell Type' = 'major_cell_type',
                                           'Cell Type' = 'cell_type'),
                             selected = 'bivariate'))
        } else {
            list(selectInput('annotation_inspect','Metadata:',
                             choices = list('Ligand-Receptor' = 'bivariate',
                                           'Ligand' = 'ligand',
                                           'Receptor' = 'receptor',
                                           'Age' = 'age_week',
                                           'Organ' = 'organ',
                                           'Tissue' = 'tissue',
                                           'Cell Type' = 'cell_type'),
                             selected = 'bivariate'))
        }
    })
    
    # create selectizeInput for genes at the server side which is much faster when dealing with many entries
    observe({
        updateSelectizeInput(session = session, inputId = 'user_receptor',
                             choices = inspector_genes(), server = TRUE)
        updateSelectizeInput(session = session, inputId = 'user_ligand',
                             choices = inspector_genes(), server = TRUE)
    })
    
    
    ## |—|— Plot labeling ####
    observe({
        inspector_labels <- reactiveValuesToList(inspectors_meta) %>% map(label_inspector)
        if (!is_empty(inspector_labels)) {
            output$label_ins1 <- renderText(paste('1',inspector_labels[['inspector_1']]))
            output$label_ins2 <- renderText(paste('2',inspector_labels[['inspector_2']]))
            output$label_ins3 <- renderText(paste('3',inspector_labels[['inspector_3']]))
            output$label_ins4 <- renderText(paste('4',inspector_labels[['inspector_4']]))
        } else {
            output$label_ins1 <- renderText('1')
            output$label_ins2 <- renderText('2')
            output$label_ins3 <- renderText('3')
            output$label_ins4 <- renderText('4')
        }
    })
    
    # |—|— Distributions ####
    
    output$stats_ui_ins <- renderUI({
            list(
                column(width = 1, 
                       selectInput('inspector_dist','Plot:',
                                   choices = inspectors())),
                column(width = 2,
                       selectInput('dist_type_ins','Group:',
                                   choices = list(All = 'All',
                                                  Organ = 'Organ',
                                                  Tissue = 'Tissue',
                                                  `Cell type` = 'Cell_type'))))
    })    
    
    output$stats_ui_filter <- renderUI({
        if (isTruthy(input$dist_type_ins != 'All')) {
            list(column(width = 3,
                        selectizeInput('filter_group','Filter for:',
                                       choices = inspector_plots[[paste0('inspector_',input$inspector_dist)]]$data %>%
                                           distinct_(input$dist_type_ins) %>% pull(),
                                       multiple = TRUE)),
                 column(width = 1,
                        numericInput('rows_facet','Rows:',
                                     value = 3,
                                     min = 1,
                                     max = 3,
                                     step = 1)))
        } else {
            list()
        }
    })
    
    inspector_genes <- reactive({
        all_genes %>% sort.genes()
    })
    
    ## |— Reactives ####
    # initialize inspector metadata list
    inspectors_meta <- reactiveValues()
    # initialize inspector plots
    inspector_plots <- reactiveValues()
    # initialize datalink
    data_ins <- reactiveValues(needed = NULL)
    # update meta data and plot lists upon selection 
    observeEvent(input$add_inspector, {
        
        inspectors_meta[[paste0('inspector_',input$inspector)]] <- list(atlas = input$inspector_data,
                                                                        annotation = input$annotation_inspect,
                                                                        pair = df_interactions() %>% filter(ID == input$pair_select),
                                                                        colorscale = input$colorscale_ins)
        data_ins[['needed']] <- inspectors_meta %>% reactiveValuesToList() %>% map(`[`,'atlas') %>% unlist() %>% unique()
        # load datasets
        withProgress(message = 'Updating loaded datasets...', value = 1, expr = {
        # get names of loaded datasets
        loaded <- names(data_loaded %>% reactiveValuesToList())
        # if empty, load selected dataset
        if (is_empty(loaded)) {
            data_loaded[[input$inspector_data]] <- load_data(input$inspector_data)
            # if not empty check whether dataset was loaded before    
        } else {
            # if not loaded before, load the dataset
            if (!(input$inspector_data %in% loaded)) {
                data_loaded[[input$inspector_data]] <- load_data(input$inspector_data) 
                # if loaded before check whether it has been deleted
            } else {
                status <- data_loaded %>% reactiveValuesToList() %>% map(is.null)
                # if it was deleted reload the dataset
                if (status[[input$inspector_data]] == TRUE) {
                    data_loaded[[input$inspector_data]] <- load_data(input$inspector_data)  
                }
            }
        }
        
        # delete datasets that are not needed anymore
        status <- data_loaded %>% reactiveValuesToList() %>% map(is.null) %>% unlist()
        loaded <- names(status)[!status]
        needed <- c(input$data,data_ins[['needed']])
        remove <- loaded[!(loaded %in% needed)]
        if (!is_empty(remove)) {
            for (i in remove) {
                data_loaded[[i]] <- NULL
            }
        }
        })
        # update plot object of selected inspector with control structures for bivariate, gene and other annotations
         withProgress(message = 'Adding plot...', value = 1, expr = {
            title <- datasets %>% filter(file == input$inspector_data) %>% pull(name) 
            if (input$annotation_inspect != 'bivariate') {
                if (input$annotation_inspect %in% c('ligand','receptor')) {
                    gene_selected <- df_interactions() %>%
                        filter(ID == input$pair_select) %>%
                        filter(Interaction == str_to_sentence(input$annotation_inspect)) %>% pull(Gene)
                    inspector_plots[[paste0('inspector_',input$inspector)]] <- plot_umap(sample_dataset(data_loaded[[input$inspector_data]],
                                                                                                        performance = input$sampling_inspector,
                                                                                                        sampling_rate = input$sampling_rate_inspector,
                                                                                                        gene = gene_selected),
                                                                                         mapping = 'gene',
                                                                                         legend.title = gene_selected,
                                                                                         main = title,
                                                                                         point.size = input$point_size_ins,
                                                                                         color_pal = input$colorscale_ins)
                } else {
                    inspector_plots[[paste0('inspector_',input$inspector)]] <- plot_umap(sample_dataset(data_loaded[[input$inspector_data]],
                                                                                                        performance = input$sampling_inspector,
                                                                                                        sampling_rate = input$sampling_rate_inspector,
                                                                                                        gene = NULL),
                                                                                         mapping = input$annotation_inspect,
                                                                                         main = title,
                                                                                         point.size = input$point_size_ins,
                                                                                         color_pal = input$colorscale_ins)
                }
            } else {
                genes_selected <- df_interactions() %>%
                    filter(ID == input$pair_select) %>% select(Interaction,Gene) %>% deframe()
                inspector_plots[[paste0('inspector_',input$inspector)]] <- plot_umap(sample_dataset_bivariate(data_loaded[[input$inspector_data]],
                                                                                                              performance = input$sampling_inspector,
                                                                                                              sampling_rate = input$sampling_rate_inspector,
                                                                                                              genes = genes_selected),
                                                                                    mapping = input$annotation_inspect,
                                                                                    main = title,
                                                                                    point.size = input$point_size_ins,
                                                                                    bivariate_pal = bivariate_pal())
            }
        })
        
    })
    
    
    ## |— Plotting ####
    # create plot grid from reactive plots selected 
    inspectors_temp <- eventReactive(input$update_inspector,{
        withProgress(message = 'Rendering plots...', value = 1, expr = {
        
        grid.draw(do.call('ggarrange', c(prepare_umaps(inspector_plots,paste('inspector_',inspectors(), sep = '')),
                                         nrow = input$nrow_inspect,
                                         draw = FALSE)))
        })
    })
    
    # render inspectors
    observeEvent(input$update_inspector, {
        output$inspections <- renderPlot({
            inspectors_temp()
        })
    })
    
    # generate bivariate scale
    bivariate_pal <- reactive({
        set_bivariate_pal(input$bivariate_scale)
    })
    
    
    observeEvent(input$update_inspector, {
        output$bivariate_legend <- renderPlot({
            
                plot_legend(bivariate_pal())
            
            
        })
    })
    
    output$inspector_plotting <- renderUI({
        if (inspectors_meta %>% reactiveValuesToList() %>% map(`[`,'annotation') %>% unlist() %>% str_detect('bivariate') %>% any()) {
            list(column(width = 1, 
                        plotOutput('bivariate_legend', height = '10vh')),
                 column(width = 11,
                        plotOutput('inspections', height = '65vh')))
        } else {
            list(list(column(width = 11,
                             plotOutput('inspections', height = '65vh'))))
        }
    })
    
    
    
    ## |—|— Download ####
    output$download_inspector <- downloadHandler(
        filename = function(){paste("inspector_",Sys.Date(),'.',input$file_format, sep = '')},
        content = function(file){
            if (input$file_format_ins == 'pdf') {
                ggsave(file,
                       plot = grid.draw(do.call('ggarrange',
                                                c(prepare_umaps(inspector_plots,
                                                                inspectors()),
                                                  nrow = input$nrow_inspect, draw = FALSE))),
                       device = input$file_format_ins,
                       unit = input$unit_ins,
                       width = input$width_ins,
                       height = input$height_ins,
                       dpi = input$resolution_ins,
                       onefile = FALSE)
            } else {
                ggsave(file,
                       plot = grid.draw(do.call('ggarrange',
                                                c(prepare_umaps(inspector_plots,
                                                                inspectors()),
                                                  nrow = input$nrow_inspect, draw = FALSE))),
                       device = input$file_format_ins,
                       unit = input$unit_ins,
                       width = input$width_ins,
                       height = input$height_ins,
                       dpi = input$resolution_ins)
            }
            
        }
    )
    
    
    # |— Statistics ####
    
    # |—|— Plotting ####
    
    distributions_ins <- eventReactive(input$refresh_distribution_ins,{
        withProgress(message = 'Updating plots...', value = 1, expr = {
        # get meta information of selected plot
        meta <- inspectors_meta[[paste0('inspector_',input$inspector_dist)]]
        if (is.null(meta)) {
            return(list())
        }
        # create umap for grouping
        if (input$dist_type_ins != 'All') {
            df_group <- inspector_plots[[paste0('inspector_',input$inspector_dist)]]$data
            p2 <- df_group %>% 
                  plot_umap(mapping = input$dist_type_ins %>% str_to_lower(),
                          main = datasets %>% filter(file == meta$atlas) %>% pull(name),
                          point.size = input$point_size_ins,
                          filter = input$filter_group)
        }
        # create histogram for bivariates
        if (isTruthy(meta$annotation == 'bivariate')) {
            browser()
            genes_selected <- inspectors_meta[[paste0('inspector_',input$inspector_dist)]]$pair %>% pull(Gene)
            # create histogram for selected input
            hist <- inspector_plots[[paste0('inspector_',input$inspector_dist)]]$data %>%
                bivariate_distribution(group = input$dist_type_ins,
                                       gene.clip = input$gene_clip_ins,
                                       scale = input$bivariate_scale,
                                       rows = input$rows_facet,
                                       filter = input$filter_group, 
                                       gene = genes_selected)
        }
        # create histogram for genes
        if (isTruthy(meta$annotation %in% c('ligand','receptor'))) {
            gene <- meta$pair %>% filter(Interaction == str_to_sentence(meta$annotation)) %>% pull(Gene)
            hist <- inspector_plots[[paste0('inspector_',input$inspector_dist)]]$data  %>%
                plot_distributions(type = 'histogram',
                                   mapping = 'gene',
                                   gene = gene,
                                   gene.clip = input$gene_clip_ins,
                                   palette = meta$colorscale)
            # add group facetting
            if (input$dist_type_ins != 'All') {
                hist <- hist + facet_wrap(input$dist_type_ins)
            }
        } 
        # create barplot when discrete umap mapping was applied
        if (!(meta$annotation %in% c('ligand','receptor','bivariate'))) {
            hist <- inspector_plots[[paste0('inspector_',input$inspector_dist)]]$data %>%
                plot_distributions(type = 'barplot',
                                   group = meta$annotation %>% str_to_sentence())
        }
        # multi grid plot with umap and histograms
        if (input$dist_type_ins == 'All' | meta$annotation %in% c('organ','tissue','cell_type','age')) {
            p1 <- ggarrange(inspector_plots[[paste0('inspector_',input$inspector_dist)]],
                            hist,
                            nrow = 1)
            return(p1)
        # add grouping umap 
        } else {
            p1 <- ggarrange(inspector_plots[[paste0('inspector_',input$inspector_dist)]],
                            hist,
                            p2,
                            nrow = 1)
            return(p1) 
        }
        })
    })
    
    
    output$distributions_plot_ins <- renderPlot({
        withProgress(message = 'Rendering plots...', value = 1, expr = {
        distributions_ins()
        })
    })
    
    output$bivariate_legend_stats <- renderPlot({
        plot_legend(bivariate_pal())
    })
    
    
    showlegend <- reactive(reactiveValuesToList(inspectors_meta)[[paste0('inspector_',input$inspector_dist)]]$annotation == 'bivariate')
    
    observeEvent(input$refresh_distribution_ins,{
        output$distributions_plots <- renderUI({
            if (isolate(showlegend())) {
                list(column(width = 1, plotOutput('bivariate_legend_stats', height = '10vh')),
                     column(width = 11, plotOutput('distributions_plot_ins',height = '65vh')))
            } else {
                list(column(width = 12, plotOutput('distributions_plot_ins',height = '65vh')))
            }
        })
    })
   
    
    # |—|— Download ####
    output$download_dist_ins <- downloadHandler(
        filename = function(){paste("distributions_inspector_", Sys.Date(), '.', input$file_format, sep = '')},
        content = function(file){
            if (input$file_format_ins == 'pdf') {
                ggsave(file, plot = distributions_ins(),
                       device = input$file_format_ins,
                       unit = input$unit_ins,
                       width = input$width_ins,
                       height = input$height_ins,
                       dpi = input$resolution_ins,
                       onefile = FALSE)
            } else {
                ggsave(file, plot = distributions_ins(),
                       device = input$file_format_ins,
                       unit = input$unit_ins,
                       width = input$width_ins,
                       height = input$height_ins,
                       dpi = input$resolution_ins)
            }
            
        }
    )
    
}


#_______________________####
# Run App ####
shinyApp(ui, server)
