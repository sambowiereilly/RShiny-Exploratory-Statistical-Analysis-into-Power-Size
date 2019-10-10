
# /*
#  *  ShinyUI element
#  */

# set working dircetory to parent project folder
# helps makes folder paths below smaller and easier 
setwd( '..' )

# set seed to ensure results are the same
set.seed( '007' )

# load global r files to support analysis
source( 'rScripts/global_Libraries.R' )
source( 'rScripts/global_Variables.R' )
source( 'rScripts/global_SimulationFunctions.R' )
source( 'rScripts/global_SupportingFunctions.R' )

shinyUI( 
  navbarPage(
    title = 'Statistical Power & Size Interactive UI', 
    id = 'STabs',
    # Home Page
    tabPanel( 'Home', 
              h4( 'Welcome. '),
              br(),
              h4( 'This application was created to enable an interactive, exploratory statistical analysis into Power & Size.'),
              br(),
              h4( 'Following the tabs at the top of the screen will take you through a journey; from selecting a given data set of interest, fine tuning your simulation attributes, to finally visualising differences in either Power or Size for both a parametric and non-parametric test.'),
              br(),
              h4( 'At this stage, this application only accepts a single X covariate and Y response variable.' ),
              br(),
              h4( 'For any X covariate with 2 factor levels, T-Test and Mann-Whitney U Test are performed. If 3 or more factor levels exist, Analysis of Variance (ANOVA) and Kruskal-Wallis Rank Sum tests are performed instead (parametric and non-parametric tests respectively).'),
              br(),
              h4( 'Process flow maps visualising how this application is designed and example scenario walkthroughs are available in the Documentation tab at the top right of the application.' ),
              #br(),
              #h4( 'If you have any questions please feel free to contact me at bowieInsights@gmail.com'),
              # add line above once project submitted and marked. 
              br(),
              h4( 'Cheers, S') ),
    # Load Data Page                
    tabPanel( 'Load Data',              
              sidebarLayout(
                sidebarPanel(
                  # select input with the list of datasets
                  selectInput(inputId = 'start', 
                              label = 'Please select data set:', 
                              choices = c( 'Movies (custom)', 'iris','movies', 'Heights' ) ),
                  br(),
                  helpText( 'Once data set is selected, all factor and character variables are screened to ensure only valid X covariate options are returned.' ),
                  helpText( '(i.e. X covariate contains at least 2 factor levels).' ),
                  uiOutput( 'vx' ), 
                  br(),
                  helpText( 'Following a similar approach, only numeric variable options are provided below.' ),
                  uiOutput( 'vy' ), 
                  br(),
                  actionButton( 'updateData','Upload Data' ) ),
                mainPanel( 
                  h4( 'So lets make a start. '),
                  br(),
                  h4( 'Please use the drop down features to your left hand side to select an example data set along with X covariate and Y response variable accordingly.' ),
                  br(),
                  h4( 'Once you are happy, click the Upload Data to progress to the next stage.' )#,
                  #helpText( 'Quote: No matter your Size or Power, twas only one ring that could rule them all.' ) 
                  ) ) ),
    
    # Simulation Set-Up Page
    tabPanel( 'Simulation Set-Up',               
              sidebarLayout(
                sidebarPanel(
                  # select input with the list of datasets
                  selectInput(inputId = 'data1', 
                              label = 'Please select Simulation Scenario Type to perform:', 
                              choices = dd_Options ),
                  br(),
                  helpText( 'Dynamically populated based selection above' ),
                  br(),
                  uiOutput( outputId = 'test'), 
                  br(),
                  selectInput( inputId = 'alpha',
                               label = 'Select Alpha level:',
                               choices = admin_Alpha,
                               selected = admin_Alpha0 ),
                  br(),
                  selectInput( inputId = 'positiveValuesOnly',
                               label = 'All simulated data values must be greater or equal to 0:',
                               choices = admin_PositiveOnly,
                               selected = admin_PositiveOnly0 ),
                  br()
                ),
                mainPanel(
                  h4( 'Editable data set attributes.' ),
                  h5( 'To make any changes, simply click into the cell you wish to change, type new value and then press update button below.' ),
                  hr(),
                  column( 10,
                          helpText( 'Editable Table' ),
                          rHandsontableOutput( 'table' ),
                          br(),
                          actionButton( 'updateBtn', 'Click, to save any edited changes' ) ),
                  br(), br(), br(), br(), br(), br(),
                  br(), br(), br(), br(), br(),
                  h4( 'Latest data set values.' ),
                  h5( 'The values in addition the selections made with the drop downs to the left hand side will be used to perform statistical analysis simulations of either Power or Size.' ),
                  h5( 'If you would like to update these values, simply follow notes at the top of this page.' ),
                  hr(),
                  column(5,
                         helpText( 'Non-Editable Table' ),
                         tableOutput( 'data' ),
                         actionButton( 'performSimBtn', 'Perform Simulation' ) ),
                  br(), br(), br(), br(), br(), br(),
                  br(), br(), br(), br(), br(), br(), br(),
                  h5( 'Please note, simulations can take up to 2 minutes to complete depending on number of simulations/sample size selected.' ),
                  h5( 'Once simulation has completed, the application will automatically move to the Simulation Visualisation tab to showcase results.' ),
                  br() ) ) ),
    
    # Visualisation output
    tabPanel( 'Simulation Visualisation',
              fluidPage(
                h4( 'Simulation Details.' ),
                textOutput( 'SimulationDetails_1' ),
                textOutput( 'SimulationDetails_2' ),
                hr(),
                h4( 'Visualition of Results - Plot' ),
                plotOutput( 'plot' ),
                downloadButton( outputId = 'downPlot', label = 'Click, to save Plot as an image.'),
                hr(),
                h4( 'Visualition of Results - Table' ),
                tableOutput( 'visual_Table' ),
                downloadButton('downTable', 'Click, to save Table as an image.' ),
                br() ) )
    
    # # Use navbarmenu to get the tab with menu capabilities
    ,navbarMenu('Documentation',
                tabPanel('Process Flow - Full Statistical Power & Size Interactive UI', tags$img( src = 'Process Flow - Full Shiny Application.png' ) ),
                tabPanel('Process Flow - Monte Carlo Simulation Only', fluidPage( tags$img( src = 'Process Flow - Simulation Only.png' ) ) ),
                tabPanel('Scenario - Example 1 (Power)', tags$img( src = 'How to guide - Scenario 1.png' ) ),
                tabPanel('Scenario - Example 2 (Size)', tags$img( src = 'How to guide - Scenario 2.png' ) ),
                tabPanel('Scenario - Example 3 (Power)', tags$img( src = 'How to guide - Scenario 3.png' ) ),
                tabPanel('Scenario - Example 4 (Size)', tags$img( src = 'How to guide - Scenario 4.png' )  )
    ) ) )
