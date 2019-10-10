
# /*
#  *  ShinyServer element
#  */

# /*
#  *  High level overview of server design (hopefully makes the actual code easier to interpret)
#  *  Initialise key data objects ( base, engine and final results)
#  *  'Load Data Tab Elements'
#  *    - Generate available X covariates and Y response options for user
#  *    - Update data event (button)
#  *  'Simulation Set Up Elements'
#  *    - dynamically generate user drop down options based on Scerario selected
#  *    - allow user to edit data attributes using 'rhandsontable' package
#  *    - Run simulation event (button)
#  *  'Simulation Visualisation Elements'
#  *    - generate bar chart
#  *    - generate tabular version
#  *    - Save chart or table event (buttons)
#  */


# beginning of the shiny server fu-nction
shinyServer(
  function( output, input, session ) ( {
    
    # initialise main reactive values to be used through the code
    # 3 objects below are used for:
    # data_base = raw copy of the data user selected
    # data_engine = local memory copy which can be updated following user ammendments/edits
    # data_FinalOutput = results from simulation on data_engine object, used for visualisations (plot and table)
    data_base <- reactiveValues( data = fun.base_simData( simData = initial_Figures, 
                                                          ID = 2, 
                                                          sampleSize = admin_Sample_Default ) ) 
    data_engine <- reactiveValues( data = fun.base_simData( simData = initial_Figures, 
                                                            ID = 2, 
                                                            sampleSize = admin_Sample_Default ) ) 
    data_FinalOutput <- reactiveValues( data = 'initialise only' )
    
    # /*
    #  *  'Load Data Tab Elements'
    #  */
    
    # return main base data set to use within the program
    data_initialise <- reactive( {  core_Data <- switch(  input$start, 'iris' = iris,
                                                'movies' = dslabs::movielens,
                                                'Movies (custom)' = customData_Movies,
                                                'Heights' = dslabs::heights )
                          return( core_Data ) } )
    
    # return base X Covariate options to the user  
    x_covOps <- reactive( {  core_Data <- data_initialise()
                      x_Options <- names( core_Data[sapply( core_Data, class ) 
                                  %in% c( 'factor', 'character' ) ] )
                          return( x_Options ) } )
    
    # return final valid x Covariate options to the user 
    # (i.e. where covariate contains atleast 2 factor levels)
    x_covFinalOps <- reactive( {  core_Data <- data_initialise()
    x_Options <- x_covOps()
    x_ValidOptions <- x_Options[ sapply( 1:length( x_Options ),
                                         fun.x_ValidateOptions, 
                                         x_Options , core_Data ) ]
    
    return( x_ValidOptions ) } )
    
    # return valid y Response variables (i.e. all none factor or character varaibles)
    y_respFinalOps <- reactive( {  core_Data <- data_initialise()
    x_Options <- x_covOps()
    y_Options <- setdiff( names( core_Data ), x_Options )
    
    return( y_Options ) } )
    
    # base data Attributes values from selected data set by user
    baseAttributes <- reactive( { 
      
      core_Data <- data_initialise()
      x_DropDown <- input$variablex
      y_DropDown <- input$variabley
      pop_Data <- core_Data %>%
                  select( x_DropDown, y_DropDown ) %>%
                  rename( y_Response = y_DropDown, x_Covariate = x_DropDown )
      
      pop_RowSize <- nrow( pop_Data )
      
      initial_Figures <-  pop_Data %>%
                          group_by( x_Covariate ) %>%
                          summarise( observations = n(), 
                                     proportion = observations / pop_RowSize, 
                                     mean = mean( y_Response ), 
                                     sd = sd( y_Response ) )
      
      return( initial_Figures ) } )
    
    # output available X covariates to the user in the form of drop down   
    output$vx <- renderUI( {  selectInput(  'variablex', 
                                            'Please select X covariate:', 
                                            choices = x_covFinalOps()) } )
    
    # output available Y response to the user in the form of drop down   
    output$vy <- renderUI( {  selectInput(  'variabley', 
                                            'Please select Y response variable:', 
                                            choices = y_respFinalOps() ) } )
    
    # check for event where user wants to update data (i.e. "push the button")
    observeEvent( input$updateData , 
                  { data_base$data <- fun.base_simData(
                    simData = baseAttributes(), 
                    ID = 2, 
                    sampleSize = admin_Sample_Default )
                  
                  data_engine$data <- fun.base_simData(
                    simData = baseAttributes(), 
                    ID = 2, 
                    sampleSize = admin_Sample_Default )
                  updateTabsetPanel( session, 'STabs', 
                                     selected = 'Simulation Set-Up') } )
    
    # /*
    #  *  'Simulation Set Up Elements'
    #  */
    
    # The following reactive function would return the column variable names 
    # corresponding to the dataset selected by the user.
    user_ID <- reactive( {  userSelection_ID <- dd_Table[ dd_Table[,2] == input$data1, 1 ]
    return( userSelection_ID ) } )
    
    
    dyn_SelectInput_One <- reactive( { return( dd_SelectInput_One[ user_ID() ] ) } )
    
    
    output$test <- renderUI( {  input$data1
      isolate( 
        num <- lapply( 1:length(admin_DynVector), 
                       function( i ) { 
                         # function that dynamically checks if a second drop down is 
                         # required or not for the UI
                         if ( !anyNA(testTable[user_ID(),i]) ) {     
                           textInput( inputId = paste0( 'test', i ), 
                                      testTable[ user_ID(), i ], 
                                      value = admin_DynValue[ i ] ) }
                       } ) ) } )
    
    # output rhandsontable - allowing user to edit the data on the fly how they wish
    # (within reason that is...)
    output$table <- renderRHandsontable( { rhandsontable( 
      # only show values we want the user to be able to update (:
      data_base$data[ , dd_UserDynamicAttributes[[ user_ID() ]] ] ) } )
    
    
    num_Sims <- reactive( {  num_Simulations <- ifelse( user_ID() == 1, input$test1, input$test2 )
    return( num_Simulations ) } )
    
    
    # when the user would like to push new values to the system, handle accordingly
    observeEvent( input$updateBtn,  
                  {   data_base$data[ , 
          dd_UserDynamicAttributes[[ user_ID() ]] ] <- hot_to_r( input$table )
                  
                  data_engine$data <- fun.base_simData(
                    simData = data_base$data, 
                    ID = as.numeric( user_ID() ), 
                    input$test1 ) } )
    
    # show final results to user - these cannot be edited and will be 
    # used to perform required simulations
    output$data <- renderTable( { data_engine$data[, dd_RevisedAttributes[[ user_ID() ]] ] } )
    
    # FINALLY... let's perform some simulations
    observeEvent( input$performSimBtn,   
                  { data_FinalOutput$data <- fun.fullSim( data_engine$data, user_ID(), 
                                                   as.numeric( input$alpha ),
                                                   dd_Vectors, num_Sims(), input$positiveValuesOnly )
                  
                  updateTabsetPanel( session, 'STabs', selected = 'Simulation Visualisation' ) } )
    
    
    # /*
    #  *  'Simulation Visualisation Elements'
    #  */
    
    output$SimulationDetails_1 <- renderText({ 
      dd_VectorDescriptions[user_ID()]
    })
    
    output$SimulationDetails_2 <- renderText({ 
      dd_VectorFriendlyDescriptions[user_ID()]
    })
    
    # to conclude, update visual element to show results from parametric 
    # and non-parameteric tests
    output$plot <- renderPlot({
      
      powerTrue <- ifelse( fun.findVariance( data_base$data ) == 0, FALSE, TRUE )
      if( powerTrue ){
        ylineintercept <- 0.8
        resultType <- 'Power'
        y_max <- 1
        
      } else {
        ylineintercept <- as.numeric( input$alpha )
        resultType <- 'Size'
        y_max <- round( max( data_FinalOutput$data[,3] ), digits = 2 ) + 0.02
      }
      chartTitle <- paste( 'Statistical', resultType, 'simulated results for scenario', input$data1,sep = ' ') 
      
      ggplot( data = data_FinalOutput$data ,
              aes(x = useCase, y = testResult, fill = testName ) ) +
        geom_bar( stat = 'summary', fun.y = 'mean', position = 'dodge' ) + 
        labs( title = chartTitle, x = 'Simulated Use-Cases', 
              y = paste( resultType, 'Result', sep = ' ' ), 
              fill = 'Colour key: ' ) +
        geom_hline( yintercept = ylineintercept, 
                    linetype = 'dashed', color = 'darkblue' ) +
        ylim( 0, y_max ) +
        scale_x_continuous( breaks = function(x) unique( floor( pretty( seq( 0, ( max(x) + 1 ) * 1.1))))) +
        theme( plot.title = element_text( size = 16 ), legend.position = 'bottom' )
    } ) 
    
    # save plot
    output$downPlot <-  downloadHandler( filename = function() { paste( 'plot', 'png', sep='.') },
                                         content = function(file){
   powerTrue <- ifelse( fun.findVariance( data_base$data ) == 0, FALSE, TRUE )
   if( powerTrue ) {
     ylineintercept <- 0.8
     resultType <- 'Power'
      y_max <- 1
    } else {
      ylineintercept <- as.numeric( input$alpha )
      resultType <- 'Size'
      y_max <- round( max( data_FinalOutput$data[,3] ), digits = 2 ) + 0.02
    }
    chartTitle <- paste( 'Statistical', resultType, 'simulated results for scenario', input$data1,sep = ' ')
    png(file)
   
    print( ggplot( data = data_FinalOutput$data ,
            aes(x = useCase, y = testResult, fill = testName ) ) +
      geom_bar( stat = 'summary', fun.y = 'mean', position = 'dodge' ) + 
      labs( title = chartTitle, x = 'Simulated Use-Cases',
            y = paste( resultType, 'Result', sep = ' ' ),
            fill = 'Colour key: ' ) +
      geom_hline( yintercept = ylineintercept,
                  linetype = 'dashed', color = 'darkblue' ) +
      ylim( 0, y_max ) +
      scale_x_continuous( breaks = function(x) unique( floor( pretty( seq( 0, ( max(x) + 1 ) * 1.1))))) +
      theme( plot.title = element_text( size = 16 ), legend.position = 'bottom' ) )
     dev.off()
  })

  
  output$visual_Table = renderTable({
    tempTable <- data_FinalOutput$data %>% 
      mutate( useCase = paste( 'Use-Case', useCase, sep = ' ' ) )  %>% 
      spread( testName, testResult  ) %>%
      mutate( useCaseDetails = unlist(dd_Vectors[user_ID()]), 
              useCaseDetails = paste( dd_Options[user_ID()], '=', useCaseDetails, sep = ' ' ) ) %>%
      rename( 'Use-Case' = useCase, 'Use-Case Detail' = useCaseDetails )  %>%
      select( c( 1, 4, 2, 3) )
  }, caption = "Table of each use-case results for scenario simulated.",  
  caption.placement = getOption("xtable.caption.placement", "top") )
  
  output$downTable <- downloadHandler(
    # Create the download file name
    filename = function() {
      paste('Table', 'png', sep='.')
    },
    content = function(file) {
      tempTable <- data_FinalOutput$data %>%
        mutate( useCase = paste( 'Use-Case', useCase, sep = ' ' ) ) %>%
        spread( testName, testResult  ) %>%
        mutate( useCaseDetails = unlist(dd_Vectors[user_ID()]),
                useCaseDetails = paste( dd_Options[user_ID()], '=', useCaseDetails, sep = ' ' ) ) %>%
         select( c( 1, 4, 2, 3) ) %>%
      rename( 'Use-Case' = useCase, 'Use-Case Detail' = useCaseDetails )  
        
      png(file=file) 
      grid.table(tempTable, rows = NULL ) #Create image of the data frame
      dev.off()
    }) 
  
  } ) )
# end of shiny application
