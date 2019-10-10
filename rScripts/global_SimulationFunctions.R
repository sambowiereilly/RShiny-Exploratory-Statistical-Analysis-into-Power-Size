# /*
#  *  Global Libraries
#  *  One stop shop for all core simulation functions utilised by rShinyApp
#  */

# /*
#  * generate simulated data
#  * function allows me to pass a single row containing { 100, 5, 1, TRUE}
#  * and this function will return 100 "random" numbers with mean approx. ~ 5 and
#  * sd ~ 1 and contain only positive values - pretty neat 
#  */
fun.generate_simulatedData <- function( index, base_simData, positiveValuesOnly ) {
  # sometime rShiny throws an error when passing a dataframe so in an attempt
  # to minimise those occurances, each column of interest is stored as a vector
  nbase <- as.numeric( base_simData[index, 2] )
  meanbase <- as.numeric( base_simData[index, 4] )
  sdbase <- as.numeric( base_simData[index, 5] )
  factorLevels <- base_simData[index, 1]
  
  # if user wishes to only generate positive simulated values, then use 
  # rtruncnorm from 'truncnorm' package, otherwise, use rnorm
  if( positiveValuesOnly == 'Yes' ) {
    y_Sample <- rtruncnorm( n = nbase, a = 0, b = Inf, mean = meanbase, sd = sdbase)
  } else {
    y_Sample <- rnorm( n = nbase, mean = meanbase, sd = sdbase )
  }
  
  # align factor level with random numbers
  x_FactorLevels <- unlist( rep( factorLevels, nbase ) )
  # convert into data frame object
  simulatedData <- data.frame( list( x_Covariate = x_FactorLevels, 
                                     y_Sample = y_Sample ) ) 
  # and return
  return( simulatedData )  
}

# /*
#  * perform simulation technique
#  * function calls fun.generate_simulatedData() to generate some random values
#  * with specified attributes, then performs a parametric and non-parametric test.
#  * If factor level is 2, then performs t-test and Mann-Whitney U Test'
#  * Otherwise, ANOVA and Kruskal-Wallis Rank Sum Test
#  */
fun.MonteCarloSimulation <- function( index, base_simData, 
                                      number_FactorLevels, 
                                      alpha, nboot, positiveValuesOnly ) {
  resultH0_Parametric <- 0
  resultH0_NonParametric <- 0
  
  pvalue.Search <- 'Pr(>F)1'
  testType_Parametric <- 'Parametric'
  testType_NonParametric <- 'Non Parametric'
  
  nboot <- as.numeric( nboot )
  
  for ( i in 1:nboot){
    
    simData <- do.call( 'rbind', lapply(  1:number_FactorLevels, 
                                          fun.generate_simulatedData, 
                                          base_simData, positiveValuesOnly ) )
    if( number_FactorLevels == 2 ){
      # tests for factor levels of two
      testName_Parametric <- 'T-Test'
      test_Parametric <- t.test( y_Sample ~ x_Covariate, data = simData ) 
      test_Parametric <- test_Parametric$p.value
      
      testName_NonParametric <- 'Mann-Whitney U Test'
      test_NonParametric <- wilcox.test( y_Sample ~ x_Covariate, data = simData ) 
      test_NonParametric <- test_NonParametric$p.value
      # code for factor levels > 2
    } else {
      # tests for factor less of three or more
      testName_Parametric <- 'Analysis of Variance (ANOVA)'
      test_Parametric <- aov( y_Sample ~ x_Covariate, data = simData )
      test_Parametric <- as.numeric( unlist( summary( test_Parametric ) )[ pvalue.Search ] )
      
      testName_NonParametric <- 'Kruskal-Wallis Rank Sum Test'
      test_NonParametric <- kruskal.test( y_Sample ~ x_Covariate, data = simData)
      test_NonParametric <- test_NonParametric$p.value
    }
    
    resultH0_Parametric <- resultH0_Parametric + ifelse( test_Parametric <= alpha, 1, 0 )
    resultH0_NonParametric <- resultH0_NonParametric + ifelse( test_NonParametric <= alpha, 1, 0 )
  }
  
  finalTable <- as.data.frame( list( 
    useCase = rep( index, 2 ),
    testName = c( testName_Parametric, testName_NonParametric ),
    testResult = c( resultH0_Parametric/nboot , 
                    resultH0_NonParametric/nboot) ) ) 
  return( finalTable )
}


# /*
#  * overarching simulation function
#  * function acts as the "go-between" from rShiny and simulation functions above
#  * Effectively, it adapts the inputs fun.MonteCarloSimulation() needs based
#  * on what type of scenario the user has selected from the drop down options.
#  *
#  * i.e. if user selected Sample size scenario, then the function will extract 
#  * the Sample Size to execute from the globalVariables info while taking the 
#  * the rest of the details from the users entered info (simulation number, etc.)
#  */
fun.fullSim <- function( base_simData, user_ID, alpha, 
                         dd_Vectors, num_Sims, positiveValuesOnly ){
  set.seed( '007' )
  dyn_simVector <- unlist( dd_Vectors[user_ID] )
  usePower <- TRUE
  if( var( base_simData$mean ) != 0 ){ usePower <- FALSE }
  
  nFactorLevels <- nrow( base_simData )
  mc_sims <- length( dyn_simVector )
  complete_Results <- NULL
  
  for( i in 1:mc_sims ){
    engine_simData <- base_simData
    # bootstrap first boys - cause its easier this way
    if( user_ID == 2 ){
      bootN <- dyn_simVector[i]
    }
    else { bootN <- num_Sims
    #bootN <- ifelse( user_ID == 1, 100, 100 )
    #bootN <- ifelse( user_ID == 1, input$test1, input$test2 )
    
    if( user_ID == 1 ){
      engine_simData[,2] <- fun.simData_UpdateSampleSize( engine_simData, dyn_simVector[i] )
    } else {
      # if the user selected "Effect Size", then its a little bit of work for us
      # as the goal is for effect size to be relative, the stored results in
      # global_Variables represent percentages.
      # therefore, we need to re-weight means of the data set ourselves
      baseMean <- sum( engine_simData[,3] * engine_simData[,4] )
      copyData <- engine_simData
      copyData$ref <- 'Same'
      copyData$ref[1] <- 'Diff'
      
      effectSize.Workings <- copyData %>%
        group_by(ref) %>%
        summarise( sumProp = sum(proportion) ) 
      
      effectPerc <- dyn_simVector[i]
      effectValue <- baseMean * effectPerc
      
      effectSize.Workings
      
      r_diffMean <- baseMean + ( effectValue * ( ifelse(effectSize.Workings[1,1]=='Diff',-1,1) + ( 1- effectSize.Workings[1,2] ) ) )
      r_sameMean <- baseMean + ( effectValue * ( ifelse(effectSize.Workings[2,1]=='Diff',1,-1) + ( 1- effectSize.Workings[2,2] ) ) )
      
      copyData$finalMeans <- ifelse( copyData$ref == 'Same', r_sameMean, r_diffMean )
      # update with new means
      engine_simData[,4] <- as.numeric( copyData$finalMeans )

      } 
    }

    complete_Results <- rbind( complete_Results, fun.MonteCarloSimulation( i, 
                                engine_simData, nFactorLevels, 
                                alpha, bootN, positiveValuesOnly ) )
  }
  return( complete_Results)
}
