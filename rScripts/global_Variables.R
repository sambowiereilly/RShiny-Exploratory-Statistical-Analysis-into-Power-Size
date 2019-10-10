# /*
#  *  Global Variables
#  *  Core area of useful variables and objects to support functionality of
#  *  rShiny application
#  */

# load custom version of 'movielens' dataset from 'dslabs' package
# add supportive columns to help perform computer intensive and statistical
# techiques to understand chosen researh question.
# Research Question:
# On average, do films from the 20th and 21st century share the same user rating?
customData_Movies <-  dslabs::movielens %>% 
  mutate( CenturyReleased  = case_when( year < 2001 ~ '20th Century',
                                        TRUE ~ '21st Century' ),
          AgeSegmentation = case_when(  year < 1960 ~ 'Group A (<1960)',
                                        year < 2000 ~ 'Group B (1960~1999)',
                                        year < 2010 ~ 'Group C (2000~2009)',
                                        TRUE ~ 'Group D (2010+)' ),
          rating_roundedDown = floor( rating ),
          rating_roundedUp = floor( rating + 0.5 ) ) %>%
  select( CenturyReleased, rating, AgeSegmentation,
          rating_roundedDown, rating_roundedUp )

# initialise starting default values of rShiny app to be custom movies dataset
core_Data <- customData_Movies
x_DropDown <- 'CenturyReleased'
y_DropDown <- 'rating'

# generate base data set attributes (mean, proportion, etc.) that user can #
# can adjust and change via rShiny app
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

# drop down features for user to allow flexibility of simulations/tests performed.
admin_Alpha <- c( 0.01, 0.05, 0.1, 0.15 )
admin_Alpha0 <- 0.05

admin_PositiveOnly <- c( 'No', 'Yes' )
admin_PositiveOnly0 <- 'Yes'

admin_Boot <- 'Number of Simulations'
admin_Boot_DropDown <- 'Please enter number of Simulations to perform:'
admin_Boot_Default <- 100

admin_SampleSize <- 'Sample Size'
admin_Sample_DropDown <- 'Please enter Sample Size:'
admin_Sample_Default <- 100

admin_DynVector <- c( admin_SampleSize, admin_Boot )
admin_DynDropDown <- c( admin_Sample_DropDown, admin_Boot_DropDown )
admin_DynValue <- c( admin_Sample_Default, admin_Boot_Default )

admin_SelectInput <- as.data.frame( list( admin_Dyn = admin_DynVector,
                                          admin_DropDown = admin_DynDropDown,
                                          admin_DynValue = admin_DynValue ) ) 

dd_OptionsID <- c( 1, 2, 3 )

dd_Options <- c( admin_SampleSize, admin_Boot, 'Effect Size' )
# other considerations included:
# 'Standard Deviation Size', 'X-Covariate Factor Level Proportions' 


dd_Table <- as.data.frame( list( ID = dd_OptionsID, Options = dd_Options ) )

dd_Vectors <- list( c( 10, 100 , 500 , 1000, 5000, 10000 ),
                    c( 10, 100, 1000, 10000 ),
                    c( 0.001, 0.01, 0.05, 0.1, 0.2, 0.5 ) ) 

dd_VectorDescriptions <- c( 'Scenario: Sample Size Variation, where n = { 10, 100, 500, 1000, 5000, 10000 }',
                            'Scenario: Increment number of Simulations, where s = { 10, 100, 1000, 10000 }',
                            'Scenario: Increment Effect Size relative to data set mean, where relative % rise ranges from { 0.001, 0.01, 0.05, 0.1, 0.2, 0.5 }' )

dd_VectorFriendlyDescriptions <- c( 'In laymans terms, Sample Size ranges from small to large.',
                                    'In laymans terms, Number of simulations ranges from small to large.',
                                    'In laymans terms, relative Effect Size ranges from small to large. ' )

dd_UserDynamicAttributes <- list( c( 'x_Covariate', 'proportion', 'mean', 'sd' ),
                                  c( 'x_Covariate', 'proportion', 'mean', 'sd' ),
                                  c( 'x_Covariate', 'proportion', 'sd' ) )


dd_RevisedAttributes <- list( c( 'x_Covariate', 'proportion', 'mean', 'sd' ),
                              c( 'x_Covariate', 'observations', 'proportion', 'mean', 'sd' ),
                              c( 'x_Covariate', 'observations', 'proportion', 'sd' ) )

dd_SelectInput_One <- c( admin_Boot_DropDown, admin_Sample_DropDown, rep( admin_Sample_DropDown, length( dd_OptionsID ) - 2 ) )
dd_SelectInput_Two <- c( rep( NA_character_, 2 ), rep( admin_Boot_DropDown, length( dd_OptionsID ) - 2 ) )

testTable <- as.data.frame( list( input_One = dd_SelectInput_One, input_Two = dd_SelectInput_Two ) )
