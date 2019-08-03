FINAL

############### wifi prediction ###############################################################
----------------------------------------------------------------------------------------
  #  0. Libraries ####
----------------------------------------------------------------------------------------
library(tidyverse)        # Package for tidying data
library(lubridate)        # For working with dates/times of a time series
library(VIM)              # Visualizing and imputing missing values
library(Hmisc)            # for descriptive statistics
library(forecast)         # forcasting package
library(broom)            # Tidy statistical summary output
library(knitr)            # report generation
library(plotly)           # visualization
library(imputeTS)         # imputing
library(ggplot2)          # visualization
library(GGally)           # visualization
library(ggfortify)        
library(naniar)           # handling missing values
library(dplyr)            # data manipulation
library(simputation)      # imputing
library(ggcorrplot)       # PLOTTING CORELATIONS
library(FactoMineR)       # Dimensions reduction
library(factoextra)       # visualization for pca
library(rgl)
library(plyr)
library(h2o)              # ml by H2O
library(rstudioapi)       # needed for h2o
----------------------------------------------------------------------------------------
  #  1. Import and cretae datasets ####
---------------------------------------------------------------------------------------
  TrainSet<-read.csv("C:/Users/Constantinos/Desktop/Ubiqum/IOT/WIFI/UJIndoorLoc/trainingData.csv")
  ValidSet<-read.csv("C:/Users/Constantinos/Desktop/Ubiqum/IOT/WIFI/UJIndoorLoc/validationData.csv")
  
  # Quick exploration
  str(TrainSet[,520:529])
  sum(is.na(TrainSet))
  str(ValidSet[,520:529])
  sum(is.na(ValidSet))
  
  #Create total dataset
  TotalData<-rbind(TrainSet,ValidSet)
  TotalData<-as.data.frame(TotalData) # Dataframe
  
  # create a tidy Total data
  Tidy_TotalData<-TotalData%>% 
    gather(key="WAP" ,value="RSSI",WAP001:WAP520, factor_key=TRUE)
  
  ----------------------------------------------------------------------------------------  
  #  2. General statistics and  exploratory visualization Part 1####
  ----------------------------------------------------------------------------------------
    ggpairs(TotalData[,521:529])#takes time to plot
  pairs(TotalData[,521:529])  
  #     -a. Plot location  ####
  -------------------------------------------------------------------------------------------
    # Comparison of Train and Validation
    TrainSet$BUILDINGID<-as.factor(TrainSet$BUILDINGID)# Convering to factors for the graphs
  ValidSet$BUILDINGID<-as.factor(ValidSet$BUILDINGID)
  
  plot(TrainSet$LONGITUDE, TrainSet$LATITUDE, col=TrainSet$BUILDINGID)
  plot(ValidSet$LONGITUDE, ValidSet$LATITUDE, col=ValidSet$BUILDINGID)
  
  #3D plot of buildings train set
  plot_ly(x=TrainSet$LONGITUDE, y=TrainSet$LATITUDE, z=TrainSet$FLOOR, type="scatter3d", mode="markers", color=TrainSet$FLOOR)
  plot_ly(x=TrainSet$LONGITUDE, y=TrainSet$LATITUDE, z=TrainSet$FLOOR, type="scatter3d", mode="markers", color=TrainSet$RELATIVEPOSITION)#in building 0 all metrics were recorded outside 
  plot_ly(x=TrainSet$LONGITUDE, y=TrainSet$LATITUDE, z=TrainSet$FLOOR, type="scatter3d", mode="markers", color=TrainSet$TIMESTAMP) 
  plot_ly(x=TrainSet$LONGITUDE, y=TrainSet$LATITUDE, z=TrainSet$FLOOR, type="scatter3d", mode="markers", color=TrainSet$USERID) 
  
  #3D plot of buildings validation set
  plot_ly(x=ValidSet$LONGITUDE, y=ValidSet$LATITUDE, z=ValidSet$FLOOR, type="scatter3d", mode="markers", color=ValidSet$FLOOR)
  
  #3D plot of buildings total data set
  plot_ly(x=TotalData$LONGITUDE, y=TotalData$LATITUDE, z=TotalData$FLOOR, type="scatter3d", mode="markers", color=TotalData$FLOOR)
  
  -----------------------------------------------------------------------------------------
  #  3. Data preprocessing ####
  -----------------------------------------------------------------------------------------
    
  # Remove duplicate rows in a data frame
    
    TotalData<- distinct(TotalData) # https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
  # https://statistical-programming.com/r-is-na-function/
  
  # To remove objects from environment : rm(dataset)
  
  
  # Replace weak signal and NAs
  TotalData[, 1:520][TotalData[, 1:520] == 100] <- NA # replacing the unrecorded
  TotalData[, 1:520][TotalData[, 1:520] < -85]<- NA  # replacing the bad signal https://www.metageek.com/training/resources/wifi-signal-strength-basics.html
  sum(is.na(TotalData))# 10.378.985 bad signals
  --------------------------------------------------------------------------------------------
  #     -a. Missing Values analysis ####
  ---------------------------------------------------------------------------------------------
    any_na(TotalData) #  TRUE
  
  n_miss(TotalData) # 10698663 total number of missing values in the data
  
  prop_miss(TotalData) #  0.005 NA %
  
  prop_complete(TotalData) #  0.9626862 Values%
  
  miss_scan_count(data = TotalData, search = list("N/A", "missing","na", " "))# No strange missing values
  gg_miss_upset(TotalData)
  
  gg_miss_fct(x = TotalData, fct = BUILDINGID)
  # https://statistical-programming.com/r-is-na-function/
  ---------------------------------------------------------------------------------------------
  #     -b. Add count of WAP's detected as feature ####
  --------------------------------------------------------------------------------------------
    # count in new column the number of waps that have readings(not NAs)
    TotalData$WAP_num <- apply(TotalData[,1:520], 1,
                               function(x) length(which(!is.na(x))))
  
  # count in new colun the number of Waps with RSSI<-60
  TotalData$Good_Signal <- apply(TotalData[,1:520], 1,
                                 function(x) length(which(x>-60)))
  ---------------------------------------------------------------------------------------------
  #     -c. Remove columns and rows that have all NAs ####
  --------------------------------------------------------------------------------------------- 
    
    
  #-Remove columns with all NA values
    TotalData <- TotalData[,colSums(is.na(TotalData))<nrow(TotalData)]
  # colSums(is.na(TotalData):  Get the vector of NAs and find how many there are per column 
  # Then retain the columns If the sum is less than the length of the rows, else they have all NAs   
  
  
  #-Remove rows with all NA values
  TotalData <- TotalData[rowSums(is.na(TotalData[,1:435])) != ncol(TotalData[,1:435]),]
  # rowSums(is.na(TotalData[,1:460])): Get the vector of NAs that are not equal to number of columns 
  
  # Check
  colSums(is.na(TotalData))
  
  
  #-Change data types
  TotalData[,1:426]<-lapply(TotalData[,1:426],as.numeric)
  TotalData$TIMESTAMP <-as.POSIXlt(TotalData$TIMESTAMP,tz = "UTC", origin = "1970/01/01 00:00:00","%Y/%m/%d %H:%M:%S")
  TotalData[,427:433]<-lapply(TotalData[,427:432],as.factor)
  
  #-rename levels for BUILDINGID and floor
  TotalData$BUILDINGID <- factor(TotalData$BUILDINGID, labels = c("Building 1", 
                                                                  "Building 2", 
                                                                  "Building 3"))
  
  TotalData$FLOOR <- factor(TotalData$FLOOR, labels = c("floor 0", 
                                                        "floor 1", 
                                                        "floor 2",
                                                        "floor 3",
                                                        "floor 4"))
  # check
  levels(TotalData$BUILDINGID)
  levels(TotalData$FLOOR)
  
  #-Data type check
  str(TotalData[,422:433])
  class(TotalData$TIMESTAMP)
  class(TotalData$BUILDINGID)
  summary(TotalData[,424:432])
  
  # Drop TIMESTAMP
  TotalData<-TotalData[,-433]
  sum(is.na(TotalData))# 8.419.529 from 10.378.98
  
  #Have an NA dataset to use for graphs(in order not to mess histgrams)
  TotalData.NA<-TotalData  
  
  -----------------------------------------------------------------------------------------
  #  4. Create new Validation and Train set ####
  -----------------------------------------------------------------------------------------
    #replace NA with -100 in the TotalDataset
    TotalData[is.na(TotalData)] <- -100 #https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
  
  sum(is.na(TotalData)) # all NAs replaced
  #     -a. Create stratified samples ####
  ------------------------------------------------------------------------------------------  
    #-create column to stratify against 
    -------------------------------------------------------------------------------------------  
    TotalData$Strata<-paste(TotalData$BUILDINGID, "-", TotalData$FLOOR)
  
  # considering response variable as strata
  set.seed(107) # better try another partition trying new seed
  # the randomness in our split perhaps matters more than the randomness in our model
  
  data_part <- caret::createDataPartition(y = TotalData$Strata, 
                                          p = 0.58, list = F)
  valid <- TotalData[-data_part,] # 42% #we will have class imbalance building 3 
  train <- TotalData[data_part,] # 58% 
  
  #Remove initial datasets
  rm(TrainSet,ValidSet)
  
  # Visual check
  plot_ly(x=train$LONGITUDE, y=train$LATITUDE, z=train$FLOOR, type="scatter3d", mode="markers", color=train$FLOOR,size = I(20))
  
  plot_ly(x=train$LONGITUDE, y=test$LATITUDE, z=test$FLOOR, type="scatter3d", mode="markers", color=test$FLOOR,size = I(20))  
  
  # General statistics
  ggpairs(TotalData[,425:432])
  pairs(TotalData[,424:432])
  
  ----------------------------------------------------------------------------------------------------  
  #     -b. sources to have a look for startification ####
  --------------------------------------------------------------------------------------------------------
    # https://stats.stackexchange.com/questions/151735/stratified-random-sampling-implementation-how-to-in-r
    # https://stackoverflow.com/questions/9703428/how-to-create-a-stratified-sample-by-state-in-r/9704805#9704805
    # https://www.r-bloggers.com/creating-a-stratified-random-sample-of-a-dataframe/
    # https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
    # https://rpubs.com/trjohns/survey-stratified
    # https://mlammens.github.io/2014/07/stratified-random-sampling-in-r-using.html
    # https://rdrr.io/cran/splitstackshape/man/stratified.html
    # https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef
    
    
  #  5. Visualization Part 2 Explanatory ####
  
  # Data are based on the Total Na Dataset
  
  # Create Pallets
  library(viridis)
  library(RColorBrewer)
  library(wesanderson)
  library(viridisLite)
  
  #Distribution of WAP count by building and floor
  
  G1<-ggplot(TotalData.NA, aes(x=WAP_num, fill=FLOOR)) + geom_bar() +
    facet_grid(BUILDINGID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Number of WAP's Detected by Building", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))+
    scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) # palet with wesanderson
  
  
  G2<-ggplot(TotalData.NA, aes(x=WAP_num, fill=BUILDINGID)) + geom_bar() +
    facet_wrap(FLOOR~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Number of WAP's Detected by  floor", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))+
    scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) # viridis package
  G2
  
  #Distribution of Good signal count by phone
  pal <- choose_palette()
  
  G3<-ggplot(TotalData.NA, aes(x=Good_Signal, fill=FLOOR)) + geom_bar() +
    facet_wrap(PHONEID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of phoneids ') +
    labs(x="Number of WAP's Detected by phone", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))+
    scale_fill_manual( values=my_palet, aesthetics = "fill")
  G3
  #Distribution of WAP count by phone
  G4<-ggplot(TotalData.NA, aes(x=WAP_num, fill=FLOOR)) + geom_bar() +
    facet_wrap(PHONEID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of phoneids ') +
    labs(x="Number of WAP's Detected by phone", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))
  
  #-Distribution of WAP count by building- boxplot
  G5<-ggplot(TraiNN, aes(x=BUILDINGID, y=WAP_num)) + 
    geom_boxplot(fill='lightbrown') +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Building Number", y= 'WAP Counts' )
  
  
  #number of waps per phoneid
  G6<-ggplot(TraiNN, aes(x=WAP_num, fill=FLOOR)) + geom_bar() +
    facet_wrap(PHONEID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Number of WAP's Detected by PHONEID", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))
  
  #good signal by building
  G7<-ggplot(TraiNN, aes(x=Good_Signa, fill=BUILDINGID)) + geom_bar() +
    facet_wrap(PHONEID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Number of Good signal WAP's Detected by PHONEID", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))
  
  #good signal by building
  G8<-ggplot(TraiNN, aes(x=Good_Signa, fill=BUILDINGID)) + geom_bar() +
    facet_wrap(PHONEID~.) +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Number of Good signal WAP's Detected by PHONEID", y= 'Counts by Building Floor') +
    theme(panel.border=element_rect(colour='black', fill=NA))
  
  #-Distribution of WAP count by building- boxplot NEEDS ADJUSTMENT
  ggplot(wifi_trainData, aes(x=BUILDINGID, y=WAP_num)) + 
    geom_boxplot(fill='lightblue') +
    theme(text = element_text(size=14)) +
    ggtitle('Distribution of Detected Wireless Access Points by Building') +
    labs(x="Building Number", y= 'WAP Counts' ) +
    theme(panel.border=element_rect(colour='black', fill=NA))
  ------------------------------------------------------------------------------------------------------------------
  #  6. Step 1 : Use WAP to predict BUILDINGID ====
  ------------------------------------------------------------------------------------------------------------------
    # Set up for H2o models
  ------------------------------------------------------------------------------------------------
  # setting parameters
  set.seed(1234)
  
  pacman::p_load(readr,h2o, rstudioapi, caret)
  # Initialize  h2o cluster
  h2o.init(nthreads = -1)
  
  # Convert data to h2o frame
  train.h2o <- as.h2o(train) 
  test.h2o <- as.h2o(valid)
  
  # Identify target and features
  
  #dependent variable (BUILDINGID)
  y.dep <- 428 #BUILDING ID
  
  #independent variables (WAPS)
  x.indep <- c(1:424) #WAPS  
  ---------------------------------------------------------------------------------------------------------------------
  #     -1) Random Forest BUILDING =====
  ---------------------------------------------------------------------------------------------------------------------  
    # Run a grid for RF
      system.time(
      grid.RF <- h2o.grid("randomForest", grid_id = "RF_2",
                          search_criteria = list(
                            strategy = "RandomDiscrete",
                            max_models = 20 
                          ),
                          hyper_params = list(
                            min_rows = c(10,20,5),
                            mtries = c(100, 50, 25),
                            col_sample_rate_per_tree = c(0.75, 0.9, 1.0),
                            sample_rate = c(0.5, 0.7, 0.9),
                            max_depth = c(40, 60, 120),
                            ntrees = c(500,1000,250)
                          ),
                          x = x.indep, y = y.dep, training_frame = train.h2o,
                          validation_frame = test.h2o,
                          stopping_tolerance = 0.0001,
                          stopping_rounds = 4,
                          score_tree_interval = 3
      )
    )
  grid.RF
  RFMmodels_Build <- lapply(grid.RF@model_ids, h2o.getModel)
  RFMmodels_Build 
  RF.best<-h2o.getModel("RF_2_model_19")# best model
  h2o.performance (RF.best)
  
  predict.RF_Build <- as.data.frame(h2o.predict(RF.best, test.h2o))
  
  Res.Build.RF<-caret::postResample(predict.RF_Build, valid$BUILDINGID)  
  ---------------------------------------------------------------------------------------------------------------------
  #     -2) GBM BUILDING =====
  ---------------------------------------------------------------------------------------------------------------------
    # Run a grid for GBM
    system.time(
      grid.GBM <- h2o.grid("gbm", grid_id ="GBM",
                           search_criteria = list(
                             strategy = "RandomDiscrete",
                             max_models = 20
                           ),
                           hyper_params = list(
                             max_depth = c(5, 20, 50),
                             min_rows = c(2, 5, 10),
                             sample_rate = c(0.5, 0.8, 0.95, 1.0),
                             col_sample_rate = c(0.5, 0.8, 0.95, 1.0),
                             col_sample_rate_per_tree = c(0.8, 0.99, 1.0),
                             learn_rate = c(0.1),
                             #Placemarker
                             seed = c(701)
                             #Placemarker
                           ),
                           x = x.indep, y = y.dep, training_frame = train.h2o, validation_frame = test.h2o,
                           stopping_tolerance = 0.001,
                           stopping_rounds=3,
                           score_tree_interval = 10,
                           ntrees = 500
      )
    )
  grid.GBM
  # From grid.GBM grid, first fetch all the models in it with:
  
  GBMmodels_Build <- lapply(grid.GBM@model_ids, h2o.getModel)
  GBMmodels_Build
  GBM.best<-h2o.getModel("GBM_model_6")# best model
  
  
  
  h2o.performance (GBM.best)# needs the model(so we saved as GBM.best the best from the grid) does not work with modelID
  
  predict.GBM_Build <- as.data.frame(h2o.predict(GBM.best, test.h2o))
  
  Res.Build<-caret::postResample(predict.GBM_Build, valid$BUILDINGID)
  caret::postResample(predict.GBM_Build, valid$BUILDINGID)
----------------------------------------------------------------------------------
# AUTOML BUILDING ====
--------------------------------------------------------------------------------------    
# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
  amlbuilding. <- h2o.automl(x = x.indep, y = y.dep,
                                training_frame =  train.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  Lead.Building <- amlbuilding.@leaderboard
  print(Lead.Building, n = nrow(LeadBX.Build_01))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Building<-amlbuilding.@leader
  
  # If you need to generate predictions on a test set, you can make
  # predictions directly on the `"H2OAutoML"` object, or on the leader
  # model object directly
  
  pred.Best.Building <- as.data.frame(h2o.predict(amlbuilding.@leader, Ntest.h2o))  # predict(aml, te
    
    
---------------------------------------------------------------------------------------------------------------------  
  #  7. Step 2 :Use wAP and predicted building to get floor ####
  ------------------------------------------------------------------------------------------------------------------
    
  # Add prediction to validation dataset and remove BUILDINGID
    
  train.FL<-valid # new trainingset is our validation replacing building with prediction 
  train.FL[,428]<-predict.GBM_Build$predict
  
  # Create 3 new datsets
  Build_1<-train.FL %>% filter(BUILDINGID =="Building 1")  
  Build_2<-train.FL %>% filter(BUILDINGID =="Building 2")
  Build_3<-train.FL %>% filter(BUILDINGID =="Building 3")
  
  #check: there are unique starta values these are the errors of the prediction for building
  unique(Build_1$Strata)
  unique(Build_2$Strata)
  unique(Build_3$Strata)
  str(Build_1[,425:435])
  
  #removing Buildingid, spaceid,phoneid,strata,relativeposition
  Build_1<-Build_1[,-c(435,432,430,429,428)]
  Build_2<-Build_2[,-c(435,432,430,429,428)]
  Build_3<-Build_3[,-c(435,432,430,429,428)]
  
  
  # Create test and train
  set.seed(666)
  # -BUILDING 1-
  data_part_B01 <- caret::createDataPartition(y = Build_1$FLOOR, 
                                              p = 0.68, list = F)
  valid.B01 <- Build_1[-data_part,] # 32% 
  train.B01 <- Build_1[data_part,] # 68% 
  # -BUILDING 2-
  data_part_B02 <- caret::createDataPartition(y = Build_2$FLOOR, 
                                              p = 0.68, list = F)
  valid.B02 <- Build_2[-data_part,] # 32% 
  train.B02 <- Build_2[data_part,] # 68% 
  # -BUILDING 3-
  data_part_B03 <- caret::createDataPartition(y = Build_3$FLOOR, 
                                              p = 0.68, list = F)
  valid.B03 <- Build_3[-data_part,] # 32% 
  train.B03 <- Build_3[data_part,] # 68% 
  
  -------------------------------------------------------------------------------------------------
  #     -a. Set Parameters for Ensemble ####
  --------------------------------------------------------------------------------------------------  
    set.seed(1234)
  
  # Convert data to h2o frame
  train_Build_1.h2o <- as.h2o(train.B01) 
  train_Build_2.h2o <- as.h2o(train.B02) 
  train_Build_3.h2o <- as.h2o(train.B03) 
  test_Build_1.h2o <- as.h2o(valid.B01)
  test_Build_2.h2o <- as.h2o(valid.B02)
  test_Build_3.h2o <- as.h2o(valid.B03)
  
  
  # Identify target and features
  
  #dependent variable (FLOOR)
  y1.dep <- 427 #floor
  y2.dep <- 427
  y3.dep <- 427
  
  #independent variables (WAPS)
  x1.indep <- c(1:424) #WAPS
  x2.indep <- c(1:424)
  x3.indep <- c(1:424)
  
  
  # There are a few ways to assemble a list of models to stack toegether:
  # 1. Train individual models and put them in a list
  # 2. Train a grid of models
  # 3. Train several grids of models
  # Note: All base models must have the same cross-validation folds and
  # the cross-validated predicted values must be kept.
  
  # Number of CV folds (to generate level-one data for stacking)
  nfolds <- 10
  
  # 1. Generate a 2-model ensemble (GBM + RF) for buildings
  ----------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a GBM BUILDING 1 ====
  ----------------------------------------------------------------------------------------------
    gbm_Build_1 <- h2o.gbm(x = x1.indep,
                           y = y1.dep,
                           training_frame = train_Build_1.h2o,
                           ntrees = 200,
                           max_depth = 02,
                           min_rows = 5,
                           learn_rate = 0.2,
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)
  gbm_Build_1
  h2o.performance (gbm_Build_1)
  predict.gbm_Build_1 <- as.data.frame(h2o.predict(gbm_Build_1, test_Build_1.h2o))
  postResample(predict.gbm_Build_1, valid.B01$FLOOR)
  ------------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a RF  BUILDING 1 =====
  ------------------------------------------------------------------------------------------------
    rf_Build_1 <- h2o.randomForest(x = x1.indep,
                                   y = y1.dep,
                                   training_frame = train_Build_1.h2o,
                                   ntrees = 500,
                                   nfolds = nfolds,
                                   fold_assignment = "Modulo",
                                   keep_cross_validation_predictions = TRUE,
                                   seed = 1)
  rf_Build_1
  h2o.performance (rf_Build_1)
  predict.rf_Build_1 <- as.data.frame(h2o.predict(rf_Build_1, test_Build_1.h2o))
  postResample(predict.rf_Build_1, valid.B01$FLOOR)
  -------------------------------------------------------------------------------------------------
  #       > Train a stacked ensemble using the GBM and RF above for Building 1 ====
  ------------------------------------------------------------------------------------------------
    ensemble.B1 <- h2o.stackedEnsemble(x = x1.indep,
                                       y = y1.dep,
                                       training_frame = train_Build_1.h2o,
                                       model_id = "ensemble_Build.01",
                                       base_models = list(gbm_Build_1, rf_Build_1))
  
  
  ----------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a GBM BUILDING 2 ====
  ----------------------------------------------------------------------------------------------
    gbm_Build_2 <- h2o.gbm(x = x2.indep,
                           y = y2.dep,
                           training_frame = train_Build_2.h2o,
                           ntrees = 300,
                           max_depth = 10,
                           min_rows = 3,
                           learn_rate = 0.2,
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)
  gbm_Build_2
  h2o.performance (gbm_Build_2)
  predict.gbm_Build_2 <- as.data.frame(h2o.predict(gbm_Build_2, test_Build_2.h2o))
  caret::postResample(predict.gbm_Build_2, valid.B02$FLOOR)
  
  
  ------------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a RF  BUILDING 2 =====
  ------------------------------------------------------------------------------------------------
    rf_Build_2 <- h2o.randomForest(x = x2.indep,
                                   y = y2.dep,
                                   training_frame = train_Build_2.h2o,
                                   ntrees = 500,
                                   nfolds = nfolds,
                                   fold_assignment = "Modulo",
                                   keep_cross_validation_predictions = TRUE,
                                   seed = 1)
  rf_Build_2
  h2o.performance (rf_Build_2)
  predict.rf_Build_2 <- as.data.frame(h2o.predict(rf_Build_2, test_Build_2.h2o))
  postResample(predict.rf_Build_2, valid.B02$FLOOR)
  
  ---------------------------------------------------------------------------------------------
  #       > Train a stacked ensemble using the GBM and RF above for Building 2 ====
  ---------------------------------------------------------------------------------------------
    ensemble.B2 <- h2o.stackedEnsemble(x = x2.indep,
                                       y = y2.dep,
                                       training_frame = train_Build_2.h2o,
                                       model_id = "ensemble_Build.02",
                                       base_models = list(gbm_Build_2, rf_Build_2))
  
  ---------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a GBM BUILDING 3 ====
  ----------------------------------------------------------------------------------------------
    gbm_Build_3 <- h2o.gbm(x = x3.indep,
                           y = y3.dep,
                           training_frame = train_Build_3.h2o,
                           ntrees = 600,
                           max_depth = 25,
                           min_rows = 4,
                           learn_rate = 0.2,
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)
  gbm_Build_3
  h2o.performance (gbm_Build_3)
  predict.gbm_Build_3 <- as.data.frame(h2o.predict(gbm_Build_3, test_Build_3.h2o))
  postResample(predict.gbm_Build_3, valid.B03$FLOOR)
  ------------------------------------------------------------------------------------------------
  #       -   Train & Cross-validate a RF  BUILDING 3 =====
  ------------------------------------------------------------------------------------------------
    rf_Build_3 <- h2o.randomForest(x = x3.indep,
                                   y = y3.dep,
                                   training_frame = train_Build_3.h2o,
                                   ntrees = 1000,
                                   nfolds = nfolds,
                                   fold_assignment = "Modulo",
                                   keep_cross_validation_predictions = TRUE,
                                   seed = 1)
  
  rf_Build_3
  h2o.performance (rf_Build_3)
  predict.rf_Build_3 <- as.data.frame(h2o.predict(rf_Build_3, test_Build_3.h2o))
  postResample(predict.rf_Build_3, valid.B03$FLOOR)
  -----------------------------------------------------------------------------------------------
  #       > Train a stacked ensemble using the GBM and RF above for Building 3 =====
  -------------------------------------------------------------------------------------------------
    ensemble.B3 <- h2o.stackedEnsemble(x = x3.indep,
                                       y = y3.dep,
                                       training_frame = train_Build_3.h2o,
                                       model_id = "ensemble_Build.03",
                                       base_models = list(gbm_Build_3, rf_Build_3))
  
  
  --------------------------------------------------------------------------------------------------
  #       > Ensemble Performance and Predictions for Floor ####
  ----------------------------------------------------------------------------------------------------------
    # Building 1 
    
  perf.Floor_B1 <- h2o.performance(ensemble.B1, newdata = test_Build_1.h2o) # Eval ensemble performance on a test set
  perf.Floor_B1
  pred.Floor_B1 <- as.data.frame(h2o.predict(ensemble.B1, newdata = test_Build_1.h2o)) # Generate predictions on a test set (if neccessary)
  pred.Floor_B1
  postResample(pred.Floor_B1, valid.B01$FLOOR)
  
  # Building 2 
  perf.Floor_B2 <- h2o.performance(ensemble.B2, newdata = test_Build_2.h2o)
  pred.Floor_B2 <- as.data.frame(h2o.predict(ensemble.B2, newdata = test_Build_2.h2o))
  perf.Floor_B2
  pred.Floor_B2
  postResample(pred.Floor_B2, valid.B02$FLOOR)
  
  # Building 3  
  
  perf.Floor_B3 <- h2o.performance(ensemble.B3, newdata = test_Build_3.h2o)
  pred.Floor_B3 <- as.data.frame(h2o.predict(ensemble.B3, newdata = test_Build_3.h2o))
  perf.Floor_B3
  pred.Floor_B3
  postResample(pred.Floor_B3, valid.B03$FLOOR)
  ------------------------------------------------------------------------------------------------------------------  
  #     -b. Predict Floor####
  ----------------------------------------------------------------------------------------------  
 
   # Building 1
  -----------------------------------------------------------------  
amlFlR.Build_01 <- h2o.automl(x = x1.indep, y = 427,
                                training_frame = train_Build_1.h2o,
                                max_models = 20,
                                seed = 1)
  # View the AutoML Leaderboard
  LeadBFlR.Build_01 <- amlFlR.Build_01@leaderboard
  print(LeadBFlR.Build_01, n = nrow(LeadBFlR.Build_01))  # Print all rows instead of default (6 rows)
  # The leader model is stored here
  Best.Floor_B1<-amlFlR.Build_01@leader
  pred.FlR_B1 <- as.data.frame(h2o.predict(amlFlR.Build_01@leader, test_Build_1.h2o))
  Res.Floor_B1<-caret::postResample(pred.FlR_B1,valid.B01$FLOOR)  
  
  # Building 2
  -----------------------------------------------------------------
amlFlR.Build_02 <- h2o.automl(x = x1.indep, y = 427,
                                training_frame = train_Build_2.h2o,
                                max_models = 20,
                                seed = 1)
  # View the AutoML Leaderboard
  LeadBFlR.Build_02 <- amlFlR.Build_02@leaderboard
  print(LeadBFlR.Build_02, n = nrow(LeadBFlR.Build_02))  # Print all rows instead of default (6 rows)
  # The leader model is stored here
  Best.Floor_B2<-amlFlR.Build_02@leader
  pred.FlR_B2 <- as.data.frame(h2o.predict(amlFlR.Build_02@leader, test_Build_2.h2o))
  Res.Floor_B2<-caret::postResample(pred.FlR_B2,valid.B02$FLOOR) 
  
  # Building 3
  -----------------------------------------------------------------
amlFlR.Build_03 <- h2o.automl(x = x3.indep, y = 427,
                                  training_frame = train_Build_3.h2o,
                                  max_models = 20,
                                  seed = 3)
  # View the AutoML Leaderboard
  LeadBFlR.Build_03 <- amlFlR.Build_03@leaderboard
  print(LeadBFlR.Build_03, n = nrow(LeadBFlR.Build_03))  # Print all rows instead of default (6 rows)
  # The leader model is stored here
  Best.Floor_B3<-amlFlR.Build_03@leader
  pred.FlR_B3 <- as.data.frame(h2o.predict(amlFlR.Build_03@leader, test_Build_3.h2o))
  Res.Floor_B3<-caret::postResample(pred.FlR_B3,valid.B03$FLOOR)   
  
  
--------------------------------------------------------------------------------------------------------
  #  8. Step 3 :Predict longtitude and latitude using Auto ML ####
-------------------------------------------------------------------------------------------------------------------
    
  # Identify target and features
    
  #dependent variable (FLOOR)
  ylon.dep <- 425 #longtitude
  ylat.dep <- 426 #latitude
  
-------------------------------------------------------------------------------------------------  
  #     -a. Predict Longtitude    ####
-------------------------------------------------------------------------------------------------
  #                        Building 1
------------------------------------------------------------------------------------
  # Run AutoML for 20 base models (limited to 1 hour max runtime by default)
    amlX.Build_01 <- h2o.automl(x = x1.indep, y = ylon.dep,
                                training_frame = train_Build_1.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBX.Build_01 <- amlX.Build_01@leaderboard
  print(LeadBX.Build_01, n = nrow(LeadBX.Build_01))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Long_B1<-amlX.Build_01@leader
  
  # If you need to generate predictions on a test set, you can make
  # predictions directly on the `"H2OAutoML"` object, or on the leader
  # model object directly
  
  pred.Long_B1 <- as.data.frame(h2o.predict(amlX.Build_01@leader, test_Build_1.h2o))  # predict(aml, test) also works
  Res.Long_B1<-caret::postResample(pred.Long_B1,valid.B01$LONGITUDE)
  
----------------------------------------------------------------------------------------
  #                        Building 2
----------------------------------------------------------------------------------------
  # Run AutoML for 20 base models (limited to 1 hour max runtime by default)
  amlX.Build_02 <- h2o.automl(x = x1.indep, y = ylon.dep,
                                training_frame = train_Build_2.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBX.Build_02 <- amlX.Build_02@leaderboard
  print(LeadBX.Build_02, n = nrow(LeadBX.Build_02))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Long_B2<-amlX.Build_02@leader
  
  pred.Long_B2 <- as.data.frame(h2o.predict(amlX.Build_02@leader, test_Build_2.h2o))  # predict(aml, test) also works
  Res.Long_B2 <-caret::postResample(pred.Long_B2,valid.B02$LONGITUDE)
  
---------------------------------------------------------------------------------------
  #                        Building 3
---------------------------------------------------------------------------------------
  # Run AutoML for 20 base models (limited to 1 hour max runtime by default)
  amlX.Build_03 <- h2o.automl(x = x1.indep, y = ylon.dep,
                                training_frame = train_Build_3.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBX.Build_03 <- amlX.Build_03@leaderboard
  print(LeadBX.Build_03, n= nrow(LeadBX.Build_03))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Long_B3<-amlX.Build_03@leader
  
  pred.Long_B3 <- as.data.frame(h2o.predict(amlX.Build_03@leader, test_Build_3.h2o))  # predict(aml, test) also works
  Res.Long_B3<-caret::postResample(pred.Long_B3,valid.B03$LONGITUDE)
  
  ------------------------------------------------------------------------------------------------  
  #     -b. Predict Latitude    ####
--------------------------------------------------------------------------------------------------
    #                        Building 1
--------------------------------------------------------------------------------------------
  # Run AutoML for 20 base models (limited to 1 hour max runtime by default)
    amlY.Build_01 <- h2o.automl(x = x1.indep, y = 426,
                                training_frame = train_Build_1.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBY.Build_01 <- amlY.Build_01@leaderboard
  print(LeadBY.Build_01, n = nrow(LeadBY.Build_01))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Lat_B1<-amlY.Build_01@leader
  
  
  pred.Lat_B1 <- as.data.frame(h2o.predict(amlY.Build_01@leader, test_Build_1.h2o))
  Res.Lat_B1<-caret::postResample(pred.Lat_B1,valid.B01$LATITUDE)
  
--------------------------------------------------------------------------------------
    #                        Building 2
--------------------------------------------------------------------------------------
  # Run AutoML for 20 base models (limited to 1 hour max runtime by default)
    amlY.Build_02 <- h2o.automl(x = x1.indep, y = ylat.dep,
                                training_frame = train_Build_2.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBY.Build_02 <- amlY.Build_02@leaderboard
  print(LeadBY.Build_02, n = nrow(LeadBY.Build_02))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Lat_B1<-amlY.Build_02@leader
  
  pred.Lat_B2 <- as.data.frame(h2o.predict(amlY.Build_02@leader, test_Build_2.h2o))
  Res.Lat_B2<-caret::postResample(pred.Lat_B2,valid.B02$LATITUDE)
  
----------------------------------------------------------------------------------------
    #                        Building 3
----------------------------------------------------------------------------------------
# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
    amlY.Build_03 <- h2o.automl(x = x1.indep, y = ylat.dep,
                                training_frame = train_Build_3.h2o,
                                max_models = 20,
                                seed = 1)
  
  # View the AutoML Leaderboard
  LeadBY.Build_03 <- amlY.Build_03@leaderboard
  print(LeadBY.Build_03, n = nrow(LeadBY.Build_03))  # Print all rows instead of default (6 rows)
  
  # The leader model is stored here
  Best.Lat_B3<-amlY.Build_03@leader
  
  pred.Lat_B3 <- as.data.frame(h2o.predict(amlY.Build_03@leader, test_Build_3.h2o))
  Res.Lat_B3<-caret::postResample(pred.Lat_B3,valid.B03$LATITUDE)
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #  9. RESULTS ====
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  # Create a predicted vs real datset column calculating deviation
  #Building 1
  Building_1 <- data.frame(matrix(ncol = 0, nrow = 1022))#empty dataframe
  Building_1$FLOOR<-valid.B01$FLOOR
  Building_1$FLOOR.Pr<-pred.FlR_B1$predict
  Building_1$LONG<-valid.B01$LONGITUDE
  Building_1$LONG.Pr<-pred.Long_B1$predict
  Building_1$LONG.Error<-valid.B01$LONGITUDE-pred.Long_B1$predict
  Building_1$LAT<-valid.B01$LATITUDE
  Building_1$LAT.Pr<-pred.Lat_B1$predict
  Building_1$LAT.Error<-valid.B01$LATITUDE-pred.Lat_B1$predict
  str(Building_1)
  
  #Building 2
  Building_2 <- data.frame(matrix(ncol = 0, nrow = 920))
  Building_2$FLOOR<-valid.B02$FLOOR
  Building_2$FLOOR.Pr<-pred.FlR_B2$predict
  Building_2$LONG<-valid.B02$LONGITUDE
  Building_2$LONG.Pr<-pred.Long_B2$predict
  Building_2$LONG.Error<-valid.B02$LONGITUDE-pred.Long_B2$predict
  Building_2$LAT<-valid.B02$LATITUDE
  Building_2$LAT.Pr<-pred.Lat_B2$predict
  Building_2$LAT.Error<-valid.B02$LATITUDE-pred.Lat_B2$predict
  
  # Building 3
  Building_3 <- data.frame(matrix(ncol = 0, nrow = 1651))
  Building_3$FLOOR<-valid.B03$FLOOR
  Building_3$FLOOR.Pr<-pred.FlR_B3$predict
  Building_3$LONG<-valid.B03$LONGITUDE
  Building_3$LONG.Pr<-pred.Long_B3$predict
  Building_3$LONG.Error<-valid.B03$LONGITUDE-pred.Long_B3$predict
  Building_3$LAT<-valid.B03$LATITUDE
  Building_3$LAT.Pr<-pred.Lat_B3$predict
  Building_3$LAT.Error<-valid.B03$LATITUDE-pred.Lat_B3$predict
  
 
  
-------------------------------------------------------------------------------------------------------------
  #     -a. Histograms with errors  ####
---------------------------------------------------------------------------------------------------------------  
  # BUILDING 1: Lat error histogram
  ggplot(Building_1, aes(x=LAT.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")+
    ggtitle("BUILDING 1: Errors in Latitude")+
    stat_function(fun=dnorm,
                  color="#DB5831",
                  args=list(mean=mean(Building_1$LAT.Error), 
                            sd=sd(Building_1$LAT.Error)))
  
  
  #BUILDING 1: Long error histogram
  ggplot(Building_1, aes(x=LONG.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.4, fill='#728FCD') +
    ggtitle("BUILDING 1: Errors in Longtitude")+
    stat_function(fun=dnorm,
                  color="#DB5831",
                  args=list(mean=mean(Building_1$LONG.Error), 
                            sd=sd(Building_1$LONG.Error)))
---------------------------------------------------------------------------------  
  # BUILDING 2: Lat error histogram
  ggplot(Building_2, aes(x=LAT.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")+
    ggtitle("BUILDING 2: Errors in Latitude") 
  stat_function(fun=dnorm,
                color="#DB5831",
                args=list(mean=mean(Building_2$LAT.Error), 
                          sd=sd(Building_2$LAT.Error)))
  
  #BUILDING 2: Long error histogram
  ggplot(Building_2, aes(x=LONG.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.4, fill='#728FCD') +
    ggtitle("BUILDING 2: Errors in Longtitude")+
    stat_function(fun=dnorm,
                  color="#DB5831",
                  args=list(mean=mean(Building_2$LONG.Error), 
                            sd=sd(Building_2$LONG.Error)))
--------------------------------------------------------------------------------
# BUILDING 3: Lat error histogram
  ggplot(Building_3, aes(x=LAT.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")+
    ggtitle("BUILDING 3: Errors in Latitude")+
    stat_function(fun=dnorm,
                  color="#DB5831",
                  args=list(mean=mean(Building_3$LAT.Error), 
                            sd=sd(Building_3$LAT.Error)))
  
  #BUILDING 3: Long error histogram
  ggplot(Building_3, aes(x=LONG.Error)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.4, fill='#728FCD') +
    ggtitle("BUILDING 3: Errors in Longtitude")+
    stat_function(fun=dnorm,
                  color="#DB5831",
                  args=list(mean=mean(Building_3$LONG.Error), 
                            sd=sd(Building_3$LONG.Error)))
---------------------------------------------------------------------------------------------------------------
  #     -b. Scatterplots: ploting predicted vs real ####
--------------------------------------------------------------------------------------------------------------
# Latitude Building 1   
  ggplot(Building_1, aes(x = LAT, y = LAT.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle(" BUILDING 1:Real vs Predicted in Latitude")
  
# Longtitude Building 1  
  ggplot(Building_1, aes(x = LONG, y = LONG.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle("BUILDING 1: Real vs Predicted in Longtitude")
-----------------------------------------------------------------------
# Latitude Building 2   
  ggplot(Building_2, aes(x = LAT, y = LAT.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle("BUILDING 2:Real vs Predicted in Latitude")
  
  # Longtitude Building 2  
  ggplot(Building_2, aes(x = LONG, y = LONG.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle("BUILDING 2:Real vs Predicted in Longtitude")

-----------------------------------------------------------------------------  
  # Latitude Building 3   
  ggplot(Building_3, aes(x = LAT, y = LAT.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle("BUILDING 3: Real vs Predicted in Latitude")
  
  # Longtitude Building 3  
  ggplot(Building_3, aes(x = LONG, y = LONG.Pr)) +
    geom_point() +
    geom_smooth()+
    ggtitle("BUILDING 3: Real vs Predicted in Longtitude")  
----------------------------------------------------------------------------------------------------------      
  #     -c. PLotly Buildings ####
-----------------------------------------------------------------------------------------------------------------------

# Plot buildings with predictions
-------------------------------------------------------------------------------------------------------------
B1_R.pl<-plot_ly(x=Building_1$LONG, y=Building_1$LAT, z=Building_1$FLOOR, type="scatter3d", mode="markers", color=Building_1$FLOOR)
B1_Pr.pl<-plot_ly(x=Building_1$LONG.Pr, y=Building_1$LAT.Pr, z=Building_1$FLOOR.Pr, type="scatter3d", mode="markers", color=Building_1$FLOOR,size = I(45)) %>% 
         layout(showlegend = TRUE, title ='BUILDING 1:Predicted Location')

B2_R.pl<-plot_ly(x=Building_2$LONG, y=Building_2$LAT, z=Building_2$FLOOR, type="scatter3d", mode="markers", color=Building_2$FLOOR)
B2_Pr.pl<-plot_ly(x=Building_2$LONG.Pr, y=Building_2$LAT.Pr, z=Building_2$FLOOR.Pr, type="scatter3d", mode="markers", color=Building_2$FLOOR,size = I(45)) %>% 
         layout(showlegend = TRUE, title ='BUILDING 2:Predicted Location')

B3_R.pl<-plot_ly(x=Building_3$LONG, y=Building_3$LAT, z=Building_3$FLOOR, type="scatter3d", mode="markers", color=Building_3$FLOOR)
B3_Pr.pl<-plot_ly(x=Building_3$LONG.Pr, y=Building_3$LAT.Pr, z=Building_3$FLOOR.Pr, type="scatter3d", mode="markers", color=Building_3$FLOOR,size = I(45)) %>% 
         layout(showlegend = TRUE, title ='BUILDING 3:Predicted Location')

# Run plots
B1_R.pl
B1_Pr.pl
B2_R.pl
B2_Pr.pl
B3_R.pl
B3_Pr.pl

# Combinations    
-----------------------------------------------------------------------------------------------------------------
#Replot without colour
B1_R.c<-plot_ly(x=Building_1$LONG, y=Building_1$LAT, z=Building_1$FLOOR, type="scatter3d", mode="markers",size=I(45))
B1_Pr.c<-plot_ly(x=Building_1$LONG.Pr, y=Building_1$LAT.Pr, z=Building_1$FLOOR.Pr, type="scatter3d", mode="markers",size=I(45))  
B2_R.c<-plot_ly(x=Building_2$LONG, y=Building_2$LAT, z=Building_2$FLOOR, type="scatter3d", mode="markers",size=I(45))
B2_Pr.c<-plot_ly(x=Building_2$LONG.Pr, y=Building_2$LAT.Pr, z=Building_2$FLOOR.Pr, type="scatter3d", mode="markers",size=I(45))  
B3_R.c<-plot_ly(x=Building_3$LONG, y=Building_3$LAT, z=Building_3$FLOOR, type="scatter3d", mode="markers",size=I(45))
B3_Pr.c<-plot_ly(x=Building_3$LONG.Pr, y=Building_3$LAT.Pr, z=Building_3$FLOOR.Pr, type="scatter3d", mode="markers",size=I(45))  

# BUILDING 1  
B1_comb.pl<-subplot(B1_R.c, B1_Pr.c,titleX = FALSE, titleY = FALSE) %>%
              layout(showlegend = TRUE, title ='BUILDING 1:Predicted VS Real Location')
# BUILDING 2
B2_comb.pl<-subplot(B2_R.c, B2_Pr.c,titleX = FALSE, titleY = FALSE) %>%
  layout(showlegend = TRUE, title ='BUILDING 2:Predicted VS Real Location')
# BUILDING 3
B3_comb.pl<-subplot(B3_R.c, B3_Pr.c,titleX = FALSE, titleY = FALSE) %>%
  layout(showlegend = TRUE, title ='BUILDING 3:Predicted VS Real Location')

# Run plots
B1_comb.pl
B2_comb.pl
B3_comb.pl

---------------------------------------------------------------------------------------------------------------
  #     -d. Table with Best Model Results ====
------------------------------------------------------------------------------------------------------------------
# Best model results Predicting building
  GBM.best
  Res.Build
# Best model results Predicting floor
  Best.Floor_B1
  Res.Floor_B1
  Best.Floor_B2
  Res.Floor_B2
  Best.Floor_B3
  Res.Floor_B3
# Best model results Predicting Longtitude 
  Best.Long_B1 
  Res.Long_B1
  Best.Long_B2 
  Res.Long_B2
  Best.Long_B3 
  Res.Long_B3
# Best model results Predicting Latitude 
  Best.Lat_B1 
  Res.Lat_B1
  Best.Lat_B2 
  Res.Lat_B2
  Best.Lat_B3 
  Res.Lat_B3
  
Results_clf <- data.frame(matrix(ncol = 0, nrow = 2))#empty dataframe
row.names(Results_clf)<-c('Accuracy','Kappa')# Name the rows
Results_clf$Building.GBM<-Res.Build
Results_clf$Building.RF<-Res.Build.RF
Results_clf$FloorB1<- Res.Floor_B1
Results_clf$FloorB2<- Res.Floor_B2
Results_clf$FloorB3<- Res.Floor_B3

Results_Reg <- data.frame(matrix(ncol = 0, nrow = 3))#empty dataframe
row.names(Results_Reg)<-c('RMSE','Rsquared','MAE')# Name the rows
Results_Reg$LongB1<-Res.Long_B1  
Results_Reg$LongB2<-Res.Long_B2 
Results_Reg$LongB3<-Res.Long_B3
Results_Reg$LatB1<-Res.Lat_B1
Results_Reg$LatB2<-Res.Lat_B2
Results_Reg$LatB3<-Res.Lat_B3
# Rounding
Results_Reg <- apply(Results_Reg, 1,
                           function(x) round(x, digits = 3))
Results_clf <- apply(Results_clf, 1,
                     function(x) round(x, digits = 3))
-----------------------------------------------------------------------------------------------
###############################################################################################