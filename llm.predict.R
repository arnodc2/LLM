#' Create Logit Leaf Model
#'
#' This function creates the logit leaf model. It assumes a dataframe with numeric
#' values as input and a corresponding vector with dependent values.
#' Decision tree parameters threshold for pruning and number of observations per
#' leaf can be set.
#'
#' @param df dataframe containing numerical independent variables
#' @param y_value numerical vector of dependent variable. Currently only two class classification is supported
#' @param llm_object output of the llm function. A LLM object containing decision rules that define segments and logistic regressions per segment
#' @param nbr_obs_leaf the minimum number of observations in a leaf node
#' @return the predictions of the llm model
#' @export
#'
#'

# TODO replace rowvector with ID parameter
llm.predict <- function(df, y_value, llm_object){
  # Custom fucntions that are used in the LLM function
  # _1_ trim.leading
  # _2_ ceiling.dec

  # _1_ trim.leading
  # Trim leading white spaces
  trim.leading <- function (x)  sub("^\\s+", "", x)

  # _2_ ceiling.dec
  # Round numerics at certain decimals ceiling
  ceiling.dec <- function(x,level=1) round(x+5*10^(-level-1),level)


  # Give the dataframe generic name (necessary to split the rules for both training and test set correctly)
  # TODO find more efficient solution to partition the data



  # Give the df a new name to be used to split
  basetabadc150392_2 <- df
  # Add a new row containing the row number (extra check to make sure we match always with corresponding Y value)
  basetabadc150392_2[,(ncol(basetabadc150392_2)+1)] <- c(1:nrow(basetabadc150392_2))
  # define the split expression from LM list that has to be changed
  tbc <- substr(llm_object[[1]][1],1,(regexpr("\\[", llm_object[[1]][1])[1]-1))

  # Create three vector where we can save the results
  predvector <- as.numeric()
  classvector <- as.numeric()
  rowvector <- as.numeric()
  # Loop over all the different splits and use the corresponding LR to make predictions for each subset
  for (i in 1:length(llm_object[[1]])) {

    llm_object[[1]][i] <- gsub(pattern = tbc, replacement = "basetabadc150392_2", x = llm_object[[1]][i])

    # Split the df according to the splits from the model
    ifelse(length(llm_object[[1]]) > 1,
           # If nbr of split is >1, than we subset according to rules DT
           val_ss <- basetabadc150392_2[which(eval(parse(text= llm_object[[1]][i]))), ],
           # otherewise we select the entire validation set
           val_ss <- basetabadc150392_2[,])

    # val_ss <- basetabadc150392_2[which(eval(parse(text= llm_object[[1]][i]))), ]
    rownbrs <- val_ss[,ncol(val_ss)]
    y_sel <- y_value[rownbrs]

    # Use the model to make a prediction on selection data.
    predLRstep <- predict(llm_object[[2]][i][[1]], newdata=val_ss[,1:(ncol(val_ss)-1)], type="response", verbose = FALSE)

    vectlen <- length(y_sel)

    # Update the prediction and class vectors
    predvector[(length(predvector)+1):(length(predvector)+vectlen)] <- predLRstep
    classvector[(length(classvector)+1):(length(classvector)+vectlen)] <- y_sel
    rowvector[(length(rowvector)+1):(length(rowvector)+vectlen)] <- rownbrs
  }

  myreturn <- as.data.frame(cbind(predvector,classvector,rowvector))
  # predLRstep <- predict(LRstep, newdata=basetabadc150392_2, type="response", verbose = FALSE)
  return(myreturn)
}
