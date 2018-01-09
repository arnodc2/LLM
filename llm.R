#' Create Logit Leaf Model
#'
#' This function creates the logit leaf model. It assumes a dataframe with numeric
#' values as input and a corresponding vector with dependent values.
#' Decision tree parameters threshold for pruning and number of observations per
#' leaf can be set.
#'
#' @param df dataframe containing numerical independent variables
#' @param y_value numerical vector of dependent variable. Currently only two class classification is supported
#' @param threshold_pruning threshold for impurity
#' @param nbr_obs_leaf the minimum number of observations in a leaf node
#' @return a LLM object containing decision rules that define segments and logistic regressions per segment
#' @export
#' 
#' 

llm <- function(df,y_value,threshold_pruning , nbr_obs_leaf) {
  # TODO ADD ERROR AND WARNING MESSSAGES WHEN INPUT IS INCORRECT
  
  # Custom fucntions that are used in the LLM function
  # _1_ trim.leading
  # _2_ ceiling.dec
  # _3_ dt.splitter
  
  # _1_ trim.leading
  # Trim leading white spaces
  trim.leading <- function (x)  sub("^\\s+", "", x)
  
  # _2_ ceiling.dec
  # Round numerics at certain decimals ceiling
  ceiling.dec <- function(x,level=1) round(x+5*10^(-level-1),level)
  
  # _3_ dt.splitter
  # Splis space into subspaces according to decision tree rules
  
  dt.splitter <- function(DTmodel, nom =ifelse(length(as.character(DTmodel$call$data))==1,nom <- as.character(DTmodel$call$data), nom<- as.character(DTmodel$call$data)[2])){
    # This function has two inputs:
    #       1) Decision tree model (J48)
    #       2) nom, standard value is the data used in the DT model (your train model) but you might want to change it for test set
    
    m2 <- partykit::as.party.Weka_tree(DTmodel)
    # Retrieve the rules of the Decision tree
    listrules <- partykit:::.list.rules.party(m2)
    newvar2 <- "" 
    
    # If the listrules does not contain a subtree, do nothing (a regular LR will be fitted)
    if(length(listrules) > 1) {
      for (l in 1:length(listrules)) {
        # Split them based on the & sign
        splittest <- strsplit(listrules[[l]],split = "&")
        new <- ""
        # Recode each condition in the splittest and save it in a newvar
        for (i in 1:length(splittest[[1]])) {
          splittest2 <- strsplit(trim.leading(splittest[[1]][i]),split = " ")
          new <- paste(paste0(nom,"[,'",splittest2[[1]][1], "']") , splittest2[[1]][2],"ceiling.dec(", splittest2[[1]][3],",4)", sep= " ")
          ifelse(i== 1, newvar <- new, newvar <- paste(newvar, "&" , new, sep = " "))
        }
        newvar2[l] <- newvar
      } 
    } 
    
    return(newvar2)  
  }
  
  
  # Give the dataframe generic name (necessary to split the rules for both training and test set correctly)
  # TODO find more efficient solution to partition the data
  basetabadc150392 <- df
  basetabadc150392[,(ncol(basetabadc150392)+1)] <- c(1:nrow(basetabadc150392))
  myreturn <- vector("list", 2)
  # Create DT model based on paramens
  m1 <- RWeka::J48(as.factor(as.character(y_value)) ~ .,
                   data = basetabadc150392[,1:(ncol(basetabadc150392)-1)],
                   control = RWeka::Weka_control(M = nbr_obs_leaf, C= threshold_pruning))
  # Split the dataset based on the number of "cluster" from the decision tree          
  newvar <- dt.splitter(m1, "basetabadc150392")
  
  listythelist <- vector("list",length(newvar))
  
  
  # If there length newvar > 1 : there are splits, so we do a LR for every split
  # if (length(newvar)>1) {
  for (l in 1:length(newvar)) {
    # Subset the train set based on the rule of the DT
    
    ifelse(length(newvar)>1,
           # If nbr of split is >1, than we subset according to rules DT
           train_ss <- basetabadc150392[which(eval(parse(text= newvar[l]))), ],
           # Otherwise we select the entire dataset
           train_ss <- basetabadc150392[,]
    )
    
    rownbrs <- train_ss[,ncol(train_ss)]
    y_sel <- y_value[rownbrs]
    
    # Train a LR for each subset with forward variable selection
    # build a glm model on the training data
    LR <- glm(y_sel ~ ., data=train_ss[,1:(ncol(basetabadc150392)-1)], family=binomial("logit"))
    LR1 <- glm(y_sel ~ 1, data=train_ss[,1:(ncol(basetabadc150392)-1)], family=binomial("logit"))
    
    # stepwise variable selection
    listythelist[[l]] <- step(LR1,direction="forward" ,scope = list(lower= LR1, upper = LR), trace = 0)
    
  } 
  # }  
  myreturn[[1]] <- newvar
  myreturn[[2]] <- listythelist
  
  return(myreturn)  
}
