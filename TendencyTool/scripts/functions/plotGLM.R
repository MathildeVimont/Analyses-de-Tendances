#' plotGLM
#' 
#' A function that plots the estimates and error bars of the formatted model estimates
#' 
#' @param summary a `data.frame` that is the formatted summary output of the model
#' @param effect a `string` that is the explanatory variable we're interested in
#' @param distribution a `string` containing the chosen distribution
#' @return 
#' 
#' @example
plotGLM <- function(summary, effect, distribution){
  
  ########################
  # Dataframe formatting #
  ########################
  
  # Filter summary for the requested effect
  summary <- summary[grep(effect,rownames(summary)),]
  
  # Add an effect column
  summary[,effect] <- rownames(summary)
  
  # Reformat the effect column
  summary[,effect] <- str_replace(string = summary[,effect],
                                  pattern = paste0(effect, " : "),
                                  replacement = "")
  
  summary[,effect] <- as.factor(summary[,effect])
  
  #############################
  # Distribution implications #
  #############################
  yInt <- 0
  
  # If gaussian distribution 
  # The reference value is 0
  if (distribution == "gaussian"){
    yInt <- 0
  }
  # If poisson distribution 
  # The reference value is exp(0) i.e, 1
  else if(distribution == "poisson"){
    yInt <- 1  
  }
  # If binomial distribution
  # The reference value is 1/(1+exp(0)) i.e, 1/2
  else if (distribution == "binomial"){
    yInt <- 0.5
  }
  
  ########
  # Plot #
  ########
  plotGLM <- ggplot(summary, aes(x = get(effect), 
                                 y = as.numeric(Estimates), 
                                 shape = Significant)) +
    
    # Add points to the graphic
    geom_point(colour = "orange") +
    
    # Add error bars to each point
    geom_errorbar(aes(ymin = as.numeric(IC_inf), 
                      ymax = as.numeric(IC_sup)), 
                  colour = "orange") +
    
    # Change visual theme (white square)
    theme_bw() +
    
    # Rotate x axis text
    theme(axis.text.x = element_text(angle = 45)) +
    
    # Rename x label
    xlab(effect) +
    
    # Rename y label
    ylab("Estimates") +

    # Add an horizontal line 
    geom_hline(yintercept = yInt, linetype = "dashed", color = "darkgray")
  
    return(plotGLM)
}