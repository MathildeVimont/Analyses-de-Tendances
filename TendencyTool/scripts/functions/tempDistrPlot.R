#' tempDistrPlot
#'
#' A function that ...
#' @param data
#' @param interestVar
#' @param siteGrouping
#'
tempDistrPlot <- function(data, interestVar, siteGrouping = FALSE){
  
  #####################################
  # EXTRACT PRESENCE / ABSENCE COUNTS #
  #####################################
  
  # Check format is data.frame
  data <- data.frame(data)
  
  # Add a column that testifies if the species was present or not
  data$presence <- 'Absence'
  data[data[,interestVar] != 0,]$presence <- 'Présence'
  
  # Count number of presence / absence observations for each year
  dataYear <- group_by(data, year, presence) %>%
    summarise(tot = n())
  
  #############
  # MAKE PLOT #
  #############
  
  # Create plot
  plot <- ggplot(dataYear, aes(x = year, y = tot, fill = presence)) +
    geom_bar(stat = "identity") +
    ylab("Nombre de points échantillonnés") +
    xlab("Années") +
    scale_fill_manual(values = c("#d7dbdd", "#16a085")) +
    theme_bw() +
    theme(legend.title = element_blank())
  
  return(plot)
} 