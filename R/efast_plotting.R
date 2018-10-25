#' Plot the parition of variance in a simulation response for each measure
#'
#' @param RESULTS_FILE_PATH Where the eFAST results were saved to
#' @param PARAMETERS Simulation parameters being explored
#' @param si Vector of Si values calculated in eFAST for all parameters
#' @param sti Vector of STi values calculated in eFAST for all parameters
#' @param errors_si Vector of confidence intervals for Si values for all
#' parameters
#' @param errors_sti Vector of confidence intervals for STi values for all
#' parameters
#' @param MEASURES Simulation output measures
#' @param TIMEPOINT Timepoint being analysed
#' @param TIMEPOINTSCALE Scale in which the timepoints are measures
#' @param output_types Files types of graph to produce (pdf,png,bmp etc)
#'
#' @export
efast_graph_Results <- function(RESULTS_FILE_PATH, PARAMETERS, si, sti,
                                errors_si, errors_sti, MEASURES, TIMEPOINT,
                                TIMEPOINTSCALE, output_types=c("pdf")) {


    for (MEASURE in 1:length(MEASURES)) {

      # October 2018 - rewritten to use ggplot2
      si_results<-data.frame(rep("Si",length(PARAMETERS)),PARAMETERS,si[,,MEASURE], (as.numeric(si[,,MEASURE]) + as.numeric(errors_si[,MEASURE])),stringsAsFactors = FALSE)
      colnames(si_results)<-c("Statistic","Parameter","Sensitivity","Error")
      sti_results<-data.frame(rep("STi",length(PARAMETERS)),PARAMETERS,sti[,,MEASURE], (as.numeric(sti[,,MEASURE]) + as.numeric(errors_sti[,MEASURE])),stringsAsFactors = FALSE)
      colnames(sti_results)<-c("Statistic","Parameter","Sensitivity","Error")

      # Merge
      graph_frame <- rbind(si_results,sti_results)

      for(out in output_types)
      {
        ggplot2::ggplot(data=graph_frame, aes(x=graph_frame$Parameter, y=as.numeric(graph_frame$Sensitivity), fill=graph_frame$Statistic)) +
          ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) + ggplot2::scale_fill_manual("", values = c("Si" = "black", "STi" = "darkgray")) +
          theme(axis.text.x = element_text(angle = 65, hjust = 1, size=ggplot2::rel(0.75))) +
          ggplot2::ggtitle(paste0("Partitioning of Variance in Simulation Results\n Measure: ",MEASURES[MEASURE])) + ggplot2::theme(plot.title = element_text(hjust = 0.5, size=ggplot2::rel(0.75))) +
          ggplot2::geom_errorbar(aes(ymin=as.numeric(graph_frame$Sensitivity), ymax=as.numeric(graph_frame$Error)), width=.2, position=ggplot2::position_dodge(.9)) + ggplot2::ylim(0,1) +
          ggplot2::xlab("Parameter")+ggplot2::ylab("Sensitivity")

        ggplot2::ggsave(file.path(RESULTS_FILE_PATH,paste0(MEASURES[MEASURE],TIMEPOINT,".",out)))
      }
    }
    message(paste("Graphs Output to ", RESULTS_FILE_PATH, sep = ""))

}

#' Plot the Si value for all parameters for multiple simulation timepoints
#'
#' Permits easy comparison of when a parameter may become more influential
#' than others throughout a simulation timecourse
#'
#' @param FILEPATH Where the eFAST results have been stored
#' @param PARAMETERS Names of simulation parameters being explored
#' @param MEASURES Names of simulation output responses
#' @param EFASTRESULTFILENAME Name of the CSV file output by eFAST Analysis,
#' containing all the Si and STi values
#' @param TIMEPOINTS Timepoints to include in this analysis
#' @param TIMEPOINTSCALE Scale in which the timepoints are measured
#'
#' @export
ploteFASTSiFromTimepointFiles <- function(FILEPATH, PARAMETERS, MEASURES,
                                          EFASTRESULTFILENAME, TIMEPOINTS,
                                          TIMEPOINTSCALE) {
  for (m in 1:length(MEASURES)) {
    MEASURE <- MEASURES[m]
    # Add si onto the measure to get this from the result set
    MEASURELABEL <- paste(MEASURE, "_Si", sep = "")

    si_measureresults <- data.frame()


    for (i in 1:length(TIMEPOINTS)) {
      hour <- TIMEPOINTS[i]

      # Add the timepoint onto the end of the filename
      efastresultfilename_format <- check_file_extension(EFASTRESULTFILENAME)
      EFASTRESULTFILENAME_FULL <- paste(substr(EFASTRESULTFILENAME, 0,
                                             nchar(EFASTRESULTFILENAME) - 4),
                                      "_", hour, ".",
                                      efastresultfilename_format, sep = "")


      # READ IN THE TIMEPOINT DATA
      efast_results <- read.csv(paste(FILEPATH, "/", EFASTRESULTFILENAME_FULL,
                                     sep = ""), header = T)

      TIMERESULT <- data.frame(hour, t(efast_results[, MEASURELABEL]))
      si_measureresults <- rbind(si_measureresults, TIMERESULT)
    }

    colnames(si_measureresults) <- c(TIMEPOINTSCALE, PARAMETERS)

    # PLOT THE GRAPH
    GRAPHFILE <- paste(FILEPATH, "/", MEASURE, "_OT.pdf", sep = "")
    pdf(GRAPHFILE, width = 7, height = 7.8)

    GRAPHTITLE <- paste("eFAST First Order Sensitivity Indexes Over Simulation
                        Time\nCell Response Measure: ", MEASURE, sep = "")

    plot(TIMEPOINTS, si_measureresults[, 2], main = GRAPHTITLE, type = "o",
         lty = 1, ylim = c(0, 1), pch = 1, xaxt = "n", xlab = TIMEPOINTSCALE,
         ylab = "eFAST First-Order Sensitivity Index (Si)")

    # -1 TO EXCLUDE DUMMY
    for (l in 2:length(PARAMETERS) - 1) {
      lines(TIMEPOINTS, si_measureresults[, l + 1], type = "o", lty = 5,
            pch = l)
    }

    axis(1, at = seq(as.numeric(min(TIMEPOINTS)), as.numeric(max(TIMEPOINTS)),
                     by = as.numeric(max(TIMEPOINTS)) / length(TIMEPOINTS)))
    legend("topleft", inset = .0, title = "Parameter",
           PARAMETERS[1:length(PARAMETERS) - 1],
           pch = 1:length(PARAMETERS) - 1, cex = 0.75)

    dev.off()
  }
}
