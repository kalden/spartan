make_lhc_plot <- function(param_data, parameter, coefficient, measure, graph_format=c("PDF")) {

  # Check coefficient isn't NA
  if(!is.na(coefficient)) {
    output_graph <- ggplot(param_data,
                         aes(x = value,
                             y = as.vector(param_data[[measure]]))) +
      geom_point(size = 0.5) +
      scale_y_continuous(limits = c(
        floor(min(as.numeric(param_data[[measure]]))), ceiling(max(as.numeric(param_data[[measure]]))))) +
      labs(x = "Parameter Value", y = "Simulation Response",
           title = paste0("LHC Analysis for Parameter: ",parameter),
           subtitle = paste("Measure: ",measure, "\nCorrelation Coefficient: ", toString(signif(coefficient, 3)))) +
      theme(axis.title = element_text(size = 7),
            axis.text = element_text(size = 7),
            plot.title = element_text(size = 9, hjust = 0.5),
            plot.subtitle = element_text(size = 8, hjust = 0.5))

    walk(as.list(graph_format),save_graph_in_desired_formats,paste(parameter,measure,sep="_"),output_graph)
    #ggsave(paste(GRAPHFILE, ".pdf", sep = ""),
    #       plot = output_graph, width = 4, height = 4)

    print(output_graph)
    #return(output_graph)
  } else {
    message(paste0("For Parameter ",parameter, " Measure ",measure, " Pairing, Correlation Coefficient was reported as NA. Excluded from plotting."))
  }
}

save_graph_in_desired_formats <-function(output_type, GRAPHFILE, output_graph) {

    # Save the graphs in the requested format
    if (output_type == "PDF") {
      ggsave(paste0(GRAPHFILE, ".pdf"),
             plot = output_graph, width = 4, height = 4)
    } else if (output_type == "PNG") {
      ggsave(paste0(GRAPHFILE, ".png"),
             plot = output_graph, width = 4, height = 4)
    } else if (output_type == "TIFF") {
      ggsave(paste0(GRAPHFILE, ".tiff"),
             plot = output_graph, width = 4, height = 4)
    } else if (output_type == "BMP") {
      ggsave(paste0(GRAPHFILE, ".bmp"),
             plot = output_graph, width = 4, height = 4)
    }

}

graph_data<-lapply(lhcresult[PARAMETERS], function(x,y){bind_cols(as_data_frame(x),as_data_frame(y))}, lhcresult[MEASURES])

# Need to read in the coefficients:
load("/home/kja505/Dropbox/spartan_3.0/spartan/tests/testthat/test_cor_coeffs.Rda")
test_cor_coeffs <- test_cor_coeffs %>% column_to_rownames(var="X") %>% as_data_frame()

walk(MEASURES,make_graphs,graph_data,PARAMETERS, test_cor_coeffs)

make_graphs<-function(measure,data_to_plot,PARAMETERS, coefficients)
{
  message(paste0("Producing plots for response ",measure))
  pwalk(list(param_data=data_to_plot, parameter=PARAMETERS, coefficient=pull(coefficients,paste0(measure,"_Estimate"))),make_lhc_plot, measure)
}
