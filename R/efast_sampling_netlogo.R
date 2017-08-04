#' Prepares Netlogo experiment files for a variance-based sensitivity analysis,
#' using eFAST
#'
#' Creates a set of parameter values, over the specified value space, using the
#' sampling method described in the eFAST technique. Then processes each of
#' these into a Netlogo experiment XML file, from which a simulation can be run.
#'
#' @param FILEPATH Directory where the parameter samples and XML models are to
#' be stored
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3.
#' @param NUMSAMPLES The number of parameter subsets to be generated for each
#' curve in the eFAST design.
#' @param MEASURES Array containing the names of the Netlogo output measures
#' which are used to analyse the simulation.
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param PARAMVALS Array containing either the parameter value (if not of
#' interest in the analysis), or range under which this is being explored
#'  (stated as as string e.g. "[5,50,5]" for a range of 5-50, increment of 5).
#'  The reader should read the relevant tutorial in detail.
#' @param EXPERIMENT_REPETITIONS The number of times Netlogo should repeat the
#' experiment for each set of parameter values.
#' @param RUNMETRICS_EVERYSTEP Boolean stating whether Netlogo should produce
#' output for each timestep.
#' @param NETLOGO_SETUP_FUNCTION The name of the function in Netlogo that sets
#' up the simulation. Commonly is named setup.
#' @param NETLOGO_RUN_FUNCTION The name of the function in Netlogo that starts
#' the simulation. Commonly named go.
#'
#' @export
efast_generate_sample_netlogo <-
  function(FILEPATH, NUMCURVES, NUMSAMPLES, MEASURES, PARAMETERS, PARAMVALS,
           EXPERIMENT_REPETITIONS, RUNMETRICS_EVERYSTEP,
           NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION) {

  if (requireNamespace("XML", quietly = TRUE)) {
    if (file.exists(FILEPATH)) {
      MIN_ARRAY <- NULL
      MAX_ARRAY <- NULL

      # Firstly we need to calculate the number of parameters being varied
      # Do this by detecting the number that are in square brackets and
      # have min & max separated by commas
      NUMPARAMS <- 0
      PARAMSOFINTEREST <- NULL
      for (PARAM in 1:length(PARAMETERS)) {
        PARAMVALSPLIT <- strsplit(PARAMVALS[PARAM], ",")[[1]]

        if (length(PARAMVALSPLIT) > 1) {
          NUMPARAMS <- NUMPARAMS + 1
          MIN <- as.numeric(substring(PARAMVALSPLIT[[1]], 2))
          MIN_ARRAY <- cbind(MIN_ARRAY, MIN)
          MAX <- as.numeric(substring(PARAMVALSPLIT[[2]], 1,
                                      nchar(PARAMVALSPLIT[[2]]) - 1))
          MAX_ARRAY <- cbind(MAX_ARRAY, MAX)

          PARAMSOFINTEREST <- cbind(PARAMSOFINTEREST, PARAMETERS[PARAM])
        }
      }

      wanted_n <- NUMSAMPLES * NUMPARAMS * NUMCURVES

      # OUTPUT
      # SI[] : first order sensitivity indices
      # STI[] : total effect sensitivity indices
      # Other used variables/constants:
      # OM[] : vector of k frequencies
      # omi : frequency for the group of interest
      # omci[] : set of freq. used for the compl. group
      # X[] : parameter combination rank matrix
      # AC[],BC[]: fourier coefficients
      # FI[] : random phase shift
      # V : total output variance (for each curve)
      # VI : partial var. of par. i (for each curve)
      # VCI : part. var. of the compl. set of par...
      # AV : total variance in the time domain
      # AVI : partial variance of par. i
      # AVCI : part. var. of the compl. set of par.
      # Y[] : model output

      MI <- 4  # maximum number of fourier coefficients
      # that may be retained in calculating the partial
      # variances without interferences between the
      # assigned frequencies

      # Computation of the frequency for the group
      # of interest omi and the # of sample points NUMSAMPLES
      # (here N=NUMSAMPLES)
      omi <- floor(((wanted_n / NUMCURVES) - 1) / (2 * MI) / NUMPARAMS)
      NUMSAMPLES <- 2 * MI * omi + 1
      if (NUMSAMPLES * NUMCURVES < 65) {
        print("Error: sample size must be >= 65 per factor")
      }

      PARAMETERVALS <- array(0, dim = c(NUMSAMPLES, NUMPARAMS, NUMPARAMS,
                                        NUMCURVES))

      for (PARAMNUM in 1:NUMPARAMS) {
        # Algorithm for selecting the set of frequencies.
        # omci(i), i=1:k-1, contains the set of frequencies
        # to be used by the complementary group.

        omci <- efast_setfreq(NUMPARAMS, omi / 2 / MI, PARAMNUM)
        OM <- array(0, dim = c(1, NUMPARAMS, 1))

        # Loop over the NUMCURVES search curves.
        for (CURVENUM in 1:NUMCURVES) {
          # Setting the vector of frequencies OM
          # for the k parameters
          cj <- 1
          for (j in 1:NUMPARAMS) {
            if (j == PARAMNUM) {
              OM[PARAMNUM] <- omi;
            } else {
              OM[j] <- omci[cj]
              cj <- cj + 1
            }
          }

          # Setting the relation between the scalar
          # variable S and the coordinates
          # {X(1),X(2),...X(k)} of each sample point.
          FI <- array(runif(NUMPARAMS, min = 0, max = 1),
                      dim = c(NUMPARAMS, 1, 1))
          FI <- FI * 2 * pi

          S_VEC <- pi * (2 * (1:NUMSAMPLES) - NUMSAMPLES - 1) / NUMSAMPLES
          OM_VEC <- OM[1:NUMPARAMS]

          FI_MAT <- array(0, dim = c(NUMPARAMS, NUMSAMPLES, 1))

          for (i in 1:NUMSAMPLES) {
            FI_MAT[, i, 1] <- FI
          }

          om_vec_svec <- array(OM_VEC %*% t(S_VEC),
                               dim = c(NUMPARAMS, NUMSAMPLES, 1))
          ANGLE <- om_vec_svec + FI_MAT

          # TRANSPOSE ARRAY
          ANGLET <- array(0, dim = c(NUMSAMPLES, NUMPARAMS, 1))
          for (i in 1:NUMSAMPLES) {
            ANGLET[i, , 1] <- ANGLE[, i, 1]
          }

          # NOW CALCULATE THE PARAMETER VALUES - THESE ARE STORED IN A
          #MULTIDIMENSIONAL ARRAY, AS EACH CURVE HAS SEVEN SETS OF PARA VALUES
          PARAMETERVALS[, , PARAMNUM, CURVENUM] <- 0.5 + asin(sin(ANGLET)) / pi

          # AS THESE VALUES WILL CURRENTLY BE BETWEEN 0 AND 1,
          # TRANSFORM THE DISTRIBUTION TO GIVE TRUE PARAMETER VALUES
          PARAMETERVALS[, , PARAMNUM, CURVENUM] <- efast_parameterdist(
            PARAMETERVALS[, , PARAMNUM, CURVENUM], MAX_ARRAY, MIN_ARRAY,
            NUMSAMPLES, NUMPARAMS)
        }
      }

      # NOW ALL WE NEED TO DO IS CREATE THE NETLOGO EXPERIMENT FILES FOR
      # EACH OF THE CURVES, AND EACH OF THE PARAMETERS
      for (CURVENUM in 1:NUMCURVES) {
        # CREATE A FOLDER TO STORE THE SETUP FILE FOR THIS CURVE
        dir.create(file.path(FILEPATH, CURVENUM), showWarnings = FALSE)

        for (PARAMNUM in 1:NUMPARAMS) {
          # CREATE A FOLDER TO STORE THE SETUP FILE FOR THIS PARAMETER
          dir.create(file.path(paste(FILEPATH, "/", CURVENUM, sep = ""),
                               PARAMNUM), showWarnings = FALSE)

          for (SAMPLE in 1:NUMSAMPLES) {
            # CREATE A FOLDER FOR THIS EXPERIMENT
            dir.create(file.path(paste(FILEPATH, "/", CURVENUM, "/", PARAMNUM,
                                       sep = ""), SAMPLE),
                       showWarnings = FALSE)

            # NOW FOR NETLOGO, WE NEED TO SPECIFY THE CONSTANT PARAMETERS AS
            # WELL AS THOSE DERIVED THROUGH eFAST
            # INITIALISE THE XML FILE
            xml <- XML::xmlOutputDOM(tag = "experiments")

            # NEXT TAG IN IS EXPERIMENT
            xml$addTag("experiment", attrs = c(
              name = paste("EFAST_Sample", SAMPLE, sep = ""),
              repetitions = EXPERIMENT_REPETITIONS,
              runMetricsEveryStep = RUNMETRICS_EVERYSTEP),
              close = FALSE)

            # THE PROCEDURES TO CALL SETUP, GO, AND OUTPUT MEASURES TO ANALYSE
            xml$addTag("setup", NETLOGO_SETUP_FUNCTION)
            xml$addTag("go", NETLOGO_RUN_FUNCTION)

            for (MEASURE in 1:length(MEASURES)) {
              xml$addTag("metric", MEASURES[MEASURE])
            }


            # WITH THERE BEING MORE PARAMETERS THAN THOSE OF INTEREST, WE KEEP
            # TRACK OF THE COLUMN NUMBER OF THE EFAST RESULT
            # WE ARE TAKING AS THE PARAMETER VALUE
            # THIS IS INCREASED AS WE WORK THROUGH PARAMETERS OF INTEREST
            EFAST_COL_REF <- 1

            # DO THE PARAMETERS
            for (PARAM in 1:length(PARAMETERS)) {
              # SPLIT THE VALUES OF THIS PARAMETER BY THE COMMA. IF NO COMMA,
              # THIS IS NOT A PARAMETER OF INTEREST
              # AND IS OF STATIC VALUE
              PARAMVALSPLIT <- strsplit(PARAMVALS[PARAM], ",")[[1]]

              if (length(PARAMVALSPLIT) == 1) {
                # JUST GET THE VALUE - WE ADD THIS TO THE XML LATER
                VALUE <- PARAMVALS[PARAM]
              } else {
                # NOW THIS IS A PARAMETER OF INTEREST, AND THUS THE VALUE NEEDS
                #TO BE TAKEN FROM THE EFAST CURVE
                VALUE <- PARAMETERVALS[SAMPLE, EFAST_COL_REF, PARAMNUM,
                                       CURVENUM]
                EFAST_COL_REF <- EFAST_COL_REF + 1
              }

              # ADD THE TAG FOR THIS PARAMETER
              # NOW CREATE THE XML FOR THIS PARAMETER
              xml$addTag("enumeratedValueSet", attrs = c(
                variable = PARAMETERS[PARAM]),
                close = FALSE)

              # NOW ADD THE VALUE
              xml$addTag("value", attrs = c(value = VALUE))

              # CLOSE THE ENUMERATED VALUE SET TAG
              xml$closeTag()
            }

            # CLOSE THE EXPERIMENT TAG
            xml$closeTag()

            # CLOSE THE EXPERIMENTS TAG
            xml$closeTag()

            # SAVE THE XML FILE IN THE FOLDER FOR THIS EXPERIMENT
            XML::saveXML(
              xml, file = paste(FILEPATH, "/", CURVENUM, "/", PARAMNUM, "/",
                                SAMPLE, "/efast_analysis_set", SAMPLE, ".xml",
                                sep = ""), indent = TRUE,
              prefix = '<?xml version="1.0" encoding="us-ascii"?>\n',
              doctype = '<!DOCTYPE experiments SYSTEM "behaviorspace.dtd">')
          }

          print(paste("Parameter Samples for Curve ", CURVENUM, " Parameter ",
                      PARAMNUM, " Complete", sep = ""))

        }

        print(paste("Curve ", CURVENUM, " Complete", sep = ""))
      }

      # AS WITH TRADITIONAL SPARTAN, IT IS USEFUL TO ALSO HAVE A CSV
      # FILE OF PARAMETER DATA FOR EACH CURVE
      # NOW OUTPUT THE RESULTS - SPLIT BY CURVE FILE
      # SO, WILL HAVE ONE FILE FOR EACH PARAMETER OF INTEREST, FOR EACH CURVE
      for (CURVENUM in 1:NUMCURVES) {
        for (PARAMNUM in 1:NUMPARAMS) {
          parameter_file <- paste(FILEPATH, "/Curve", CURVENUM, "_",
                                 PARAMSOFINTEREST[PARAMNUM], ".csv", sep = "")
          output_params <- PARAMETERVALS[, , PARAMNUM, CURVENUM]
          colnames(output_params) <- c(PARAMSOFINTEREST)

          write.csv(output_params, parameter_file, quote = FALSE,
                    row.names = FALSE)

          print(paste("Parameter Set for ", CURVENUM,
                      " Generated and Output to ", FILEPATH, "/Curve",
                      CURVENUM, "_", PARAMETERS[PARAMNUM], ".csv",
                      sep = ""))
        }
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No parameter samples generated")
    }
  } else {
    print("efast_generate_sample_netlogo requires the XML package")
  }
}
