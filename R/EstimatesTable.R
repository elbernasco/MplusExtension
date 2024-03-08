#' @title Create Mplus Estimates Table
#'
#' @description This function takes a list of models as input (in which each list item
#' is the result of an mplus model), and creates a table for all models'
#' estimates. (Each model is below the previous.)
#'
#' @param ModelsIn List of one or more model objects resulting from MplusAutomation::readModels
#' @param std Vector of one to four standardization methods to be included ("unstd", "stdyx", "stdy" or "std").
#' @param ci Display confidence intervals? Default is False, meaning se and p values will be displayed.
#' @param ci.limit Which confidence interval limits to display ("90", "95" or "99").
#' @param one.path.col If true, path is displayed as one column. If false, three path columns are created (X, Direction and Y)
#' @param show.p Display p-values? If TRUE, p-values will be presented in a separate column
#' @param show.sign Display significance? If TRUE, significance asterisks will be displayed with the estimates.
#' @param alpha A vector of one to three numbers in descending order to assess significance levels.
#' @param decimals A single number indicating to how many decimals should be rounded ( applies to all numbers except p-values)
#' @param p.decimals A single number indicating to how many decimals p values should be rounded.
#'
#' @details The standardization methods (std) are based on those exported by Mplus. The options are:
#' (1) "unstd": provides unstandardized estimates;
#' (2) "stdyx": provides stdyx-standardized estimates;
#' (3) "stdy": provides stdy-standardized estimates;
#' (4)"std": provides std-standardized estimates.
#'  By default, Mplus output only shows unstandardized estimates. Use the command "Output: standardized;"
#'  in Mplus to request all three standardization methods. Note that if you enter an invalid standardization method,
#'  or request standardized estimates where there were none in the original output file, you will receive an error.
#'
#'  The options for confidence intervals are based on those exported by Mplus. The options are:
#'  90%, 95% or 99% confidence intervals. By default, Mplus output does not show confidence intervals.
#'  Use the command "Output: cinterval;" in Mplus to request confidence intervals. Note that if you
#'  enter invalid limits, or request confidence intervals where there were none in the original output file,
#'  you will receive an error.
#'
#' @examples
#' M1output <- readModels("M1.out") # Uses the MplusAutomation package
#' M1estimates <- EstimatesTable(M1output)
#'
#' # Create a table with stdyx-standardized estimates and p values in a separate column
#' M1estimates <- EstimatesTable(M1output, std = "stdyx", show.p = T, show.sign = F)
#'
#' # Create a table with unstandardized and standardized estimates
#' M1estimates <- EstimatesTable(M1output, std = c("unstd","stdyx"))
#'
#' # Create a table with 95% confidence intervals
#' M1estimates <- estimatesTable(M1output, std="unstd", ci=T, ci.limit="95")
#'
#' @export
EstimatesTable <- function(ModelsIn, std="unstd",
                           ci = F, ci.limit = "", one.path.col = T,
                           show.p = F, show.sign = T,
                           alpha = c(.001, .01,.05),
                           decimals = 2, p.decimals = 3) {


  # First checks --------------------------------------------------------------

  # Init names for standardization method. (Used for checks, loops and naming.)
  std.options <- c("unstd","stdyx","stdy","std")

  # Init final df with parameters
  AllModels <- data.frame()

  # If any incorrect standardization method was requested: Error.
  if (any(!(std %in% std.options)))
    stop("Incorrect standardization method was provided.")

  # If CI is requested AND p specifications are made: Warning.
  if (ci == T) {
    if (show.p == T | show.sign == T | p.decimals == T)
      warning("You tried to specify show.p, show.sign, or p.decimals while requesting confidence intervals.")
  }

  # Prepare the number of asterisks (*) depending on how many alpha levels are
  # provided. If more than 3 levels are provided, or they are not in descending
  # order: Error.
  if (length(alpha) > 3) {
    # Error if more than 3 alpha levels
    stop("Error: alpha includes too many (>3) levels")
  } else if (length(alpha) == 1) {
    # If 1 alpha level, 1 astersks
    asterisks = c("*", "")
  } else if (length(alpha) == 2) {
    # If 2 alpha levels, 2 asterisks + error if they are not descending
    if (alpha[1] > alpha[2]) {
      stop("Error: alpha should be descending")}
    asterisks = c("**","*", "")
  } else if(length(alpha) == 3) {
    # If 3 alpha levels, 3 asterisks + error if not descending
    if (alpha[1] > alpha[2] | alpha[2] > alpha[3]) {
      stop("Error: alpha should be descending")}
    asterisks = c("***","**","*", "")}

  # Detect if only one model is imputed
  SingleModel <- (names(ModelsIn)[1] == "input")

  if (SingleModel) {
    # If only one model is used as input, first turn that into a list with one
    # Model inside
    SingleModelList <- list()
    SingleModelList[[1]] <- ModelsIn
    ModelsIn <- SingleModelList
  }


  # Loop over all models------------------------------------------------------

  # Loop over all models inputted into the function
  for (i in 1:length(ModelsIn)) { # Do this for all models
    # Temporarily save ALL of the current model's parameters
    ModelPar <- ModelsIn[[i]]$parameters

    if (ci == T & !any(grepl("ci", names(ModelPar)))) {
      stop("Confidence intervals are requested but are not in Mplus output file.")
    }


    if (any(!(std == "unstd")) & !any(!grepl("unstandardized", names(ModelPar)))) {
      stop("Standardized estimates are requested but are not in Mplus output file.")
    }

    # Save all different est types in different sublists
    if(ci == T) {std.params <- list(
      unstd = ModelPar$ci.unstandardized,
      stdyx = ModelPar$ci.stdyx.standardized,
      stdy = ModelPar$ci.stdy.standardized,
      std = ModelPar$ci.std.standardized)
    } else {std.params <- list(
      unstd = ModelPar$unstandardized,
      stdyx = ModelPar$stdyx.standardized,
      stdy = ModelPar$stdy.standardized,
      std = ModelPar$std.standardized)}


    # init a matrix that contains all SELECTED parameters
    # (i.e., stdyx, unstandardized, etc.)
    AllParams <- matrix()


    # Loop over all estimate types --------------------------------------------

    # For each of the 4 est types (loop over j):
    # save estimates, change names, then merge with previous ests
    for (j in 1:4){
      if (any(std == std.options[j])){ # only do this for chosen ests
        ParamSet <- std.params[[j]]


        # Round estimate
        ParamSet <- dplyr::mutate(ParamSet,
                                  estimate = format(round(est,2),nsmall = decimals))


        # a) Steps if ci = F only ---------------------------------------------

        if (ci == F) {
          # Round SE (only generated if ci=F)
          ParamSet <- dplyr::mutate(ParamSet, se = format(round(se, decimals), nsmall = decimals))
          # Remove est_se (only generated if ci=F)
          ParamSet <- dplyr::select(ParamSet, -est_se)

          if (show.sign == T) {
            # If user wants to show significance, Est = estimate + stars
            ParamSet <- dplyr::mutate(ParamSet,
                                      # Change the p values into signifance (N/A if ci = T)
                                      p.sign = cut(as.numeric(ParamSet$pval),
                                                   breaks=c(-Inf, alpha, Inf),
                                                   labels=asterisks),
                                      # Combine estimates + significance
                                      Est = paste(estimate, p.sign))
            # Remove tmp vars (separate est/sign vars)
            ParamSet <- dplyr::select(ParamSet, -c(p.sign, estimate))
          } else {
            # If user does not want to show significance, Est = estimate only
            ParamSet <- dplyr::rename(ParamSet, Est = estimate)}

          if (show.p == T) {
            # If user wants to show pval column, create p
            ParamSet <-
              # create + round p values
              dplyr::mutate(ParamSet, p = format(round(pval, p.decimals), nsmall = p.decimals))
          }
          # Remove pval (tmp var)
          ParamSet <- dplyr::select(ParamSet, -pval)


        } else {

          # b) Steps if ci = F only -------------------------------------------

          # If user request CI-estimates: Select + round correct limits

          # 1. Select UL/LL based on CI% user selects (90/95/99)
          if (ci.limit == "90"){ParamSet <- dplyr::mutate(ParamSet, LL = low5, UL = up5)}
          else if (ci.limit == "95"){ParamSet <- dplyr::mutate(ParamSet, LL = low2.5, UL = up2.5)}
          else if (ci.limit == "99"){ParamSet <- dplyr::mutate(ParamSet, LL = low.5, UL = up.5)}
          # If none of these were chosen: Error.
          else stop("Error: incorrect ci limits specified. Choose 90, 95 or 99.")

          # Round UL/LL
          ParamSet <- dplyr::mutate(ParamSet,
                                    LL = format(round(ParamSet$LL, decimals), nsmall = decimals),
                                    UL = format(round(ParamSet$UL, decimals), nsmall = decimals))

          # Rename estimate var + delete redundant LL/UL vars
            # Est = estimate only (no sign, because ci = T)
          ParamSet <- dplyr::rename(ParamSet,Est = estimate)
            # Remove unused limits
          ParamSet <- dplyr::select(ParamSet, -c(low.5, low2.5,  low5, up5, up2.5, up.5))}


        # c) Path variables ---------------------------------------------------

        # Create and relocate paths vars
        ParamSet <- dplyr::mutate(ParamSet,
                        # Create 3 variables to indicate the estimated path
                        X = paste0("  ", param), # indent param (= X var)
                        Direction = gsub(".*\\.","", paramHeader), # delete all text before and including the dot
                        Y = gsub("\\..*","",paramHeader), # delete all text after and including the dot
                        Path = paste(param, Direction, Y)) # create one path col
         ParamSet <- dplyr::relocate(ParamSet, c(Path, X, Direction, Y, Est)) # reorder new vars
          # Always delete paramHeader, param, est (they were temporary variables)
          ParamSet <- dplyr::select(ParamSet, -c(paramHeader, param, est))

        # If user wants to have the path in one column, remove the three separate
        # columns (X, Direction, Y)
        if (one.path.col == T){
          ParamSet <- dplyr::select(ParamSet, -c(X, Direction, Y))
        } else {
          # If user wants to have the path in three columns, remove the combined
          # path variable (Path)
          ParamSet <- dplyr::select(ParamSet, -Path)
        }

        # d) End of loop over all estimates types -----------------------------

        # Add the estimate type to the beginning of all var names
        # e.g. "unstd_Est" = Estimate for the unstandardized parameters
        names(ParamSet) <- paste0(std.options[j],"_", names(ParamSet)) # append est type

        # Combine current type of parameter (= ParamSet) with all other
        # types of parameters/estimates (=AllParams)
        AllParams <- cbind(AllParams, ParamSet)
      } # end of if statement ( if any(std == std.options[j]) )
    } # end of loop over est types ( for (j in 1:4) )


    # End of loop iver all models ---------------------------------------------

    # Add current scriptname + count to the model
    AllParams <- dplyr::mutate (AllParams,
                                ScriptName = ModelsIn[[i]]$input$title,
                                ScriptCount = i)

    # Combine rows already in the df (ParamSet) with new estimates (AllParams)
    AllModels <- rbind(AllModels, AllParams)

  } # End of loop over models




  # Post merge edits -----------------------------------------------------------

  # If there is only one model in the input, remove ScriptName and Scriptcount
  # (they are the same across all rows)
  if (SingleModel == T){
    AllModels <- dplyr::select(AllModels, -c(ScriptName, ScriptCount))
  }

  # Remove the "AllParams" variable that was redundantly created
  AllModels <- dplyr::select(AllModels, -AllParams)


  # If user requests multiple standardization methods, multiple path vars are
  # created as well. Here, we delete those.
  if (one.path.col == T){
    # If user requests 1 path column, we rename the first one and delete
    # all other Path copies
    colnames(AllModels)[1] <- "Path"
    AllModels <- dplyr::select(AllModels, -ends_with("_Path"))
  } else {
    # If user requests 1 path column, we rename the first three vars and delete
    # all other X/Y/Direction copies
    colnames(AllModels)[1:3] <- c("X", "Direction", "Y")
    AllModels <- dplyr::select(AllModels, -ends_with("_X"), -ends_with("_Y"),-ends_with("_Direction"))
  }

  # If user requests multiple standardization methods, multiple Group vars are
  # created as well. Here, we delete those.
  if (any(grepl("_Group", names(AllModels)))) {
    colnames(AllModels)[dim(AllModels)[2]] <- "Group"
    AllModels <-
      dplyr::select(AllModels, -ends_with("_Group"))
  }

  # Replace WITH, ON, BY statements with arrows (aesthetic edit)
  if (one.path.col==T) {
    AllModels <-
      dplyr::mutate(AllModels,
                    Path = gsub("WITH", "\U2194", Path), # double headed arrow
                    Path = gsub("ON", "\U21FE", Path), # arrow pointing right
                    Path = gsub("BY", "\U2190", Path)) # arrow pointing left
  } else {
    AllModels <-
      dplyr::mutate(AllModels,
                    Direction = gsub("WITH", "\U2194", Direction), # double headed arrow
                    Direction = gsub("ON", "\U21FE", Direction), # arrow pointing right
                    Direction = gsub("BY", "\U2190", Direction)) # arrow pointing left
  }

  # Pivot multigroup models ---------------------------------------------------------

  # If any of the column names are "Group" (i.e., it is a multigroup model)
  # Go from wide to long format
  if (any(colnames(AllModels) == "Group")) {

    # Create a vector containing all value variable (i.e., excluding the group
    # and path variables)
    if (one.path.col == T) {values = colnames(dplyr::select(AllModels, -c(Group, Path)))
    } else {values = colnames(dplyr::select(AllModels, -c(Group, X, Direction, Y)))}

    # Transpose data from long to wide format
    AllModels <- reshape(data = AllModels, direction = "wide",
                         idvar = "Path", timevar = "Group",
                         v.names = values)

  } # end of if group statement

  return(AllModels)
} # End of EstimatesTable function
