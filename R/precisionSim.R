#' Estimate precision assurance by simulation
#'
#' `precisionSim()` estimates the probability that confidence intervals
#' for one or more user-defined estimands are narrower than specified
#' target widths.
#'
#' The function repeatedly simulates a response from a fitted linear
#' mixed-effects model, refits the model, calculates confidence intervals
#' using a user-supplied function, and compares the resulting interval
#' widths with the requested targets.
#'
#' @param fit A fitted linear mixed-effects model inheriting from
#' [lme4::lmerMod].
#'
#' @param interval_fun A function that accepts a fitted model and returns
#' a data frame containing the columns `estimand`, `estimate`, `lower`,
#' and `upper`.
#'
#' @param target_width A positive numeric vector giving the maximum
#' acceptable total confidence-interval widths.
#'
#' @param prob A single numeric value between 0 and 1 giving the target
#' probability of achieving the requested interval width.
#'
#' @param mc_conf_level A single numeric value between 0 and 1 giving the
#' confidence level used for Monte Carlo confidence intervals.
#'
#' @param nsim A positive whole number giving the number of simulations.
#'
#' @param seed An optional integer seed for reproducible simulation.
#'
#' @param singular_action A character value specifying how singular fits
#' are handled. Available options are `"include"`, `"exclude"`, and
#' `"failure"`.
#'
#' @param nonconverged_action A character value specifying how
#' non-converged fits are handled. Available options are `"include"`,
#' `"exclude"`, and `"failure"`.
#'
#' @param tol Tolerance for determining singular fit. Default is 1e-4.
#'
#' @return An object of class `"precisionSim"`. The object contains:
#'
#' * `results`: simulation-level interval results;
#' * `summary`: precision-assurance estimates;
#' * `diagnostics`: fitting and interval diagnostics;
#' * `settings`: simulation settings;
#' * `model`: model metadata;
#' * `elapsed`: elapsed computation time.
#'
#' @details
#' For each simulation, a new response is generated from `fit`. The model
#' is then refitted and `interval_fun` is applied to the fitted model.
#' Total interval width is calculated as `upper - lower`.
#'
#' The precision-assurance estimate is the proportion of simulated studies
#' in which the confidence-interval width is below `target_width`.
#'
#' @section Interval function:
#' `interval_fun` must return a data frame with one row per estimand and
#' the following columns:
#'
#' * `estimand`: a unique character label;
#' * `estimate`: the point estimate;
#' * `lower`: the lower confidence limit;
#' * `upper`: the upper confidence limit.
#'
#' @section Failed and excluded simulations:
#' Simulations classified as `"failure"` are treated as not achieving the
#' target. Simulations classified as `"exclude"` are removed from the
#' analysable denominator. The returned object reports both the requested
#' and analysable denominators.
#'
#' @seealso
#' [precisionCurve()], [simr::extend()], [stats::quantile()]
#'
#' @examples
#' \dontrun{
#' library(lme4)
#'
#' fit <- lmer(
#'   response ~ task + velocity + (1 | participant),
#'   data = example_data
#' )
#'
#' my_interval <- function(fitted_model) {
#'   # Calculate and return estimand, estimate, lower, and upper.
#' }
#'
#' result <- precisionSim(
#'   fit = fit,
#'   interval_fun = my_interval,
#'   target_width = c(10, 20, 50),
#'   nsim = 1000,
#'   seed = 123
#' )
#'
#' print(result)
#' summary(result)
#' plot(result)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
precisionSim <- function(
    fit,
    target_width,
    interval_fun,
    prob = 0.80,
    mc_conf_level = 0.95,
    nsim = 1000,
    seed = 123456,
    singular_action = "include",
    nonconverged_action = "exclude",
    tol = 1e-4
){

  #timing
  start_time <- proc.time()

  #############################################################################
  ############################### input checks ################################
  #############################################################################

  if(!requireNamespace("lme4", quietly = T)){
    stop("The lme4 package is required")
  }

  if(!requireNamespace("simr", quietly = T)){
    stop("The simr package is required")
  }

  if(!is.function(interval_fun)){
    stop("interval_fun must be a function")
  }

  if(!lme4::isLMM(fit)){
    stop("fit must be an lme4 linear mixed model")
  }

  if(any(!is.finite(target_width)) || length(target_width) == 0L || any(target_width <= 0)){
    stop("target_width must be a non-empty vector with finite values greater than 0")
  }

  if(any(!is.finite(prob)) || length(prob) == 0L || any(prob < 0 | prob > 1)){
    stop("prob must be a non-empty vector with finite values between 0 and 1")
  }

  if(mc_conf_level < 0 || mc_conf_level > 1){
    stop("mc_conf_level must be between 0 and 1")
  }

  if(nsim < 1){
    stop("nsim must be at least 1")
  }

  if(length(singular_action) != 1 || !singular_action %in% c("include", "exclude", "failure") ){
    stop("singular must be one of:", "'include', 'exclude', or 'failure'")
  }

  if(length(nonconverged_action) != 1 || !nonconverged_action %in% c("include", "exclude", "failure") ){
    stop("nonconverged must be one of:", "'include', 'exclude', or 'failure'")
  }


  #############################################################################
  ################################ initialise #################################
  #############################################################################

  #set seed
  set.seed(seed)

  #original data
  data_original <- lme4::getData(fit)

  #original response
  response_char <- stats::formula(fit)[[2]]

  #storage
  sim_results <- list()

  #diagnostics for later
  diagnostics <- data.frame(
    simulation = seq_len(nsim),
    fit_error = FALSE,
    converged = NA,
    singular = NA,
    interval_error = FALSE,
    warning_message = NA_character_,
    error_message = NA_character_
  )

  #############################################################################
  ################################ simulation #################################
  #############################################################################

  #basic loop, could be parrallelised??
  for(i in seq_len(nsim)){

    #run simulation
    sim_response <- stats::simulate(fit, nsim=1, re.form = NA)[[1]]

    #replace response
    data_sim <- data_original
    data_sim[[response_char]] <- sim_response

    #catch fit warnings
    fit_warnings <- character(0)

    #refit with error handling
    mod_sim <- tryCatch(withCallingHandlers(lme4::lmer(stats::formula(fit), data=data_sim, control = lme4::lmerControl(optimizer = fit@optinfo$optimizer)),
                                            warning = function(w){
                                              fit_warnings <<- c(fit_warnings, conditionMessage(w))
                                              invokeRestart("muffleWarning")
                                            }),
                        error = function(e){
                          diagnostics$fit_error[i] <<- TRUE
                          diagnostics$error_message[i] <<- conditionMessage(e)

                          NULL
                        }
    )

    #if null, do not compute interval_fun or anything else
    if(is.null(mod_sim)){
      next
    }

    ################################ convergence #################################

    #check convergence message (NULL if fine)
    convergence_messages <- (mod_sim@optinfo$conv$lme4$messages)

    #check optimisation convergence (0 if fine)
    optimizer_code <- (mod_sim@optinfo$conv$opt)

    #check if any issues with messages
    has_lme4_messages <- (!is.null(convergence_messages) && length(convergence_messages) > 0)

    #check if optimiser has issues
    optimizer_ok <- (length(optimizer_code) == 1 && isTRUE(optimizer_code == 0))

    #add to diagnostics
    diagnostics$converged[i] <- (optimizer_ok && !has_lme4_messages)

    #handle nonconvergence
    if(isFALSE(diagnostics$converged[i]) && nonconverged_action == "exclude"){
      next
    }

    if(isFALSE(diagnostics$converged[i]) && nonconverged_action == "failure"){
      next
    }

    ################################ singularity #################################

    #check singularity
    diagnostics$singular[i] <- lme4::isSingular(mod_sim, tol = tol)

    #handle singularity
    if(isTRUE(diagnostics$singular[i]) && singular_action == "exclude"){
      next
    }

    if(isTRUE(diagnostics$singular[i]) && singular_action == "failure"){
      next
    }

    ################################ intervals #################################

    #new intervals
    sim_intervals <- interval_fun(mod_sim)

    #check interval output from function
    required_names <- c("estimand", "estimate", "lower", "upper")

    if(!all(required_names %in% names(sim_intervals))){
      stop("interval fun must return a data frame with columns: estimand, estimate, lower, upper")
    }

    #add key diagnostics for later computation
    sim_intervals$simulation <- i
    sim_intervals$converged <- diagnostics$converged[i]
    sim_intervals$singular <- diagnostics$singular[i]

    #store intervals
    sim_results[[i]] <- sim_intervals

  }


  #get full results
  full_results <- dplyr::bind_rows(sim_results) %>%

    #compute width
    dplyr::mutate(width = .data$upper - .data$lower)

  #create a summary
  precision_summary <-  full_results %>%

    #find policy
    dplyr::mutate(

      policy_failure = (.data$singular & singular_action == "failure") | (!.data$converged & nonconverged_action == "failure"),
      policy_exclude = (.data$singular & singular_action == "exclude") | (!.data$converged & nonconverged_action == "exclude"),

      analysis_status = dplyr::case_when(
        .data$policy_failure ~ "failure",
        .data$policy_exclude ~ "exclude",
        TRUE ~ "include"
      )
    ) %>%

    #add width targets (increase rows in df if target_width > 1)
    tidyr::crossing(width_target = target_width) %>%

    #check achieved
    dplyr::mutate(achieved = dplyr::case_when(

      #count as an unsuccessful simulation
      .data$analysis_status == "failure" ~ FALSE,

      #remove from the valid denominator by converting to NA
      .data$analysis_status == "exclude" ~ NA,

      #otherwise evaluate the width criterion
      .data$analysis_status == "include" ~ .data$width < .data$width_target)) %>%

    #group by any estimands and width_targets
    dplyr::group_by(.data$estimand, .data$width_target) %>%

    #get summary
    dplyr::summarise(n_success = sum(.data$achieved, na.rm = T),
              n_valid = sum(!is.na(.data$achieved)),
              n_excluded = sum(.data$analysis_status == "exclude", na.rm = T),
              n_failure = sum(.data$analysis_status == "failure", na.rm = T),
              n_sim_requested = nsim,
              probability_valid = ifelse(.data$n_valid > 0, .data$n_success / .data$n_valid, NA),
              probability_all = .data$n_success / .data$n_sim_requested,
              n_singular = sum(.data$singular, na.rm = T),
              n_converged = sum(.data$converged, na.rm = T),
              median_width = stats::median(.data$width[.data$analysis_status == "include"], na.rm = T),
              width_at_target_prob = stats::quantile(.data$width[.data$analysis_status == "include"], probs = prob, na.rm = T)) %>%

    #ungroup
    dplyr::ungroup()  %>%

    #compute by each row
    dplyr::rowwise() %>%

    #compute the ci
    dplyr::mutate(

      #temp results (to be deleted)
      mc_ci_valid = list(
        binomial_ci(
          successes = .data$n_success,
          trials = .data$n_valid,
          conf_level = mc_conf_level
        )
      ),

      #extract ci
      mc_valid_lower = .data$mc_ci_valid[["lower"]],
      mc_valid_upper = .data$mc_ci_valid[["upper"]],

      #temp results (to be deleted)
      mc_ci_all = list(
        binomial_ci(
          successes = .data$n_success,
          trials = .data$n_sim_requested,
          conf_level = mc_conf_level
        )
      ),

      #extract ci
      mc_all_lower = .data$mc_ci_all[["lower"]],
      mc_all_upper = .data$mc_ci_all[["upper"]]
    ) %>%

    #ungroup
    dplyr::ungroup() %>%

    #get rid of these columns
    dplyr::select(
      -.data$mc_ci_valid,
      -.data$mc_ci_all
    )

  #timing
  end_time <- proc.time()
  elapsed_time <- end_time - start_time

  #create the return structure
  result <- new_precision_sim(
    call = match.call(),
    results = full_results,
    summary = precision_summary,
    diagnostics = diagnostics,
    settings = list(
      target_width = target_width,
      prob = prob,
      mc_conf_level = mc_conf_level,
      nsim = nsim,
      seed = seed,
      singular_action = singular_action,
      nonconverged_action = nonconverged_action
    ),
    model = list(
      formula = stats::formula(fit),
      class = class(fit),
      nobs = stats::nobs(fit),
      groups = lme4::ngrps(fit)
    ),
    elapsed = elapsed_time)

  #return
  return(result)

}
