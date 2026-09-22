#' Re-evaluate precision assurance
#'
#' `assurance_test()` re-evaluates stored confidence-interval widths
#' against new assurance settings without rerunning the original model
#' simulations.
#'
#' This is an S3 generic with methods for objects of class
#' `"precisionSim"` and `"precisionCurve"`.
#'
#' @param x An object of class `"precisionSim"` or `"precisionCurve"`.
#'
#' @param prob A single numeric value between 0 and 1 specifying the
#'   target assurance probability. By default, the value stored in `x`
#'   is used.
#'
#' @param target_width A positive numeric vector containing the
#'   confidence-interval width targets. By default, the targets stored
#'   in `x` are used.
#'
#' @param mc_conf_level A single numeric value between 0 and 1 specifying
#'   the confidence level for the Monte Carlo confidence intervals. By default,
#'   the value stored in `x` is used.
#'
#' @param singular_action A character value specifying how singular fits
#'   are handled. Available options are `"include"`, `"exclude"`, and
#'   `"failure"`. By default, the value stored in `x` is used.
#'
#' @param nonconverged_action A character value specifying how
#'   non-converged fits are handled. Available options are `"include"`,
#'   `"exclude"`, and `"failure"`. By default, the value stored in `x` is used.
#'
#' @param ... Additional arguments passed to class-specific methods.
#'
#' @return An object of class `"precisionSim"` or `"precisionCurve"`. The object contains:
#'
#' * `results`: simulation-level interval results;
#' * `summary`: precision-assurance estimates;
#' * `diagnostics`: fitting and interval diagnostics;
#' * `settings`: simulation settings;
#' * `model`: model metadata;
#' * `elapsed`: elapsed computation time.
#'
#' @details
#' `assurance_test()` uses the simulation-level intervals already stored
#' in `x`. It does not simulate new responses or refit the original model.
#'
#' For each estimand and width target, assurance is calculated as the
#' proportion of simulations whose confidence-interval width is less than
#' the target width, after applying the requested rules for singular and
#' non-converged fits.
#'
#' @seealso
#' [precisionSim()], [precisionCurve()]
#'
#' @examples
#' \dontrun{
#' sim_result <- precisionSim(...)
#'
#' assurance_test(
#'   sim_result,
#'   target_width = c(10, 20, 50),
#'   prob = 0.80
#' )
#'
#' curve_result <- precisionCurve(...)
#'
#' assurance_test(
#'   curve_result,
#'   target_width = c(10, 20, 50)
#' )
#' }
#'
#' @export
assurance_test <- function(
    x,
    prob = x$settings$prob,
    target_width = x$settings$target_width,
    mc_conf_level = x$settings$mc_conf_level,
    singular_action = x$settings$singular_action,
    nonconverged_action = x$settings$nonconverged_action,
    ...
) {
  UseMethod("assurance_test")
}

#' @rdname assurance_test
#' @export
assurance_test.default <- function(
    x,
    prob = NULL,
    target_width = NULL,
    mc_conf_level = NULL,
    singular_action = NULL,
    nonconverged_action = NULL,
    ...
) {
  stop(
    "`assurance_test()` does not support objects of class: ",
    paste(class(x), collapse = ", "),
    call. = FALSE
  )
}

#' @rdname assurance_test
#' @export
assurance_test.precisionSim <- function(x,
                                        prob = x$settings$prob,
                                        target_width = x$settings$target_width,
                                        mc_conf_level = x$settings$mc_conf_level,
                                        singular_action = x$settings$singular_action,
                                        nonconverged_action = x$settings$nonconverged_action,
                                        ...){

  #############################################################################
  ################################ validation #################################
  #############################################################################

  validate_precision_sim(x)

  #############################################################################
  ############################### input checks ################################
  #############################################################################

  if(any(!is.finite(prob)) || length(prob) == 0L || any(prob < 0 | prob > 1)){
    stop("prob must be a non-empty vector with finite values between 0 and 1")
  }

  if(any(!is.finite(target_width)) || length(target_width) == 0L || any(target_width <= 0)){
    stop("target_width must be a non-empty vector with finite values greater than 0")
  }

  if(mc_conf_level < 0 || mc_conf_level > 1){
    stop("mc_conf_level must be between 0 and 1")
  }

  if(length(singular_action) != 1 || !singular_action %in% c("include", "exclude", "failure") ){
    stop("singular must be one of:", "'include', 'exclude', or 'failure'")
  }

  if(length(nonconverged_action) != 1 || !nonconverged_action %in% c("include", "exclude", "failure") ){
    stop("nonconverged must be one of:", "'include', 'exclude', or 'failure'")
  }

  #############################################################################
  ############################# assurance testing #############################
  #############################################################################

  precision_summary <-  x$results %>%

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
                     n_sim_requested = x$settings$nsim,
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

  #create the return structure
  result <- new_precision_sim(
    call = match.call(),
    results = x$results,
    summary = precision_summary,
    diagnostics = x$diagnostics,
    settings = list(
      target_width = target_width,
      prob = prob,
      mc_conf_level = mc_conf_level,
      nsim = x$settings$nsim,
      seed = x$settings$seed,
      singular_action = singular_action,
      nonconverged_action = nonconverged_action
    ),
    model = x$model,
    elapsed = x$elapsed)

  #return
  return(result)

}

#' @rdname assurance_test
#' @export
assurance_test.precisionCurve <- function(x,
                                          prob = x$settings$prob,
                                          target_width = x$settings$target_width,
                                          mc_conf_level = x$settings$mc_conf_level,
                                          singular_action = x$settings$singular_action,
                                          nonconverged_action = x$settings$nonconverged_action,
                                          ...){

  #############################################################################
  ################################ validation #################################
  #############################################################################

  validate_precision_curve(x)

  #hard coded rather than arguments
  along = x$settings$along
  breaks = x$settings$breaks

  #############################################################################
  ############################### input checks ################################
  #############################################################################

  if(any(!is.finite(prob)) || length(prob) == 0L || any(prob < 0 | prob > 1)){
    stop("prob must be a non-empty vector with finite values between 0 and 1")
  }

  if(any(!is.finite(target_width)) || length(target_width) == 0L || any(target_width <= 0)){
    stop("target_width must be a non-empty vector with finite values greater than 0")
  }

  if(mc_conf_level < 0 || mc_conf_level > 1){
    stop("mc_conf_level must be between 0 and 1")
  }

  if(length(singular_action) != 1 || !singular_action %in% c("include", "exclude", "failure") ){
    stop("singular must be one of:", "'include', 'exclude', or 'failure'")
  }

  if(length(nonconverged_action) != 1 || !nonconverged_action %in% c("include", "exclude", "failure") ){
    stop("nonconverged must be one of:", "'include', 'exclude', or 'failure'")
  }

  if( !is.numeric(breaks) ||
      length(breaks) == 0L ||
      anyNA(breaks) ||
      any(!is.finite(breaks)) ||
      any(breaks < 1) ||
      any(breaks != floor(breaks))){
    stop("Breaks must be non-empty numeric vector of positive whole numbers",
         call. = FALSE)
  }

  #############################################################################
  ############################# assurance testing #############################
  #############################################################################

  #############################################################################
  ############################### sample size ################################
  #############################################################################

  #prepare sample size sims
  sample_sizes <- sort(unique(breaks))

  #storage
  full_results_list <- list()
  precision_summary_list <-  list()

  for(ij in 1:length(sample_sizes)){

    #get current sample size
    current_size <- sample_sizes[ij]

    #get data of interest only
    full_results_ij <- x$results %>% dplyr::filter(.data$nlevels == current_size)

    #create a summary
    precision_summary_list[[ij]] <-  full_results_ij %>%

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
                       n_sim_requested = x$settings$nsim,
                       probability_valid = ifelse(.data$n_valid > 0, .data$n_success / .data$n_valid, NA),
                       probability_all = .data$n_success / .data$n_sim_requested,
                       n_singular = sum(.data$singular, na.rm = T),
                       n_converged = sum(.data$converged, na.rm = T),
                       median_width = stats::median(.data$width[.data$analysis_status == "include"], na.rm = T),
                       width_at_target_prob = stats::quantile(.data$width[.data$analysis_status == "include"], probs = prob, na.rm = T),
                       nlevels = unique(.data$nlevels),
                       nrow = unique(.data$nrow)) %>%

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

  }

  #bind
  precision_summary <- dplyr::bind_rows(precision_summary_list)

  #create the return structure
  result <- new_precision_curve(
    call = match.call(),
    results = x$results,
    summary = precision_summary,
    diagnostics = x$diagnostics,
    settings = list(
      target_width = target_width,
      prob = prob,
      mc_conf_level = mc_conf_level,
      nsim = x$settings$nsim,
      seed = x$settings$seed,
      singular_action = singular_action,
      nonconverged_action = nonconverged_action,
      breaks = breaks,
      along = along
    ),
    model = list(
      formula = x$model$formula,
      class = x$model$class,
      nobs = x$model$nobs,
      groups = x$model$groups
    ),
    elapsed = x$elapsed)

  #return
  return(result)

}
