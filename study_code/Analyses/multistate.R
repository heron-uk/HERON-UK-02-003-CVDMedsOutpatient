cohort_ids <- cdm$study_final|>
  collect() |>
  group_by(cohort_definition_id) |> 
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100) |>
  pull(cohort_definition_id)

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$msm_cohorts <- cdm$study_final |>
  subsetCohorts(
    cohortId = cohort_ids,
    name = "msm_cohorts"
  ) |>
  PatientProfiles::addCohortIntersectDate(
    window = c(-Inf, Inf),
    censorDate = "cohort_end_date",
    targetCohortTable = "acute_mi_first",
    nameStyle = "mi_date",
    name = "msm_cohorts"
  ) |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "msm_cohorts",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150)),
    indexDate = "mi_date"
  ) |>
  addSES() |>
  addCountry()
} else {
  cdm$msm_cohorts <- cdm$study_final |>
    subsetCohorts(
      cohortId = cohort_ids,
      name = "msm_cohorts"
    ) |>
    PatientProfiles::addCohortIntersectDate(
      window = c(-Inf, Inf),
      censorDate = "cohort_end_date",
      targetCohortTable = "acute_mi_first",
      nameStyle = "mi_date",
      name = "msm_cohorts"
    )|>
    addDemographics(
      sex = TRUE,
      age = TRUE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = "msm_cohorts",
      ageGroup = list(
        "18 to 39" = c(18, 39),
        "40 to 49" = c(40, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+" = c(90, 150)),
      indexDate = "mi_date"
    ) |>
    addSES()
}

nm_1 <- omopgenerics::uniqueTableName()

if(db_name == "GOLD" | db_name == "GOLD_100k"){
xd <- cdm$msm_cohorts |>
  addCohortName() |>
  group_by(cohort_name, subject_id, age_group, sex, ses) |>
  mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    start_discontinuation = date_count_between(t0, cohort_end_date),
    start_drug = date_count_between(t0, cohort_start_date)
  ) |>
  compute(name = nm_1) |>
  addDeathDays(indexDate = "t0", name = nm_1) |>
  addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm_1) |>
  addCohortIntersectDays(indexDate = "t0", targetCohortTable = "acute_mi_second", window = c(-Inf,Inf)) |>
  rename(second_event = acute_mi_minf_to_inf) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "country", "start_drug", "start_discontinuation", "days_to_death", "future_observation", "second_event") |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999L),
    second_event = coalesce(second_event, 9999L),
    future_observation = pmin(days_to_death, future_observation, second_event),
    start_discontinuation = start_discontinuation + 1
  ) |>
  arrange(cohort_name, subject_id, start_drug) |>
  filter(second_event > 0)
  } else {
    xd <- cdm$msm_cohorts |>
      addCohortName() |>
      group_by(cohort_name, subject_id, age_group, sex, ses) |>
      mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
      ungroup() |>
      mutate(
        start_discontinuation = date_count_between(t0, cohort_end_date),
        start_drug = date_count_between(t0, cohort_start_date)
      ) |>
      compute(name = nm_1) |>
      addDeathDays(indexDate = "t0", name = nm_1) |>
      addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm_1) |>
      addCohortIntersectDays(indexDate = "t0", targetCohortTable = "acute_mi_second", window = c(-Inf,Inf)) |>
      rename(second_event = acute_mi_minf_to_inf) |>
      select("cohort_name", "subject_id", "age_group", "sex", "ses", "start_drug", "start_discontinuation", "days_to_death", "future_observation", "second_event") |>
      collect() |>
      mutate(
        days_to_death = coalesce(days_to_death, 9999L),
        second_event = coalesce(second_event, 9999L),
        future_observation = pmin(days_to_death, future_observation, second_event),
        start_discontinuation = start_discontinuation + 1
      ) |>
      arrange(cohort_name, subject_id, start_drug) |>
      filter(second_event > 0)
}

omopgenerics::dropSourceTable(cdm = cdm, name = nm_1)


# transitions
tmat <- matrix(NA, 3, 3)
tmat[1, 2] <- 1
tmat[2, 1] <- 2
tmat[1, 3] <- 3
tmat[2, 3] <- 4
states <- c("treated", "untreated", "death")
dimnames(tmat) <- list(from = states, to = states)

# prepare transitions from treated
transitionsTreated <- xd |>
  filter(start_drug < future_observation) |>
  mutate(
    transition = case_when(
      days_to_death <= future_observation & days_to_death <= start_discontinuation ~ "death",
      future_observation <= start_discontinuation ~ "censor",
      .default = "discontinue"
    ),
    Tstart = start_drug, 
    Tstop = case_when(
      transition == "death" ~ days_to_death,
      transition == "censor" ~ future_observation,
      transition == "discontinue" ~ start_discontinuation
    )
  )

if(db_name == "GOLD" | db_name == "GOLD_100k"){
  transitionsTreated <- transitionsTreated |>
    select("cohort_name", "subject_id", "age_group", "sex", "ses", "country", "Tstart", "Tstop", "transition")
} else {
  transitionsTreated <- transitionsTreated |>
    select("cohort_name", "subject_id", "age_group", "sex", "ses",  "Tstart", "Tstop", "transition")
}

# prepare transitions from untreated
transitionsUntreated <- xd |>
  filter(start_discontinuation < future_observation) |>
  group_by(cohort_name, subject_id) |>
  mutate(restart_drug = coalesce(lead(start_drug), 9999L)) |>
  ungroup() |>
  mutate(
    transition = case_when(
      days_to_death <= future_observation & days_to_death <= restart_drug ~ "death",
      future_observation <= restart_drug ~ "censor",
      .default = "restart"
    ),
    Tstart = start_discontinuation, 
    Tstop = case_when(
      transition == "death" ~ days_to_death,
      transition == "censor" ~ future_observation,
      transition == "restart" ~ restart_drug
    )
  ) 

if(db_name == "GOLD" | db_name == "GOLD_100k"){
  transitionsUntreated <- transitionsUntreated |>
    select("cohort_name", "subject_id", "age_group", "sex", "ses", "country", "Tstart", "Tstop", "transition")
} else {
  transitionsUntreated <- transitionsUntreated |>
    select("cohort_name", "subject_id", "age_group", "sex", "ses",  "Tstart", "Tstop", "transition")
}

# treated to untreated
if(db_name == "GOLD" | db_name == "GOLD_100k"){
x <- transitionsTreated |>
  mutate(
    from = 1L, 
    to = 2L, 
    trans = 1L,
    status = if_else(transition == "discontinue", 1, 0)
  ) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "country", "from", "to", "trans", "Tstart", "Tstop", "status") |>
  # treated to death
  union_all(
    transitionsTreated |>
      mutate(
        from = 1L, 
        to = 3L, 
        trans = 3L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "country", "from", "to", "trans", "Tstart", "Tstop", "status")
  ) |>
  # untreated to treated
  union_all(
    transitionsUntreated |>
      mutate(
        from = 2L, 
        to = 1L, 
        trans = 2L,
        status = if_else(transition == "restart", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "country", "from", "to", "trans", "Tstart", "Tstop", "status")
  ) |>
  # untreated to death
  union_all(
    transitionsUntreated |>
      mutate(
        from = 2L, 
        to = 3L, 
        trans = 4L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "country", "from", "to", "trans", "Tstart", "Tstop", "status")
  )
} else {
  x <- transitionsTreated |>
    mutate(
      from = 1L, 
      to = 2L, 
      trans = 1L,
      status = if_else(transition == "discontinue", 1, 0)
    ) |>
    select("cohort_name", "subject_id", "age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status") |>
    # treated to death
    union_all(
      transitionsTreated |>
        mutate(
          from = 1L, 
          to = 3L, 
          trans = 3L,
          status = if_else(transition == "death", 1, 0)
        ) |>
        select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
    ) |>
    # untreated to treated
    union_all(
      transitionsUntreated |>
        mutate(
          from = 2L, 
          to = 1L, 
          trans = 2L,
          status = if_else(transition == "restart", 1, 0)
        ) |>
        select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
    ) |>
    # untreated to death
    union_all(
      transitionsUntreated |>
        mutate(
          from = 2L, 
          to = 3L, 
          trans = 4L,
          status = if_else(transition == "death", 1, 0)
        ) |>
        select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
    )
}

cohorts <- unique(x$cohort_name)
sexes <- unique(x$sex)
sesq <- unique(x$ses)
age_groups <- unique(x$age_group)

#Overall
msm_overall_results <- list()

if(db_name == "GOLD" | db_name == "GOLD_100k") {
countries <- unique(x$country)

for (coh in cohorts) {
  for(cou in countries) {
  msdata <- x |>
    filter(cohort_name == coh,
           country == cou) 
  
  cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {cou}}"))
    
  # Fit the transition-specific Cox model
  cox_mod <- coxph(
    Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
    data = msdata
  )
  
  msf <- msfit(cox_mod, trans = tmat) 
  pt_list <- probtrans(msf, predt = 0)
  
  xp<- pt_list[[1]] |>
    as_tibble() |>
    select(time, pstate1, pstate2, pstate3) |>
    pivot_longer(starts_with("pstate"),
                 names_to = "state",
                 values_to = "probability") |>
    mutate(
      state = recode(state,
                     pstate1 = "Treated",
                     pstate2 = "Discontinued",
                     pstate3 = "Death"),
      cohort_name = coh,
      country = cou,
      result_type = "msm_probabilities"
    ) |>
    arrange(time, state) |>
    filter(time <= 1830)
  ######
  
  sum_xp <- omopgenerics::transformToSummarisedResult(
    x = xp,
    group = c("cohort_name"),
    strata = c("country"),
    estimates = c("probability"),
    additional = c("time", "state"),
    settings = c("result_type")
  ) |>
    mutate(cdm_name = omopgenerics::cdmName(cdm))
  
  
  msm_overall_results[[paste0("msm_prob_",coh,"_",cou)]] <- sum_xp
    
}
}
} else {
  for (coh in cohorts) {
      msdata <- x |>
        filter(cohort_name == coh) 
      
      cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh}}"))
      
      # Fit the transition-specific Cox model
      cox_mod <- coxph(
        Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
        data = msdata
      )
      
      msf <- msfit(cox_mod, trans = tmat) 
      pt_list <- probtrans(msf, predt = 0)
      
      xp<- pt_list[[1]] |>
        as_tibble() |>
        select(time, pstate1, pstate2, pstate3) |>
        pivot_longer(starts_with("pstate"),
                     names_to = "state",
                     values_to = "probability") |>
        mutate(
          state = recode(state,
                         pstate1 = "Treated",
                         pstate2 = "Discontinued",
                         pstate3 = "Death"),
          cohort_name = coh,
          result_type = "msm_probabilities"
        ) |>
        arrange(time, state) |>
        filter(time <= 1830)
      ######
      
      sum_xp <- omopgenerics::transformToSummarisedResult(
        x = xp,
        group = c("cohort_name"),
        estimates = c("probability"),
        additional = c("time", "state"),
        settings = c("result_type")
      ) |>
        mutate(cdm_name = omopgenerics::cdmName(cdm))
      
      
      msm_overall_results[[paste0("msm_prob_",coh)]] <- sum_xp
      
    }
  }

# By sex
msm_sex_results <- list()

if(db_name == "GOLD" | db_name == "GOLD_100k") {
  countries <- unique(x$country)
  
  for (coh in cohorts) {
    for(cou in countries) {
      for(s in sexes){
      msdata <- x |>
        filter(cohort_name == coh,
               country == cou, 
               sex == s) 
      
      cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {cou} {s}}"))
      
      # Fit the transition-specific Cox model
      cox_mod <- coxph(
        Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
        data = msdata
      )
      
      msf <- msfit(cox_mod, trans = tmat) 
      pt_list <- probtrans(msf, predt = 0)
      
      xp<- pt_list[[1]] |>
        as_tibble() |>
        select(time, pstate1, pstate2, pstate3) |>
        pivot_longer(starts_with("pstate"),
                     names_to = "state",
                     values_to = "probability") |>
        mutate(
          state = recode(state,
                         pstate1 = "Treated",
                         pstate2 = "Discontinued",
                         pstate3 = "Death"),
          cohort_name = coh,
          country = cou,
          sex = s,
          result_type = "msm_probabilities"
        ) |>
        arrange(time, state) |>
        filter(time <= 1830)
      ######
      
      sum_xp <- omopgenerics::transformToSummarisedResult(
        x = xp,
        group = c("cohort_name"),
        strata = c("country", "sex"),
        estimates = c("probability"),
        additional = c("time", "state"),
        settings = c("result_type")
      ) |>
        mutate(cdm_name = omopgenerics::cdmName(cdm))
      
      
      msm_sex_results[[paste0("msm_prob_",coh,"_",cou, "_", s)]] <- sum_xp
      
    }
    }
  }
} else {
  for (coh in cohorts) {
    for(s in sexes){
    msdata <- x |>
      filter(cohort_name == coh,
             sex == s) 
    
    cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {s}}"))
    
    # Fit the transition-specific Cox model
    cox_mod <- coxph(
      Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
      data = msdata
    )
    
    msf <- msfit(cox_mod, trans = tmat) 
    pt_list <- probtrans(msf, predt = 0)
    
    xp<- pt_list[[1]] |>
      as_tibble() |>
      select(time, pstate1, pstate2, pstate3) |>
      pivot_longer(starts_with("pstate"),
                   names_to = "state",
                   values_to = "probability") |>
      mutate(
        state = recode(state,
                       pstate1 = "Treated",
                       pstate2 = "Discontinued",
                       pstate3 = "Death"),
        cohort_name = coh,
        sex = s,
        result_type = "msm_probabilities"
      ) |>
      arrange(time, state) |>
      filter(time <= 1830)
    ######
    
    sum_xp <- omopgenerics::transformToSummarisedResult(
      x = xp,
      group = c("cohort_name"),
      strata = c("sex"),
      estimates = c("probability"),
      additional = c("time", "state"),
      settings = c("result_type")
    ) |>
      mutate(cdm_name = omopgenerics::cdmName(cdm))
    
    
    msm_sex_results[[paste0("msm_prob_",coh, "_", s)]] <- sum_xp
    
    }
  }
}

# By SES
msm_ses_results <- list()

if(db_name == "GOLD" | db_name == "GOLD_100k") {
  countries <- unique(x$country)
  
  for (coh in cohorts) {
    for(cou in countries) {
      if(cou == "Northern Ireland") {
        sesq <- c("2", "3", "4", "5")
      } else {
        sesq <- c("1", "2", "3", "4", "5")
      }
      for(q in sesq){
        msdata <- x |>
          filter(cohort_name == coh,
                 country == cou, 
                 ses == q) 
        
        cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {cou} {q}}"))
        
        # Fit the transition-specific Cox model
        cox_mod <- coxph(
          Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
          data = msdata
        )
        
        msf <- msfit(cox_mod, trans = tmat) 
        pt_list <- probtrans(msf, predt = 0)
        
        xp<- pt_list[[1]] |>
          as_tibble() |>
          select(time, pstate1, pstate2, pstate3) |>
          pivot_longer(starts_with("pstate"),
                       names_to = "state",
                       values_to = "probability") |>
          mutate(
            state = recode(state,
                           pstate1 = "Treated",
                           pstate2 = "Discontinued",
                           pstate3 = "Death"),
            cohort_name = coh,
            country = cou,
            ses = q,
            result_type = "msm_probabilities"
          ) |>
          arrange(time, state) |>
          filter(time <= 1830)
        ######
        
        sum_xp <- omopgenerics::transformToSummarisedResult(
          x = xp,
          group = c("cohort_name"),
          strata = c("country", "ses"),
          estimates = c("probability"),
          additional = c("time", "state"),
          settings = c("result_type")
        ) |>
          mutate(cdm_name = omopgenerics::cdmName(cdm))
        
        
        msm_ses_results[[paste0("msm_prob_", coh, "_", cou, "_", q)]] <- sum_xp
        
      }
    }
  }
} else {
  for (coh in cohorts) {
    for(q in sesq){
      msdata <- x |>
        filter(cohort_name == coh,
               ses == q) 
      
      cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {q}}"))
      
      # Fit the transition-specific Cox model
      cox_mod <- coxph(
        Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
        data = msdata
      )
      
      msf <- msfit(cox_mod, trans = tmat) 
      pt_list <- probtrans(msf, predt = 0)
      
      xp<- pt_list[[1]] |>
        as_tibble() |>
        select(time, pstate1, pstate2, pstate3) |>
        pivot_longer(starts_with("pstate"),
                     names_to = "state",
                     values_to = "probability") |>
        mutate(
          state = recode(state,
                         pstate1 = "Treated",
                         pstate2 = "Discontinued",
                         pstate3 = "Death"),
          cohort_name = coh,
          ses = q,
          result_type = "msm_probabilities"
        ) |>
        arrange(time, state) |>
        filter(time <= 1830)
      ######
      
      sum_xp <- omopgenerics::transformToSummarisedResult(
        x = xp,
        group = c("cohort_name"),
        strata = c("ses"),
        estimates = c("probability"),
        additional = c("time", "state"),
        settings = c("result_type")
      ) |>
        mutate(cdm_name = omopgenerics::cdmName(cdm))
      
      
      msm_ses_results[[paste0("msm_prob_",coh, "_", q)]] <- sum_xp
      
    }
  }
}

# By age_group
msm_age_results <- list()

if(db_name == "GOLD" | db_name == "GOLD_100k") {
  countries <- unique(x$country)
  
  for (coh in cohorts) {
    for(cou in countries) {
      for(age in age_groups){
        msdata <- x |>
          filter(cohort_name == coh,
                 country == cou, 
                 age_group == age) 
        
        cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {cou} {age}}"))
        
        # Fit the transition-specific Cox model
        cox_mod <- coxph(
          Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
          data = msdata
        )
        
        msf <- msfit(cox_mod, trans = tmat) 
        pt_list <- probtrans(msf, predt = 0)
        
        xp<- pt_list[[1]] |>
          as_tibble() |>
          select(time, pstate1, pstate2, pstate3) |>
          pivot_longer(starts_with("pstate"),
                       names_to = "state",
                       values_to = "probability") |>
          mutate(
            state = recode(state,
                           pstate1 = "Treated",
                           pstate2 = "Discontinued",
                           pstate3 = "Death"),
            cohort_name = coh,
            country = cou,
            age_group = age,
            result_type = "msm_probabilities"
          ) |>
          arrange(time, state) |>
          filter(time <= 1830)
        ######
        
        sum_xp <- omopgenerics::transformToSummarisedResult(
          x = xp,
          group = c("cohort_name"),
          strata = c("country", "age_group"),
          estimates = c("probability"),
          additional = c("time", "state"),
          settings = c("result_type")
        ) |>
          mutate(cdm_name = omopgenerics::cdmName(cdm))
        
        
        msm_age_results[[paste0("msm_prob_", coh, "_", cou, "_", age)]] <- sum_xp
        
      }
    }
  }
} else {
  for (coh in cohorts) {
    for(age in age_groups){
      msdata <- x |>
        filter(cohort_name == coh,
               age_group == age) 
      
      cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh} {age}}"))
      
      # Fit the transition-specific Cox model
      cox_mod <- coxph(
        Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
        data = msdata
      )
      
      msf <- msfit(cox_mod, trans = tmat) 
      pt_list <- probtrans(msf, predt = 0)
      
      xp<- pt_list[[1]] |>
        as_tibble() |>
        select(time, pstate1, pstate2, pstate3) |>
        pivot_longer(starts_with("pstate"),
                     names_to = "state",
                     values_to = "probability") |>
        mutate(
          state = recode(state,
                         pstate1 = "Treated",
                         pstate2 = "Discontinued",
                         pstate3 = "Death"),
          cohort_name = coh,
          age_group = age,
          result_type = "msm_probabilities"
        ) |>
        arrange(time, state) |>
        filter(time <= 1830)
      ######
      
      sum_xp <- omopgenerics::transformToSummarisedResult(
        x = xp,
        group = c("cohort_name"),
        strata = c("age_group"),
        estimates = c("probability"),
        additional = c("time", "state"),
        settings = c("result_type")
      ) |>
        mutate(cdm_name = omopgenerics::cdmName(cdm))
      
      
      msm_age_results[[paste0("msm_prob_",coh, "_", age)]] <- sum_xp
      
    }
  }
}

overall_msm_results <- msm_overall_results |>
  purrr::compact() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results[["msm_overall"]] <- overall_msm_results

sex_msm_results <- msm_sex_results |>
  purrr::compact() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results[["msm_sex"]] <- sex_msm_results

ses_msm_results <- msm_ses_results |>
  purrr::compact() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results[["msm_ses"]] <- ses_msm_results

age_msm_results <- msm_age_results |>
  purrr::compact() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results[["msm_age"]] <- age_msm_results


