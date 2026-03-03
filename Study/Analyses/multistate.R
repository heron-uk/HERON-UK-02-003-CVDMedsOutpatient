library(PatientProfiles)
library(dplyr)
library(clock)

# prepare data drug episodes
nm <- omopgenerics::uniqueTableName()
xd <- cdm$stroke_drugs_after_event |>
  addCohortName() |>
  group_by(cohort_name, subject_id) |>
  mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    start_discontinuation = date_count_between(t0, cohort_end_date),
    start_drug = date_count_between(t0, cohort_start_date)
  ) |>
  compute(name = nm) |>
  addDeathDays(indexDate = "t0", name = nm) |>
  addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm) |>
  select("cohort_name", "subject_id", "start_drug", "start_discontinuation", "days_to_death", "future_observation") |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999L),
    future_observation = pmin(days_to_death, future_observation),
    start_discontinuation = start_discontinuation + 1
  ) |>
  arrange(cohort_name, subject_id, start_drug)
omopgenerics::dropSourceTable(cdm = cdm, name = nm)

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
  ) |>
  select("cohort_name", "subject_id", "Tstart", "Tstop", "transition")

# prepare transitions from untreated
transitionsUntreated <- xd |>
  filter(start_discontinuation < future_observation) |>
  group_by(cohort_name, subject_id) |>
  mutate(start_drug = coalesce(lead(start_drug), 9999L)) |>
  ungroup() |>
  mutate(
    transition = case_when(
      days_to_death <= future_observation & days_to_death <= start_drug ~ "death",
      future_observation <= start_drug ~ "censor",
      .default = "restart"
    ),
    Tstart = start_discontinuation, 
    Tstop = case_when(
      transition == "death" ~ days_to_death,
      transition == "censor" ~ future_observation,
      transition == "restart" ~ start_drug
    )
  ) |>
  select("cohort_name", "subject_id", "Tstart", "Tstop", "transition")

# treated to untreated
x <- transitionsTreated |>
  mutate(
    from = 1L, 
    to = 2L, 
    trans = 1L,
    status = if_else(transition == "discontinue", 1, 0)
  ) |>
  select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status") |>
  # treated to death
  union_all(
    transitionsTreated |>
      mutate(
        from = 1L, 
        to = 3L, 
        trans = 3L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
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
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
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
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
  )

library(survival)
library(mstate)
library(dplyr)
library(tidyr)
library(ggplot2)

msdata <- x |>
  filter(cohort_name == "p2y12_inhibitors_stroke")

cox_mod <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
  data = msdata
)

msf <- msfit(cox_mod, trans = tmat) 

pt_list <- probtrans(msf, predt = 0)

xp <- pt_list[[1]] |>
  as_tibble() |>
  select(time, pstate1, pstate2, pstate3) |>
  pivot_longer(starts_with("pstate"), names_to = "state", values_to = "prob") |>
  mutate(state = recode(state, pstate1 = "Treated", pstate2 = "Discontinued", pstate3 = "Death"))

xp <- xp |>
  mutate(step = 1) |>
  union_all(
    xp |>
      group_by(state) |>
      mutate(time = lead(time), step = -1) |>
      ungroup()
  ) |>
  arrange(time, state, step)

ggplot(xp, aes(x = time, y = prob, fill = state)) +
  geom_area() +
  xlim(c(0, 5*365)) +
  labs(x = "Time",
       y = "Probability",
       fill = "State") +
  theme_minimal()
