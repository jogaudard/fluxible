library(tidyverse)
library(fluxible)
library(cpop)
library(broom)

# from cpop documentation
# simulate data with change in gradient
set.seed(1)
x <- (1:50/5)^2
y <- simchangeslope(x,changepoints=c(10,50),change.slope=c(0.25,-0.25),sd=1)
# determine changepoints
res <- cpop(y,x,beta=2*log(length(y)))
# calculate segments
fit <- fitted(res)
# plot the results
plot(res) 



# mimicking what is done in flux_fitting_segment
test_df <- tibble(x, y)

# adding segment ID

# fct to extract changepoints
changepts <- function(x) {
    v <- x@changepoints
    d <- diff(v)
    d[1] <- d[1] + 1
    rep(seq_along(v[-length(v)]), times = d)
  }

# fct to extract cpop gradient
gradient <- function(cpo, segID){
    cpo <- fitted(cpo)
    cpo$gradient[segID]
}

# running cpop on our test_df and adding segmentID based on changepoints
# tricky because changepoints are shared between segments (look at the plot)
# now we will do it the way it was done in flux_fitting_segment,
# which is the next segment starts just after the end of the previous one
# (it is not what cpop is doing)


conc_df <- flux_match(co2_df_short, record_short, measurement_length = 150)
#   filter(f_fluxID == 1) |>
# cpop does not deal with datetime format
test_segment <- function(conc_df, minseglen){
conc_df <- conc_df |>
  mutate(time = row_number(), .by = f_fluxID) |> # for simplicity we assume no data are missing
  select(f_fluxID, f_datetime, f_conc, time)

conc_seg <- conc_df |>
  group_by(f_fluxID) |>
  nest() |>
  mutate(cpo = map(data, \(x)cpop (x$f_conc, x$time, minseglen = minseglen)),
         segID = map(cpo, changepts),
         fit = map(cpo, fitted)
         ) |>
  unnest(cols = c(data, segID)) |>
  mutate( # getting the slope calculated by cpop
         slope_cpop = fit[[1]]$gradient[segID], # I have done better but it works ;-)
         intercept_cpop = fit[[1]]$intercept[segID],
         fit_cpop = intercept_cpop + slope_cpop * time
  ) |>
  select(!c(cpo, fit)) # they annoy me

# ggplot(conc_seg, aes(f_datetime)) +
# geom_line(aes(y = fit_cpop), colour = "red") +
# geom_point(aes(y = f_conc), colour = "blue", size = 0.2) +
# facet_wrap(vars(f_fluxID), scales = "free")

# let's add the lm slope as done in flux_fitting_segment
conc_slope <- conc_seg |>
  group_by(f_fluxID, segID) |>
  nest() |>
  mutate(
    mod_seg = map(.x = data, \(.x) lm(f_conc ~ time, data = .x)),
    tidy = map(mod_seg, tidy),
    # slope_lm_seg = map(mod_seg, coef),
    fit_lm_seg = map(mod_seg, predict)
         ) |>
         unnest(tidy) |>
    filter(term == "time") |>
    rename(slope_lm_seg = "estimate") |>
  unnest(cols = c(data, fit_lm_seg)) |>
  select(!c(mod_seg, term, std.error, statistic, p.value))

# for comparison, we will take the weighted mean of the slopes from both ideas
# and also the slope of a LM on the entire flux

conc_slope2 <- conc_slope |>
    group_by(f_fluxID) |>
    nest() |>
    mutate(
        mod_lm = map(.x = data, \(.x) lm(f_conc ~ time, data = .x)),
        fit_lm = map(mod_lm, predict),
        tidy = map(mod_lm, tidy)
    ) |>
    unnest(tidy) |>
    filter(term == "(Intercept)") |>
    rename(intercept_lm = "estimate") |>
    unnest(cols = c(f_fluxID, data, fit_lm)) |>
  select(!c(mod_lm, term, std.error, statistic, p.value))

conc_slope_avg <- conc_slope2 |>
    group_by(f_fluxID) |>
    mutate(
        slope_cpop_avg = mean(slope_cpop),
        slope_lm_seg_avg = mean(slope_lm_seg),
        fit_lm_seg_avg = intercept_lm + slope_lm_seg_avg * time,
        fit_cpop_avg = intercept_lm + slope_cpop_avg * time
    ) |>
    ungroup()

ggplot(conc_slope_avg, aes(f_datetime)) +
# measurement data points
geom_point(aes(y = f_conc), size = 0.2) +
# geom_smooth(aes(y = f_conc), method = lm, formula = y ~ poly(x, 1)) +
# the fit of segments done by cpop
geom_line(aes(y = fit_cpop), colour = "green") +
# the fit of segments wiht lm, using cpop changepoints
geom_line(aes(y = fit_lm_seg, group = segID), colour = "purple") +
# weighted mean of lm segments
geom_line(aes(y = fit_lm_seg_avg), colour = "red") +
# weighted mean of cpop segments
geom_line(aes(y = fit_cpop_avg), colour = "orange") +
# lm on entire measurement
geom_line(aes(y = fit_lm), colour = "blue") +
facet_wrap(vars(f_fluxID), scales = "free")

}

test_segment(conc_df, 20)
test_pftc7 <- pftc7_short  |>
    rename(
        f_conc = "co2_conc",
        f_datetime = "date_time",
        f_fluxID = "file_name"
    )

test_segment(test_pftc7, 30)
