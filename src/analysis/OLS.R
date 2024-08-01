df <- d |> 
  mutate(pid2 = case_when(
    pid == 0 ~ "No PC",
    pid == 1 ~ "Weak PC",
    pid == 2 ~ "Moderate PC",
    TRUE ~ "Strong PC"
  ),
  pid2 = factor(pid2),
  affpol_z = scale(affpol_div),
  picom_z = scale(picom_index))

df <- within(df, pid2 <- relevel(pid2, ref = "No PC"))

m1a <- broom.mixed::tidy(lmer(affpol_z ~ pid2 + (1 | country), data = df))
m1b <- broom.mixed::tidy(lmer(affpol_z ~ pid2 + gender + age + educ + (1|country), data = df)) |> 
  mutate(model = "Model 1 (H1)\n DV: Affective Polarization",
         dv = "Affective Polarization")

m2a <- broom.mixed::tidy(lmer(picom_z ~ pid2 + (1 | country), data = df))
m2b <- broom.mixed::tidy(lmer(picom_z ~ pid2 + gender + age + educ + (1|country), data = df))  |> 
  mutate(model = "Model 2 (H2a)\n DV: Conspiracy Mentality",
         dv = "Conspiracy Mentality")

m3a <- broom.mixed::tidy(lmer(affpol_z ~ picom_index + (1 | country), data = df))
m3b <- broom.mixed::tidy(lmer(affpol_z ~ picom_index + gender + age + educ + (1|country), data = df))  |> 
  mutate(model = "Model 3 (H2b)\n DV: Affective Polarization",
         dv = "Affective Polarization")

p1 <- m1b |> 
  add_case(m2b) |> 
  add_case(m3b) |> 
  filter(term %in% c("(Intercept)","pid2NoPC", "pid2Strong PC",
                     "pid2Moderate PC", "pid2Weak PC", "picom_index")) |> 
  mutate(term = recode(term,
    `(Intercept)` = "Intercept",
    `pid2No PC` = "No PC",
    `pid2Strong PC` = "Strong PC",
    `pid2Weak PC` = "Weak PC",
    `pid2Moderate PC` = "Moderate PC",
    `picom_index` = "Conspiracy Mentality"),
  term = factor(term,
                levels = c("Conspiracy Mentality",
                           "Strong PC", "Moderate PC", "Weak PC", "No PC",
                           "Intercept")),
  lower = estimate - (1.96*std.error),
  upper = estimate + (1.96*std.error),
  sign = case_when(
    statistic >1.96 | statistic < -1.96 ~ "Statistically Significant",
    TRUE ~ "Not Statistically Significant"
  )) |> 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             shape = sign)) +
  geom_point(color = fig_cols[6], size = 3) +
  geom_errorbar(color = fig_cols[6], width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray75", size = .8) +
  facet_grid(cols = vars(model),
             scales = "free_x") +
  theme_ipsum() +
  #scale_color_manual(values = fig_cols) +
  scale_shape_manual(values=c(1, 16)) +
  labs(x = "Estimated Effect on Dependent Variable", y = "",
       caption = "Models are controlled for age, gender, and educational level \n No Party Closeness is references category\n DVs are standardized to ease interpretation") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

rm(m1a, m1b, m2a, m2b, m3a, m3b)

#m4a <- broom.mixed::tidy(lmer(affpol_div ~ picom_index + pid + (1 | country), data = d))
#m4b <- broom.mixed::tidy(lmer(affpol_div ~ picom_index + pid + gender + age + educ + (1 | country), data = df))  |> 
#  mutate(model = "Model 4 (H3), Affective Polarization",
#         dv = "Affective Polarization")



## Assumption Checks
#summary(m1a)
#plot(m1a)
#bptest(m1a) #Breusch–Pagan test
#summary(m1b)
#plot(m1b)
#bptest(m1b)

#summary(m2a)
#plot(m2a)
#bptest(m2a) #Breusch–Pagan test
#summary(m2b)
#plot(m2b)
#bptest(m2b)

#summary(m3a)
#plot(m3a)
#bptest(m3a) #Breusch–Pagan test
#summary(m3b)
#plot(m3b)
#bptest(m3b)

#summary(m4a)
#plot(m4a)
#bptest(m4a) #Breusch–Pagan test
#summary(m4b)
#plot(m4b)
#bptest(m4b)