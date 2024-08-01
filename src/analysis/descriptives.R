t1a <- tibble(
  vars = c("Affective Polarization", "Conspiracy Mentality", "Political Trust"),
  mean = c(mean(d$affpol_div, na.rm = TRUE),
           mean(d$picom_index, na.rm = TRUE),
           mean(d$trust_index, na.rm = TRUE)),
  stdev = c(sd(d$affpol_div, na.rm = TRUE),
            sd(d$picom_index, na.rm = TRUE),
            sd(d$trust_index, na.rm = TRUE)),
  type = "Full Data (N=6044)"
)

t1b <- d |> 
  select(pc_type, affpol_div, picom_index, trust_index) |> 
  pivot_longer(cols=affpol_div:trust_index,
               names_to = "vars") |> 
  group_by(pc_type, vars) |> 
  summarise(mean = mean(value, na.rm = T),
            stdev = sd(value, na.rm = T),
            n = n()) |> 
  ungroup() |> 
  mutate(pc_type = case_when(
    pc_type == 0 ~ "No PC",
    pc_type == 1 ~ "Mainstream PC",
    TRUE ~ "Populist PC"
  ),
  vars = case_when(
    vars == "affpol_div" ~ "Affective Polarization",
    vars == "picom_index" ~ "Conspiracy Mentality",
    TRUE ~ "Political Trust"
  ),
  type = paste0(pc_type, " (N=", n,")")) |> 
  select(vars, mean, stdev, type)

# Check wheter the differences in means of t1b are statistically significant
df <- d |> 
  select(pc_type, pc, affpol_div, picom_index, trust_index) |>
  mutate(pc_type = case_when(
    pc_type == 0 ~ "No PC",
    pc_type == 1 ~ "Mainstream PC",
    TRUE ~ "Populist PC"),
    pc_type = factor(pc_type),
    pc = factor(pc))

aov1 <- aov(affpol_div ~ pc_type, data = df)
aov2 <- aov(picom_index ~ pc_type, data = df)
aov3 <- aov(trust_index ~ pc_type, data = df)
s1 <- summary(aov1)
s2 <- summary(aov2)
s3 <- summary(aov3)

t1c <- tibble(
  vars = c("Affective Polarization", "Conspiracy Mentality", "Political Trust"),
  df1 = 2,
  df2 = c(s1[[1]]["Residuals", "Df"],
          s2[[1]]["Residuals", "Df"],
          s3[[1]]["Residuals", "Df"]),
  Fvalue = c(s1[[1]]["pc_type", "F value"],
             s2[[1]]["pc_type", "F value"],
             s3[[1]]["pc_type", "F value"]),
  pvalue = c(s1[[1]]["pc_type", "Pr(>F)"],
             s2[[1]]["pc_type", "Pr(>F)"],
             s3[[1]]["pc_type", "Pr(>F)"]),)

t1c_2 <- broom::tidy(TukeyHSD(aov1, conf.level=.95)) |> 
  select(contrast, estimate:conf.high)
#plot(TukeyHSD(aov1, conf.level=.95))
summary(aov2)
tmp <- broom::tidy(TukeyHSD(aov2, conf.level=.95)) |> 
  select(contrast, estimate:conf.high)
t1c_2 <- t1c_2 |> 
  add_case(tmp)
#plot(TukeyHSD(aov2, conf.level=.95))
summary(aov3)
tmp <- broom::tidy(TukeyHSD(aov3, conf.level=.95)) |> 
  select(contrast, estimate:conf.high)
t1c_2 <- t1c_2 |> 
  add_case(tmp)
#plot(TukeyHSD(aov3, conf.level=.95))

## Correlations
df_pop = d %>% filter(pc_type == 2) # extra data frames per PC Type
df_main = d %>% filter(pc_type == 1)
df_nopc = d %>% filter(pc_type == 0)

c1a <- cor.test(d$affpol_div, d$picom_index, 
               method = 'pearson', use = "complete.obs")
c2a <- cor.test(df_nopc$affpol_div, df_nopc$picom_index, 
               method = 'pearson', use = "complete.obs")
c3a <- cor.test(df_main$affpol_div, df_main$picom_index, 
         method = 'pearson', use = "complete.obs")
c4a <- cor.test(df_pop$affpol_div, df_pop$picom_index, 
         method = 'pearson', use = "complete.obs")
  
c1b <- cor.test(d$trust_index, d$picom_index, method = 'pearson', 
         use = "complete.obs")
c2b <- cor.test(df_nopc$trust_index, df_nopc$picom_index, method = 'pearson', 
         use = "complete.obs")
c3b <- cor.test(df_main$trust_index, df_main$picom_index, method = 'pearson', 
         use = "complete.obs")
c4b <- cor.test(df_pop$trust_index, df_pop$picom_index, method = 'pearson', 
         use = "complete.obs")

c1c <- cor.test(d$trust_index, d$affpol_div, method = 'pearson', 
              use = "complete.obs")
c2c <- cor.test(df_nopc$trust_index, df_nopc$affpol_div, method = 'pearson', 
                use = "complete.obs")
c3c <- cor.test(df_main$trust_index, df_main$affpol_div, method = 'pearson', 
               use = "complete.obs")
c4c <- cor.test(df_pop$trust_index, df_pop$affpol_div, method = 'pearson', 
                use = "complete.obs")

t1d <- tibble(
  x = c(rep("Affective Polarization", 8), rep("Conspiracy Mentality",4)),
  y = c(rep("Conspiracy Mentality", 4), rep("Political Trust", 8)),
  type = rep(c("Full Data", "No PC", "Mainstream PC", "Populist PC"), 3),
  cor = c(c1a$estimate, c2a$estimate, c3a$estimate, c4a$estimate,
          c1c$estimate, c2c$estimate, c3c$estimate, c4c$estimate,
          c1b$estimate, c2b$estimate, c3b$estimate, c4b$estimate)
)

rm(aov1, aov2, aov3, c1a, c1b, c1c, c2a, c2b, c2c,
   c3a, c3b, c3c, c4a, c4b, c4c, df,
   df_main, df_nopc, df_pop, s1, s2, s3, tmp)
