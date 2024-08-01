d <- d %>%
  zap_labels() |> 
  select(id = RESPID, 
         country = DCOUNTRY, 
         gender = Q1, #1=male, 2=female
         age = Q2_1, 
         age_rec = DQ2BIS, #1=18-24, 2=25-34, 3=35-44, 4=45-54, 5=55-64, 6>=65
         educ = DQ4X1, #1=low, 2=middle, 3=high, 4=NA
         starts_with('Q55X'), #party close to
         Q56, #party closeness: 1 = very close, 2 = fairly close, 3 = merely sympathizer
         starts_with('Q79'), #Q79_1:Q79_5; political efficacy, Q79_6 & Q79_7 = affective pol!
         starts_with('Q80'),  #conspiracy mentality
         #starts_with('Q57'), #covid conspiracies, only w1
         starts_with('Q66AX'), #PTV
         Q71, #political ideology: 1=radical left, 2=left, 3=center-left, 4=center, 5=center-right, 6=right, 7=radical right, 100=none of these
         Q65, #self placement
         #starts_with('Q66X'), #party placement, only w1
         #Q31_1, #In your opinion, in the context of a pandemic crisis, is it better to establish all the restrictions that could preserve public health or limiting them to preserve people's individual freedom? Please answer with a score from 0 to 10.
         #Q32_1, # In your opinion, in the context of a pandemic crisis, is it better to establish all the restrictions that could preserve public health or is it better to leave economic activities open to protect jobs? Please answer on a score from 0-10."
         #starts_with('Q33X'), #ideological placement, only w1
         Q13_1, #social trust
         starts_with('Q14'), #political trust
         #Q41B_1, #Do you feel that the pandemic has increased the solidarity among people (0), or do you feel that it has deepened the divisions among them (10)?"
         starts_with('Q72'), #party vote EU elections 2019
         starts_with('Q73X'), #vote intention
         wave = WAVE) |> 
  filter(wave == 2) |> 
  mutate(popu_list = case_when( #indicate whether party is populist, based on popu-list (Rooduijn et al)
    Q55XIT == 1 | Q55XIT > 2 | Q55XIT < 6 ~ 1,
    Q55XPL == 1 | Q55XPL == 6 ~ 1,
    Q55XSP == 4 | Q55XSP == 5 ~ 1, 
    Q55XFR == 2 | Q55XFR == 5 | Q55XFR == 7 ~ 1, 
    Q55XDE == 4 | Q55XDE == 6 ~ 1, 
    Q55XSW == 3 ~ 1,
    TRUE ~ 0),
  popu_list = replace_na(popu_list, 0),
  pid = case_when( #create strength of partisanship, 0 = not at all close, 3 = very close
    Q55XIT == 90 ~ NA, 
    Q55XIT == 100 ~ 0,
    Q55XPL == 90 ~ NA, 
    Q55XPL == 100 ~ 0,
    Q55XSP == 90 ~ NA, 
    Q55XSP == 100 ~ 0,
    Q55XFR == 90 ~ NA, 
    Q55XFR == 100 ~ 0,
    Q55XDE == 90 ~ NA, 
    Q55XDE == 100 ~ 0,
    Q55XSW == 90 ~ NA, 
    Q55XSW == 100 ~ 0,
    TRUE ~ (4 - Q56)),
  pid = replace_na(pid, 0),
  pc = ifelse(pid == 0, 0, 1),
  pc_type = case_when(
    popu_list == 1 ~ 2,
    popu_list == 0 & pc == 1 ~ 1,
    TRUE ~ 0),
  gender = replace_na(gender, 2), # imputed 8 missing values by the mode (2 = female)
  gender = case_when(
    gender == 2 ~ 0,
    TRUE ~ 1),
  educ = replace_na(educ, 2), # imputed 72 (1%) missing values by the mode (2 = middle)
  Q79_5 = 6 - Q79_5, #reverse coding
  Q79_7 = 6 - Q79_7, #reverse coding
  across(Q80_1:Q80_5, ~replace_na(., mean(., na.rm=T))), #replace missings with mean,
  across(Q79_1:Q79_7, ~replace_na(., mean(., na.rm=T))), #replace missings with mean,
  across(Q14_1:Q14_6, ~replace_na(., mean(., na.rm=T))), #replace missings with mean,
  Q65 = replace_na(Q65, mean(Q65, na.rm=T)),
  Q71 = na_if(Q71, 100),
  Q71 = replace_na(Q71, mean(Q71, na.rm=T))) |> 
  tidycomm::add_index(populism_index, Q79_1, Q79_3, Q79_5,
            type = "mean") |>  
  tidycomm::add_index(picom_index, matches("Q80_"),
            type = "mean")  |>  
  tidycomm::add_index(trust_index, matches("Q14_"),
            type = "mean") |> 
  tidycomm::add_index(ap_index, Q79_6, Q79_7,
            type = "mean")

# AP measures per country
it <- d |> 
  filter(country == 1) |> 
  select(id, starts_with("Q66AXIT")) |> 
  mutate(ptv_max = pmax(Q66AXIT_1, Q66AXIT_2, 
                        Q66AXIT_3, Q66AXIT_4, 
                        Q66AXIT_5, Q66AXIT_6, 
                        Q66AXIT_7, Q66AXIT_8,
                        Q66AXIT_9, Q66AXIT_10, na.rm = T),
         ptv_min = pmin(Q66AXIT_1, Q66AXIT_2, 
                        Q66AXIT_3, Q66AXIT_4, 
                        Q66AXIT_5, Q66AXIT_6, 
                        Q66AXIT_7, Q66AXIT_8,
                        Q66AXIT_9, Q66AXIT_10, na.rm = T),
         aff_pol = ptv_max - ptv_min,
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXIT_1:Q66AXIT_10) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

pl <- d |> 
  filter(country == 2) |> 
  select(id, starts_with("Q66AXPL")) |> 
  mutate(ptv_max = pmax(Q66AXPL_1, Q66AXPL_2, 
                        Q66AXPL_3, Q66AXPL_4, 
                        Q66AXPL_5, Q66AXPL_6, na.rm = T),
         ptv_min = pmin(Q66AXPL_1, Q66AXPL_2, 
                        Q66AXPL_3, Q66AXPL_4, 
                        Q66AXPL_5, Q66AXPL_6, na.rm = T),
         aff_pol = ptv_max - ptv_min, 
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXPL_1:Q66AXPL_6) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

sp <- d |> 
  filter(country == 3) |> 
  select(id, starts_with("Q66AXSP")) |> 
  mutate(ptv_max = pmax(Q66AXSP_1, Q66AXSP_2, 
                        Q66AXSP_3, Q66AXSP_4, 
                        Q66AXSP_5, Q66AXSP_6, 
                        Q66AXSP_7, Q66AXSP_8, 
                        Q66AXSP_9, Q66AXSP_10, 
                        Q66AXSP_11, Q66AXSP_12, 
                        Q66AXSP_13, Q66AXSP_14, na.rm = T),
         ptv_min = pmin(Q66AXSP_1, Q66AXSP_2, 
                        Q66AXSP_3, Q66AXSP_4, 
                        Q66AXSP_5, Q66AXSP_6, 
                        Q66AXSP_7, Q66AXSP_8, 
                        Q66AXSP_9, Q66AXSP_10, 
                        Q66AXSP_11, Q66AXSP_12, 
                        Q66AXSP_13, Q66AXSP_14, na.rm = T),
         aff_pol = ptv_max - ptv_min, 
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXSP_1:Q66AXSP_14) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

fr <- d |> 
  filter(country == 4) |> 
  select(id, starts_with("Q66AXFR")) |> 
  mutate(ptv_max = pmax(Q66AXFR_1, Q66AXFR_2, 
                        Q66AXFR_3, Q66AXFR_4, 
                        Q66AXFR_5, Q66AXFR_6, 
                        Q66AXFR_7, Q66AXFR_8, 
                        Q66AXFR_9,na.rm = T),
         ptv_min = pmin(Q66AXFR_1, Q66AXFR_2, 
                        Q66AXFR_3, Q66AXFR_4, 
                        Q66AXFR_5, Q66AXFR_6, 
                        Q66AXFR_7, Q66AXFR_8, 
                        Q66AXFR_9,na.rm = T),
         aff_pol = ptv_max - ptv_min, 
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXFR_1:Q66AXFR_9) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

de <- d |> 
  filter(country == 5) |> 
  select(id, starts_with("Q66AXDE")) |> 
  mutate(ptv_max = pmax(Q66AXDE_1, Q66AXDE_2, 
                        Q66AXDE_3, Q66AXDE_4, 
                        Q66AXDE_5, Q66AXDE_6, na.rm = T),
         ptv_min = pmin(Q66AXDE_1, Q66AXDE_2, 
                        Q66AXDE_3, Q66AXDE_4, 
                        Q66AXDE_5, Q66AXDE_6, na.rm = T),
         aff_pol = ptv_max - ptv_min, 
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXDE_1:Q66AXDE_6) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

sw <- d |> 
  filter(country == 6) |> 
  select(id, starts_with("Q66AXSW")) |> 
  mutate(ptv_max = pmax(Q66AXSW_1, Q66AXSW_2, 
                        Q66AXSW_3, Q66AXSW_4, 
                        Q66AXSW_5, Q66AXSW_6,  
                        Q66AXSW_7, Q66AXSW_8, na.rm = T),
         ptv_min = pmin(Q66AXSW_1, Q66AXSW_2, 
                        Q66AXSW_3, Q66AXSW_4, 
                        Q66AXSW_5, Q66AXSW_6,  
                        Q66AXSW_7, Q66AXSW_8, na.rm = T),
         aff_pol = ptv_max - ptv_min, 
         aff_pol = replace_na(aff_pol, mean(aff_pol, na.rm=T))) |>
  pivot_longer(cols = Q66AXSW_1:Q66AXSW_8) |> 
  mutate(stdev = sd(value, na.rm=T),
         affpol_div = aff_pol * stdev) |> 
  distinct(id, aff_pol, affpol_div) 

ap <- it |> 
  add_case(pl) |> 
  add_case(sp) |> 
  add_case(fr) |> 
  add_case(de) |> 
  add_case(sw)


d <- full_join(d, ap, by = "id") |> 
  select(id:educ,
         popu_list:affpol_div,
         selfplacement = Q65,
         ideology = Q71) |> 
  mutate(country = case_when( #recode country values to names
    country == 1 ~ "Italy",
    country == 2 ~ "Poland",
    country == 3 ~ "Spain",
    country == 4 ~ "France",
    country == 5 ~ "Germany",
    country == 6 ~ "Sweden"))

rm(ap, de, fr, it, pl, sp, sw)

#d$Q79_7 %>% attr(which = "label")
#d %>% surveytoolbox::extract_vallab("Q79_7")
#d %>% .[,304:306] %>% surveytoolbox::varl_tb()
