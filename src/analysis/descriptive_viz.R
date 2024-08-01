df <- df |> 
  mutate(pc_type3 = case_when(
    pc_type == 2 ~ "Populist PC",
    pc_type == 1 ~ "Mainstrain PC",
    TRUE ~ "No PC"),
  pc_type3 = factor(pc_type3,
                    levels = c("No PC",
                               "Mainstrain PC",
                               "Populist PC")))

df2 <- df |> 
  group_by(pid2, pc_type3) |> 
  summarize(affpol_div = mean(affpol_div, na.rm=TRUE)) |> 
  ungroup() 


p2 <- ggplot(data = df, aes(x = pid2, y = affpol_div, color=pc_type3)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, outlier.shape = 16) +
  geom_jitter(size=0.1, alpha=0.2) +
  geom_line(data = df2, aes(group=pc_type3), 
            position = position_dodge(width=0.78), linewidth = 0.7) +
  geom_point(data = df2, aes(group=pc_type3), color = '#555555',
             position = position_dodge(width=0.78)) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "Affective Polarization") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

df3 <- df |> 
  group_by(pid2, pc_type3) |> 
  summarize(picom_index = mean(picom_index, na.rm=TRUE)) |> 
  ungroup() 

p3 <- ggplot(data = df, aes(x = pid2, y = picom_index, color=pc_type3)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, outlier.shape = 16) +
  geom_jitter(size=0.1, alpha=0.2) +
  geom_line(data = df3, aes(group=pc_type3), 
            position = position_dodge(width=0.78), linewidth = 0.7) +
  geom_point(data = df3, aes(group=pc_type3), color = '#555555',
             position = position_dodge(width=0.78)) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "Conspiracy Mentality") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


p4 <- ggplot(data = df, aes(x = picom_index, y = affpol_div, 
                      color = pc_type3, fill = pc_type3)) +
  geom_point(size=0.1, alpha=0.2) +
  geom_jitter(size=0.1, alpha=0.2) +
  geom_smooth(method = "lm", linewidth = 0.8) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  labs(y = "Affective Polarization", x = "Conspiracy Mentality") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
