# Blog figures. Data (tibble and ggplot object) for each chart is imported from blog/blog_data folder

library(noradplot)
library(ggplot2)
library(scales)
library(patchwork)
ggnorad()

# Plot 1 ------------------------------------------------------------------

load("blog/blog_data/conflict_nor_dac.RData")

p_df_conflict_nor_dac <- df_conflict_nor_dac |> 
  ggplot(aes(x = year, y = pct, color = donor)) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = label_maxyear), na.rm = T, hjust = -0.5, size = 5.5, show.legend = F, color = "black") +
  scale_x_continuous(breaks = seq(from = min(df_conflict_nor_dac$year), to = max(df_conflict_nor_dac$year), by = 3), expand = expansion(c(0.05, 0.2))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = expansion(c(0, 0))) +
  labs(
    #title = "Bistand til land med konflikt, prosent av landspesifikk bistand",
    #subtitle = "ODA fra Norge og andre OECD/DAC-medlemmer, 2010-2022",
    x = NULL,
    y = NULL,
    color = NULL,
    #caption = "Land med over 150 konfliktrelaterte d\u00F8dsfall i l\u00F8pet av \u00E5ret (Verdensbank-metodologi).\nUCDP"
  )+
  scale_color_manual(values = c("OECD/DAC-land" = "#03542d", "Norge" = "#ff8ad2"), name = NULL) + 
  theme(
    legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(family = "Norad Sans", size = 18, face = "plain", margin = margin(b = 20)),
        plot.background = element_rect(fill = "#EFF6EA", color = "#EFF6EA"),
        panel.background = element_rect(fill = "#EFF6EA")
    ) +
  guides(color = guide_legend(reverse = FALSE))


# Plot 2 ------------------------------------------------------------------

load("blog/blog_data/recipient_groups_pct.RData")

p_recipient_groups_pct <- df_recipient_groups_pct |> 
  ggplot(aes(year, pst, fill = recipient_group)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = expansion(c(0, 0))) +
  scale_x_continuous(breaks = seq(from = min(df_recipient_groups_pct$year), to = max(df_recipient_groups_pct$year), by = 2), expand = expansion(c(0.05, 0.05))) +
  scale_fill_manual(values = c("#ff8ad2", "#b4eac9", "#03542d")) +
  labs(
    #title = "Prosent av norsk \u00F8remerket bistand",
    x = NULL, y = NULL,
    fill = NULL
  ) +
  theme(
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    axis.text = element_text(size = 14),
    #plot.title = element_text(family = "Norad Sans", size = 18, face = "plain", margin = margin(b = 20)),
    # plot.subtitle = element_text(margin = margin(b = 20)),
    plot.background = element_rect(fill = "#EEE8E7", color = "#EEE8E7"),
    panel.background = element_rect(fill = "#EEE8E7")
  ) +
  guides(fill = guide_legend(reverse = TRUE))


# Plot 3 ------------------------------------------------------------------

load("blog/blog_data/topten.RData")

p_topten2014 <- df_topten2014 |> 
  ggplot(aes(x= recipient_country_no, y = nok_mill, fill = violence_150_character)) +
  geom_col() + 
  coord_flip() +
  scale_fill_norad() +
  scale_y_continuous(expand = expansion(c(0, 0.1)), limits = c(0, 9000)) +
  labs(title = "2014", x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom", axis.ticks.y = element_blank())

p_topten2023 <- df_topten2023_adjusted |> 
  ggplot(aes(x= recipient_country_no, y = nok_mill, fill = violence_150_character)) +
  geom_col() + 
  coord_flip() +
  scale_fill_norad() +
  scale_y_continuous(expand = expansion(c(0, 0.1)), limits = c(0, 9000)) +
  labs(title = "2023", x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom", axis.ticks.y = element_blank())

p_topten_combine <- p_topten2014 + p_topten2023 +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Ti st\u00F8rste samarbeidsland i norsk bistand",
                  caption = "Konfliktland er land med minst 150 konfliktrelaterte d\u00F8dsfall i minst 1 konflikt i løpet av \u00E5ret (Verdensbank-metodologi).\nAfghanistan og S\u00F8r-Sudan var i 2023 under konflikt-terskelen, men er likevel kategorisert konflikt fordi 2023-data er forel\u00F8pige og\nlandene t.o.m. 2022 var over konfliktterskelen.\nKilde: UCDP") &
  theme(legend.position = 'bottom', plot.caption = element_text(size = 10))


# Plot 4 ------------------------------------------------------------------

load("blog/blog_data/region_pct_time.RData")

p_region_pct_time <- df_region_pct_time |> 
  ggplot(aes(year, pct, color = main_region_no)) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(breaks = seq(min(df_region_pct_time$year), max(df_region_pct_time$year), by = 2)) +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent, expand = c(0, NA)) +
  scale_color_manual(values = c("#03542d", "#ff8ad2", "#e1e11f", "grey", "grey", "grey")) +
  labs(
    #title = "Prosent av landspesifikk bistand til konfliktland i utvalgte regioner",
    #subtitle = "Prosent av landspesifikk bistand",
    x = NULL, y = NULL,
    color = NULL
  ) +
  theme(
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(family = "Norad Sans", size = 18, face = "plain", margin = margin(b = 20)),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.background = element_rect(fill = "#EFF6EA", color = "#EFF6EA"),
    panel.background = element_rect(fill = "#EFF6EA")
  )


# Plot 5 ------------------------------------------------------------------

load("blog/blog_data/type.RData")

p_type <- df_type |> 
  ggplot(aes(year, nok_percent, color = aid_type)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = expansion(c(0, 0))) +
  scale_x_continuous(breaks = seq(from = min(df_type$year), to = max(df_type$year), by = 2), expand = expansion(c(0.05, 0.05))) +
  scale_color_manual(values = c("#03542d", "#ff570d", "#e1e11f")) +
  labs(
    #title = "Prosent av norsk \u00F8remerket bistand",
    x = NULL, y = NULL,
    color = NULL
  ) +
  theme(
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(family = "Norad Sans", size = 18, face = "plain", margin = margin(b = 20)),
    plot.background = element_rect(fill = "#EEE8E7", color = "#EEE8E7"),
    panel.background = element_rect(fill = "#EEE8E7")
  )


# Plot 6 ------------------------------------------------------------------

load("blog/blog_data/peace_dac_time.RData")
load("blog/blog_data/peace_topfive.RData")

p_peace_dac_time <- df_peace_dac_time |> 
  ggplot(aes(x = year, y = peace_share)) +
  geom_line(linewidth = 1.5, color = "#03542d") +
  #geom_text(aes(label = label_maxyear), na.rm = T, hjust = -0.5, size = 5.5, show.legend = F, color = "black") +
  scale_x_continuous(breaks = seq(from = min(df_peace_dac_time$year), to = max(df_peace_dac_time$year), by = 3), expand = expansion(c(0.05, 0.2))) +
  scale_y_continuous(
    limits = c(0, 0.1),
    labels = label_percent(accuracy = 1),
    expand = expansion(c(0, 0))
  ) +
  labs(
    #title = "Fredsbistand (152) fra OECD/DAC-medlemmer",
    #subtitle = "Prosent av bilateral bisatnd. 2013-2022",
    x = NULL,
    y = NULL,
    color = NULL,
  )+
  scale_color_manual("#03542d") + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(family = "Norad Sans", size = 18, face = "plain", margin = margin(b = 20)),
        plot.background = element_rect(fill = "#EFF6EA", color = "#EFF6EA"),
        panel.background = element_rect(fill = "#EFF6EA")
  )

p_peace_topfive <- df_peace_topfive |> 
  ggplot(aes(x = donor_name, y = total_disbursed, fill = donor_name)) +
  geom_bar(stat = "identity", show.legend = F) +
  # geom_text(aes(label = scales::number(total_disbursed, accuracy = 1)), hjust = 0, nudge_y = 50) +
  geom_text(aes(label = donor_name), y = 200, hjust = 0, family = "Norad Display", size = 3.5, color = c("#b4eac9", "#b4eac9", "#b4eac9", "black", "#b4eac9")) +
  scale_y_continuous(expand = expansion(c(0, NA))) +
  coord_flip() +
  labs(
    # title = "Fem st\u00F8rste givere til konfliktl\u00F8sning og forebygging",
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c("#03542d", "#e1e11f", "#03542d", "#03542d", "#03542d", "#03542d")) +
  theme(
    plot.margin = unit(c(1,1,1,1), "mm"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#EFF6EA", color = "#EFF6EA"),
    panel.background = element_rect(fill = "#EFF6EA", color = "#EFF6EA")
  )

# Combine
p_peace_dac_time + p_peace_topfive
