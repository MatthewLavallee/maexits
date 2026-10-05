# Draws man/figures/exits-by-year.png, the README figure, from the exits
# table, and prints the figure's numbers as a markdown table.
# Run from the repository root with the package installed; needs ggplot2.

library(maexitsv2)
library(ggplot2)

e <- maexits_data("exits", variables = c("exit_type", "dec_enrollment", "sep_enrollment"))

share <- function(enr) e[!is.na(get(enr)), .(
  total = sum(get(enr)),
  terminated = 100 * sum(get(enr)[exit_type == "terminated"]) / sum(get(enr)),
  sar = 100 * sum(get(enr)[exit_type == "service_area_reduction"]) / sum(get(enr)),
  both = 100 * sum(get(enr)[exit_type != "none"]) / sum(get(enr))), keyby = dec_year]
sep <- share("sep_enrollment")
dec <- share("dec_enrollment")
gap <- sep[dec, on = "dec_year"][, both - i.both]

types <- c(both = "Terminated or SAR", terminated = "Plan terminated", sar = "Service-area reduction (SAR)")
long <- melt(sep, id.vars = "dec_year", measure.vars = names(types), variable.name = "type", value.name = "pct")
long[, type := factor(types[as.character(type)], levels = types)]
end <- long[dec_year == max(dec_year)][, label := sprintf("%.1f%%", pct)]
lab <- function(y) paste0(y, "–", substr(y + 1, 3, 4))

ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#8b8a85"; grid <- "#e6e5e1"; surface <- "#fcfcfb"
cols <- setNames(c("#2b2b2b", "#2a78d6", "#eb6834"), types)

p <- ggplot(long, aes(dec_year, pct, colour = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4, stroke = 0) +
  geom_text(data = end, aes(label = label), hjust = -0.35, size = 3.7, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = sep$dec_year, labels = lab(sep$dec_year),
                     expand = expansion(add = c(0.3, 0.7))) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), breaks = seq(0, 12, 2),
                     limits = c(0, ceiling(max(long$pct)) + 0.2), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Share of September MA enrollment",
       title = "Medicare Advantage enrollees in plans that CMS terminated or cut from their county",
       subtitle = "September enrollment in plans terminated, or ending service in the county (SAR), the following January",
       caption = paste0(
         "September enrollment is used in every year so the newest transition compares like for like ",
         "before its December enrollment is out.\n",
         sprintf("December enrollment gives shares %.2f–%.2f points lower (%d–%d).\n",
                 min(gap), max(gap), min(dec$dec_year), max(dec$dec_year)),
         "Individual-market MA plans and SNPs. CMS-suppressed counts (1–10 enrollees) counted as 10.\n",
         "Source: maexitsv2 exits table (CMS CPSC enrollment, MA landscape and Part C&D plan crosswalk files).")) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = surface, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = grid, linewidth = 0.4),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 10.5),
        plot.title = element_text(colour = ink, face = "bold", size = 15),
        plot.subtitle = element_text(colour = ink2, size = 11, margin = margin(b = 8)),
        plot.caption = element_text(colour = muted, size = 8.5, hjust = 0, margin = margin(t = 10),
                                    lineheight = 1.15),
        plot.caption.position = "plot", plot.title.position = "plot",
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(colour = ink2, size = 10.5), legend.key.width = unit(1.1, "cm"),
        plot.margin = margin(14, 16, 10, 12))

ggsave(file.path("man", "figures", "exits-by-year.png"), p, width = 9.5, height = 6, dpi = 200, bg = surface)

# The figure's numbers, for the README
tab <- dec[, .(dec_year, dec_both = both)][sep, on = "dec_year"]
f <- function(v) ifelse(is.na(v), "not out yet", sprintf("%.2f", v))
cat("| Transition | Plan terminated | Service-area reduction | Terminated or SAR | Terminated or SAR, December enrollment |\n",
    "|---|--:|--:|--:|--:|\n", sep = "")
cat(tab[, sprintf("| %s | %s | %s | %s | %s |\n", lab(dec_year), f(terminated), f(sar), f(both), f(dec_both))],
    sep = "")
