# Draws man/figures/lost-coverage-by-year.png, the README figure, from the
# displacement table, and prints the figure's numbers as a markdown table.
# Run from the repository root with the package installed; needs ggplot2.

library(maexitsv2)
library(ggplot2)

d <- maexits_data("displacement", variables = c("dec_enrollment", "lost_coverage", "forced_reason"))

types <- c("Plan terminated", "Service-area reduction",
           "Moved to a plan outside the county, or not listed")
d[, type := fcase(forced_reason == "terminated", types[1],
                  forced_reason == "service_area_reduction", types[2],
                  lost_coverage, types[3])]

yr <- d[, .(december = sum(dec_enrollment), lost = sum(dec_enrollment[lost_coverage])),
        keyby = dec_year][, pct := 100 * lost / december]
by_type <- d[lost_coverage == TRUE, .(lost = sum(dec_enrollment)), keyby = .(dec_year, type)]
by_type <- yr[, .(dec_year, december)][by_type, on = "dec_year"][, pct := 100 * lost / december]

lab <- function(y) paste0(y, "–", substr(y + 1, 3, 4))
by_type[, `:=`(x = factor(lab(dec_year), levels = lab(yr$dec_year)),
               type = factor(type, levels = rev(types)))]    # first type drawn at the bottom
yr[, `:=`(x = factor(lab(dec_year), levels = lab(dec_year)), label = sprintf("%.1f%%", pct))]

ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#8b8a85"; grid <- "#e6e5e1"; surface <- "#fcfcfb"
fills <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), types)

p <- ggplot(by_type, aes(x = x, y = pct, fill = type)) +
  geom_col(width = 0.62, colour = surface, linewidth = 0.7) +
  geom_text(data = yr, aes(x = x, y = pct, label = label), inherit.aes = FALSE,
            vjust = -0.6, size = 3.6, colour = ink, fontface = "bold") +
  scale_fill_manual(values = fills, breaks = types, name = NULL) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), breaks = seq(0, 10, 2),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "December to January transition", y = "Share of December enrollment",
       title = "Medicare Advantage enrollees who lost their plan, by year",
       subtitle = "December enrollment in plans that did not continue in the enrollee's county the next January",
       caption = paste0("Individual-market MA plans and SNPs. CMS-suppressed counts (1–10 enrollees) counted as 10.\n",
                        "Source: maexitsv2 displacement table (CMS CPSC enrollment, MA landscape and Part C&D plan crosswalk files).")) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = surface, colour = NA),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = grid, linewidth = 0.4),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 10.5),
        plot.title = element_text(colour = ink, face = "bold", size = 15),
        plot.subtitle = element_text(colour = ink2, size = 11, margin = margin(b = 8)),
        plot.caption = element_text(colour = muted, size = 8.5, hjust = 0, margin = margin(t = 10)),
        plot.caption.position = "plot", plot.title.position = "plot",
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(colour = ink2, size = 10), legend.key.size = unit(0.45, "cm"),
        plot.margin = margin(14, 16, 10, 12))

ggsave(file.path("man", "figures", "lost-coverage-by-year.png"), p,
       width = 9.5, height = 5.6, dpi = 200, bg = surface)

# The figure's numbers, for the README
wide <- dcast(by_type, dec_year ~ type, value.var = "pct")
tab <- yr[wide, on = "dec_year"]
f <- function(v) sprintf("%.2f", v)
cat("| Transition | Lost their plan | Share of December enrollment (%) | Plan terminated | Service-area reduction | Moved to a plan outside the county, or not listed |\n",
    "|---|--:|--:|--:|--:|--:|\n", sep = "")
cat(tab[, sprintf("| %s | %s | %s | %s | %s | %s |\n", lab(dec_year), format(lost, big.mark = ","),
                  f(pct), f(get(types[1])), f(get(types[2])), f(get(types[3])))], sep = "")
