library(ggpmisc)
library(dplyr)
library(tidyr)

dimmer.tb <- read.csv("./data-raw/yocotopuce/dimmer-V-A.csv") |>
  pivot_longer(cols = ends_with("A"),
               names_to = "channel",
               values_to = "A") |>
  mutate(channel = sub("\\.A$", "", channel))


dimmer_V_A.fig <-
  ggplot(dimmer.tb, aes(V, A)) +
  stat_poly_line(data = function(x) {subset(x, V < 4.5)},
                 se = FALSE, colour = "black") +
  stat_poly_eq(data = function(x) {subset(x, V < 4.5)},
               mapping = use_label("eq", "R2")) +
  geom_point(aes(colour = channel), shape = "circle open", size = 3, na.rm = TRUE) +
  scale_x_continuous(transform = "reverse") +
  ggtitle("LED dimmer Constant current control settings",
          subtitle = "Yocto-0-10V-Tx (YoctoPuce) + RCD-48-1.2M (RECOM Power)") +
  theme_bw(14) + theme(plot.title = element_text(size = 12),
                       plot.subtitle = element_text(size = 11),
                       legend.position = "inside",
                       legend.position.inside = c(0.85, 0.3))

dimmer_V_A.fig

pdf(file = "./data-raw/yocotopuce/dimmer-V-A-fig.PDF", width = 6.5, height = 5)
print(dimmer_V_A.fig)
dev.off()
