rm(list = ls())
graphics.off()
library(factoextra)
library(ggplot2)
unique_0_GP <- read.csv("pca-1.csv", row.names = 1)
GPs <- t(unique_0_GP)
GPs[GPs == 0] <- 1e-5
GPs <- log(GPs)
GPs.pca <- prcomp(GPs)
df <- as.data.frame(GPs.pca$x[, 1:2])
df$Group <- factor(
  c(rep("Ctrl", times = 541), rep("PD", times = 2084)),
  levels = c("Ctrl", "PD")
)
var_exp <- round(GPs.pca$sdev^2 / sum(GPs.pca$sdev^2) * 100, 1)
p <- ggplot(df, aes(x = PC1, y = PC2, colour = Group, shape = Group)) +
  geom_point(
    aes(size = Group),
    alpha = 0.7
  ) +
  scale_colour_manual(
    values = c("Ctrl" = "#1E90FF", "PD" = "#FF6347"),
    labels = c("Ctrl (n = 541)", "PD (n = 2084)")
  ) +
  scale_shape_manual(
    values = c("Ctrl" = 16, "PD" = 17),
    labels = c("Ctrl (n = 541)", "PD (n = 2084)")
  ) +
  scale_size_manual(
    values = c("Ctrl" = 3, "PD" = 3),
    guide = "none"
  ) +
  stat_ellipse(
    geom = "polygon",
    aes(fill = Group),
    level = 0.95,
    alpha = 0.08,
    linetype = "blank"
  ) +
  stat_ellipse(
    level = 0.95,
    linewidth = 1.0,
    linetype = "solid",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("Ctrl" = "#1E90FF", "PD" = "#FF6347"),
    guide = "none"
  ) +
  labs(
    x = paste0("PC1 (", var_exp[1], "%)"),
    y = paste0("PC2 (", var_exp[2], "%)"),
    title = "PCA of Ctrl vs PD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black", linewidth = 0.8),
    axis.text = element_text(colour = "black", size = 12)
  )
ggsave(
  filename = "PCA-1.pdf",
  plot = p,
  width = 10,
  height = 10
)
