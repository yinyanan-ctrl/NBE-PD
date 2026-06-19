

rm(list = ls())
graphics.off()

library(factoextra)
library(ggplot2)

# 读取数据
unique_0_GP <- read.csv("pca-1.csv", row.names = 1)
GPs <- t(unique_0_GP)

# 数据预处理
GPs[GPs == 0] <- 1e-5
GPs <- log(GPs)

# PCA
GPs.pca <- prcomp(GPs)

# 提取 PCA 坐标
df <- as.data.frame(GPs.pca$x[, 1:2])

# 分组信息
df$Group <- factor(
  c(rep("Ctrl", times = 541), rep("PD", times = 2084)),
  levels = c("Ctrl", "PD")
)

# 解释方差
var_exp <- round(GPs.pca$sdev^2 / sum(GPs.pca$sdev^2) * 100, 1)

# 绘图
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
    # ??? 添加黑色坐标轴
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black", linewidth = 0.8),
    axis.text = element_text(colour = "black", size = 12)
  )

# 保存 PDF
ggsave(
  filename = "PCA-1.pdf",
  plot = p,
  width = 10,
  height = 10
)
