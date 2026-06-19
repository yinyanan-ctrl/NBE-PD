rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(pheatmap)

data <- read.csv('heatmap.csv', row.names = 1, header = TRUE)
data_scaled <- t(scale(t(data)))  # ת?ú???׼??????ת?û?��

min_val <- quantile(data_scaled, 0.01, na.rm = TRUE)
max_val <- quantile(data_scaled, 0.99, na.rm = TRUE)
bk <- seq(min_val, max_val, length = 50)  # ??̬???? 50 ???ϵ?

pdf("heatmap_fixed.pdf", width = 5, height = 7)
p1 <- pheatmap(
  data,
  scale = "row",          
  cluster_cols = FALSE,
  cluster_rows = FALSE,
  breaks = bk,            
  show_colnames = TRUE,
  show_rownames = TRUE,
  color = colorRampPalette(c("#1E90FF", "#FFFFFF", "#FF6347"))(50),  
  border_color = NA,
  treeheight_row = 5,
  treeheight_col = 5,
  fontsize = 10,
  fontsize_row = 6,
  legend = TRUE,
  annotation_legend = TRUE
)
dev.off()