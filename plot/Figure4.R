library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
setwd("~/Library/CloudStorage/OneDrive-UniversityofNebraskaMedicalCenter/OneDrive - University of Nebraska Medical Center 2/group R_new_20231003/plot_design/")
plot.design31 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90", "black","gray50"), labels = c("0", "Unif(0,2)","Unif(0,1)")) +
    scale_color_manual(values = c("gray90","black","gray50"), labels = c("0", "Unif(0,2)","Unif(0,1)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  return(pp)
  #pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
}

plot.design32 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90", "gray50", "black"), labels = c("0", "Unif(0,1)","Unif(0,2)")) +
    scale_color_manual(values = c("gray90","gray50","black"), labels = c("0", "Unif(0,1)","Unif(0,2)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  
 # pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
  return(pp)
}

plot.design41 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, V4, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90","black","gray50"), labels = c("0", "Unif(0,4)", "Unif(0,1)")) +
    scale_color_manual(values = c("gray90","black","gray50"), labels = c("0", "Unif(0,4)", "Unif(0,1)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  
  #pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
  return(pp)
}


plot.design42 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, V4, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90","gray50","black"), labels = c("0", "Unif(0,1)", "Unif(0,4)")) +
    scale_color_manual(values = c("gray90","gray50","black"), labels = c("0", "Unif(0,1)", "Unif(0,4)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  
 # pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
  return(pp)
}


plot.design51 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, V4, V5, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90","black","gray50"), labels = c("0", "Unif(0,4)", "Unif(0,1)")) +
    scale_color_manual(values = c("gray90","black","gray50"), labels = c("0", "Unif(0,4)", "Unif(0,1)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  
 # pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
  return(pp)
}

plot.design52 <- function(beta, title) {
  df <- as_tibble(beta) %>%
    mutate(Variable = 1:nrow(beta)) %>%
    gather(V1, V2, V3, V4, V5, key = "Dataset", value = "Effect") %>%
    mutate(Dataset = str_replace(Dataset, "V", ""))
  
  pp <- df %>%
    mutate(Effect = as.factor(Effect)) %>%
    ggplot(aes(x = Dataset, y = Variable, fill = Effect, color = Effect)) +
    geom_tile() +
    scale_fill_manual(values = c("gray90", "gray50","black"), labels = c("0", "Unif(0,1)", "Unif(0,4)")) +
    scale_color_manual(values = c("gray90","gray50","black"), labels = c("0", "Unif(0,1)", "Unif(0,4)")) +
    labs(x = "Dataset", y = "Groups of Predictors",color = "Signal\nStrength", fill = "Signal\nStrength") +
    theme_bw() +
    theme(
      legend.position = "left",
      plot.title = element_text(size = 10,hjust = 0.5,vjust=1),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.title = element_text(size = 10)
    ) +
    ggtitle(title)
  
 # pp %>% ggsave(file = filename, width = 3, height = 2.25, units = "in")
  return(pp)
}


################ K=3 #############################
plot1 <- plot.design31(beta = matrix(c(rep(c(1, 1, 1), 12), rep(c(0, 0, 0), 28)), nrow = 40, byrow = TRUE), title = "Choice 1")
plot2 <- plot.design32(beta = matrix(c(rep(c(1, 1, 1), 12), rep(c(0.5, 0, 0), 6), rep(c(0, 0.5, 0), 6), rep(c(0, 0, 0.5), 6), rep(c(0.5, 0.5, 0), 2), rep(c(0, 0.5, 0.5), 2), rep(c(0, 0, 0), 6)), nrow = 40, byrow = TRUE), title = "Choice 2")
plot3 <- plot.design32(beta = matrix(c(rep(c(1, 1, 1), 12), rep(c(0.5, 0.5, 0), 6), rep(c(0, 0.5, 0.5), 6), rep(c(0.5, 0, 0.5), 6), rep(c(0, 0, 0), 10)), nrow = 40, byrow = TRUE), title = "Choice 3")

################ K=4 #############################
plot4 <- plot.design41(beta = matrix(c(rep(c(1, 1, 1, 1), 12),
                            rep(c(0, 0, 0, 0), 28)),
                          nrow = 40, byrow = TRUE),
            title = "Choice 1")

plot5 <- plot.design42(beta = matrix(c(rep(c(1, 1, 1, 1), 12),
                             rep(c(0.5, 0, 0, 0), 4),
                             rep(c(0, 0.5, 0, 0), 4), 
                             rep(c(0, 0, 0.5, 0), 4),
                             rep(c(0, 0, 0, 0.5), 4),
                             rep(c(0, 0, 0, 0), 12)),
                           nrow = 40, byrow = TRUE),
             title = "Choice 2")

plot6 <- plot.design42(beta = matrix(c(rep(c(1, 1, 1, 1), 12),
                             rep(c(0.5, 0.5, 0.5, 0), 6),
                             rep(c(0.5, 0.5, 0, 0.5), 6),
                             rep(c(0.5, 0, 0.5, 0.5), 6),
                             rep(c(0, 0.5, 0.5, 0.5), 6),
                             rep(c(0, 0, 0, 0), 4)),
                           nrow = 40, byrow = TRUE),
             title = "Choice 3")

plot7 <- plot.design51(beta = matrix(c(rep(c(1, 1, 1, 1, 1), 12),
                            rep(c(0, 0, 0, 0, 0), 28)),
                          nrow = 40, byrow = TRUE),
            title = "Choice 1")

plot8 <- plot.design52(beta = matrix(c(rep(c(1, 1, 1, 1, 1), 12),
                             rep(c(0, 0.5, 0.5, 0.5, 0), 4),
                             rep(c(0, 0.5, 0.5, 0, 0.5), 4), 
                             rep(c(0, 0.5, 0, 0.5, 0.5), 4),
                             rep(c(0, 0, 0.5, 0.5, 0.5), 4),
                             rep(c(0, 0, 0, 0, 0), 12)),
                           nrow = 40, byrow = TRUE),
             title = "Choice 2")

plot9 <- plot.design52(beta = matrix(c(rep(c(1, 1, 1, 1, 1), 12),
                             rep(c(0.5, 0.5, 0.5, 0.5, 0), 5),
                             rep(c(0.5, 0.5, 0.5, 0, 0.5), 5),
                             rep(c(0.5, 0.5, 0, 0.5, 0.5), 5),
                             rep(c(0.5, 0, 0.5, 0.5, 0.5), 5),
                             rep(c(0, 0.5, 0.5, 0.5, 0.5), 5),
                             rep(c(0, 0, 0, 0, 0), 3)),
                           nrow = 40, byrow = TRUE),
             title = "Choice 3")

combined_plot <- grid.arrange(plot1, plot2, plot3, plot4,plot5,plot6,plot7,plot8,plot9,ncol = 3,nrow=3)


tiff("Figure4.tif", width = 15, height = 9, units = 'in', res = 300)
print(combined_plot)
dev.off()