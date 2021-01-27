# ggplot2 clustering vizualization in the style of BigQueryML kmeans evaluation viz 

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# variables
cluster_var <- "clarity"
variables_to_viz <- c("depth", "table", "price",
                      "x",     "y",     "z")

# colors mgmt
colourCount = 12
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(colourCount)

diamonds %>%
    select(cluster_var, variables_to_viz) %>%
    rename(cluster_id=cluster_var) %>%
    gather(key='variable', value = 'value', -cluster_id) %>%
    drop_na() %>%
    group_by(cluster_id, variable) %>%
    summarise(mean_value=mean(value)) %>%
    group_by(variable) %>%
    mutate(mean_value_std=(mean_value - min(mean_value)) / (max(mean_value) - min(mean_value)) + 0.05) %>%
    ggplot(aes(x=cluster_id, y=mean_value_std, fill=cluster_id)) +
    geom_col() + 
    coord_flip() +
    facet_grid(. ~ variable, space = "free") +
    scale_fill_manual(values = mycolors)
