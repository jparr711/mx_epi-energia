# load libraries
knitr::opts_chunk$set(echo = FALSE, cache=TRUE, eval=TRUE,
                      tidy.opts=list(width.cutoff=60),
                      tidy=TRUE)
library(RCurl)
library(tidyverse)
library(RColorBrewer)
library(broom)
library(dplyr)
library(data.table)
#library(ggplot2)
# library(expss) 
library(plyr)
library(scales)
library(MASS)
library(ggridges)
# library(ggrepel)
library(reshape)
library(reshape2)
library(ggthemes)
library(psycho)
library(stringr)
library(gridExtra)
library(grid)
library(haven)
library(sp)
library(rgdal)
library(maptools)
library(sf)
library(stringi)
library(EpiModel)
library(magrittr)
library(lubridate)
library(tibble)
# library(gt)
library(knitr)
library(incidence)
library(earlyR)
library(ggspatial)
library(devtools)
library(mxmaps)
library(fmsb)
library(leaflet)
library(readr)
library(plotly)
library(tidytext)
library(corrr)
library(networkD3)
library(leaflet.extras)
library(igraph)
library(DT)
library(tidygraph)
library(lubridate)
library(tidytext)
library(tm)
library(hwordcloud)
library(wordcloud2)
library(highcharter)
library(htmltools)
library(sigmaNet)

# setwd("C:/Users/jparr/OneDrive - DAI/Mexico_FCO_Energy/MXENG/mx_fco_mep/")
load('data/final_sj.RData')
load('data/final_sj_en.RData')
load('data/final_sj_es.RData')
load('data/wordpct.RData')
load('data/subsector_word_pct.RData')

final_sj <- final_sj %>%
  dplyr::filter(company != "One Key Resources Pty Ltd") %>%
  dplyr::filter(company != "Workpac - Gunnedah")


final_sj_en <- 
  final_sj_en %>%
  dplyr::filter(company != "One Key Resources Pty Ltd") %>%
  dplyr::filter(company != "Workpac - Gunnedah")
# 
final_sj_es <- 
   final_sj_es %>%
   dplyr::filter(company != "One Key Resources Pty Ltd") %>%
   dplyr::filter(company != "Workpac - Gunnedah")
# 

final_sj <- final_sj %>%
   dplyr::mutate(date = ifelse(date == "Wrong Argument format", NA, date))
 
final_sj_en <- final_sj_en %>%
   dplyr::mutate(date = ifelse(date == "Wrong Argument format", NA, date))
 
final_sj_es <- final_sj_es %>%
   dplyr::mutate(date = ifelse(date == "Wrong Argument format", NA, date))



# join jobs data to map
location_sj_mun <- 
  final_sj %>%
#   dplyr::mutate(state = str_trim(state),
#                 municipality = str_trim(municipality)) %>%
   dplyr::group_by(municipality, state) %>%
#   dplyr::mutate(state = case_when(state == "Ciudad de México (Distrito Federal)" ~ "Ciudad de México",
#                                   state == "Puebla (de los Angeles)" ~ "Puebla",
#                                   state == "Estado de México" ~ "México",
#                                   TRUE ~ as.character(state))) %>%
  dplyr::count(.) %>%
  dplyr::mutate(n = ifelse(is.na(n), 0 , n))

location_sj_state <- 
  final_sj %>%
#  dplyr::mutate(state = str_trim(state)) %>%
  dplyr::group_by(state) %>%
#  dplyr::mutate(state = case_when(state == "Ciudad de México (Distrito Federal)" ~ "Ciudad de México",
#                                  state == "Puebla (de los Angeles)" ~ "Puebla",
#                                  state == "Estado de México" ~ "México",
#                                  TRUE ~ as.character(state))) %>%
  dplyr::count(.) %>%
  dplyr::mutate(n = ifelse(is.na(n), 0 , n))


df_mxs <- 
  mxmaps::df_mxstate %>% 
  dplyr::left_join(., location_sj_state, by = c("state_name" = "state")) 

df_mxm <- 
  mxmaps::df_mxmunicipio %>% 
  dplyr::left_join(location_sj_mun, by = c("municipio_name" = "municipality",
                          "state_name" = "state")) 




df_mxm$value <- as.numeric(as.character(df_mxm$n))
df_mxm$region <- as.factor(as.character(df_mxm$region))



palette <- colorNumeric(palette = "PuBuGn", domain = df_mxm$value)

 output$mun_map <- renderLeaflet({
  df_mxm %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = df_mxm %>% dplyr::filter(!is.na(value)), 
                     lng = ~ as.numeric(as.character(long)), 
                     lat = ~ as.numeric(as.character(lat)), 
                     radius = ~(value/10), 
                     popup =  ~sprintf("State: %s<br/>Municipio : %s<br/>Value: %s",
                                       state_name, municipio_name, round(value, 1)))
 })


 


df_mxs$value <- as.numeric(as.character(df_mxs$n))
df_mxs$region <- as.factor(as.character(df_mxs$region))


palette <- colorNumeric(palette = "PuBuGn", domain = df_mxs$value)



output$statemap <- renderLeaflet({
    mxstate_leaflet(df_mxs, 
                    popup =  ~ sprintf("State: %s<br/>Total Jobs: %s<br/>Jobs per 10K: %s",
                                       state_name,round(n, 1), round(value, 2)), 
                    fillOpacity = 0.1, opacity = .2, 
                    fillColor = ~palette(value)) %>% 
    addLegend(position = "bottomright", title = "Energy Jobs", pal = palette,
              values = (df_mxs$value)) %>%
    addProviderTiles("CartoDB.Positron")
})

### page 1
# aggregated jobs across time

df1 <- 
  final_sj %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::arrange(date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 


output$p1 <- renderPlotly({
  plot_ly(df1, x = ~date, y = ~n) %>%
    add_lines() %>%
    layout(xaxis = list(title = "Date"), 
           yaxis = list( title = "Total"))
})

# sector jobs across time
df2 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                   oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::group_by(sector_1, date) %>%
  dplyr::arrange(sector_1, date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 
  
output$p2 <- renderPlotly({
  plot_ly(df2, x = ~date, y = ~n, color = ~sector_1) %>%
  add_lines() %>%
  layout(xaxis = list(title = "Date"), 
           yaxis = list( title = "Total")) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5))             # put legend in center of x-axis
})

# subsector across time

df2.1 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                     oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::select(date, contains("subsector_")) %>%
  dplyr::filter_at(dplyr::vars(contains("subsector_")), dplyr::any_vars(. == 1)) %>%
  tidyr::pivot_longer(cols = contains("subsector")) %>%
  dplyr::filter(value == 1) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, "subsector_"),
                name = stringr::str_replace_all(name, "_", " "),
                name = str_to_title(name)) %>%
  dplyr::group_by(name, date) %>%
  dplyr::arrange(name, date) %>%
  dplyr::count(value) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  dplyr::rename("subsector" = "name") %>%
  as.data.frame(.) 

output$p2.1 <- renderPlotly({
  plot_ly(df2.1, x = ~date, y = ~n, color = ~subsector) %>%
    add_lines() %>%
    layout(xaxis = list(title = "Date"), 
           yaxis = list( title = "Total"))
})




output$p3 <- renderPlotly({
  df3 <- 
    final_sj %>%
    dplyr::mutate(company = str_remove(company, "Plc|Llc|LLC|LTD|PLC|Ltd|, Inc.|, Inc|Inc.|Inc|Corp/.|Corp$|S.A DE C.V.|SAS de CV"),
                  company = str_trim(company)) %>%
    dplyr::filter(!str_detect(company, "Sector Industrial Petrolero Oil &amp; Gas|None")) %>%
    dplyr::mutate(company = str_replace(company, "&amp;", "&")) %>%
    dplyr::mutate(company = str_replace_all(company, "Enel$", "Enel Green Power"),
                  company = str_to_title(company)) %>%
    dplyr::filter(state %in% input$state) %>%
    dplyr::group_by(company) %>%
    dplyr::count(.) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(company = str_trim(company)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(company != "Oilandgas") %>%
    dplyr::filter(company != "None", company != "Chili'S") %>%
    dplyr::top_n(10, n) %>%
    as.data.frame(.)
  
  df3$company <- factor(df3$company, levels = unique(df3$company)[order(df3$n, decreasing = F)])
  
  plot_ly(df3, y = ~company, x = ~n, type = "bar")  %>%
    layout(xaxis = list(title = "Total"), 
           yaxis = list( title = "Company"))
})

output$p3.1 <- renderPlotly({
df3.1 <- 
  final_sj %>%
  dplyr::mutate(company = str_remove(company, "Plc|Llc|LLC|LTD|PLC|Ltd|, Inc.|, Inc|Inc.|Inc|Corp/.|Corp$|S.A DE C.V.|SAS de CV"),
                company = str_trim(company)) %>%
  dplyr::filter(!str_detect(company, "Sector Industrial Petrolero Oil &amp; Gas|None")) %>%
  dplyr::mutate(company = str_replace(company, "&amp;", "&")) %>%
  dplyr::mutate(company = str_replace_all(company, "Enel$", "Enel Green Power"),
                company = str_to_title(company)) %>%
  tidyr::pivot_longer(cols = contains("subsector")) %>%
  dplyr::filter(value == 1) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, "subsector_"),
                name = stringr::str_replace_all(name, "_", " "),
                name = str_to_title(name)) %>%
  dplyr::filter(state %in% input$state) %>%
  dplyr::rename("subsector" = "name") %>%
  dplyr::filter(subsector %in% input$subsector) %>%
  dplyr::group_by(company, subsector) %>%
  dplyr::count(.) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(company = str_trim(company)) %>%
  dplyr::ungroup()%>%
  dplyr::filter(company != "Oilandgas") %>%
  dplyr::filter(company != "None", company != "Chili'S") %>%
  dplyr::top_n(10, n) %>%
  as.data.frame(.)

plot_ly(df3.1, y = ~company, x = ~n, type = "bar")  %>%
  layout(xaxis = list(title = "Total"), 
         yaxis = list( title = "Company"))
})


output$p4 <- renderPlotly({
  df4 <- 
    final_sj %>%
    dplyr::mutate(company = str_trim(title)) %>%
    dplyr::filter(state %in% input$state) %>%
    dplyr::group_by(title) %>%
    dplyr::count(.) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup()%>%
    dplyr::top_n(10, n) %>%
    as.data.frame(.)
  
  df4$title <- factor(df4$title, levels = unique(df4$title)[order(df4$n, decreasing = F)])
  
  plot_ly(df4, y = ~title, x = ~n, type = "bar") %>%
    layout(xaxis = list(title = "Total"), 
           yaxis = list( title = "Job Title"))
})
# jobs by sector tag
output$p5 <- renderPlotly({
df5 <- 
  final_sj %>%
  dplyr::filter(source == "LinkedIn") %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::mutate(sector = gsub(",\\s", ",", sector)) %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::group_by(sector) %>%
  dplyr::count(.) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::ungroup(.) %>%
  dplyr::top_n(10, n)
  
df5$sector <- factor(df5$sector, levels = unique(df5$sector)[order(df5$n, decreasing = F)])
  

plot_ly(df5, y = ~sector, x = ~n, type = "scatter", mode = "markers", marker = list(color = "green", size = 12)) %>%
    add_segments(y = ~sector, yend = ~sector, x = ~n, xend=~n, showlegend = FALSE) %>%
    layout(xaxis = list(title = "Total"), yaxis = list( title = ""))
  })

# jobs by function
output$p6 <- renderPlotly({
df6 <- 
  final_sj %>%
  dplyr::filter(source == "LinkedIn") %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::mutate(`function` = gsub("([a-z])([A-Z])", "\\1,\\2", `function`)) %>%
  dplyr::mutate(`function` = gsub(",\\s", ",", `function`)) %>%
  separate_rows(`function`, sep = ',') %>%
  dplyr::group_by(`function`) %>%
  dplyr::count(.) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::ungroup(.) %>%
  dplyr::top_n(10, n)

df6$`function` <- factor(df6$`function`, levels = unique(df6$`function`)[order(df6$n, decreasing = F)])

plot_ly(df6, y = ~`function`, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "purple", size = 12)) %>%
    add_segments(y = ~`function`, yend = ~`function`, x = ~n, xend=~n, showlegend = FALSE) %>%
    layout(xaxis = list(title = "Total"), yaxis = list( title = ""))
})

# correlation heatmaps - sectors
# library(shinyHeatmaply)
library(heatmaply)

final_sj_correlation <- 
  final_sj %>%
  dplyr::filter(source == "LinkedIn") %>%
  dplyr::mutate(sector = gsub(",\\s", ",", sector)) %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::group_by(link) %>%
  dplyr::mutate(g = group_indices())%>%
  ungroup(.) %>%
  dplyr::count(g, sector) %>% 
  spread(sector, n, fill = 0) # %>%
#  clean_names(.) 


final_sj_correlation <- correlate(final_sj_correlation[, -1]) %>% column_to_rownames("term")

output$heatmap <- renderPlotly({
  heatmaply_cor(final_sj_correlation %>% dplyr::select(contains("Oil"), contains("Renewable"), contains("Utilities"),
                                                       contains("Environment"), contains("Automotive"), contains("Airline"),
                                                       contains("Chemical"), contains("Construct"), contains("Electrical"),
                                                       contains("Computer"), contains("Information"), contains("Logistics"),
                                                       contains("Mining"), contains("Transportation")), 
                show_dendrogram = F, fontsize_row = 6, fontsize_col = 6, height = 10, width = 15)
})

# correlation heatmaps - sector and function

final_sj_correlation2 <- 
  final_sj %>%
  dplyr::filter(source == "LinkedIn") %>%
  dplyr::mutate(sector = gsub(",\\s", ",", sector)) %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::group_by(link) %>%
  dplyr::mutate(g = group_indices())%>%
  ungroup(.) %>%
  dplyr::mutate(`function` = gsub("([a-z])([A-Z])", "\\1,\\2", `function`)) %>%
  separate_rows(`function`, sep = ',') %>%
  dplyr::count(`function`, sector) %>% 
  spread(sector, n, fill = 0) # %>%
#  clean_names(.) 



x2 <- 
  final_sj_correlation2 %>% 
  column_to_rownames("function") %>%
  chisq.test() 

x2 <- as.data.frame(x2$residuals)



output$heatmap2 <- renderPlotly({
  heatmaply(x2 %>% dplyr::select(contains("Oil"), contains("Renewable"), contains("Utilities"),
                                 contains("Environment"), contains("Automotive"), contains("Airline"),
                                 contains("Chemical"), contains("Construct"), contains("Electrical"),
                                 contains("Computer"), contains("Information"), contains("Logistics"),
                                 contains("Mining"), contains("Transportation")), show_dendrogram = F, fontsize_row = 6, fontsize_col = 6, 
               height = 12, width = 15)
})


# jobs by function - ren. env.
output$p7 <- renderPlotly({
df7 <- 
  final_sj_correlation2 %>%
  dplyr::select(`function`, `Renewables & Environment`) %>%
  dplyr::mutate(`function` = str_trim(`function`)) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::arrange(desc(`Renewables & Environment`)) %>%
  dplyr::top_n(10, `Renewables & Environment`) %>%
  dplyr::rename("Frequency" = "Renewables & Environment")

df7$`function` <- factor(df7$`function`, levels = unique(df7$`function`)[order(df7$Frequency, decreasing = F)])



plot_ly(df7, y = ~`function`, x = ~Frequency, 
          type = "scatter", mode = "markers", 
          marker = list(color = "blue", size = 12))
})
# jobs by function - oil energy
output$p8 <- renderPlotly({
df8 <- 
  final_sj_correlation2 %>%
  dplyr::select(`function`, `Oil & Energy`) %>%
  dplyr::mutate(`function` = str_trim(`function`)) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::arrange(desc(`Oil & Energy`)) %>%
  dplyr::top_n(10, `Oil & Energy`) %>%
  dplyr::rename("Frequency" = "Oil & Energy")

df8$`function` <- factor(df8$`function`, levels = unique(df8$`function`)[order(df8$Frequency, decreasing = F)])



plot_ly(df8, y = ~`function`, x = ~Frequency, 
          type = "scatter", mode = "markers", 
          marker = list(color = "blue", size = 12)) %>%
    layout(xaxis = list(title = "Total"), yaxis = list( title = ""))
})


# network graph


output$network <- renderSigmaNet({
  
  final_sj_network_es <-
    final_sj %>%
    dplyr::mutate(company = str_remove(company, "Plc|Llc|LLC|LTD|PLC|Ltd|, Inc.|, Inc|Inc.|Inc|Corp/.|Corp$|S.A DE C.V.|SAS de CV"),
                  company = str_trim(company)) %>%
    dplyr::filter(!str_detect(company, "Sector Industrial Petrolero Oil &amp; Gas|None")) %>%
    dplyr::mutate(company = str_replace(company, "&amp;", "&")) %>%
    dplyr::mutate(company = str_replace_all(company, "Enel$", "Enel Green Power"),
                  company = str_to_title(company)) %>%
    dplyr::filter(source == "LinkedIn") %>%
    tidyr::separate_rows(sector, sep = ", ") %>%
    dplyr::filter(state %in% input$state_2) %>%
    dplyr::select(sector, company) %>%
    dplyr::group_by(sector) %>%
    dplyr::count(company)
  
  
  final_sj_nw_es <- igraph::graph_from_data_frame(final_sj_network_es, directed = F)
  
  clust <- cluster_edge_betweenness(final_sj_nw_es)$membership
  V(final_sj_nw_es)$group <- clust
  
  layout  <- layout_with_fr(final_sj_nw_es)
  
  sigmaFromIgraph(final_sj_nw_es, layout = layout)  %>%
  addEdgeSize(sizeAttr = "n") %>%
  addEdgeColors("#d9d9d9") %>%
  addNodeColors(colorAttr = 'group') %>%
  addNodeLabels(labelAttr = "name") %>%
  addNodeSize(sizeMetric = "degree",  minSize = 2, maxSize = 15)
})


#### dashboard ####

### longitudinal

#### location

# firms

### job title

# sectors

# subsectors (not ready)

# relationship between sectors

# others: skill level, function

# 1

### additional work ####
# sankey chart 
df9.1 <- 
  final_sj %>%
  dplyr::filter(oil_energy == 1|renewables_environment == 1) %>%
  dplyr::mutate(sector = str_extract(sector, "Oil & Energy|Renewables & Environment")) %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::group_by(sector, level) %>%
  dplyr::count(.) %>%
#  dplyr::mutate(column = ifelse(sector == "Oil& Energy", 0, 1)) %>%
  dplyr::rename("source" = sector, "target" = level, "value" = n)


nodes <- data.frame(name = c(unique(df9.1$source), unique(df9.1$target)) %>% unique())

df9.1$IDsource=match(df9.1$source, nodes$name)-1 
df9.1$IDtarget=match(df9.1$target, nodes$name)-1


nodes <- nodes %>% dplyr::select(name)






# create nodes data by determining all unique nodes found in your data
# 
output$p9 <- renderSankeyNetwork({
  sankeyNetwork(Links = df9.1, Nodes = nodes, Source = 'IDsource',
                Target = 'IDtarget', Value = 'value', NodeID = 'name',
                fontSize = 15, nodeWidth = 30)
})



# jobs across time - week, month, disaggragates and total
df10 <- 
  final_sj %>%
  dplyr::mutate(date = floor_date(as.Date(date), "week")) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::arrange(date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 

output$p10 <- renderPlotly({
  plot_ly(df10, x = ~date, y = ~n) %>%
    add_bars(color = "orange") %>%
    layout(xaxis = list(title = "Date"), yaxis = list( title = "Total"))
})


df10.2 <- 
  final_sj %>%
  dplyr::mutate(date = floor_date(as.Date(date), "month")) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::arrange(date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 

output$p10.2 <- renderPlotly({
  plot_ly(df10.2, x = ~date, y = ~n) %>%
    add_bars(color = "orange") %>%
    layout(xaxis = list(title = "Date"), yaxis = list( title = "Total"))
})

df11 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = floor_date(as.Date(date), "week")) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                     oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::group_by(sector_1, date) %>%
  dplyr::arrange(sector_1, date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 

output$p11 <- renderPlotly({
  plot_ly(df11, x = ~date, y = ~n, color = ~sector_1) %>%
    add_bars() %>%
    layout(xaxis = list(title = "Date"), yaxis = list( title = "Total")) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5))             # put legend in center of x-axis
})
# jobs by date
df11.1.1 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = floor_date(as.Date(date), "month")) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                     oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::group_by(sector_1, date) %>%
  dplyr::arrange(sector_1, date) %>%
  dplyr::count(.) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  as.data.frame(.) 

output$p11.1.1 <- renderPlotly({
  plot_ly(df11.1.1, x = ~date, y = ~n, color = ~sector_1) %>%
    add_bars() %>%
    layout(xaxis = list(title = "Date"), yaxis = list( title = "Total")) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5))             # put legend in center of x-axis
})

# subsector analysis
# 
df11.1 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(date = floor_date(as.Date(date), "week")) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                     oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::select(date, contains("subsector_")) %>%
  dplyr::filter_at(dplyr::vars(contains("subsector_")), dplyr::any_vars(. == 1)) %>%
  tidyr::pivot_longer(cols = contains("subsector")) %>%
  dplyr::filter(value == 1) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, "subsector_"),
                name = stringr::str_replace_all(name, "_", " "),
                name = str_to_title(name)) %>%
  dplyr::group_by(name, date) %>%
  dplyr::arrange(name, date) %>%
  dplyr::count(value) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  dplyr::rename("subsector" = "name") %>%
  as.data.frame(.) 

output$p11.1 <- renderPlotly({
  plot_ly(df11.1, x = ~date, y = ~n, color = ~subsector) %>%
    add_bars() %>%
    layout(xaxis = list(title = "Date"), 
           yaxis = list( title = "Total")) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5))             # put legend in center of x-axis
})

df11.2 <-  
  final_sj %>%
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1|oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(date = floor_date(as.Date(date), "month")) %>%
  dplyr::mutate(sector_1 = case_when(renewables_environment == 1|renewables_environment_pred == 1 ~ "Renewables & Environment",
                                     oil_energy == 1|oil_energy_pred == 1 ~ "Oil & Energy")) %>%
  dplyr::select(date, contains("subsector_")) %>%
  dplyr::filter_at(dplyr::vars(contains("subsector_")), dplyr::any_vars(. == 1)) %>%
  tidyr::pivot_longer(cols = contains("subsector")) %>%
  dplyr::filter(value == 1) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, "subsector_"),
                name = stringr::str_replace_all(name, "_", " "),
                name = str_to_title(name)) %>%
  dplyr::group_by(name, date) %>%
  dplyr::arrange(name, date) %>%
  dplyr::count(value) %>%
  dplyr::filter(date > "2020-07-01" & date < Sys.Date()) %>%
  dplyr::rename("subsector" = "name") %>%
  as.data.frame(.) 

output$p11.2 <- renderPlotly({
  plot_ly(df11.2, x = ~date, y = ~n, color = ~subsector) %>%
    add_bars() %>%
    layout(xaxis = list(title = "Date"), 
           yaxis = list( title = "Total"))%>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5))             # put legend in center of x-axis
})

# subsector



output$map_subsector <- renderLeaflet({
  df_mxm1 <- 
    final_sj %>%
    #   dplyr::mutate(state = str_trim(state),
    #                 municipality = str_trim(municipality)) %>%
    dplyr::select(contains("subsector_"), municipality, state) %>%
    tidyr::pivot_longer(cols = contains("subsector_"), names_to = "subsector") %>%
    dplyr::mutate(subsector = str_remove(subsector, "subsector_")) %>%
    dplyr::filter(value == 1) %>%
    dplyr::group_by(municipality, state, subsector) %>%
    dplyr::count(.) %>%
    dplyr::filter(subsector == input$subsector_choice) %>%
    dplyr::right_join(mxmaps::df_mxmunicipio, by = c("municipality" = "municipio_name",
                                                     "state" = "state_name")) %>%
    dplyr::mutate(n = ifelse(is.na(n), 0 , n))
  
  
  df_mxm1$value <- as.numeric(as.character(df_mxm1$n))
  df_mxm1$region <- as.factor(as.character(df_mxm1$region))
  
  leaflet(df_mxm1) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addHeatmap(lng=~as.numeric(long),
             lat=~as.numeric(lat),
             intensity = ~((value/pop) * 1000000), minOpacity = .4,
             blur = .9, radius = 8)
})


# wordcloud - key skills  
# sector_tags_word_tf_idf_en$freq <- as.numeric(sector_tags_word_tf_idf_en$freq)
# wordcloud graph
output$wordcloud <- renderWordcloud2({
wordclouddf <- 
    wordpct %>%
    dplyr::rename_("freq" = input$wordcloudsector) %>%
    dplyr::select(word, freq)
wordcloud2(wordclouddf)
})

# jobs by level - ren env
output$p12 <- renderPlotly({
df12 <- 
  final_sj %>%
  dplyr::filter(renewables_environment == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(level) %>%
  dplyr::count(.)

plot_ly(df12, y = ~level, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "orange", size = 12)) %>% 
    layout(yaxis = list(title = "Level"), xaxis = list( title = "Total"))
})


output$p13 <- renderPlotly({
df13 <- 
  final_sj %>%
  dplyr::filter(oil_energy == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(level) %>%
  dplyr::count(.)



plot_ly(df13, y = ~level, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "red", size = 12)) %>%
    layout(yaxis = list(title = "Level"), xaxis = list( title = "Total"))
})     


output$p14 <- renderPlotly({
df14 <- 
  final_sj %>%
  dplyr::filter(oil_energy == 1|renewables_environment == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(level) %>%
  dplyr::count(.)


plot_ly(df14, y = ~level, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "red", size = 12)) %>%
    layout(yaxis = list(title = "Level"), xaxis = list( title = "Total"))
}) 

# frequency of jobs by type
output$p15.1 <- renderPlotly({
df15.1 <- 
  final_sj %>%
  dplyr::filter(renewables_environment == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(type) %>%
  dplyr::count(.)


plot_ly(df15.1, y = ~type, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "orange", size = 12)) %>%
    layout(yaxis = list(title = "Type"), xaxis = list( title = "Total"))
})


output$p15 <- renderPlotly({
df15 <- 
  final_sj %>%
  dplyr::filter(oil_energy == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(type) %>%
  dplyr::count(.)


plot_ly(df15, y = ~type, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "red", size = 12)) %>%
    layout(yaxis = list(title = "Type"), xaxis = list( title = "Total"))
})     

output$p16 <- renderPlotly({
df16 <- 
  final_sj %>%
  dplyr::filter(oil_energy == 1|renewables_environment == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::group_by(type) %>%
  dplyr::count(.)


plot_ly(df16, y = ~type, x = ~n, 
          type = "scatter", mode = "markers", 
          marker = list(color = "red", size = 12)) %>%
    layout(yaxis = list(title = "Type"), xaxis = list( title = "Total"))
})

output$p17 <- renderPlotly({
  subsector_ren_eng <-
  final_sj %>%
    #   dplyr::mutate(state = str_trim(state),
    #                 municipality = str_trim(municipality)) %>%
    dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1) %>%
    dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
    dplyr::select(contains("subsector_")) %>%
    tidyr::pivot_longer(cols = contains("subsector_"), names_to = "subsector") %>%
    dplyr::mutate(subsector = str_remove(subsector, "subsector_")) %>%
    dplyr::filter(value == 1) %>%
    dplyr::group_by(subsector) %>%
    dplyr::count(.) %>%
    dplyr::filter(str_detect(subsector, "solar|energy_efficiency|wind|electromobility|biomass|renewables_crosscutting")) %>% 
    dplyr::filter(subsector != "renewables_crosscutting", subsector != "solar_pv" ) %>% 
    dplyr::mutate(subsector = str_to_title(str_remove(subsector, "_")))
  
  
  
  plot_ly(subsector_ren_eng, x = ~subsector, y = ~n, color = ~subsector, textinfo = 'label+percent') %>%
    add_bars() %>%
    layout(title = "",  showlegend = F)
})

output$p18 <- renderPlotly({
subsector_og <-
  final_sj %>%
  #   dplyr::mutate(state = str_trim(state),
  #                 municipality = str_trim(municipality)) %>%
  dplyr::filter(oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::filter(between(date, input$daterange[1], input$daterange[2])) %>%
  dplyr::select(contains("subsector_")) %>%
  tidyr::pivot_longer(cols = contains("subsector_"), names_to = "subsector") %>%
  dplyr::mutate(subsector = str_remove(subsector, "subsector_")) %>%
  dplyr::filter(value == 1) %>%
  dplyr::group_by(subsector) %>%
  dplyr::count(.) %>%
  dplyr::filter(!str_detect(subsector, "solar|energy_efficiency|wind|electromobility|biomass|renewables_crosscutting")) %>%
  dplyr::filter(subsector != "oil_gas_crosscutting" ) %>% 
  dplyr::mutate(subsector = str_to_title(str_remove(subsector, "_")))

plot_ly(subsector_og, x = ~subsector, y = ~n, color = ~subsector, textinfo = 'label+percent') %>%
  add_bars() %>%
  layout(title = "",  showlegend = F)
})

# salary histogram
resal <- 
  final_sj %>% 
  dplyr::filter(renewables_environment == 1|renewables_environment_pred == 1) %>%
  dplyr::filter(!is.na(salary) & salary != "None" & salary != "NaN") %>%
  dplyr::select(salary) %>%
  dplyr::mutate(salary = as.numeric(salary))

refit <- density(resal$salary)
  

ogsal <- 
  final_sj %>% 
  dplyr::filter(oil_energy == 1|oil_energy_pred == 1) %>%
  dplyr::filter(!is.na(salary) & salary != "None" & salary != "NaN") %>%
  dplyr::select(salary) %>%
  dplyr::mutate(salary = as.numeric(salary))

ogfit <- density(ogsal$salary)


options(scipen = 999)

output$p19 <- renderPlotly({
plot_ly(alpha = 0.6) %>% 
  add_trace(x = ~(refit$x), y = ~(refit$y), type = "scatter", mode = "lines", fill = "tozeroy", name = "Renewables & Environment") %>%
  add_trace(x = ~(ogfit$x), y = ~(ogfit$y), type = "scatter", mode = "lines", fill = "tozeroy", name = "Oil & Gas") %>%
  layout(xaxis = list(range = c(0, 130000), tickprefix = "$", ticksuffix= "MXP", title = "Monthly Salary (MXP)"), 
         yaxis = list(showexponent = "all",
                      exponentformat = "e", title = "Density"))
})  


df19 <- final_sj %>%
  select(title:date, municipality:salary, description, sector:`function`, type:language)

sample_data <- reactive({df19})

output$table <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ' Edited Table.csv', sep='')
  },
  content = function(file) {
    write.csv(sample_data(), file, row.names = FALSE)
})


facet_graph_re <-  
  subsector_word_pct %>%
  ungroup() %>%
  dplyr::filter(sector_type == "Renewables") %>%
  dplyr::group_by(tot_subsector) %>%
  dplyr::arrange(desc(percent), .by_group = T) %>%
  top_n(10, percent)

facet_graph_og <-  
  subsector_word_pct %>%
  ungroup() %>%
  dplyr::filter(sector_type == "Oil & Gas") %>%
  dplyr::group_by(tot_subsector) %>%
  dplyr::arrange(desc(percent), .by_group = T) %>%
  top_n(10, percent)

facet_graph <- rbind(facet_graph_og, facet_graph_re)

output$p21 <- renderUI({
  facet_graph <- facet_graph %>%
    dplyr::filter(sector_type == input$sector_type)
  
  facet2 <-
    map(unique(facet_graph$tot_subsector), function(x){
      facet_graph %>% 
        dplyr::filter(tot_subsector == x) %>%
        hchart(., "bar", hcaes(x = word, y = percent), showInLegend = FALSE) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>% 
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = x) %>% 
        hc_yAxis(title = list(text = ""))
    }) %>% 
    hw_grid(rowheight = 300)

facet2

})
