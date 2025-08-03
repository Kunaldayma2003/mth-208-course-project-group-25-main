# Install required packages if not installed

# install.packages("shiny")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("bslib")
# install.packages("RColorBrewer")
# install.packages("scales")
# install.packages("fmsb")
# install.packages("treemapify")


library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(bslib)
library(treemapify)
library(RColorBrewer)
library(scales)
library(fmsb)

load("./../Data/global-trendings-dataset.Rdata")

ui <- page_sidebar(
  
  title = tags$h1(tags$b("A Deep-dive into Youtube's Global Trends")),
  
  sidebar = sidebar(
    
    helpText(tags$i("It is better to select a reasonable number of filters to achieve better results. \n
             For better visibility, avoid overlapping too many plots; displaying 2 to 3 plots is ideal.")),
    
    
    selectizeInput("region", tags$b("Country:"), 
                   choices = sort(unique(global.trendings.dataset$region)), 
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = 'Global')
    ),
    
    selectizeInput("category", tags$b("Category:"),
                   choices = sort(unique(global.trendings.dataset$category)),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = 'All')
    ),
    
    selectizeInput("language", tags$b("Language:"),
                   choices = sort(unique(global.trendings.dataset$language)),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = 'All')
    ),
    
    helpText(tags$i("*Data as on Oct 31 2024 16:47:14 (India Standard Time)")),
  ),
  
  navset_card_pill( 
    nav_panel("Views and Duration Relationship", plotOutput("Views_Duration_Relationship")), 
    nav_panel("Views Distribution", plotOutput("views_dist_plot")),
    nav_panel("Engagement of Category", plotOutput("Engagement_of_Category")),
    nav_panel("Engagements Relationship", plotOutput("Engagement_Relationship")),
    nav_panel("Viewing Habit", plotOutput("Viewing_Habit")),
    nav_panel("Languages in Countries", plotOutput("Languages_in_Countries")),
  ),
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    filtered <- global.trendings.dataset
    
    if (!is.null(input$region)) {
      filtered <- filtered %>% filter(region %in% input$region)
    }
    
    if (!is.null(input$category)) {
      filtered <- filtered %>% filter(category %in% input$category)
    }
    
    if (!is.null(input$language)) {
      filtered <- filtered %>% filter(language %in% input$language)
    }
    
    return(filtered)
  })
  
  # Engagement_of_Category
  output$Engagement_of_Category <- renderPlot({
    data <- filtered_data() %>%
      select(category, likes, views) %>%
      mutate(views = log(views), likes = log(likes))
      
      max_views <- max(data$views, na.rm = TRUE)
      max_likes <- max(data$likes, na.rm = TRUE)
      data <- data %>%
        mutate(likes = likes * (max_views / max_likes))

      data %>%
        pivot_longer(cols = c(views, likes), names_to = "metric", values_to = "value") %>%
        
        ggplot( aes(x = category, y = value, fill = metric)) +
        geom_boxplot() +
        scale_fill_manual(values = c("views" = "lightblue", "likes" = "lightgreen"), labels = c("Views", "Likes")) +
        labs(x = "Category", y = "Views", fill = "Engagement") +
        ggtitle("Engagement Metrics (views, likes) over different Category",
                subtitle = "This would illustrate how various categories perform regarding engagement, which types of content are more likely to attract views or likes") +
        scale_y_continuous(
          name = "Views (log scale)",
          sec.axis = sec_axis(~ . * (max_likes / max_views), name = "Likes (log scale)")
        ) +
        theme(axis.text.x = element_text(angle = 15),
              panel.background = element_rect(fill = "white"),
              panel.grid = element_line(color = "grey",size = 0.25,linetype = 1)
              ) 
  })
  
  # Engagement_Relationship
  output$Engagement_Relationship <- renderPlot({
    filtered_data() %>%
      mutate(likes = log(likes), comments = log(comments)) %>%
      ggplot(aes(x=likes, y=comments, size=views, color=region)) +
      geom_point(alpha=0.3) +
      labs(x = "Likes (log scale)", y = "Comments (log scale)", size = "Views", color = "Countries") +
      ggtitle("Relationship between Engagement Metrics (views, likes and comments) over different Countries",
              subtitle = "This could uncover the relation between likes and comments, and how they relate to views. It may help demonstrate whether specific videos receive a high number of likes, comments, and views") +
      theme_minimal()
  })
  
  # Viewing_Habit
  output$Viewing_Habit <- renderPlot({
    
    bivariate_table <- filtered_data() %>%
      group_by(category, region) %>%
      summarise(video_count = n()) %>%
      group_by(region) %>%
      mutate(proportion = video_count / sum(video_count)) %>%
      mutate(proportion = proportion*1000) %>%
      select(-video_count) %>%
      pivot_wider(names_from = category, values_from = proportion, values_fill = 0) %>%
      as.data.frame() %>%
      top_n(12)
    
    row.names(bivariate_table) <- bivariate_table$region
    bivariate_table <- bivariate_table[,2:ncol(bivariate_table)]
    
    colors_border <- brewer.pal(nrow(bivariate_table)+3, "Paired")
    colors_in <- alpha(colors_border,0.3)
    
    radarchart( rbind(rep(max(bivariate_table),ncol(bivariate_table)) , rep(min(bivariate_table),ncol(bivariate_table))  , bivariate_table) , axistype=0 , 
                
                pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
             
                cglcol="grey", cglty=2, axislabcol="black", cglwd=0.5, 
              
                vlcex=0.8,
                
                title = "Viewing habit of different countries"
    )
    
    legend("topright", legend = row.names(bivariate_table), col = colors_border, fill = colors_in)
  })
  
  # Languages_in_Countries
  output$Languages_in_Countries <- renderPlot({
    
    treeMapCoordinates <- filtered_data() %>%
        filter(language != "Unknown") %>%
        group_by(region, language) %>%
        summarise(video_count = n()) %>%
        group_by(region) %>%
        mutate(proportion = video_count / sum(video_count)) %>%
        mutate(proportion = proportion*1000) %>%
        select(-video_count)
      
      ggplot(treeMapCoordinates, aes(area = proportion, fill = language, label = language, subgroup = region)) +
        geom_treemap() +
        geom_treemap_subgroup_border(colour = "white", size = 1) +
        geom_treemap_subgroup_text(place = "bottom",alpha = 0.25, colour = "black") +
        geom_treemap_text(colour = "black", place = "centre", size = 10) +
        labs(title = "Language Distribution by Country", 
             subtitle = "This would reveal the distribution of video languages across countries, showing where different languages are most popular",
             fill = "Languages")
  })
  
  # Views distribution 
  output$views_dist_plot <- renderPlot({
    filtered_data() %>%
      distinct(title, .keep_all = TRUE) %>%
      ggplot(aes(x = views)) +
      geom_histogram(bins = 50, color = "grey", alpha = 0.7, aes(y = ..density..)) +
      geom_density(lwd = 1, colour = 4,
                   fill = 4, alpha = 0.25) +
      labs(title = "Distribution of Views", x = "Views", y = "") +
      theme_minimal() +
      scale_x_log10()
  })
  
  # Duration vs Views plot
  output$Views_Duration_Relationship <- renderPlot({
    filtered_data() %>%
      select(category, duration_second, views) %>%
      mutate(duration_second = log(duration_second), views = log(views)) %>%
      ggplot(aes(x = duration_second, y = views)) +
      geom_point(aes(color = category), alpha = 0.6) +
      labs(title = "Relation between Views and Duration of videos", 
           x = "Duration (in seconds, Log Scale)", y = "Views (Log Scale)",
           subtitle = "This would help determine if certain categories tend to engage more with longer or shorter videos",
           color = "Categories") +
      scale_x_continuous(breaks = pretty(seq(0, max(log(filtered_data()$duration_second), na.rm = TRUE), na.rm = TRUE), n = 10)) +  # Use pretty breaks
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)