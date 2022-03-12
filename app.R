library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(tidyr)

workshop_topics <- c('Stress Optimization',
                     'Mindset Coaching',
                     'Sleep Strategies',
                     'Social Wellbeing',
                     'Leadership and Teamwork',
                     'Physical Health & Fitness',
                     'Nutrition & Gut Health')

qwl_df <- read_csv("bei_vita_qwl_assessment.csv") 
names(qwl_df)[5] = "Country"

temp <- qwl_df[,56:62]
colnames(temp) <- workshop_topics
temp$Country = qwl_df$Country

temp <- temp %>%
  pivot_longer(!Country, names_to = "Workshop", values_to = "Preference")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot'),
      dccDropdown(
        id='country-select',
        options = unique(temp$Country)%>%map(function(con) list(label = con, value = con)), 
        value = 'Japan'
      )
    )
  )
)

app$callback(
  output(id = 'plot', property = 'figure'),
  list(input(id = 'country-select', property = 'value')),
  function(c) {
    temp <- temp %>% filter(Country == c)
    p <- ggplot(temp)+
      aes(x = Workshop, y = Preference, color = Workshop) +
      geom_boxplot(aes(middle = mean(Preference))) + 
      theme(axis.text.x = element_text(angle = 90)) +
      #ggtitle(title) +
      ggthemes::scale_color_tableau()
    ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')