# Script is used to create functions for plotting

# Script 1: Visits Table ----
general_metrics_tbl_function <- function(data = trial_data_cleaned_tbl){
  
  general_metrics_tbl <- data %>% 
    select(vis_date,Height, Weight,BMI,TB_percent_fat,TH_Tscore,LS_Tscore,DOB) %>% 
    mutate(
      Age = round(decimal_date(vis_date) - decimal_date(DOB),1)
    ) %>% 
    rename(
      `Visit Date` = vis_date,
      `Height (cm)` = Height,
      `Weight (kg)` = Weight,
      `BMI (kg/cm^2^)` = BMI,
      `Body Composition (%)` = TB_percent_fat,
      `T-Score Hip` = TH_Tscore,
      `T-Score Spine` = LS_Tscore
    ) %>% 
    select(`Visit Date`,Age,everything(),-DOB) %>% 
    kable(format = 'pandoc', digits = 1, align = 'c', caption = 'Visits') 
  
  return(general_metrics_tbl)
}


# Script 2: Create Single Line Charts ----
line_plot_function <- function( data,  # data set to be used
                                metric, # variable to be used in plot
                                title,  # plot title to be used
                                xlab, # x-axis label text to be used
                                nudge_y = 3, # can manually adjust text labels for data points vertically
                                include_normal_rng = TRUE, # toggles grey normal range on/off
                                y_axis_min = 0,  # starting value of y-axis, default = 0
                                y_axis_max = NA, # max value for y-axis, default is whatever the data is
                                include_labels = TRUE, # toggle data point labels on/off
                                accuracy = 1) # number of decimal places, default = 1
  { 
  
  metric <- enquo(metric)
  
  # Begin the plot
  line_plot <- data %>%
    filter(metric == !!metric) %>%
    mutate(label_text = format(`Measurement Value`,digits = 2, nsmall = accuracy, trim = TRUE)) %>% 
    ggplot(aes(x = Months, y = `Measurement Value`))
  
  # Convert accuracy to correct format for y-axis scale
  y_axis_accuracy <- case_when(
    accuracy == 0 ~ 1,
    accuracy == 1 ~ 0.1,
    accuracy == 2 ~ 0.01,
    accuracy == 3 ~ 0.001
  )
  
  # Toggle normal range on and off
  if(include_normal_rng){
    line_plot <- line_plot + geom_ribbon(aes(ymin = normal_range_min, ymax = normal_range_max), fill = '#DCDCDC')
  }
  
  # Geoms
  line_plot <- line_plot +
    geom_line(color = '#ff5252', alpha = 0.5) +
    geom_point(size = 1,color = '#ff5252', alpha = 0.5)
  
  # toggle data labels on and off
  if(include_labels){
    line_plot <- line_plot + geom_text_repel(aes(label = label_text),nudge_y = nudge_y,
                                             size = 3, segment.color = NA)
  }
    
  # Formatting
  line_plot <- line_plot +  
  labs(
      title = title,
      y = '',
      x = xlab
    ) +
    theme_classic() +
    theme(
      axis.line =  element_line(colour = "lightgrey"), # change color of axis lines
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.margin = margin(b = ,r = 5, t = 10, l = 5, unit = 'pt'), # increase margins
      strip.background = element_rect(       # Removes black border in titles
        colour = 'white',
        size = 0.7
      ),
      strip.text = element_text(          # change text within title boxes
        color = 'black',
        size = 9
      )
    ) +
    scale_x_continuous(breaks = c(0,12,24,36))  + # set breaks on x axis
    scale_y_continuous(labels = scales::number_format(accuracy = y_axis_accuracy)) +
    expand_limits(y = c(y_axis_min,y_axis_max)) 

  return(line_plot)
}


# Script 3: Create Multi-line Charts ----
multi_line_plot_function <- function(data,metric=NULL, title, xlab, nudge_y = 3){
  
  metric_list <- metric
  
  multi_line_plot <- data %>% 
    filter(metric %in% metric_list) %>% 
    ggplot(aes(x = Months, y = `Measurement Value`, color = variable_label)) +
  
  # Geoms
    geom_line(aes()) +
    geom_point(size = 1) +
    
    # Formatting
    labs(
      title = title,
      y = '',
      x = xlab
    ) +
    theme_classic() +
    guides(fill=guide_legend(
      keywidth=0.1,
      keyheight=0.1,
      default.unit="cm")
    ) +
    theme(
      legend.justification = c(1, 0), # justify legend properly for bottom right hand corder
      legend.position = c(1, 0), # put legend in bottom right hand corner
      legend.direction = 'vertical', # change legend items to be vertical
      legend.key.size = unit(0.5,"cm"),  # change legend box size
      legend.key.height = unit(0.25,'cm'), # change legend height between variables
      legend.margin=margin(0,0,0,0), # remove legend margins
      legend.title = element_blank(), # remove legend title
      legend.text=element_text(size=6), # change legend text size
      axis.line =  element_line(colour = "lightgrey"), # change color of axis lines
      plot.title = element_text(hjust = 0.5, size = 10), # plot title formatting
      plot.margin = margin(b = ,r = 5, t = 10, l = 5, unit = 'pt'), # increase margins
      strip.background = element_rect(       # Removes black border in titles
        colour = 'white',
        size = 0.7
      ),
      strip.text = element_text(          # change text within title boxes
        color = 'black',
        size = 9
      )
    ) +
    scale_x_continuous(breaks = c(0,12,24,36)) + # set breaks on x axis
    expand_limits(y = 0) # sets y-axis to begin at 0
  
  
  return(multi_line_plot)
}



