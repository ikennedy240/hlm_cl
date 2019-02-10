
estimate_effects <- function(data, covariates, topics = NULL, transform_y = NULL){
  if(is.null(topics)){ #if no topic vector gets passed
    # then we grab all of the names that are of the format ^Topic[numbers]$
    topics <- names(data)[str_detect(names(data), '^Topic\\d+$')]
    if(length(topics)==0){
      stop("ERROR: No topics found in Data, please include vector of topics")
    }
  }
  return(map_dfr(topics, topic_coef, data = data, covariates = covariates, transform_y))
}

topic_coef <- function(i, data, covariates,transform_y = NULL){
  model_formula <- paste0(transform_y,'(',i, ')~', paste(covariates, collapse = '+'))
  model_i <- glm(model_formula,
                 data = data)
  sum_i <- summary(model_i)
  row <- tibble(topic = i,
                    covariate = covariates,
                    coefs = sum_i$coefficients[2:(1+length(covariates)),1], 
                    stderrs = sum_i$coefficients[2:(1+length(covariates)),2]
  )
  return(row)
}

plot_coef_sum<- function(coef_sum, topics_to_examine = NULL, topic_discriptions = NULL){
  if(is.null(topics_to_examine)){
    topics_to_examine <- factor(str_extract(coef_sum$topic, '\\d+'))
  }
  coef_sum %>%  
    #left_join(topic_discriptions, by =  c('topic', 'covariate')) %>%
    mutate(high_est = coefs + stderrs, low_est = coefs - stderrs, topic = factor(str_extract(topic, '\\d+'))) %>% 
    arrange(desc(coefs))  %>%
    filter(topic %in% topics_to_examine) %>% 
    ggplot(aes(x = reorder(topic, -coefs), y = coefs))+
    geom_hline(yintercept = 0, color = "red", alpha=.5) + # plot a line at 0
    geom_pointrange(aes(ymax = high_est, ymin = low_est, color=covariate), size=.2, alpha = .8)+
    theme_minimal() + # auto exlude backgound shading
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle("Estimated Effects of neighborhood type on Topic Distributions, no Covariates",
            subtitle = 'Showing only topics of interest as *Topic Title (Topic Number)*')+
    scale_color_discrete(name="Neighborhood Type")+
    ylim(-max(abs(coef_sum$coefs))-.01,max(abs(coef_sum$coefs))+.01)+
    scale_x_discrete(expand = c(.05,.6))+
    ylab("Coefficient")+
    xlab("Topic Number")+
    coord_flip()
}