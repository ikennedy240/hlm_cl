
estimate_effects <- function(data, focal_covariates, other_covariates = NULL, model_type = 'glm', topics = NULL, transform_y = NULL, grouping_var = NULL, ...){
  covariates <- c(focal_covariates, other_covariates)
  if(is.null(topics)){ #if no topic vector gets passed
    # then we grab all of the names that are of the format ^Topic[numbers]$
    topics <- names(data)[str_detect(names(data), '^Topic\\d+$')]
    if(length(topics)==0){
      stop("ERROR: No topics found in Data, please include vector of topics")
    }
  }
  if(model_type == 'glm'){
    right_side <- paste(covariates, collapse = '+')
  }
  if(model_type == 'lmer'){
    if(is.null(grouping_var)) stop("must include grouping variable")
    require(lme4)
    right_side <- paste(paste(covariates, collapse = '+'), '+ ( 1 |', grouping_var, ')')
  }
  return(map_dfr(topics, topic_coef, 
                 data = data, 
                 covariates,
                 focal_covariates,
                 right_side = right_side, 
                 model_type = model_type, 
                 transform_y=transform_y, 
                 ...))
}

topic_coef <- function(i, data, covariates, focal_covariates, right_side, model_type ='glm', transform_y = NULL, ...){
  model_type <- get(model_type)
  model_formula <- paste0(transform_y,'(',i, ')~', right_side)
  model_i <- model_type(model_formula,
                 data = data, ...)
  sum_i <- summary(model_i)
  row <- tibble(topic = i,
                    covariate = focal_covariates,
                    coefs = sum_i$coefficients[2:(1+length(focal_covariates)),1], 
                    stderrs = sum_i$coefficients[2:(1+length(focal_covariates)),2]
  )
  return(row)
}

plot_coef_sum<- function(coef_sum, topics_to_examine = NULL, topic_descriptions = NULL){
  suppressWarnings(if(is.null(coef_sum$upper)){
    if(is.null(coef_sum$stderrs)){
      stop("Dataframe must include coefs and stderrs or coefs and upper and lower")
    }else{
      coef_sum <- coef_sum %>% mutate(upper = coefs + stderrs, lower = coefs - stderrs)
    }
  })
  if(is.null(topics_to_examine)){
    #topics_to_examine <- factor(str_extract(coef_sum$topic, '\\d+'))
    topics_to_examine <- c(7,18,20,25,34)
  }
  if(is.null(topic_descriptions)){
    if(file.exists('topic_descriptions.txt')){
      topic_descriptions <- read_csv('topic_descriptions.txt')
      coef_sum <- coef_sum %>% left_join(topic_descriptions, by =  c('topic'))
    } else {
      coef_sum <- coef_sum %>% mutate(description = '')
    }
  } else {
    coef_sum <- coef_sum %>% left_join(topic_descriptions, by =  c('topic'))
  }
  coef_sum %>%  
    mutate(topic = factor(str_extract(topic, '\\d+'))) %>% 
    arrange(desc(coefs))  %>%
    filter(topic %in% topics_to_examine) %>% 
    ggplot(aes(x = reorder(description, -coefs), y = coefs))+
    geom_hline(yintercept = 0, color = "red", alpha=.5) + # plot a line at 0
    geom_pointrange(aes(ymax = upper, ymin = lower, color=covariate), size=.2, alpha = .8)+
    theme_minimal() + # auto exlude backgound shading
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle("Estimated Effects of neighborhood type on Topic Distributions, no Covariates",
            subtitle = 'Showing only topics of interest as *Topic Title (Topic Number)*')+
    scale_color_discrete(name="Neighborhood Type")+
    ylim(min(coef_sum$lower)-.01,max(coef_sum$upper)+.01)+
    scale_x_discrete(expand = c(.05,.6))+
    ylab("Coefficient")+
    xlab("Topic Number")+
    coord_flip()
}
