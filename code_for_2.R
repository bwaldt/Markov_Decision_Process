# Step 0 
t_1 <- .5
t_11 <- .75
t_10 <- .25
t_21 <- .5
t_20 <- .25
# Step 1
# t_1 <-  (4 + 6/7 + 6/7 + 2/11 + 2/11)/12
# t_11 <- (3 + 12/7)/(4+ 12/7 + 4/11)
# t_10 <- ( 1 + 1/7 + 1/7 )/(4+2/7+18/11)
# t_21 <- ( 2 + 12/7)/(4+12/7+4/11)
# t_20 <- ( 1 + 1/7 + 1/7 )/(4+2/7+18/11)


labeled <- NULL
labeled$x1 <- c(1,1,1,0,1,0,0,0)
labeled$x2 <- c(1,1,0,0,0,1,0,0)
labeled$y <- c(1,1,1,1,-1,-1,-1,-1)
labeled <- data.frame(labeled)

labeled <- labeled %>% mutate(p_y = ifelse(y ==1 ,t_1,(1-t_1))) %>%
            mutate(p_x1y = ifelse(x1 ==1 & y ==1 ,t_11, 
                           ifelse(x1 == 0 & y == 1, 1 - t_11, 
                           ifelse(x1 == 1 & y ==-1, t_10,
                           ifelse(x1 ==0 & y ==-1,1 -t_10,0 ))))) %>%
            mutate(p_x2y = ifelse(x2 ==1 & y ==1 ,t_21, 
                           ifelse(x2 == 0 & y == 1, 1 - t_21, 
                           ifelse(x2 == 1 & y ==-1, t_20,
                           ifelse(x2 ==0 & y ==-1,1 -t_20,0 ))))) %>%
            mutate(log_lik = log(p_y*p_x1y*p_x2y))


labeled_log_lik <- sum(labeled$log_lik)


unlabeled_log_lik <- 2*log( (t_1*t_11*t_21) + ( (1-t_1)* t_10 * t_20  )) +
                     2*log( (t_1*(1- t_11)*(1-t_21)) + ( (1-t_1)*(1- t_10) *(1- t_20  )))

labeled_log_lik + unlabeled_log_lik
