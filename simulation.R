# function calculating size/power using p-values, returning
# proportion of rejections (size/power) and their confidence intervals
# default values: seed = 28, simulations = 1000, sample size = 20, mean = 0, sd = 1

rejection_rate <- function(seed = 28, S = 1000, n=c(20,20), means=c(0,0), sds=c(1,1)) {
    # generate data and store the p-values for both tests
    # Student's t-test
    set.seed(seed)
    ttest_p <- replicate(S, 
                          t.test(rnorm(n[1], mean = means[1], sd = sds[1]), 
                                 rnorm(n[2], mean = means[2], sd = sds[2]),
                                 var.equal = TRUE, paired = FALSE)$p.value)
    # Welch test
    set.seed(seed)
    welch_p <- replicate(S, 
                         t.test(rnorm(n[1], mean = means[1], sd = sds[1]), 
                                rnorm(n[2], mean = means[2], sd = sds[2]),
                                var.equal = FALSE, paired = FALSE)$p.value)
    
    
    # function to calculate proportion of rejections and their confidence intervals
    CI <- function(x, S = length(x)) {
        # prop rejections
        p_hat <- sum(x < 0.05) / S
        
        # standard error
        SD <- sqrt((p_hat*(1-p_hat)) / S )
        
        #lower and upper values
        CI_lower <- p_hat - 1.96 * SD
        CI_upper <- p_hat + 1.96 * SD
        
        #return p_hat and confidence interval
        out <- cbind(p_hat, CI_lower, CI_upper)
        return(out)
    }
    
    # call CI() get estimated rate and confidence intervals 
    ttest <- CI(ttest_p)
    welch <- CI(welch_p)
    
    # return size/power with confidence intervals in a table
    result <- rbind(ttest, welch)
    row.names(result) <- c("ttest", "welch")
    
    return(result)
}

# run simulation with 100,000 simulations, both samples having sample size = 5,
# mean = 0 and sd = 1 
rejection_rate(S = 100000, n = c(5,5))



#### Compare how sample size affects type I error ####
#  Scenario: large and small samples (equal sample sizes), equal variances

# vector with the sample sizes we will run simulations for
sample_size <- c(3, 5, 20, 200)

# run simulation four times, one for each sample size
# returns a list with four tables, one for each sample size
Scenario_1 <- sapply(sample_size, function(n)rejection_rate(S=100000, n=c(n,n),
                                               sds = c(2,2)), simplify = FALSE)
                                               


#### Compare how sample size affects type I error ####
#  Scenario: large and small samples (equal sample sizes), unequal variances

# vector with the sample sizes we will run simulations for
sample_size <- c(3, 5, 20, 200)

# run simulation four times, one for each sample size
# returns a list with four tables, one for each sample size
Scenario_2 <- sapply(sample_size, function(n)rejection_rate(S=100000, n=c(n,n),
                                               sds = c(2,4)), simplify = FALSE)

### Compare power for differences in means (delta: 0, 0.5, 1, 1.5, 2, 2.5, 3) when 
### variances are equal
#   Scenario: equal sample sizes, equal variances

# vector with values to calculate mean values of sample2
delta <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
mu1 <- 80
mu2_vals <- mu1+10*delta

power_equal_variance <- sapply(delta, function(n)rejection_rate(S=100000, n=c(20,20), 
                                  means=c(80,80+10*n), sds=c(10,10)), simplify=FALSE)



#### Compare power for differences in means (delta: 0, 0.5, 1, 1.5, 2, 2.5, 3) when variances are unequal ####
#   Scenario: equal sample sizes, unequal variances

# vector with values to calculate mean values of sample2
delta <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
mu1 <- 80
mu2_vals <- mu1+10*delta

power_uequal_variance <- sapply(delta, function(n)rejection_rate(S=100000, n=c(20,20), 
                                       means=c(80,80+10*n), sds=c(10,20)), simplify=FALSE)
