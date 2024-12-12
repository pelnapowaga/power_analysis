library(lme4)
library(lmerTest)

# parameters
params <- list(
    b0c = 5,
    b0c_sd = 1,

    b0f = 66,
    b0f_sd = 5,

    b1_c = 1,
    b1_f = 3,
    b1_i = 1,
    residuals_sd = 1
)


# conditions factor
conds <- rep(c(0,1,2), each = 5, times =1 )

#feature factor
feature <- rep(c(1:5), times = 3)

make_ziutek <- function(sub_id, params) {
    # add intercept for condition
    intercept_cond <- rep(rnorm(3,params$b0c, params$b0c_sd), each = 5)

    # add feature intercept
    intercept_feature <- rep(rnorm(5, params$b0f, params$b0f_sd), times=3)

    # add condition and feature effects
    beta_cond <- params$b1_c * conds
    beta_feature <- params$b1_f * feature

    # add interaction
    beta_interaction <- params$b1_i * conds * feature

    # add residual noise
    residuals <- rnorm(length(conds), 0, params$residuals_sd)


    # collect in a dataframe and calculate simulated measured outcome (y)
    d <- data.frame('cond' = as.character(conds), 
                    'feature' = as.character(feature),
                    'sub' = sub_id,
                    'b0_c' = intercept_cond,
                    'b0_f' = intercept_feature,
                    'b1_c' = beta_cond,
                    'b1_f' = beta_feature,
                    'res' = residuals,
                    'y' = intercept_cond + intercept_feature + beta_cond + beta_feature + beta_interaction + residuals)
    return(d)
}

N <- 30
ziutek_list <- lapply(1:N, make_ziutek, params)
d <- do.call(rbind, ziutek_list)
d

# fit models
m0 <- lmer(y ~ cond + (1|sub), data = d, REML = FALSE)
m1 <- lmer(y ~ cond*feature + (1|sub), data = d, REML = FALSE)
anova(m1)

# perform likelihood ratio test
anova(m0,m1) # tutaj się zatrzymałam

# now store output
