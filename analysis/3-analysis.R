#
# Analysis (terrorism-judiciary paper)
# -----------------------------------------------------
# Steve Miller
# Date: 31 January 2017
# License




# Models
# -----------------------------------------------

M1 <- glmer(concourtsd ~ post911 + we + zg_udsmean + zg_explogrgdppc + zg_loggti5ya + injud + 
              (1 | country) + (1 | country:year), data=subset(Data_EVS),
            family=binomial(link="logit")
            ,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)  
            )

M2 <- glmer(concourtsd ~ post911 + we + zg_udsmean + zg_explogrgdppc + zg_loggti5ya*injud +
              (1 | country) + (1 | country:year) , data=subset(Data_EVS),
            family=binomial(link="logit")
            ,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)  
)

M3 <- glmer(concourtsd ~ zg_age + female + collegeed + zg_incgroup + unemployed + zg_conparl + 
              post911 + we + zg_udsmean + zg_explogrgdppc + zg_loggti5ya*injud + 
              (1 | country) + (1 | country:year) , data=subset(Data_EVS),
            family=binomial(link="logit")
            ,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)  
)

M4 <- glmer(concourtsd ~ zg_age + female + collegeed + zg_incgroup + unemployed + zg_conparl + we + 
              zg_udsmean + zg_explogrgdppc + zg_loggti5ya*injud + z_timedays +
              (1 | country), data=subset(Data_EVS),
            family=binomial(link="logit")
            ,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)  
)


# Make Table
# -----------------------------------------------

Regtable <- capture.output(stargazer(M1, M2, M3, M4, style = "ajps", header=FALSE,
                    title="Mixed Effects Models of Judicial Confidence in European Values Survey", 
                    covariate.labels=c("Age", "Female", "College Education", "Income Groups", 
                                       "Unemployed", "Confidence in Parliament", 
                                       "Post-9/11 Dummy", "Western Europe Dummy", "Level of Democracy", 
                                       "Real GDP per Capita", "Level of Terrorism", 
                                       "Judicial Independence", "Judicial Inefficiency", 
                                       "Level of Terrorism*Judicial Independence"), 
                    omit=c("Constant"), model.names = FALSE, dep.var.labels.include = FALSE, 
                    notes=c("* $p$ $<$ 0.05 (Micro-level)"), omit.stat =c("aic","ll","bic"), 
                    star.cutoffs = c(.05, NA, NA), 
                    star.char = c("*", "*", "*"), digit.separator=",", label="tab:regtable"))

Regtable <- Regtable[-39]

randomeffect <- "{\\bf Random Effect} & & & &  \\\\"
microlevel <- "{\\bf Micro-level} & & & & \\\\"
macrolevel <- "{\\bf Macro-level} & & & &  \\\\"
hline <- "\\hline"
newline <- "\\\\"

ranef_sd <- function(model, grp){
  vc <- as.data.frame(VarCorr(model))
  result <- vc$sdcor[ vc$grp == grp]
  result <- sprintf("%.3f", round(result, 3))
  return(result)
}

row_sd_c <- paste("Country Standard Deviation & ", 
                        ranef_sd(M1, "country"), "&", 
                        ranef_sd(M2, "country"), "&",
                        ranef_sd(M3, "country"), "&",
                        ranef_sd(M4, "country"), "\\\\")



row_sd_cy <- paste("Country-Year Standard Deviation & ", 
                     ranef_sd(M1, "country:year"), "&", 
                     ranef_sd(M2, "country:year"), "&",
                     ranef_sd(M3, "country:year"), "&",
                     "\\\\")

row_n_c <- paste("\\# of Countries &",
                           sapply(ranef(M1),nrow)["country"], "&", 
                           sapply(ranef(M2),nrow)["country"], "&",
                           sapply(ranef(M3),nrow)["country"], "&",
                           sapply(ranef(M4),nrow)["country"], "\\\\")

row_n_cy <- paste("\\# of Country-Years &",
                 sapply(ranef(M1),nrow)["country:year"], "&", 
                 sapply(ranef(M2),nrow)["country:year"], "&",
                 sapply(ranef(M3),nrow)["country:year"], "&",
                "\\\\")

insertres <- c(hline, randomeffect, hline, row_n_c, row_sd_c,
               newline, row_n_cy, row_sd_cy,
               newline, hline)


Regtable <- c(Regtable[a <- 1:36], insertres, Regtable[-a])
Regtable <- c(Regtable[a <- 1:20], macrolevel, Regtable[-a])
Regtable <- c(Regtable[a <- 1:8], microlevel, Regtable[-a])

# Make Graph
# -----------------------------------------------

M2_values <- with(Data_EVS, 
                  data.frame(
                    post911=median(post911),
                    we=median(we),
                    zg_udsmean=0,
                    zg_explogrgdppc = 0, 
                    zg_loggti5ya=rep(seq(from = min(zg_loggti5ya, na.rm=T), 
                                           to = max(zg_loggti5ya, na.rm=T), length.out = 301), 2), 
                    injud=c(min(injud, na.rm=TRUE), max(injud, na.rm=TRUE))

                  ))

M2_values <- arrange(M2_values, injud)
M2_values$"zg_loggti5ya:injud" <- with(M2_values, zg_loggti5ya*injud)

M2_predvalues <- cbind(M2_values, predictSE(M2, newdata = M2_values, 
                                            type = "link", se.fit = TRUE, re.form=NA))
M2_predvalues$pp <- with(M2_predvalues, plogis(fit))
M2_predvalues$lb <- with(M2_predvalues, plogis(fit - (1.645 * se.fit)))
M2_predvalues$ub <- with(M2_predvalues, plogis(fit + (1.645 * se.fit)))

M2_predvalues$cat <- NA
M2_predvalues$cat[M2_predvalues$injud == 0] <- "No Judicial Independence"
M2_predvalues$cat[M2_predvalues$injud == 2] <- "Judicial Independence"
M2_predvalues <- plyr::rename(M2_predvalues, c(cat="Category"))
