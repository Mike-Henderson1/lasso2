# You only need to run install.packages() for any package once on your computer. 
# Once installed, just run library command to load needed packages in subsequent sessions. 
install.packages(c("readr", "tidyverse", "glmnet", "mice", "here"))
library(readr) 
library(tidyverse)
library(glmnet) # lasso
library(mice) # imputation
library(here) # defines "here" as location of current R project on computer. 
# Thus, no need to specify file path of working directory.



# 1. load & organize data (this will take a minute or two)
cle <- read_csv(here("data", "clev_data_v4.csv"))


# Gotta do some data cleaning & reorganizing..


# Convert market values from text to number
cle2 <- cle %>% 
  mutate_at(vars(contains("mktval")), funs(as.numeric(gsub('[$,]', '', .))))


# the hex_id variables and the variables that have been aggregated up to the various hex levels all have
# the same suffix (_500,_1000, etc). To perform some of the data transformations that apply a function
# to multiple named variables simultaneously, need to change the suffix of the id var so it 
# doesn't match the aggregated variables

cle2$hex1000 <- cle2$hex_1000
cle2 <- cle2 %>% select(-(hex_1000))

# aggregate median market value of parcel for 1000-ft hexagons, and also
# make a count of parcels in hexagon variable
by_hex_mktval <- cle2 %>% 
  select_at(c(vars(contains("hex1000")),
              vars(starts_with("mktval"))))

by_hex_n <- by_hex_mktval %>% 
  group_by(hex1000) %>% 
  summarise(n=n()) 
by_hex_mkt <- by_hex_mktval %>% 
  group_by(hex1000) %>% 
  summarise_at(vars(contains("mktval")), funs(median), na.rm=T) %>% 
  rename_at(vars(-hex1000), funs(paste0("med_",.)))
by_hex <- by_hex_n %>% 
  left_join(by_hex_mkt)

cle3 <- cle2 %>% 
  left_join(by_hex)



# Then go back to hexagon-level dataframe with all hex-level variables available
by_hex <- cle3 %>% 
  group_by(hex1000) %>% 
  summarise_at(c(vars(contains("_1000")),
                 vars(contains("med_mktval"))),
               funs(first))


# recode a couple of the count variables NA -->0
by_hex <- by_hex %>% 
  mutate_at(vars(starts_with("ct_commercial")), funs( ifelse(is.na(.),0,.))) %>% 
  mutate_at(vars(starts_with("ct_indust")), funs( ifelse(is.na(.),0,.))) %>% 
  mutate_at(vars(starts_with("ct_residential")), funs( ifelse(is.na(.),0,.))) %>%
  mutate_at(vars(starts_with("mn_bldgs")), funs( ifelse(is.na(.),0,.))) %>%
  mutate_at(vars(starts_with("ct_landbank")), funs( ifelse(is.na(.),0,.))) 
# drop count of demos, and others that are highly correlated with other inlcuded variables
by_hex <- by_hex %>% 
  select_at(c(vars(-starts_with("ct_demo")), 
              vars(-starts_with("ct_resid2")), 
              vars(-starts_with("ct_vacantlot")), 
              vars(-starts_with("mn_struct")), 
              vars(-starts_with("mn_bath")), 
              vars(-starts_with("ct_totbldg")), 
              vars(-starts_with("any_res")), 
              vars(-starts_with("ct_vclotcheck")), 
              vars(-starts_with("ct_taxfc")), 
              vars(-starts_with("md_taxowed2")) ))

# make dataset w/relevant variables, drop any hexagons that had 0 buildings in the given year
# (if no buildings, then presumably no chance for demolition)
v1011 <- by_hex %>% 
  select_at(c(
    vars(contains("2010")), "hex1000", "any_demo2011_1000"))

v1011 <- v1011 %>% 
  filter(any_bldgs2010_1000>0 )

v1011 <- v1011 %>%   select(c(-starts_with("any_demo2010_1000"), 
                              -starts_with("any_bldgs2010_1000"))
                            )

# to check correlations of variables to include in model:
  # v1011.m <- as.matrix(v1011[,-20])
  # corr1 <- cor(v1011.m, use="pairwise.complete.obs")
  # corr1 <- ifelse(corr1==1,0,corr1)
  # View(corr1)

# Make the rest of the year-pair subsets
v1112 <- by_hex %>% 
  select_at(c(
    vars(contains("2011")), "hex1000", "any_demo2012_1000"))

v1112 <- v1112 %>% 
  filter(any_bldgs2011_1000>0 )

v1112 <- v1112 %>%   select(c(-starts_with("any_demo2011_1000"), 
                              -starts_with("any_bldgs2011_1000"))
)
v1213 <- by_hex %>% 
  select_at(c(
    vars(contains("2012")), "hex1000", "any_demo2013_1000"))

v1213 <- v1213 %>% 
  filter(any_bldgs2012_1000>0 )

v1213 <- v1213 %>%   select(c(-starts_with("any_demo2012_1000"), 
                              -starts_with("any_bldgs2012_1000"))
)
v1314 <- by_hex %>% 
  select_at(c(
    vars(contains("2013")), "hex1000", "any_demo2014_1000"))

v1314 <- v1314 %>% 
  filter(any_bldgs2013_1000>0 )

v1314 <- v1314 %>%   select(c(-starts_with("any_demo2013_1000"), 
                              -starts_with("any_bldgs2013_1000"))
)
v1415 <- by_hex %>% 
  select_at(c(
    vars(contains("2014")), "hex1000", "any_demo2015_1000"))

v1415 <- v1415 %>% 
  filter(any_bldgs2014_1000>0 )

v1415 <- v1415 %>%   select(c(-starts_with("any_demo2014_1000"), 
                              -starts_with("any_bldgs2014_1000"))
)
v1516 <- by_hex %>% 
  select_at(c(
    vars(contains("2015")), "hex1000", "any_demo2016_1000"))

v1516 <- v1516 %>% 
  filter(any_bldgs2015_1000>0 )

v1516 <- v1516 %>%   select(c(-starts_with("any_demo2015_1000"), 
                              -starts_with("any_bldgs2015_1000"))
)
v1617 <- by_hex %>% 
  select_at(c(
    vars(contains("2016")), "hex1000", "any_demo2017_1000"))

v1617 <- v1617 %>% 
  filter(any_bldgs2016_1000>0 )

v1617 <- v1617 %>%   select(c(-starts_with("any_demo2016_1000"), 
                              -starts_with("any_bldgs2016_1000"))
)
  


####################################

# Imputation to address missing data- can't run glmnet w/NAs
v1011 <- v1011 %>% select(-hex1000)
by_hex_imp <- mice(v1011, method="cart")
m1011 <- complete(by_hex_imp)
x1011 <- data.matrix(m1011[,1:(ncol(v1011)-1)])
y1011 <- data.matrix(m1011[,ncol(v1011)])

v1112 <- v1112 %>% select(-hex1000)
by_hex_imp <- mice(v1112, method="cart")
m1112 <- complete(by_hex_imp)
x1112 <- data.matrix(m1112[,1:(ncol(v1112)-1)])
y1112 <- data.matrix(m1112[,ncol(v1112)])

v1213 <- v1213 %>% select(-hex1000)
by_hex_imp <- mice(v1213, method="cart")
m1213 <- complete(by_hex_imp)
x1213 <- data.matrix(m1213[,1:(ncol(v1213)-1)])
y1213 <- data.matrix(m1213[,ncol(v1213)])

v1314 <- v1314 %>% select(-hex1000)
by_hex_imp <- mice(v1314, method="cart")
m1314 <- complete(by_hex_imp)
x1314 <- data.matrix(m1314[,1:(ncol(v1314)-1)])
y1314 <- data.matrix(m1314[,ncol(v1314)])

v1415 <- v1415 %>% select(-hex1000)
by_hex_imp <- mice(v1415, method="cart")
m1415 <- complete(by_hex_imp)
x1415 <- data.matrix(m1415[,1:(ncol(v1415)-1)])
y1415 <- data.matrix(m1415[,ncol(v1415)])

v1516 <- v1516 %>% select(-hex1000)
by_hex_imp <- mice(v1516, method="cart")
m1516 <- complete(by_hex_imp)
x1516 <- data.matrix(m1516[,1:(ncol(v1516)-1)])
y1516 <- data.matrix(m1516[,ncol(v1516)])

v1617 <- v1617 %>% select(-hex1000)
by_hex_imp <- mice(v1617, method="cart")
m1617 <- complete(by_hex_imp)
x1617 <- data.matrix(m1617[,1:(ncol(v1617)-1)])
y1617 <- data.matrix(m1617[,ncol(v1617)])



# Run lasso on each model
glmfun <- function(glm_yr,x,y){
  glm_yr <- glmnet(x,y,  family = "binomial", alpha=1)
  plot(glm_yr, label=T)
}
glm1011 <- glmfun(glm_m1011,x1011,y1011)
glm1112 <- glmfun(glm_m1112,x1112,y1112)
glm1213 <- glmfun(glm_m1213,x1213,y1213)
glm1314 <- glmfun(glm_m1314,x1314,y1314)
glm1415 <- glmfun(glm_m1415,x1415,y1415)
glm1516 <- glmfun(glm_m1516,x1516,y1516)
glm1617 <- glmfun(glm_m1617,x1617,y1617)


cv_glm1011 <- cv.glmnet(x1011,y1011,  family = "binomial",type.measure = "class")
cv_glm1112 <- cv.glmnet(x1112,y1112,  family = "binomial",type.measure = "class")
cv_glm1213 <- cv.glmnet(x1213,y1213,  family = "binomial",type.measure = "class")
cv_glm1314 <- cv.glmnet(x1314,y1314,  family = "binomial",type.measure = "class")
cv_glm1415 <- cv.glmnet(x1415,y1415,  family = "binomial",type.measure = "class")
cv_glm1516 <- cv.glmnet(x1516,y1516,  family = "binomial",type.measure = "class")
cv_glm1617 <- cv.glmnet(x1617,y1617,  family = "binomial",type.measure = "class")


png(filename = here("output", "misclass_all1011.png"))
plot(cv_glm1011)
dev.off()

png(filename = here("output", "misclass_all1112.png"))
plot(cv_glm1112)
dev.off()

png(filename = here("output", "misclass_all1213.png"))
plot(cv_glm1213)
dev.off()
png(filename = here("output", "misclass_all1314.png"))
plot(cv_glm1314)
dev.off()
png(filename = here("output", "misclass_all1415.png"))
plot(cv_glm1415)
dev.off()
png(filename = here("output", "misclass_all1516.png"))
plot(cv_glm1516)
dev.off()
png(filename = here("output", "misclass_all1617.png"))
plot(cv_glm1617)
dev.off()



plot(cv_glm1112)
plot(cv_glm1213)
plot(cv_glm1314)
plot(cv_glm1415)
plot(cv_glm1516)
plot(cv_glm1617)

m1011_coefs <- as.data.frame(as.matrix(coef(cv_glm1011, s="lambda.1se")))
m1112_coefs <- as.data.frame(as.matrix(coef(cv_glm1112, s="lambda.1se")))
m1213_coefs <- as.data.frame(as.matrix(coef(cv_glm1213, s="lambda.1se")))
m1314_coefs <- as.data.frame(as.matrix(coef(cv_glm1314, s="lambda.1se")))
m1415_coefs <- as.data.frame(as.matrix(coef(cv_glm1415, s="lambda.1se")))
m1516_coefs <- as.data.frame(as.matrix(coef(cv_glm1516, s="lambda.1se")))
m1617_coefs <- as.data.frame(as.matrix(coef(cv_glm1617, s="lambda.1se")))








# Merge non-zero coefficients into one data frame
m1011_coefs$vars <- rownames(m1011_coefs)  
colnames(m1011_coefs) <- c("coefs1011", "vars")
m1011_coefs$vars <-  str_replace(rownames(m1011_coefs), "2010", "")
m1112_coefs$vars <- rownames(m1112_coefs)  
colnames(m1112_coefs) <- c("coefs1112", "vars")
m1112_coefs$vars <-  str_replace(rownames(m1112_coefs), "2011", "")  
m1213_coefs$vars <- rownames(m1213_coefs)  
colnames(m1213_coefs) <- c("coefs1213", "vars")
m1213_coefs$vars <-  str_replace(rownames(m1213_coefs), "2012", "")
m1314_coefs$vars <- rownames(m1314_coefs)  
colnames(m1314_coefs) <- c("coefs1314", "vars")
m1314_coefs$vars <-  str_replace(rownames(m1314_coefs), "2013", "")
m1415_coefs$vars <- rownames(m1415_coefs)  
colnames(m1415_coefs) <- c("coefs1415", "vars")
m1415_coefs$vars <-  str_replace(rownames(m1415_coefs), "2014", "")
m1516_coefs$vars <- rownames(m1516_coefs)  
colnames(m1516_coefs) <- c("coefs1516", "vars")
m1516_coefs$vars <-  str_replace(rownames(m1516_coefs), "2015", "")
m1617_coefs$vars <- rownames(m1617_coefs)  
colnames(m1617_coefs) <- c("coefs1617", "vars")
m1617_coefs$vars <-  str_replace(rownames(m1617_coefs), "2016", "")

sig_coefs <- m1011_coefs %>% 
  left_join(m1112_coefs) %>% 
  left_join(m1213_coefs) %>% 
  left_join(m1314_coefs) %>% 
  left_join(m1415_coefs) %>% 
  left_join(m1516_coefs) %>% 
  left_join(m1617_coefs) 
sig_coefs <- sig_coefs[c(2,1,3:8)]
sig_coefs2 <- sig_coefs %>% 
  mutate(coefs1011 = ifelse(coefs1011==0,".",coefs1011), 
         coefs1112 = ifelse(coefs1112==0,".",coefs1112),
         coefs1213 = ifelse(coefs1213==0,".",coefs1213),
         coefs1314 = ifelse(coefs1314==0,".",coefs1314),
         coefs1415 = ifelse(coefs1415==0,".",coefs1415),
         coefs1516 = ifelse(coefs1516==0,".",coefs1516),
         coefs1617 = ifelse(coefs1617==0,".",coefs1617))

write_csv(sig_coefs2, here("output", "sig_coefs_all_demos_02_27_2019.csv"))



