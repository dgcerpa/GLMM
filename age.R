
# Sex, age and years of education


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)


###############################################################################
# Import Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

## Split data by group
df_grupo_c <- subset(df, grupo == "0")
df_grupo_v <- subset(df, grupo == "1")


###############################################################################
# Demographic descriptives

## Age: mean and standard deviation
mean(df_grupo_c$Edad, na.rm = TRUE)   # M
sd(df_grupo_c$Edad, na.rm = TRUE)     # SD

## Count by sex
table(df_grupo_c$Sexo)

## Age: mean and standard deviation
mean(df_grupo_v$Edad, na.rm = TRUE)   # M
sd(df_grupo_v$Edad, na.rm = TRUE)     # SD

## Count by sex
table(df_grupo_v$Sexo)


###############################################################################
# Years of education

## Years of education: n with data, mean and standard deviation
sum(!is.na(df_grupo_c$Anos_estudio))          # n
mean(df_grupo_c$Anos_estudio, na.rm = TRUE)   # M
sd(df_grupo_c$Anos_estudio, na.rm = TRUE)     # SD

## Years of education: n with data, mean and standard deviation
sum(!is.na(df_grupo_v$Anos_estudio))          # n
mean(df_grupo_v$Anos_estudio, na.rm = TRUE)   # M
sd(df_grupo_v$Anos_estudio, na.rm = TRUE)     # SD

## Group comparison (Welch t-test)
t.test(Anos_estudio ~ grupo, data = df)
