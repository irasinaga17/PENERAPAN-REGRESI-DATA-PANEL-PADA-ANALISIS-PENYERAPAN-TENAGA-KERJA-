> library(readxl)
> PKL <- read_excel("D:/SEMESTER 5/PKL/Data_PKL.xlsx") 
> str(PKL)
tibble [25 x 6] (S3: tbl_df/tbl/data.frame)
 $ Wilayah                   : chr [1:25] "Kulon Progo" "Kulon Progo" "Kulon Progo" "Kulon Progo" ...
 $ Tahun                     : num [1:25] 2016 2017 2018 2019 2020 ...
 $ Tenaga_kerja              : num [1:25] 252691 239542 249186 252018 259421 ...
 $ Angkatan_Kerja            : num [1:25] 258815 244415 252966 256632 269426 ...
 $ Indeks_Pembangunan_Manusia: num [1:25] 72.4 73.2 73.8 74.4 74.5 ...
 $ Upah_Minimum              : num [1:25] 1268870 1373600 1493250 1613200 1750500 ...
> summary(PKL)
   Wilayah              Tahun       Tenaga_kerja    Angkatan_Kerja   Indeks_Pembangunan_Manusia  Upah_Minimum    
 Length:25          Min.   :2016   Min.   :213591   Min.   :225013   Min.   :67.82              Min.   :1235700  
 Class :character   1st Qu.:2017   1st Qu.:249186   1st Qu.:252966   1st Qu.:73.23              1st Qu.:1404760  
 Mode  :character   Median :2018   Median :422391   Median :429476   Median :79.45              Median :1571000  
                    Mean   :2018   Mean   :419001   Mean   :433618   Mean   :78.27              Mean   :1553783  
                    3rd Qu.:2019   3rd Qu.:561731   3rd Qu.:579229   3rd Qu.:83.84              3rd Qu.:1705000  
                    Max.   :2020   Max.   :667823   Max.   :703666   Max.   :86.65              Max.   :2004000  
> tk = sd(PKL$Tenaga_kerja)
> tk
[1] 168291
> ak = sd(PKL$Angkatan_Kerja)
> ak
[1] 174674.1
> um = sd(PKL$Upah_Minimum)
> um
[1] 204651
> ipm = sd(PKL$Indeks_Pembangunan_Manusia)
> ipm
[1] 6.339607
> library(plm)
> library(lmtest)
> PKL.ce <- plm(log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + log(Upah_Minimum), data = PKL, model = "pooling")
> summary(PKL.ce)
Pooling Model

Call:
plm(formula = log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + 
    log(Upah_Minimum), data = PKL, model = "pooling")

Balanced Panel: n = 5, T = 5, N = 25

Residuals:
       Min.     1st Qu.      Median     3rd Qu.        Max. 
-0.03076442 -0.00534553  0.00095867  0.00520989  0.01554641 

Coefficients:
                                  Estimate Std. Error  t-value  Pr(>|t|)    
(Intercept)                      1.2193436  0.2473061   4.9305 7.064e-05 ***
log(Angkatan_Kerja)              1.0056025  0.0050169 200.4440 < 2.2e-16 ***
log(Indeks_Pembangunan_Manusia) -0.1312196  0.0295115  -4.4464 0.0002235 ***
log(Upah_Minimum)               -0.0529784  0.0186345  -2.8430 0.0097409 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    4.4146
Residual Sum of Squares: 0.0022422
R-Squared:      0.99949
Adj. R-Squared: 0.99942
F-statistic: 13775.4 on 3 and 21 DF, p-value: < 2.22e-16
> PKL.fe <- plm(log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + log(Upah_Minimum), data = PKL, model = "within","effect" = "time",index=c("Wilayah","Tahun"))
> summary(PKL.fe)
Oneway (time) effect Within Model

Call:
plm(formula = log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + 
    log(Upah_Minimum), data = PKL, effect = "time", model = "within", 
    index = c("Wilayah", "Tahun"))

Balanced Panel: n = 5, T = 5, N = 25

Residuals:
       Min.     1st Qu.      Median     3rd Qu.        Max. 
-2.3701e-02 -2.6469e-03 -1.7015e-05  4.5826e-03  1.3069e-02 

Coefficients:
                                  Estimate Std. Error  t-value Pr(>|t|)    
log(Angkatan_Kerja)              0.9977315  0.0062234 160.3188  < 2e-16 ***
log(Indeks_Pembangunan_Manusia) -0.0394566  0.0583075  -0.6767  0.50770    
log(Upah_Minimum)               -0.2015889  0.0846600  -2.3812  0.02922 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    4.4074
Residual Sum of Squares: 0.001416
R-Squared:      0.99968
Adj. R-Squared: 0.99955
F-statistic: 17631.8 on 3 and 17 DF, p-value: < 2.22e-16
> 
> PKL.re <- plm(log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + log(Upah_Minimum), data = PKL, model = "random",index=c("Wilayah","Tahun"))
> summary(PKL.re)
Oneway (individual) effect Random Effect Model 
   (Swamy-Arora's transformation)

Call:
plm(formula = log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + 
    log(Upah_Minimum), data = PKL, model = "random", index = c("Wilayah", 
    "Tahun"))

Balanced Panel: n = 5, T = 5, N = 25

Effects:
                    var   std.dev share
idiosyncratic 5.872e-05 7.663e-03     1
individual    0.000e+00 0.000e+00     0
theta: 0

Residuals:
       Min.     1st Qu.      Median     3rd Qu.        Max. 
-0.03076442 -0.00534553  0.00095867  0.00520989  0.01554641 

Coefficients:
                                  Estimate Std. Error  z-value  Pr(>|z|)    
(Intercept)                      1.2193436  0.2473061   4.9305 8.202e-07 ***
log(Angkatan_Kerja)              1.0056025  0.0050169 200.4440 < 2.2e-16 ***
log(Indeks_Pembangunan_Manusia) -0.1312196  0.0295115  -4.4464 8.733e-06 ***
log(Upah_Minimum)               -0.0529784  0.0186345  -2.8430  0.004469 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    4.4146
Residual Sum of Squares: 0.0022422
R-Squared:      0.99949
Adj. R-Squared: 0.99942
Chisq: 41326.1 on 3 DF, p-value: < 2.22e-16
> 
> 
> #### best model ####
> # Chow test for fixed effects vs common effect
> pFtest(PKL.fe,PKL.ce)

	F test for time effects

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
F = 2.4796, df1 = 4, df2 = 17, p-value = 0.08309
alternative hypothesis: significant effects

> 
> # Haussman test for fixed vs random effect
> phtest(PKL.fe, PKL.re)

	Hausman Test

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
chisq = 3.2261, df = 3, p-value = 0.3581
alternative hypothesis: one model is inconsistent

> 
> gr <- plm(log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + log(Upah_Minimum), data = PKL, model = "random")
> plmtest(gr,effect="twoways",type="bp") # uji efek individu maupun waktu

	Lagrange Multiplier Test - two-ways effects (Breusch-Pagan) for
	balanced panels

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
chisq = 0.4051, df = 2, p-value = 0.8166
alternative hypothesis: significant effects

> 
> plmtest(gr,effect="individual",type="bp") # uji efek individu

	Lagrange Multiplier Test - (Breusch-Pagan) for balanced panels

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
chisq = 0.18111, df = 1, p-value = 0.6704
alternative hypothesis: significant effects

> 
> plmtest(gr,effect="time",type="bp") # uji efek waktu

	Lagrange Multiplier Test - time effects (Breusch-Pagan) for balanced
	panels

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
chisq = 0.22399, df = 1, p-value = 0.636
alternative hypothesis: significant effects

> 
>  
> ##Uji Asumsi Klasik
> #autokorelasi
> glc=plm(log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) + log(Upah_Minimum), data = PKL,model="pooling")
> pbgtest(glc, order=2)

	Breusch-Godfrey/Wooldridge test for serial correlation in panel models

data:  log(Tenaga_kerja) ~ log(Angkatan_Kerja) + log(Indeks_Pembangunan_Manusia) +  ...
chisq = 0.48942, df = 2, p-value = 0.7829
alternative hypothesis: serial correlation in idiosyncratic errors

> 
> #Heteroscedasticity
> bptest(glc)

	studentized Breusch-Pagan test

data:  glc
BP = 5.7835, df = 3, p-value = 0.1226

> 
> #uji multikolinearitas
> library(car)
> vif(glc)
            log(Angkatan_Kerja) log(Indeks_Pembangunan_Manusia) 
                       1.025506                        1.318831 
              log(Upah_Minimum) 
                       1.334266 
> 
> #normalitas
> qqnorm(resid(glc))


