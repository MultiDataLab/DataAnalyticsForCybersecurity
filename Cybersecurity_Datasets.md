Introduction
============

Here is a list of cybersecurity datasets with a brief introduction about them.

``` r
# Libraries

library (dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
# Define a function that outputs a quick data quality report.

data_report <- function(df) {
  
  #DataFrame with column names and their data types
  data_types <- data.frame('data_type'= sapply(df, class))

  #DataFrame with Count
  data_count <- data.frame('count' = colSums(!is.na(df)))

  #DataFrame with unique values
  unique_value_counts <- data.frame('unique_values'= sapply(df, function(x) length(unique(x))))
 
  #Dataframe with number of missing values for each column
  missing_data_counts <- data.frame('missing_values'= sapply(df, function(x) sum(length(which(is.na(x))))))
  
  #Combine all dataframes
  data_quality_report <- cbind(data_types, data_count, unique_value_counts, missing_data_counts )
  print('Data Quality Report')

  return(data_quality_report)
}
```

### 1. KDD Dataset

One of the most popular datasets for building a network intrusion detector and contains a great number of intrusions simulated in a military network environment.

<http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html>

``` r
kdd = read.csv('kddcup.data.corrected.csv', header = FALSE)
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(kdd))
```

    ## [1] 4898431      42

``` r
str(kdd)
```

    ## 'data.frame':    4898431 obs. of  42 variables:
    ##  $ V1 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V2 : Factor w/ 3 levels "icmp","tcp","udp": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ V3 : Factor w/ 70 levels "aol","auth","bgp",..: 22 22 22 22 22 22 22 22 22 22 ...
    ##  $ V4 : Factor w/ 11 levels "OTH","REJ","RSTO",..: 10 10 10 10 10 10 10 10 10 10 ...
    ##  $ V5 : int  215 162 236 233 239 238 235 234 239 181 ...
    ##  $ V6 : int  45076 4528 1228 2032 486 1282 1337 1364 1295 5450 ...
    ##  $ V7 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V8 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V9 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V10: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V11: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V12: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ V13: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V14: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V15: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V16: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V17: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V18: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V19: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V20: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V21: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V22: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V23: int  1 2 1 2 3 4 5 6 7 8 ...
    ##  $ V24: int  1 2 1 2 3 4 5 6 7 8 ...
    ##  $ V25: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V26: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V27: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V28: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V29: num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ V30: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V31: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V32: int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ V33: int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ V34: num  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ V35: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V36: num  0 1 0.5 0.33 0.25 0.2 0.17 0.14 0.12 0.11 ...
    ##  $ V37: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V38: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V39: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V40: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V41: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V42: Factor w/ 23 levels "back.","buffer_overflow.",..: 12 12 12 12 12 12 12 12 12 12 ...

``` r
#Call 'data_report' function for the dataset
data_report(kdd)
```

    ## [1] "Data Quality Report"

    ##     data_type   count unique_values missing_values
    ## V1    integer 4898431          9883              0
    ## V2     factor 4898431             3              0
    ## V3     factor 4898431            70              0
    ## V4     factor 4898431            11              0
    ## V5    integer 4898431          7195              0
    ## V6    integer 4898431         21493              0
    ## V7    integer 4898431             2              0
    ## V8    integer 4898431             3              0
    ## V9    integer 4898431             6              0
    ## V10   integer 4898431            30              0
    ## V11   integer 4898431             6              0
    ## V12   integer 4898431             2              0
    ## V13   integer 4898431            98              0
    ## V14   integer 4898431             2              0
    ## V15   integer 4898431             3              0
    ## V16   integer 4898431            93              0
    ## V17   integer 4898431            42              0
    ## V18   integer 4898431             3              0
    ## V19   integer 4898431            10              0
    ## V20   integer 4898431             1              0
    ## V21   integer 4898431             2              0
    ## V22   integer 4898431             2              0
    ## V23   integer 4898431           512              0
    ## V24   integer 4898431           512              0
    ## V25   numeric 4898431            96              0
    ## V26   numeric 4898431            87              0
    ## V27   numeric 4898431            89              0
    ## V28   numeric 4898431            76              0
    ## V29   numeric 4898431           101              0
    ## V30   numeric 4898431            95              0
    ## V31   numeric 4898431            72              0
    ## V32   integer 4898431           256              0
    ## V33   integer 4898431           256              0
    ## V34   numeric 4898431           101              0
    ## V35   numeric 4898431           101              0
    ## V36   numeric 4898431           101              0
    ## V37   numeric 4898431            76              0
    ## V38   numeric 4898431           101              0
    ## V39   numeric 4898431           100              0
    ## V40   numeric 4898431           101              0
    ## V41   numeric 4898431           101              0
    ## V42    factor 4898431            23              0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(kdd, is.numeric)
head(numeric_columns)
```

    ##   V1  V5    V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21
    ## 1  0 215 45076  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ## 2  0 162  4528  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ## 3  0 236  1228  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ## 4  0 233  2032  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ## 5  0 239   486  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ## 6  0 238  1282  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0
    ##   V22 V23 V24 V25 V26 V27 V28 V29 V30 V31 V32 V33 V34 V35  V36 V37 V38 V39
    ## 1   0   1   1   0   0   0   0   1   0   0   0   0   0   0 0.00   0   0   0
    ## 2   0   2   2   0   0   0   0   1   0   0   1   1   1   0 1.00   0   0   0
    ## 3   0   1   1   0   0   0   0   1   0   0   2   2   1   0 0.50   0   0   0
    ## 4   0   2   2   0   0   0   0   1   0   0   3   3   1   0 0.33   0   0   0
    ## 5   0   3   3   0   0   0   0   1   0   0   4   4   1   0 0.25   0   0   0
    ## 6   0   4   4   0   0   0   0   1   0   0   5   5   1   0 0.20   0   0   0
    ##   V40 V41
    ## 1   0   0
    ## 2   0   0
    ## 3   0   0
    ## 4   0   0
    ## 5   0   0
    ## 6   0   0

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 38"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(kdd, is.factor)
head(categorical_columns)
```

    ##    V2   V3 V4     V42
    ## 1 tcp http SF normal.
    ## 2 tcp http SF normal.
    ## 3 tcp http SF normal.
    ## 4 tcp http SF normal.
    ## 5 tcp http SF normal.
    ## 6 tcp http SF normal.

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 4"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'V2')
```

    ##     V2    freq
    ## 1 icmp 2833545
    ## 2  tcp 1870598
    ## 3  udp  194288

``` r
count(categorical_columns, 'V3')
```

    ##             V3    freq
    ## 1          aol       2
    ## 2         auth    3382
    ## 3          bgp    1047
    ## 4      courier    1021
    ## 5     csnet_ns    1051
    ## 6          ctf    1068
    ## 7      daytime    1056
    ## 8      discard    1059
    ## 9       domain    1113
    ## 10    domain_u   57782
    ## 11        echo    1059
    ## 12       eco_i   16338
    ## 13       ecr_i 2811660
    ## 14         efs    1042
    ## 15        exec    1045
    ## 16      finger    6891
    ## 17         ftp    5214
    ## 18    ftp_data   40697
    ## 19      gopher    1077
    ## 20     harvest       2
    ## 21   hostnames    1050
    ## 22        http  623091
    ## 23   http_2784       1
    ## 24    http_443    1044
    ## 25   http_8001       2
    ## 26       imap4    1069
    ## 27         IRC     521
    ## 28    iso_tsap    1052
    ## 29      klogin    1050
    ## 30      kshell    1040
    ## 31        ldap    1041
    ## 32        link    1069
    ## 33       login    1045
    ## 34         mtp    1076
    ## 35        name    1067
    ## 36 netbios_dgm    1052
    ## 37  netbios_ns    1054
    ## 38 netbios_ssn    1055
    ## 39     netstat    1056
    ## 40        nnsp    1038
    ## 41        nntp    1059
    ## 42       ntp_u    3833
    ## 43       other   72653
    ## 44     pm_dump       5
    ## 45       pop_2    1055
    ## 46       pop_3    1981
    ## 47     printer    1045
    ## 48     private 1100831
    ## 49       red_i       9
    ## 50  remote_job    1073
    ## 51         rje    1070
    ## 52       shell    1051
    ## 53        smtp   96554
    ## 54     sql_net    1052
    ## 55         ssh    1075
    ## 56      sunrpc    1056
    ## 57      supdup    1060
    ## 58      systat    1056
    ## 59      telnet    4277
    ## 60      tftp_u       3
    ## 61       tim_i      12
    ## 62        time    1579
    ## 63       urh_i     148
    ## 64       urp_i    5378
    ## 65        uucp    1041
    ## 66   uucp_path    1057
    ## 67       vmnet    1053
    ## 68       whois    1073
    ## 69         X11     135
    ## 70      Z39_50    1078

``` r
count(categorical_columns, 'V4')
```

    ##        V4    freq
    ## 1     OTH      57
    ## 2     REJ  268874
    ## 3    RSTO    5344
    ## 4  RSTOS0     122
    ## 5    RSTR    8094
    ## 6      S0  869829
    ## 7      S1     532
    ## 8      S2     161
    ## 9      S3      50
    ## 10     SF 3744328
    ## 11     SH    1040

``` r
count(categorical_columns, 'V42')
```

    ##                 V42    freq
    ## 1             back.    2203
    ## 2  buffer_overflow.      30
    ## 3        ftp_write.       8
    ## 4     guess_passwd.      53
    ## 5             imap.      12
    ## 6          ipsweep.   12481
    ## 7             land.      21
    ## 8       loadmodule.       9
    ## 9         multihop.       7
    ## 10         neptune. 1072017
    ## 11            nmap.    2316
    ## 12          normal.  972781
    ## 13            perl.       3
    ## 14             phf.       4
    ## 15             pod.     264
    ## 16       portsweep.   10413
    ## 17         rootkit.      10
    ## 18           satan.   15892
    ## 19           smurf. 2807886
    ## 20             spy.       2
    ## 21        teardrop.     979
    ## 22     warezclient.    1020
    ## 23     warezmaster.      20

### 2. NSL-KDD Dataset

This dataset is an improvement over KDD dataset and is created with the aim of overcoming some of the problems that existed in KDD dataset. Having a considerable number of records for train and test data, makes this dataset a good choice for experiments.

<https://www.unb.ca/cic/datasets/nsl.html>

``` r
nsl_kdd = read.table('KDDTrain+_20Percent.txt',sep=",", header = FALSE )
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(nsl_kdd))
```

    ## [1] 25192    43

``` r
str(nsl_kdd)
```

    ## 'data.frame':    25192 obs. of  43 variables:
    ##  $ V1 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V2 : Factor w/ 3 levels "icmp","tcp","udp": 2 3 2 2 2 2 2 2 2 2 ...
    ##  $ V3 : Factor w/ 66 levels "auth","bgp","courier",..: 17 40 45 20 20 45 45 45 47 45 ...
    ##  $ V4 : Factor w/ 11 levels "OTH","REJ","RSTO",..: 10 10 6 10 10 2 6 6 6 6 ...
    ##  $ V5 : int  491 146 0 232 199 0 0 0 0 0 ...
    ##  $ V6 : int  0 0 0 8153 420 0 0 0 0 0 ...
    ##  $ V7 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V8 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V9 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V10: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V11: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V12: int  0 0 0 1 1 0 0 0 0 0 ...
    ##  $ V13: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V14: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V15: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V16: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V17: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V18: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V19: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V20: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V21: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V22: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V23: int  2 13 123 5 30 121 166 117 270 133 ...
    ##  $ V24: int  2 1 6 5 32 19 9 16 23 8 ...
    ##  $ V25: num  0 0 1 0.2 0 0 1 1 1 1 ...
    ##  $ V26: num  0 0 1 0.2 0 0 1 1 1 1 ...
    ##  $ V27: num  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ V28: num  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ V29: num  1 0.08 0.05 1 1 0.16 0.05 0.14 0.09 0.06 ...
    ##  $ V30: num  0 0.15 0.07 0 0 0.06 0.06 0.06 0.05 0.06 ...
    ##  $ V31: num  0 0 0 0 0.09 0 0 0 0 0 ...
    ##  $ V32: int  150 255 255 30 255 255 255 255 255 255 ...
    ##  $ V33: int  25 1 26 255 255 19 9 15 23 13 ...
    ##  $ V34: num  0.17 0 0.1 1 1 0.07 0.04 0.06 0.09 0.05 ...
    ##  $ V35: num  0.03 0.6 0.05 0 0 0.07 0.05 0.07 0.05 0.06 ...
    ##  $ V36: num  0.17 0.88 0 0.03 0 0 0 0 0 0 ...
    ##  $ V37: num  0 0 0 0.04 0 0 0 0 0 0 ...
    ##  $ V38: num  0 0 1 0.03 0 0 1 1 1 1 ...
    ##  $ V39: num  0 0 1 0.01 0 0 1 1 1 1 ...
    ##  $ V40: num  0.05 0 0 0 0 1 0 0 0 0 ...
    ##  $ V41: num  0 0 0 0.01 0 1 0 0 0 0 ...
    ##  $ V42: Factor w/ 22 levels "back","buffer_overflow",..: 12 12 10 12 12 10 10 10 10 10 ...
    ##  $ V43: int  20 15 19 21 21 21 21 21 21 21 ...

``` r
#Call 'data_report' function for the dataset
data_report(nsl_kdd)
```

    ## [1] "Data Quality Report"

    ##     data_type count unique_values missing_values
    ## V1    integer 25192           758              0
    ## V2     factor 25192             3              0
    ## V3     factor 25192            66              0
    ## V4     factor 25192            11              0
    ## V5    integer 25192          1665              0
    ## V6    integer 25192          3922              0
    ## V7    integer 25192             2              0
    ## V8    integer 25192             3              0
    ## V9    integer 25192             2              0
    ## V10   integer 25192            22              0
    ## V11   integer 25192             5              0
    ## V12   integer 25192             2              0
    ## V13   integer 25192            28              0
    ## V14   integer 25192             2              0
    ## V15   integer 25192             3              0
    ## V16   integer 25192            28              0
    ## V17   integer 25192            20              0
    ## V18   integer 25192             2              0
    ## V19   integer 25192             7              0
    ## V20   integer 25192             1              0
    ## V21   integer 25192             1              0
    ## V22   integer 25192             2              0
    ## V23   integer 25192           466              0
    ## V24   integer 25192           414              0
    ## V25   numeric 25192            70              0
    ## V26   numeric 25192            56              0
    ## V27   numeric 25192            72              0
    ## V28   numeric 25192            42              0
    ## V29   numeric 25192            97              0
    ## V30   numeric 25192            79              0
    ## V31   numeric 25192            57              0
    ## V32   integer 25192           256              0
    ## V33   integer 25192           256              0
    ## V34   numeric 25192           101              0
    ## V35   numeric 25192           101              0
    ## V36   numeric 25192           101              0
    ## V37   numeric 25192            63              0
    ## V38   numeric 25192           100              0
    ## V39   numeric 25192            88              0
    ## V40   numeric 25192           101              0
    ## V41   numeric 25192           100              0
    ## V42    factor 25192            22              0
    ## V43   integer 25192            22              0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(nsl_kdd, is.numeric)
head(numeric_columns)
```

    ##   V1  V5   V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22
    ## 1  0 491    0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 2  0 146    0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 3  0   0    0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 4  0 232 8153  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0   0
    ## 5  0 199  420  0  0  0   0   0   1   0   0   0   0   0   0   0   0   0   0
    ## 6  0   0    0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   V23 V24 V25 V26 V27 V28  V29  V30  V31 V32 V33  V34  V35  V36  V37  V38
    ## 1   2   2 0.0 0.0   0   0 1.00 0.00 0.00 150  25 0.17 0.03 0.17 0.00 0.00
    ## 2  13   1 0.0 0.0   0   0 0.08 0.15 0.00 255   1 0.00 0.60 0.88 0.00 0.00
    ## 3 123   6 1.0 1.0   0   0 0.05 0.07 0.00 255  26 0.10 0.05 0.00 0.00 1.00
    ## 4   5   5 0.2 0.2   0   0 1.00 0.00 0.00  30 255 1.00 0.00 0.03 0.04 0.03
    ## 5  30  32 0.0 0.0   0   0 1.00 0.00 0.09 255 255 1.00 0.00 0.00 0.00 0.00
    ## 6 121  19 0.0 0.0   1   1 0.16 0.06 0.00 255  19 0.07 0.07 0.00 0.00 0.00
    ##    V39  V40  V41 V43
    ## 1 0.00 0.05 0.00  20
    ## 2 0.00 0.00 0.00  15
    ## 3 1.00 0.00 0.00  19
    ## 4 0.01 0.00 0.01  21
    ## 5 0.00 0.00 0.00  21
    ## 6 0.00 1.00 1.00  21

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 39"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(nsl_kdd, is.factor)
head(categorical_columns)
```

    ##    V2       V3  V4     V42
    ## 1 tcp ftp_data  SF  normal
    ## 2 udp    other  SF  normal
    ## 3 tcp  private  S0 neptune
    ## 4 tcp     http  SF  normal
    ## 5 tcp     http  SF  normal
    ## 6 tcp  private REJ neptune

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 4"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'V2')
```

    ##     V2  freq
    ## 1 icmp  1655
    ## 2  tcp 20526
    ## 3  udp  3011

``` r
count(categorical_columns, 'V3')
```

    ##             V3 freq
    ## 1         auth  189
    ## 2          bgp  146
    ## 3      courier  164
    ## 4     csnet_ns  111
    ## 5          ctf  127
    ## 6      daytime  107
    ## 7      discard  105
    ## 8       domain  109
    ## 9     domain_u 1820
    ## 10        echo   65
    ## 11       eco_i  909
    ## 12       ecr_i  613
    ## 13         efs  110
    ## 14        exec   91
    ## 15      finger  366
    ## 16         ftp  345
    ## 17    ftp_data 1396
    ## 18      gopher  109
    ## 19   hostnames   96
    ## 20        http 8003
    ## 21    http_443  113
    ## 22   http_8001    1
    ## 23       imap4  138
    ## 24         IRC   40
    ## 25    iso_tsap  131
    ## 26      klogin   92
    ## 27      kshell   67
    ## 28        ldap   90
    ## 29        link   85
    ## 30       login   79
    ## 31         mtp   90
    ## 32        name   92
    ## 33 netbios_dgm   85
    ## 34  netbios_ns   76
    ## 35 netbios_ssn   67
    ## 36     netstat   78
    ## 37        nnsp  123
    ## 38        nntp   61
    ## 39       ntp_u   32
    ## 40       other  858
    ## 41     pm_dump    3
    ## 42       pop_2   17
    ## 43       pop_3   53
    ## 44     printer   12
    ## 45     private 4351
    ## 46       red_i    3
    ## 47  remote_job   17
    ## 48         rje   20
    ## 49       shell   11
    ## 50        smtp 1449
    ## 51     sql_net   46
    ## 52         ssh   58
    ## 53      sunrpc   67
    ## 54      supdup  114
    ## 55      systat   88
    ## 56      telnet  483
    ## 57       tim_i    2
    ## 58        time  155
    ## 59       urh_i    4
    ## 60       urp_i  124
    ## 61        uucp  157
    ## 62   uucp_path  133
    ## 63       vmnet  107
    ## 64       whois  145
    ## 65         X11   22
    ## 66      Z39_50  172

``` r
count(categorical_columns, 'V4')
```

    ##        V4  freq
    ## 1     OTH     5
    ## 2     REJ  2216
    ## 3    RSTO   304
    ## 4  RSTOS0    21
    ## 5    RSTR   497
    ## 6      S0  7009
    ## 7      S1    88
    ## 8      S2    21
    ## 9      S3    15
    ## 10     SF 14973
    ## 11     SH    43

``` r
count(categorical_columns, 'V42')
```

    ##                V42  freq
    ## 1             back   196
    ## 2  buffer_overflow     6
    ## 3        ftp_write     1
    ## 4     guess_passwd    10
    ## 5             imap     5
    ## 6          ipsweep   710
    ## 7             land     1
    ## 8       loadmodule     1
    ## 9         multihop     2
    ## 10         neptune  8282
    ## 11            nmap   301
    ## 12          normal 13449
    ## 13             phf     2
    ## 14             pod    38
    ## 15       portsweep   587
    ## 16         rootkit     4
    ## 17           satan   691
    ## 18           smurf   529
    ## 19             spy     1
    ## 20        teardrop   188
    ## 21     warezclient   181
    ## 22     warezmaster     7

### 3. Credit Card Fraud

The highly unbalanced dataset belongs to 2013 transactions of European cardholders. The features in this dataset are the result of PCA transformation and we do not have any information about the actual features.

This is a labeled dataset.

<https://www.kaggle.com/samkirkiles/credit-card-fraud/data>

``` r
credit_data = read.csv('creditcard.csv')
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(credit_data))
```

    ## [1] 284807     31

``` r
str(credit_data)
```

    ## 'data.frame':    284807 obs. of  31 variables:
    ##  $ Time  : num  0 0 1 1 2 2 4 7 7 9 ...
    ##  $ V1    : num  -1.36 1.192 -1.358 -0.966 -1.158 ...
    ##  $ V2    : num  -0.0728 0.2662 -1.3402 -0.1852 0.8777 ...
    ##  $ V3    : num  2.536 0.166 1.773 1.793 1.549 ...
    ##  $ V4    : num  1.378 0.448 0.38 -0.863 0.403 ...
    ##  $ V5    : num  -0.3383 0.06 -0.5032 -0.0103 -0.4072 ...
    ##  $ V6    : num  0.4624 -0.0824 1.8005 1.2472 0.0959 ...
    ##  $ V7    : num  0.2396 -0.0788 0.7915 0.2376 0.5929 ...
    ##  $ V8    : num  0.0987 0.0851 0.2477 0.3774 -0.2705 ...
    ##  $ V9    : num  0.364 -0.255 -1.515 -1.387 0.818 ...
    ##  $ V10   : num  0.0908 -0.167 0.2076 -0.055 0.7531 ...
    ##  $ V11   : num  -0.552 1.613 0.625 -0.226 -0.823 ...
    ##  $ V12   : num  -0.6178 1.0652 0.0661 0.1782 0.5382 ...
    ##  $ V13   : num  -0.991 0.489 0.717 0.508 1.346 ...
    ##  $ V14   : num  -0.311 -0.144 -0.166 -0.288 -1.12 ...
    ##  $ V15   : num  1.468 0.636 2.346 -0.631 0.175 ...
    ##  $ V16   : num  -0.47 0.464 -2.89 -1.06 -0.451 ...
    ##  $ V17   : num  0.208 -0.115 1.11 -0.684 -0.237 ...
    ##  $ V18   : num  0.0258 -0.1834 -0.1214 1.9658 -0.0382 ...
    ##  $ V19   : num  0.404 -0.146 -2.262 -1.233 0.803 ...
    ##  $ V20   : num  0.2514 -0.0691 0.525 -0.208 0.4085 ...
    ##  $ V21   : num  -0.01831 -0.22578 0.248 -0.1083 -0.00943 ...
    ##  $ V22   : num  0.27784 -0.63867 0.77168 0.00527 0.79828 ...
    ##  $ V23   : num  -0.11 0.101 0.909 -0.19 -0.137 ...
    ##  $ V24   : num  0.0669 -0.3398 -0.6893 -1.1756 0.1413 ...
    ##  $ V25   : num  0.129 0.167 -0.328 0.647 -0.206 ...
    ##  $ V26   : num  -0.189 0.126 -0.139 -0.222 0.502 ...
    ##  $ V27   : num  0.13356 -0.00898 -0.05535 0.06272 0.21942 ...
    ##  $ V28   : num  -0.0211 0.0147 -0.0598 0.0615 0.2152 ...
    ##  $ Amount: num  149.62 2.69 378.66 123.5 69.99 ...
    ##  $ Class : int  0 0 0 0 0 0 0 0 0 0 ...

``` r
#Call 'data_report' function for the dataset
data_report(credit_data)
```

    ## [1] "Data Quality Report"

    ##        data_type  count unique_values missing_values
    ## Time     numeric 284807        124592              0
    ## V1       numeric 284807        275663              0
    ## V2       numeric 284807        275663              0
    ## V3       numeric 284807        275663              0
    ## V4       numeric 284807        275663              0
    ## V5       numeric 284807        275663              0
    ## V6       numeric 284807        275663              0
    ## V7       numeric 284807        275663              0
    ## V8       numeric 284807        275663              0
    ## V9       numeric 284807        275663              0
    ## V10      numeric 284807        275663              0
    ## V11      numeric 284807        275663              0
    ## V12      numeric 284807        275663              0
    ## V13      numeric 284807        275663              0
    ## V14      numeric 284807        275663              0
    ## V15      numeric 284807        275663              0
    ## V16      numeric 284807        275663              0
    ## V17      numeric 284807        275663              0
    ## V18      numeric 284807        275663              0
    ## V19      numeric 284807        275663              0
    ## V20      numeric 284807        275663              0
    ## V21      numeric 284807        275663              0
    ## V22      numeric 284807        275663              0
    ## V23      numeric 284807        275663              0
    ## V24      numeric 284807        275663              0
    ## V25      numeric 284807        275663              0
    ## V26      numeric 284807        275663              0
    ## V27      numeric 284807        275663              0
    ## V28      numeric 284807        275663              0
    ## Amount   numeric 284807         32767              0
    ## Class    integer 284807             2              0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(credit_data, is.numeric)
head(numeric_columns)
```

    ##   Time         V1          V2        V3         V4          V5          V6
    ## 1    0 -1.3598071 -0.07278117 2.5363467  1.3781552 -0.33832077  0.46238778
    ## 2    0  1.1918571  0.26615071 0.1664801  0.4481541  0.06001765 -0.08236081
    ## 3    1 -1.3583541 -1.34016307 1.7732093  0.3797796 -0.50319813  1.80049938
    ## 4    1 -0.9662717 -0.18522601 1.7929933 -0.8632913 -0.01030888  1.24720317
    ## 5    2 -1.1582331  0.87773675 1.5487178  0.4030339 -0.40719338  0.09592146
    ## 6    2 -0.4259659  0.96052304 1.1411093 -0.1682521  0.42098688 -0.02972755
    ##            V7          V8         V9         V10        V11         V12
    ## 1  0.23959855  0.09869790  0.3637870  0.09079417 -0.5515995 -0.61780086
    ## 2 -0.07880298  0.08510165 -0.2554251 -0.16697441  1.6127267  1.06523531
    ## 3  0.79146096  0.24767579 -1.5146543  0.20764287  0.6245015  0.06608369
    ## 4  0.23760894  0.37743587 -1.3870241 -0.05495192 -0.2264873  0.17822823
    ## 5  0.59294075 -0.27053268  0.8177393  0.75307443 -0.8228429  0.53819555
    ## 6  0.47620095  0.26031433 -0.5686714 -0.37140720  1.3412620  0.35989384
    ##          V13        V14        V15        V16         V17         V18
    ## 1 -0.9913898 -0.3111694  1.4681770 -0.4704005  0.20797124  0.02579058
    ## 2  0.4890950 -0.1437723  0.6355581  0.4639170 -0.11480466 -0.18336127
    ## 3  0.7172927 -0.1659459  2.3458649 -2.8900832  1.10996938 -0.12135931
    ## 4  0.5077569 -0.2879237 -0.6314181 -1.0596472 -0.68409279  1.96577500
    ## 5  1.3458516 -1.1196698  0.1751211 -0.4514492 -0.23703324 -0.03819479
    ## 6 -0.3580907 -0.1371337  0.5176168  0.4017259 -0.05813282  0.06865315
    ##           V19         V20          V21          V22         V23
    ## 1  0.40399296  0.25141210 -0.018306778  0.277837576 -0.11047391
    ## 2 -0.14578304 -0.06908314 -0.225775248 -0.638671953  0.10128802
    ## 3 -2.26185710  0.52497973  0.247998153  0.771679402  0.90941226
    ## 4 -1.23262197 -0.20803778 -0.108300452  0.005273597 -0.19032052
    ## 5  0.80348692  0.40854236 -0.009430697  0.798278495 -0.13745808
    ## 6 -0.03319379  0.08496767 -0.208253515 -0.559824796 -0.02639767
    ##           V24        V25        V26          V27         V28 Amount Class
    ## 1  0.06692807  0.1285394 -0.1891148  0.133558377 -0.02105305 149.62     0
    ## 2 -0.33984648  0.1671704  0.1258945 -0.008983099  0.01472417   2.69     0
    ## 3 -0.68928096 -0.3276418 -0.1390966 -0.055352794 -0.05975184 378.66     0
    ## 4 -1.17557533  0.6473760 -0.2219288  0.062722849  0.06145763 123.50     0
    ## 5  0.14126698 -0.2060096  0.5022922  0.219422230  0.21515315  69.99     0
    ## 6 -0.37142658 -0.2327938  0.1059148  0.253844225  0.08108026   3.67     0

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 31"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(credit_data, is.factor)
head(categorical_columns)
```

    ## data frame with 0 columns and 6 rows

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 0"

``` r
#The frequency of different levels of each categorical column
count(credit_data, 'Class')
```

    ##   Class   freq
    ## 1     0 284315
    ## 2     1    492

### 4. DDS Dataset Collection

There are two datasets, one gives the information about AWS honeypots and the second one adds some information about the geolocation.

<http://datadrivensecurity.info/blog/pages/dds-dataset-collection.html>

``` r
marx = read.csv('marx.csv')
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(marx))
```

    ## [1] 451581      7

``` r
str(marx)
```

    ## 'data.frame':    451581 obs. of  7 variables:
    ##  $ datetime: Factor w/ 380323 levels "2013-03-03 21:53:59",..: 1 2 4 3 5 6 7 8 9 10 ...
    ##  $ host    : Factor w/ 9 levels "groucho-eu","groucho-norcal",..: 3 3 3 8 5 7 3 5 3 5 ...
    ##  $ src     : num  1.03e+09 1.35e+09 2.95e+09 8.42e+08 3.59e+09 ...
    ##  $ proto   : Factor w/ 3 levels "ICMP","TCP","UDP": 2 3 2 3 2 2 2 2 2 2 ...
    ##  $ type    : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ spt     : int  6000 5270 2489 43235 56577 32628 6000 6000 6000 6000 ...
    ##  $ dpt     : int  1433 5060 1080 1900 80 2323 1433 3306 1433 1433 ...

``` r
#Call 'data_report' function for the dataset
data_report(marx)
```

    ## [1] "Data Quality Report"

    ##          data_type  count unique_values missing_values
    ## datetime    factor 451581        380323              0
    ## host        factor 451581             9              0
    ## src        numeric 451581         69602              0
    ## proto       factor 451581             3              0
    ## type       integer  44811             8         406770
    ## spt        integer 406770         46189          44811
    ## dpt        integer 406770          4042          44811

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(marx, is.numeric)
head(numeric_columns)
```

    ##          src type   spt  dpt
    ## 1 1032051418   NA  6000 1433
    ## 2 1347834426   NA  5270 5060
    ## 3 2947856490   NA  2489 1080
    ## 4  841842716   NA 43235 1900
    ## 5 3587648279   NA 56577   80
    ## 6 3323217250   NA 32628 2323

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 4"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(marx, is.factor)
head(categorical_columns)
```

    ##              datetime              host proto
    ## 1 2013-03-03 21:53:59    groucho-oregon   TCP
    ## 2 2013-03-03 21:57:01    groucho-oregon   UDP
    ## 3 2013-03-03 21:58:10    groucho-oregon   TCP
    ## 4 2013-03-03 21:58:09   groucho-us-east   UDP
    ## 5 2013-03-03 21:58:20 groucho-singapore   TCP
    ## 6 2013-03-03 21:58:41     groucho-tokyo   TCP

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 3"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'host')
```

    ##                host   freq
    ## 1        groucho-eu  23954
    ## 2    groucho-norcal  24566
    ## 3    groucho-oregon  94076
    ## 4        groucho-sa  24316
    ## 5 groucho-singapore  78151
    ## 6    groucho-sydney  24456
    ## 7     groucho-tokyo 126189
    ## 8   groucho-us-east  31779
    ## 9      zeppo-norcal  24094

``` r
count(categorical_columns, 'proto')
```

    ##   proto   freq
    ## 1  ICMP  44811
    ## 2   TCP 327991
    ## 3   UDP  78779

``` r
count(marx, 'type')
```

    ##   type   freq
    ## 1    0    536
    ## 2    3   4251
    ## 3    5    127
    ## 4    8  38597
    ## 5   11   1156
    ## 6   12      2
    ## 7   13    142
    ## 8   NA 406770

``` r
marx_geo = read.csv('marx-geo.csv')
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(marx_geo))
```

    ## [1] 451581     16

``` r
str(marx_geo)
```

    ## 'data.frame':    451581 obs. of  16 variables:
    ##  $ datetime  : Factor w/ 185118 levels "3/10/13 0:00",..: 17291 17292 17293 17293 17293 17293 17294 17295 17296 17297 ...
    ##  $ host      : Factor w/ 9 levels "groucho-eu","groucho-norcal",..: 3 3 3 8 5 7 3 5 3 5 ...
    ##  $ src       : num  1.03e+09 1.35e+09 2.95e+09 8.42e+08 3.59e+09 ...
    ##  $ proto     : Factor w/ 3 levels "ICMP","TCP","UDP": 2 3 2 3 2 2 2 2 2 2 ...
    ##  $ type      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ spt       : int  6000 5270 2489 43235 56577 32628 6000 6000 6000 6000 ...
    ##  $ dpt       : int  1433 5060 1080 1900 80 2323 1433 3306 1433 1433 ...
    ##  $ srcstr    : Factor w/ 69602 levels "1.0.0.38","1.1.162.110",..: 56491 62595 23980 52966 40813 34587 46361 45824 43150 55791 ...
    ##  $ cc        : Factor w/ 177 levels "","AD","AE","AF",..: 36 43 161 165 56 165 36 36 36 36 ...
    ##  $ country   : Factor w/ 178 levels "","Afghanistan",..: 38 59 159 171 56 171 38 38 38 38 ...
    ##  $ locale    : Factor w/ 1180 levels "","Aargau","Abu Dhabi",..: 392 1 1022 683 1 370 349 956 346 492 ...
    ##  $ localeabbr: Factor w/ 613 levels ""," D.C.","0",..: 39 1 1 417 1 282 45 59 11 20 ...
    ##  $ postalcode: Factor w/ 2778 levels "","100","10000",..: 1 1 1 2506 1 1546 1 1 1 1 ...
    ##  $ latitude  : num  28.6 51 25 45.6 48.9 ...
    ##  $ longitude : num  115.93 9 121.53 -122.91 2.35 ...
    ##  $ X         : num  NA NA NA NA NA NA NA NA NA NA ...

``` r
#Call 'data_report' function for the dataset
data_report(marx_geo)
```

    ## [1] "Data Quality Report"

    ##            data_type  count unique_values missing_values
    ## datetime      factor 451581        185118              0
    ## host          factor 451581             9              0
    ## src          numeric 451581         69602              0
    ## proto         factor 451581             3              0
    ## type         integer  44811             8         406770
    ## spt          integer 406770         46189          44811
    ## dpt          integer 406770          4042          44811
    ## srcstr        factor 451581         69602              0
    ## cc            factor 451580           178              1
    ## country       factor 451581           178              0
    ## locale        factor 451581          1180              0
    ## localeabbr    factor 451547           614             34
    ## postalcode    factor 451581          2778              0
    ## latitude     numeric 448113          5091           3468
    ## longitude    numeric 448153          5255           3428
    ## X            numeric     82             6         451499

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(marx_geo, is.numeric)
head(numeric_columns)
```

    ##          src type   spt  dpt latitude longitude  X
    ## 1 1032051418   NA  6000 1433  28.5500  115.9333 NA
    ## 2 1347834426   NA  5270 5060  51.0000    9.0000 NA
    ## 3 2947856490   NA  2489 1080  25.0392  121.5250 NA
    ## 4  841842716   NA 43235 1900  45.5848 -122.9117 NA
    ## 5 3587648279   NA 56577   80  48.8600    2.3500 NA
    ## 6 3323217250   NA 32628 2323  41.8825  -87.6441 NA

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 7"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(marx_geo, is.factor)
head(categorical_columns)
```

    ##       datetime              host proto          srcstr cc       country
    ## 1 3/3/13 21:53    groucho-oregon   TCP  61.131.218.218 CN         China
    ## 2 3/3/13 21:57    groucho-oregon   UDP     80.86.82.58 DE       Germany
    ## 3 3/3/13 21:58    groucho-oregon   TCP 175.180.184.106 TW        Taiwan
    ## 4 3/3/13 21:58   groucho-us-east   UDP    50.45.128.28 US United States
    ## 5 3/3/13 21:58 groucho-singapore   TCP   213.215.43.23 FR        France
    ## 6 3/3/13 21:58     groucho-tokyo   TCP    198.20.69.98 US United States
    ##          locale localeabbr postalcode
    ## 1 Jiangxi Sheng         36           
    ## 2                                    
    ## 3        Taipei                      
    ## 4        Oregon         OR      97124
    ## 5                                    
    ## 6      Illinois         IL      60661

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 9"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'host')
```

    ##                host   freq
    ## 1        groucho-eu  23954
    ## 2    groucho-norcal  24566
    ## 3    groucho-oregon  94076
    ## 4        groucho-sa  24316
    ## 5 groucho-singapore  78151
    ## 6    groucho-sydney  24456
    ## 7     groucho-tokyo 126189
    ## 8   groucho-us-east  31779
    ## 9      zeppo-norcal  24094

``` r
count(categorical_columns, 'proto')
```

    ##   proto   freq
    ## 1  ICMP  44811
    ## 2   TCP 327991
    ## 3   UDP  78779

``` r
count(marx_geo, 'type')
```

    ##   type   freq
    ## 1    0    536
    ## 2    3   4251
    ## 3    5    127
    ## 4    8  38597
    ## 5   11   1156
    ## 6   12      2
    ## 7   13    142
    ## 8   NA 406770

### 5. UNSW-NB15 Dataset

The Australian Center for Cyber Security (ACCS) created this dataset that contains nine types of attack. Another dataset for network intrusion detection which can be compared with KDD dataset.

This is a labeled dataset.

<https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/>

``` r
unsw_data = read.csv('UNSW-NB15_1.csv', header = FALSE)
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(unsw_data))
```

    ## [1] 700001     49

``` r
str(unsw_data)
```

    ## 'data.frame':    700001 obs. of  49 variables:
    ##  $ V1 : Factor w/ 40 levels "10.40.170.2",..: 31 31 37 36 34 31 37 3 36 38 ...
    ##  $ V2 : Factor w/ 64541 levels "-","0","0x000b",..: 4273 26019 5088 28514 43624 24322 12548 2 33790 2909 ...
    ##  $ V3 : Factor w/ 44 levels "10.40.170.2",..: 23 26 24 22 7 26 21 2 23 21 ...
    ##  $ V4 : Factor w/ 62222 levels "-","0","0x20205321",..: 45601 240 45601 45601 45601 1160 45601 2 45601 45601 ...
    ##  $ V5 : Factor w/ 135 levels "3pc","a/n","aes-sp3-d",..: 121 121 121 121 121 121 121 7 121 121 ...
    ##  $ V6 : Factor w/ 16 levels "ACC","CLO","CON",..: 3 3 3 3 3 3 3 7 3 3 ...
    ##  $ V7 : num  0.00105 0.03613 0.00112 0.00121 0.00117 ...
    ##  $ V8 : int  132 528 146 132 146 568 132 46 146 132 ...
    ##  $ V9 : int  164 304 178 164 178 312 164 0 178 164 ...
    ##  $ V10: int  31 31 31 31 31 31 31 0 31 31 ...
    ##  $ V11: int  29 29 29 29 29 29 29 0 29 29 ...
    ##  $ V12: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V13: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V14: Factor w/ 13 levels "-","dhcp","dns",..: 3 1 3 3 3 1 3 1 3 3 ...
    ##  $ V15: num  500474 87676 521895 436725 499572 ...
    ##  $ V16: num  621801 50480 636282 542597 609068 ...
    ##  $ V17: int  2 4 2 2 2 4 2 1 2 2 ...
    ##  $ V18: int  2 4 2 2 2 4 2 0 2 2 ...
    ##  $ V19: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V20: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V21: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V22: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V23: int  66 132 73 66 73 142 66 46 73 66 ...
    ##  $ V24: int  82 76 89 82 89 78 82 0 89 82 ...
    ##  $ V25: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V26: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V27: num  0 9.89 0 0 0 ...
    ##  $ V28: num  0 10.7 0 0 0 ...
    ##  $ V29: int  1421927414 1421927414 1421927414 1421927414 1421927414 1421927414 1421927414 1421927415 1421927415 1421927415 ...
    ##  $ V30: int  1421927414 1421927414 1421927414 1421927414 1421927414 1421927414 1421927414 1421927415 1421927415 1421927415 ...
    ##  $ V31: num  0.017 7.005 0.017 0.043 0.005 ...
    ##  $ V32: num  0.013 7.564 0.013 0.014 0.003 ...
    ##  $ V33: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V34: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V35: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V36: int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ V37: int  0 0 0 0 0 0 0 2 0 0 ...
    ##  $ V38: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V39: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V40: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V41: int  3 2 12 6 7 2 12 2 6 6 ...
    ##  $ V42: int  7 4 8 9 9 4 7 2 7 7 ...
    ##  $ V43: int  1 2 1 1 1 2 1 2 3 2 ...
    ##  $ V44: int  3 3 2 1 1 3 2 2 1 1 ...
    ##  $ V45: int  1 1 2 1 1 1 2 2 1 1 ...
    ##  $ V46: int  1 1 1 1 1 1 1 2 1 1 ...
    ##  $ V47: int  1 2 1 1 1 2 1 2 1 1 ...
    ##  $ V48: Factor w/ 10 levels ""," Fuzzers",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ V49: int  0 0 0 0 0 0 0 0 0 0 ...

``` r
#Call 'data_report' function for the dataset
data_report(unsw_data)
```

    ## [1] "Data Quality Report"

    ##     data_type  count unique_values missing_values
    ## V1     factor 700001            40              0
    ## V2     factor 700001         64541              0
    ## V3     factor 700001            44              0
    ## V4     factor 700001         62222              0
    ## V5     factor 700001           135              0
    ## V6     factor 700001            16              0
    ## V7    numeric 700001        243828              0
    ## V8    integer 700001          7948              0
    ## V9    integer 700001         11898              0
    ## V10   integer 700001            13              0
    ## V11   integer 700001            11              0
    ## V12   integer 700001           275              0
    ## V13   integer 700001           572              0
    ## V14    factor 700001            13              0
    ## V15   numeric 700001        391840              0
    ## V16   numeric 700001        403499              0
    ## V17   integer 700001           715              0
    ## V18   integer 700001          1086              0
    ## V19   integer 700001            12              0
    ## V20   integer 700001             4              0
    ## V21   numeric 700001        441843              0
    ## V22   numeric 700001        441640              0
    ## V23   integer 700001          1288              0
    ## V24   integer 700001          1356              0
    ## V25   integer 700001             6              0
    ## V26   integer 700001           543              0
    ## V27   numeric 700001        426221              0
    ## V28   numeric 700001        454152              0
    ## V29   integer 700001         27568              0
    ## V30   integer 700001         27566              0
    ## V31   numeric 700001        350430              0
    ## V32   numeric 700001        351748              0
    ## V33   numeric 700001         17050              0
    ## V34   numeric 700001         15769              0
    ## V35   numeric 700001         13690              0
    ## V36   integer 700001             2              0
    ## V37   integer 700001             7              0
    ## V38   integer 700001            13              0
    ## V39   integer 700001             2              0
    ## V40   integer 700001             8              0
    ## V41   integer 700001            43              0
    ## V42   integer 700001            40              0
    ## V43   integer 700001            41              0
    ## V44   integer 700001            45              0
    ## V45   integer 700001            34              0
    ## V46   integer 700001            32              0
    ## V47   integer 700001            33              0
    ## V48    factor 700001            10              0
    ## V49   integer 700001             2              0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(unsw_data, is.numeric)
head(numeric_columns)
```

    ##         V7  V8  V9 V10 V11 V12 V13       V15       V16 V17 V18 V19 V20 V21
    ## 1 0.001055 132 164  31  29   0   0 500473.94 621800.94   2   2   0   0   0
    ## 2 0.036133 528 304  31  29   0   0  87676.09  50480.17   4   4   0   0   0
    ## 3 0.001119 146 178  31  29   0   0 521894.53 636282.38   2   2   0   0   0
    ## 4 0.001209 132 164  31  29   0   0 436724.56 542597.19   2   2   0   0   0
    ## 5 0.001169 146 178  31  29   0   0 499572.25 609067.56   2   2   0   0   0
    ## 6 0.078339 568 312  31  29   0   0  43503.23  23896.14   4   4   0   0   0
    ##   V22 V23 V24 V25 V26      V27      V28        V29        V30    V31
    ## 1   0  66  82   0   0  0.00000  0.00000 1421927414 1421927414  0.017
    ## 2   0 132  76   0   0  9.89101 10.68273 1421927414 1421927414  7.005
    ## 3   0  73  89   0   0  0.00000  0.00000 1421927414 1421927414  0.017
    ## 4   0  66  82   0   0  0.00000  0.00000 1421927414 1421927414  0.043
    ## 5   0  73  89   0   0  0.00000  0.00000 1421927414 1421927414  0.005
    ## 6   0 142  78   0   0 29.68222 34.37034 1421927414 1421927414 21.003
    ##         V32 V33 V34 V35 V36 V37 V38 V39 V40 V41 V42 V43 V44 V45 V46 V47
    ## 1  0.013000   0   0   0   0   0   0   0   0   3   7   1   3   1   1   1
    ## 2  7.564333   0   0   0   0   0   0   0   0   2   4   2   3   1   1   2
    ## 3  0.013000   0   0   0   0   0   0   0   0  12   8   1   2   2   1   1
    ## 4  0.014000   0   0   0   0   0   0   0   0   6   9   1   1   1   1   1
    ## 5  0.003000   0   0   0   0   0   0   0   0   7   9   1   1   1   1   1
    ## 6 24.315000   0   0   0   0   0   0   0   0   2   4   2   3   1   1   2
    ##   V49
    ## 1   0
    ## 2   0
    ## 3   0
    ## 4   0
    ## 5   0
    ## 6   0

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 41"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(unsw_data, is.factor)
head(categorical_columns)
```

    ##           V1    V2            V3   V4  V5  V6 V14 V48
    ## 1 59.166.0.0  1390 149.171.126.6   53 udp CON dns    
    ## 2 59.166.0.0 33661 149.171.126.9 1024 udp CON   -    
    ## 3 59.166.0.6  1464 149.171.126.7   53 udp CON dns    
    ## 4 59.166.0.5  3593 149.171.126.5   53 udp CON dns    
    ## 5 59.166.0.3 49664 149.171.126.0   53 udp CON dns    
    ## 6 59.166.0.0 32119 149.171.126.9  111 udp CON   -

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 8"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'V1')
```

    ##                 V1  freq
    ## 1      10.40.170.2   874
    ## 2      10.40.182.1  1670
    ## 3      10.40.182.3   874
    ## 4       10.40.85.1  1680
    ## 5      10.40.85.30   888
    ## 6        127.0.0.1     1
    ## 7    149.171.126.0   192
    ## 8    149.171.126.1   251
    ## 9   149.171.126.10     3
    ## 10  149.171.126.11    16
    ## 11  149.171.126.12     2
    ## 12  149.171.126.13     1
    ## 13  149.171.126.15     8
    ## 14  149.171.126.16     6
    ## 15  149.171.126.17     1
    ## 16  149.171.126.18  6010
    ## 17  149.171.126.19     3
    ## 18   149.171.126.2   232
    ## 19   149.171.126.3   231
    ## 20   149.171.126.4   225
    ## 21   149.171.126.5   249
    ## 22   149.171.126.6   241
    ## 23   149.171.126.7   207
    ## 24   149.171.126.8   217
    ## 25   149.171.126.9   217
    ## 26    175.45.176.0  4782
    ## 27    175.45.176.1 14325
    ## 28    175.45.176.2  3236
    ## 29    175.45.176.3  5128
    ## 30 192.168.241.243   108
    ## 31      59.166.0.0 67128
    ## 32      59.166.0.1 66587
    ## 33      59.166.0.2 67209
    ## 34      59.166.0.3 66145
    ## 35      59.166.0.4 66722
    ## 36      59.166.0.5 67091
    ## 37      59.166.0.6 64689
    ## 38      59.166.0.7 63725
    ## 39      59.166.0.8 64640
    ## 40      59.166.0.9 64187

``` r
count(categorical_columns, 'V3')
```

    ##                 V3  freq
    ## 1      10.40.170.2   874
    ## 2      10.40.182.3  1582
    ## 3     10.40.198.10     6
    ## 4       10.40.85.1   888
    ## 5      10.40.85.30   718
    ## 6        127.0.0.1     1
    ## 7    149.171.126.0 66925
    ## 8    149.171.126.1 66973
    ## 9   149.171.126.10  1975
    ## 10  149.171.126.11  1796
    ## 11  149.171.126.12  4815
    ## 12  149.171.126.13  2113
    ## 13  149.171.126.14  1955
    ## 14  149.171.126.15  1631
    ## 15  149.171.126.16  1400
    ## 16  149.171.126.17  2109
    ## 17  149.171.126.18  8213
    ## 18  149.171.126.19  1449
    ## 19   149.171.126.2 66674
    ## 20   149.171.126.3 66697
    ## 21   149.171.126.4 67331
    ## 22   149.171.126.5 66723
    ## 23   149.171.126.6 63952
    ## 24   149.171.126.7 64057
    ## 25   149.171.126.8 63604
    ## 26   149.171.126.9 65187
    ## 27    175.45.176.0    70
    ## 28    175.45.176.1  5965
    ## 29    175.45.176.2    10
    ## 30    175.45.176.3    10
    ## 31 192.168.241.243   108
    ## 32       224.0.0.1    14
    ## 33       224.0.0.5  1912
    ## 34     32.50.32.66     2
    ## 35      59.166.0.0   241
    ## 36      59.166.0.1   273
    ## 37      59.166.0.2   253
    ## 38      59.166.0.3   201
    ## 39      59.166.0.4   218
    ## 40      59.166.0.5   214
    ## 41      59.166.0.6   211
    ## 42      59.166.0.7   203
    ## 43      59.166.0.8   215
    ## 44      59.166.0.9   233

``` r
count(categorical_columns, 'V6')
```

    ##     V6   freq
    ## 1  ACC     22
    ## 2  CLO    111
    ## 3  CON 187505
    ## 4  ECO     26
    ## 5  ECR      2
    ## 6  FIN 487911
    ## 7  INT  21799
    ## 8  MAS      2
    ## 9   no      2
    ## 10 PAR      4
    ## 11 REQ   2429
    ## 12 RST     74
    ## 13 TST      2
    ## 14 TXD      2
    ## 15 URH    108
    ## 16 URN      2

``` r
count(categorical_columns, 'V48')
```

    ##               V48   freq
    ## 1                 677786
    ## 2         Fuzzers   5051
    ## 3        Analysis    526
    ## 4       Backdoors    534
    ## 5             DoS   1167
    ## 6        Exploits   5409
    ## 7         Generic   7522
    ## 8  Reconnaissance   1759
    ## 9       Shellcode    223
    ## 10          Worms     24

### 6. CSIC 2010 http dataset

The CSIC 2010 HTTP dataset includes web application penetration testing packets and is created with the goal of feature selection before working on classification.

This is a labeled dataset.

<https://petescully.co.uk/research/csic-2010-http-dataset-in-csv-format-for-weka-analysis/>

``` r
csic_data = read.csv('output_http_csic_2010.csv')
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(csic_data))
```

    ## [1] 223585     18

``` r
str(csic_data)
```

    ## 'data.frame':    223585 obs. of  18 variables:
    ##  $ index         : int  0 0 0 0 0 1 1 1 1 1 ...
    ##  $ method        : Factor w/ 3 levels "GET","POST","PUT": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ url           : Factor w/ 1643 levels "http://localhost:8080.bak",..: 1212 1212 1212 1212 1212 1212 1212 1212 1212 1212 ...
    ##  $ protocol      : Factor w/ 1 level "HTTP/1.1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ userAgent     : Factor w/ 1 level "Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ pragma        : Factor w/ 1 level "no-cache": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ cacheControl  : Factor w/ 1 level "no-cache": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ accept        : Factor w/ 1 level "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ acceptEncoding: Factor w/ 1 level "x-gzip, x-deflate, gzip, deflate": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ acceptCharset : Factor w/ 1 level "utf-8, utf-8;q=0.5, *;q=0.5": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ acceptLanguage: Factor w/ 1 level "en": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ host          : Factor w/ 2 levels "localhost:8080",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ connection    : Factor w/ 1 level "close": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ contentLength : Factor w/ 383 levels "100","101","102",..: 383 383 383 383 383 383 383 383 383 383 ...
    ##  $ contentType   : Factor w/ 2 levels "application/x-www-form-urlencoded",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ cookie        : Factor w/ 61065 levels "JSESSIONID=0000442E26C2F96F52C97120FF00254B",..: 44255 44255 44255 44255 44255 58577 58577 58577 58577 58577 ...
    ##  $ payload       : Factor w/ 33770 levels "","apellidos=_Bignes+Crisp%ED",..: 17163 22942 31314 2960 2712 17184 22942 31314 3032 2712 ...
    ##  $ label         : Factor w/ 2 levels "anom","norm": 1 1 1 1 1 1 1 1 1 1 ...

``` r
#Call 'data_report' function for the dataset
data_report(csic_data)
```

    ## [1] "Data Quality Report"

    ##                data_type  count unique_values missing_values
    ## index            integer 223585         36000              0
    ## method            factor 223585             3              0
    ## url               factor 223585          1643              0
    ## protocol          factor 223585             1              0
    ## userAgent         factor 223585             1              0
    ## pragma            factor 223585             1              0
    ## cacheControl      factor 223585             1              0
    ## accept            factor 223585             1              0
    ## acceptEncoding    factor 223585             1              0
    ## acceptCharset     factor 223585             1              0
    ## acceptLanguage    factor 223585             1              0
    ## host              factor 223585             2              0
    ## connection        factor 223585             1              0
    ## contentLength     factor 223585           383              0
    ## contentType       factor 223585             2              0
    ## cookie            factor 223585         61065              0
    ## payload           factor 223585         33770              0
    ## label             factor 223585             2              0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(csic_data, is.numeric)
head(numeric_columns)
```

    ##   index
    ## 1     0
    ## 2     0
    ## 3     0
    ## 4     0
    ## 5     0
    ## 6     1

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 1"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(csic_data, is.factor)
head(categorical_columns)
```

    ##   method                                              url protocol
    ## 1    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ## 2    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ## 3    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ## 4    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ## 5    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ## 6    GET http://localhost:8080/tienda1/publico/anadir.jsp HTTP/1.1
    ##                                                                 userAgent
    ## 1 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ## 2 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ## 3 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ## 4 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ## 5 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ## 6 Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.8 (like Gecko)
    ##     pragma cacheControl
    ## 1 no-cache     no-cache
    ## 2 no-cache     no-cache
    ## 3 no-cache     no-cache
    ## 4 no-cache     no-cache
    ## 5 no-cache     no-cache
    ## 6 no-cache     no-cache
    ##                                                                                                accept
    ## 1 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ## 2 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ## 3 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ## 4 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ## 5 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ## 6 text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
    ##                     acceptEncoding               acceptCharset
    ## 1 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ## 2 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ## 3 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ## 4 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ## 5 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ## 6 x-gzip, x-deflate, gzip, deflate utf-8, utf-8;q=0.5, *;q=0.5
    ##   acceptLanguage           host connection contentLength contentType
    ## 1             en localhost:8080      close          null        null
    ## 2             en localhost:8080      close          null        null
    ## 3             en localhost:8080      close          null        null
    ## 4             en localhost:8080      close          null        null
    ## 5             en localhost:8080      close          null        null
    ## 6             en localhost:8080      close          null        null
    ##                                        cookie
    ## 1 JSESSIONID=B92A8B48B9008CD29F622A994E0F650D
    ## 2 JSESSIONID=B92A8B48B9008CD29F622A994E0F650D
    ## 3 JSESSIONID=B92A8B48B9008CD29F622A994E0F650D
    ## 4 JSESSIONID=B92A8B48B9008CD29F622A994E0F650D
    ## 5 JSESSIONID=B92A8B48B9008CD29F622A994E0F650D
    ## 6 JSESSIONID=F563B5262843F12ECAE41815ABDEEA54
    ##                                                                               payload
    ## 1                                                                                id=2
    ## 2                                                            nombre=Jam%F3n+Ib%E9rico
    ## 3                                                                           precio=85
    ## 4 cantidad=%27%3B+DROP+TABLE+usuarios%3B+SELECT+*+FROM+datos+WHERE+nombre+LIKE+%27%25
    ## 5                                                              B1=A%F1adir+al+carrito
    ## 6                                                                             id=2%2F
    ##   label
    ## 1  anom
    ## 2  anom
    ## 3  anom
    ## 4  anom
    ## 5  anom
    ## 6  anom

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 17"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'method')
```

    ##   method   freq
    ## 1    GET 123450
    ## 2   POST  97942
    ## 3    PUT   2193

``` r
count(categorical_columns, 'host')
```

    ##             host   freq
    ## 1 localhost:8080 221392
    ## 2 localhost:9090   2193

``` r
count(categorical_columns, 'label')
```

    ##   label   freq
    ## 1  anom 119585
    ## 2  norm 104000

### 7. Malware apps from Drebin project

The dataset contains more than 200 features from malware and benign apps and is useful for developing and evaluating multilevel classification.

This is a labeled dataset.

<https://figshare.com/articles/Android_malware_dataset_for_machine_learning_2/5854653/1>

``` r
drebin_data = read.csv('drebin.csv')
print("Shape of Dataset:")
```

    ## [1] "Shape of Dataset:"

``` r
print(dim(drebin_data))
```

    ## [1] 15036   216

``` r
str(drebin_data)
```

    ## 'data.frame':    15036 obs. of  216 variables:
    ##  $ transact                                       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ onServiceConnected                             : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ bindService                                    : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ attachInterface                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ServiceConnection                              : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ android.os.Binder                              : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ SEND_SMS                                       : int  1 1 1 0 0 0 0 0 1 1 ...
    ##  $ Ljava.lang.Class.getCanonicalName              : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ljava.lang.Class.getMethods                    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ljava.lang.Class.cast                          : int  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ Ljava.net.URLDecoder                           : int  0 0 0 1 1 0 0 1 0 0 ...
    ##  $ android.content.pm.Signature                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.telephony.SmsManager                   : int  1 1 1 0 0 0 0 0 1 0 ...
    ##  $ READ_PHONE_STATE                               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ getBinder                                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ClassLoader                                    : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ Landroid.content.Context.registerReceiver      : int  1 1 0 1 0 0 0 1 0 0 ...
    ##  $ Ljava.lang.Class.getField                      : int  0 0 1 0 0 0 0 0 0 0 ...
    ##  $ Landroid.content.Context.unregisterReceiver    : int  1 1 0 0 0 0 0 0 0 0 ...
    ##  $ GET_ACCOUNTS                                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ RECEIVE_SMS                                    : int  0 1 0 0 0 0 0 0 1 0 ...
    ##  $ Ljava.lang.Class.getDeclaredField              : int  0 0 0 1 1 0 0 1 0 0 ...
    ##  $ READ_SMS                                       : int  0 1 0 1 0 1 0 1 1 1 ...
    ##  $ getCallingUid                                  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ljavax.crypto.spec.SecretKeySpec               : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ android.intent.action.BOOT_COMPLETED           : int  1 1 0 1 1 1 1 1 1 1 ...
    ##  $ USE_CREDENTIALS                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MANAGE_ACCOUNTS                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.content.pm.PackageInfo                 : int  0 0 0 1 0 1 1 1 0 1 ...
    ##  $ KeySpec                                        : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ TelephonyManager.getLine1Number                : int  0 0 0 1 0 1 1 1 1 1 ...
    ##  $ DexClassLoader                                 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HttpGet.init                                   : int  0 0 0 1 1 0 1 1 1 0 ...
    ##  $ SecretKey                                      : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ Ljava.lang.Class.getMethod                     : int  0 0 1 0 1 0 1 0 0 0 ...
    ##  $ System.loadLibrary                             : int  0 0 0 1 0 1 0 1 0 0 ...
    ##  $ android.intent.action.SEND                     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ljavax.crypto.Cipher                           : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ WRITE_SMS                                      : int  0 0 0 1 0 1 0 1 1 1 ...
    ##  $ READ_SYNC_SETTINGS                             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AUTHENTICATE_ACCOUNTS                          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.telephony.gsm.SmsManager               : int  0 0 0 0 0 0 0 0 1 1 ...
    ##  $ WRITE_HISTORY_BOOKMARKS                        : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ TelephonyManager.getSubscriberId               : int  0 0 0 1 0 1 1 1 0 1 ...
    ##  $ mount                                          : int  0 0 0 1 1 1 1 1 0 1 ...
    ##  $ INSTALL_PACKAGES                               : int  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ Runtime.getRuntime                             : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ CAMERA                                         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ljava.lang.Object.getClass                     : int  0 0 1 1 1 1 1 1 0 1 ...
    ##  $ WRITE_SYNC_SETTINGS                            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_HISTORY_BOOKMARKS                         : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ Ljava.lang.Class.forName                       : int  0 0 1 1 1 0 1 1 0 0 ...
    ##  $ INTERNET                                       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ android.intent.action.PACKAGE_REPLACED         : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ Binder                                         : int  0 0 0 1 0 1 1 1 1 0 ...
    ##  $ android.intent.action.SEND_MULTIPLE            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ RECORD_AUDIO                                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ IBinder                                        : int  0 0 0 1 0 1 1 1 1 0 ...
    ##  $ android.os.IBinder                             : int  0 0 0 1 0 1 1 1 1 0 ...
    ##  $ createSubprocess                               : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ NFC                                            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ACCESS_LOCATION_EXTRA_COMMANDS                 : int  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ URLClassLoader                                 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ WRITE_APN_SETTINGS                             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ abortBroadcast                                 : int  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ BIND_REMOTEVIEWS                               : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.intent.action.TIME_SET                 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_PROFILE                                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ TelephonyManager.getDeviceId                   : int  0 0 0 1 1 1 1 1 1 1 ...
    ##  $ MODIFY_AUDIO_SETTINGS                          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ getCallingPid                                  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_SYNC_STATS                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ BROADCAST_STICKY                               : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.intent.action.PACKAGE_REMOVED          : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ android.intent.action.TIMEZONE_CHANGED         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ WAKE_LOCK                                      : int  0 1 0 0 0 1 0 1 1 0 ...
    ##  $ RECEIVE_BOOT_COMPLETED                         : int  0 0 0 0 1 0 1 0 0 1 ...
    ##  $ RESTART_PACKAGES                               : int  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ Ljava.lang.Class.getPackage                    : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ chmod                                          : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ Ljava.lang.Class.getDeclaredClasses            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.intent.action.ACTION_POWER_DISCONNECTED: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ android.intent.action.PACKAGE_ADDED            : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ PathClassLoader                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ TelephonyManager.getSimSerialNumber            : int  0 0 0 1 0 1 1 1 0 0 ...
    ##  $ Runtime.load                                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ TelephonyManager.getCallState                  : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ BLUETOOTH                                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_CALENDAR                                  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_CALL_LOG                                  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SUBSCRIBED_FEEDS_WRITE                         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ READ_EXTERNAL_STORAGE                          : int  0 0 0 1 0 0 0 1 1 0 ...
    ##  $ TelephonyManager.getSimCountryIso              : Factor w/ 3 levels "?","0","1": 2 2 2 2 2 2 2 2 3 2 ...
    ##  $ sendMultipartTextMessage                       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PackageInstaller                               : int  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ VIBRATE                                        : int  0 0 0 0 1 0 0 0 0 1 ...
    ##  $ remount                                        : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ android.intent.action.ACTION_SHUTDOWN          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sendDataMessage                                : int  0 0 0 0 0 0 0 0 0 0 ...
    ##   [list output truncated]

``` r
#Call 'data_report' function for the dataset
data_report(drebin_data)
```

    ## [1] "Data Quality Report"

    ##                                                 data_type count
    ## transact                                          integer 15036
    ## onServiceConnected                                integer 15036
    ## bindService                                       integer 15036
    ## attachInterface                                   integer 15036
    ## ServiceConnection                                 integer 15036
    ## android.os.Binder                                 integer 15036
    ## SEND_SMS                                          integer 15036
    ## Ljava.lang.Class.getCanonicalName                 integer 15036
    ## Ljava.lang.Class.getMethods                       integer 15036
    ## Ljava.lang.Class.cast                             integer 15036
    ## Ljava.net.URLDecoder                              integer 15036
    ## android.content.pm.Signature                      integer 15036
    ## android.telephony.SmsManager                      integer 15036
    ## READ_PHONE_STATE                                  integer 15036
    ## getBinder                                         integer 15036
    ## ClassLoader                                       integer 15036
    ## Landroid.content.Context.registerReceiver         integer 15036
    ## Ljava.lang.Class.getField                         integer 15036
    ## Landroid.content.Context.unregisterReceiver       integer 15036
    ## GET_ACCOUNTS                                      integer 15036
    ## RECEIVE_SMS                                       integer 15036
    ## Ljava.lang.Class.getDeclaredField                 integer 15036
    ## READ_SMS                                          integer 15036
    ## getCallingUid                                     integer 15036
    ## Ljavax.crypto.spec.SecretKeySpec                  integer 15036
    ## android.intent.action.BOOT_COMPLETED              integer 15036
    ## USE_CREDENTIALS                                   integer 15036
    ## MANAGE_ACCOUNTS                                   integer 15036
    ## android.content.pm.PackageInfo                    integer 15036
    ## KeySpec                                           integer 15036
    ## TelephonyManager.getLine1Number                   integer 15036
    ## DexClassLoader                                    integer 15036
    ## HttpGet.init                                      integer 15036
    ## SecretKey                                         integer 15036
    ## Ljava.lang.Class.getMethod                        integer 15036
    ## System.loadLibrary                                integer 15036
    ## android.intent.action.SEND                        integer 15036
    ## Ljavax.crypto.Cipher                              integer 15036
    ## WRITE_SMS                                         integer 15036
    ## READ_SYNC_SETTINGS                                integer 15036
    ## AUTHENTICATE_ACCOUNTS                             integer 15036
    ## android.telephony.gsm.SmsManager                  integer 15036
    ## WRITE_HISTORY_BOOKMARKS                           integer 15036
    ## TelephonyManager.getSubscriberId                  integer 15036
    ## mount                                             integer 15036
    ## INSTALL_PACKAGES                                  integer 15036
    ## Runtime.getRuntime                                integer 15036
    ## CAMERA                                            integer 15036
    ## Ljava.lang.Object.getClass                        integer 15036
    ## WRITE_SYNC_SETTINGS                               integer 15036
    ## READ_HISTORY_BOOKMARKS                            integer 15036
    ## Ljava.lang.Class.forName                          integer 15036
    ## INTERNET                                          integer 15036
    ## android.intent.action.PACKAGE_REPLACED            integer 15036
    ## Binder                                            integer 15036
    ## android.intent.action.SEND_MULTIPLE               integer 15036
    ## RECORD_AUDIO                                      integer 15036
    ## IBinder                                           integer 15036
    ## android.os.IBinder                                integer 15036
    ## createSubprocess                                  integer 15036
    ## NFC                                               integer 15036
    ## ACCESS_LOCATION_EXTRA_COMMANDS                    integer 15036
    ## URLClassLoader                                    integer 15036
    ## WRITE_APN_SETTINGS                                integer 15036
    ## abortBroadcast                                    integer 15036
    ## BIND_REMOTEVIEWS                                  integer 15036
    ## android.intent.action.TIME_SET                    integer 15036
    ## READ_PROFILE                                      integer 15036
    ## TelephonyManager.getDeviceId                      integer 15036
    ## MODIFY_AUDIO_SETTINGS                             integer 15036
    ## getCallingPid                                     integer 15036
    ## READ_SYNC_STATS                                   integer 15036
    ## BROADCAST_STICKY                                  integer 15036
    ## android.intent.action.PACKAGE_REMOVED             integer 15036
    ## android.intent.action.TIMEZONE_CHANGED            integer 15036
    ## WAKE_LOCK                                         integer 15036
    ## RECEIVE_BOOT_COMPLETED                            integer 15036
    ## RESTART_PACKAGES                                  integer 15036
    ## Ljava.lang.Class.getPackage                       integer 15036
    ## chmod                                             integer 15036
    ## Ljava.lang.Class.getDeclaredClasses               integer 15036
    ## android.intent.action.ACTION_POWER_DISCONNECTED   integer 15036
    ## android.intent.action.PACKAGE_ADDED               integer 15036
    ## PathClassLoader                                   integer 15036
    ## TelephonyManager.getSimSerialNumber               integer 15036
    ## Runtime.load                                      integer 15036
    ## TelephonyManager.getCallState                     integer 15036
    ## BLUETOOTH                                         integer 15036
    ## READ_CALENDAR                                     integer 15036
    ## READ_CALL_LOG                                     integer 15036
    ## SUBSCRIBED_FEEDS_WRITE                            integer 15036
    ## READ_EXTERNAL_STORAGE                             integer 15036
    ## TelephonyManager.getSimCountryIso                  factor 15036
    ## sendMultipartTextMessage                          integer 15036
    ## PackageInstaller                                  integer 15036
    ## VIBRATE                                           integer 15036
    ## remount                                           integer 15036
    ## android.intent.action.ACTION_SHUTDOWN             integer 15036
    ## sendDataMessage                                   integer 15036
    ## ACCESS_NETWORK_STATE                              integer 15036
    ## chown                                             integer 15036
    ## HttpPost.init                                     integer 15036
    ## Ljava.lang.Class.getClasses                       integer 15036
    ## SUBSCRIBED_FEEDS_READ                             integer 15036
    ## TelephonyManager.isNetworkRoaming                 integer 15036
    ## CHANGE_WIFI_MULTICAST_STATE                       integer 15036
    ## WRITE_CALENDAR                                    integer 15036
    ## android.intent.action.PACKAGE_DATA_CLEARED        integer 15036
    ## MASTER_CLEAR                                      integer 15036
    ## HttpUriRequest                                    integer 15036
    ## UPDATE_DEVICE_STATS                               integer 15036
    ## WRITE_CALL_LOG                                    integer 15036
    ## DELETE_PACKAGES                                   integer 15036
    ## GET_TASKS                                         integer 15036
    ## GLOBAL_SEARCH                                     integer 15036
    ## DELETE_CACHE_FILES                                integer 15036
    ## WRITE_USER_DICTIONARY                             integer 15036
    ## android.intent.action.PACKAGE_CHANGED             integer 15036
    ## android.intent.action.NEW_OUTGOING_CALL           integer 15036
    ## REORDER_TASKS                                     integer 15036
    ## WRITE_PROFILE                                     integer 15036
    ## SET_WALLPAPER                                     integer 15036
    ## BIND_INPUT_METHOD                                 integer 15036
    ## divideMessage                                     integer 15036
    ## READ_SOCIAL_STREAM                                integer 15036
    ## READ_USER_DICTIONARY                              integer 15036
    ## PROCESS_OUTGOING_CALLS                            integer 15036
    ## CALL_PRIVILEGED                                   integer 15036
    ## Runtime.exec                                      integer 15036
    ## BIND_WALLPAPER                                    integer 15036
    ## RECEIVE_WAP_PUSH                                  integer 15036
    ## DUMP                                              integer 15036
    ## BATTERY_STATS                                     integer 15036
    ## ACCESS_COARSE_LOCATION                            integer 15036
    ## SET_TIME                                          integer 15036
    ## android.intent.action.SENDTO                      integer 15036
    ## WRITE_SOCIAL_STREAM                               integer 15036
    ## WRITE_SETTINGS                                    integer 15036
    ## REBOOT                                            integer 15036
    ## BLUETOOTH_ADMIN                                   integer 15036
    ## TelephonyManager.getNetworkOperator               integer 15036
    ## X.system.bin                                      integer 15036
    ## MessengerService                                  integer 15036
    ## BIND_DEVICE_ADMIN                                 integer 15036
    ## WRITE_GSERVICES                                   integer 15036
    ## IRemoteService                                    integer 15036
    ## KILL_BACKGROUND_PROCESSES                         integer 15036
    ## SET_ALARM                                         integer 15036
    ## ACCOUNT_MANAGER                                   integer 15036
    ## X.system.app                                      integer 15036
    ## android.intent.action.CALL                        integer 15036
    ## STATUS_BAR                                        integer 15036
    ## TelephonyManager.getSimOperator                   integer 15036
    ## PERSISTENT_ACTIVITY                               integer 15036
    ## CHANGE_NETWORK_STATE                              integer 15036
    ## onBind                                            integer 15036
    ## Process.start                                     integer 15036
    ## android.intent.action.SCREEN_ON                   integer 15036
    ## Context.bindService                               integer 15036
    ## RECEIVE_MMS                                       integer 15036
    ## SET_TIME_ZONE                                     integer 15036
    ## android.intent.action.BATTERY_OKAY                integer 15036
    ## CONTROL_LOCATION_UPDATES                          integer 15036
    ## BROADCAST_WAP_PUSH                                integer 15036
    ## BIND_ACCESSIBILITY_SERVICE                        integer 15036
    ## ADD_VOICEMAIL                                     integer 15036
    ## CALL_PHONE                                        integer 15036
    ## ProcessBuilder                                    integer 15036
    ## BIND_APPWIDGET                                    integer 15036
    ## FLASHLIGHT                                        integer 15036
    ## READ_LOGS                                         integer 15036
    ## Ljava.lang.Class.getResource                      integer 15036
    ## defineClass                                       integer 15036
    ## SET_PROCESS_LIMIT                                 integer 15036
    ## android.intent.action.PACKAGE_RESTARTED           integer 15036
    ## MOUNT_UNMOUNT_FILESYSTEMS                         integer 15036
    ## BIND_TEXT_SERVICE                                 integer 15036
    ## INSTALL_LOCATION_PROVIDER                         integer 15036
    ## android.intent.action.CALL_BUTTON                 integer 15036
    ## android.intent.action.SCREEN_OFF                  integer 15036
    ## findClass                                         integer 15036
    ## SYSTEM_ALERT_WINDOW                               integer 15036
    ## MOUNT_FORMAT_FILESYSTEMS                          integer 15036
    ## CHANGE_CONFIGURATION                              integer 15036
    ## CLEAR_APP_USER_DATA                               integer 15036
    ## intent.action.RUN                                 integer 15036
    ## android.intent.action.SET_WALLPAPER               integer 15036
    ## CHANGE_WIFI_STATE                                 integer 15036
    ## READ_FRAME_BUFFER                                 integer 15036
    ## ACCESS_SURFACE_FLINGER                            integer 15036
    ## Runtime.loadLibrary                               integer 15036
    ## BROADCAST_SMS                                     integer 15036
    ## EXPAND_STATUS_BAR                                 integer 15036
    ## INTERNAL_SYSTEM_WINDOW                            integer 15036
    ## android.intent.action.BATTERY_LOW                 integer 15036
    ## SET_ACTIVITY_WATCHER                              integer 15036
    ## WRITE_CONTACTS                                    integer 15036
    ## android.intent.action.ACTION_POWER_CONNECTED      integer 15036
    ## BIND_VPN_SERVICE                                  integer 15036
    ## DISABLE_KEYGUARD                                  integer 15036
    ## ACCESS_MOCK_LOCATION                              integer 15036
    ## GET_PACKAGE_SIZE                                  integer 15036
    ## MODIFY_PHONE_STATE                                integer 15036
    ## CHANGE_COMPONENT_ENABLED_STATE                    integer 15036
    ## CLEAR_APP_CACHE                                   integer 15036
    ## SET_ORIENTATION                                   integer 15036
    ## READ_CONTACTS                                     integer 15036
    ## DEVICE_POWER                                      integer 15036
    ## HARDWARE_TEST                                     integer 15036
    ## ACCESS_WIFI_STATE                                 integer 15036
    ## WRITE_EXTERNAL_STORAGE                            integer 15036
    ## ACCESS_FINE_LOCATION                              integer 15036
    ## SET_WALLPAPER_HINTS                               integer 15036
    ## SET_PREFERRED_APPLICATIONS                        integer 15036
    ## WRITE_SECURE_SETTINGS                             integer 15036
    ## class                                              factor 15036
    ##                                                 unique_values
    ## transact                                                    2
    ## onServiceConnected                                          2
    ## bindService                                                 2
    ## attachInterface                                             2
    ## ServiceConnection                                           2
    ## android.os.Binder                                           2
    ## SEND_SMS                                                    2
    ## Ljava.lang.Class.getCanonicalName                           2
    ## Ljava.lang.Class.getMethods                                 2
    ## Ljava.lang.Class.cast                                       2
    ## Ljava.net.URLDecoder                                        2
    ## android.content.pm.Signature                                2
    ## android.telephony.SmsManager                                2
    ## READ_PHONE_STATE                                            2
    ## getBinder                                                   2
    ## ClassLoader                                                 2
    ## Landroid.content.Context.registerReceiver                   2
    ## Ljava.lang.Class.getField                                   2
    ## Landroid.content.Context.unregisterReceiver                 2
    ## GET_ACCOUNTS                                                2
    ## RECEIVE_SMS                                                 2
    ## Ljava.lang.Class.getDeclaredField                           2
    ## READ_SMS                                                    2
    ## getCallingUid                                               2
    ## Ljavax.crypto.spec.SecretKeySpec                            2
    ## android.intent.action.BOOT_COMPLETED                        2
    ## USE_CREDENTIALS                                             2
    ## MANAGE_ACCOUNTS                                             2
    ## android.content.pm.PackageInfo                              2
    ## KeySpec                                                     2
    ## TelephonyManager.getLine1Number                             2
    ## DexClassLoader                                              2
    ## HttpGet.init                                                2
    ## SecretKey                                                   2
    ## Ljava.lang.Class.getMethod                                  2
    ## System.loadLibrary                                          2
    ## android.intent.action.SEND                                  2
    ## Ljavax.crypto.Cipher                                        2
    ## WRITE_SMS                                                   2
    ## READ_SYNC_SETTINGS                                          2
    ## AUTHENTICATE_ACCOUNTS                                       2
    ## android.telephony.gsm.SmsManager                            2
    ## WRITE_HISTORY_BOOKMARKS                                     2
    ## TelephonyManager.getSubscriberId                            2
    ## mount                                                       2
    ## INSTALL_PACKAGES                                            2
    ## Runtime.getRuntime                                          2
    ## CAMERA                                                      2
    ## Ljava.lang.Object.getClass                                  2
    ## WRITE_SYNC_SETTINGS                                         2
    ## READ_HISTORY_BOOKMARKS                                      2
    ## Ljava.lang.Class.forName                                    2
    ## INTERNET                                                    2
    ## android.intent.action.PACKAGE_REPLACED                      2
    ## Binder                                                      2
    ## android.intent.action.SEND_MULTIPLE                         2
    ## RECORD_AUDIO                                                2
    ## IBinder                                                     2
    ## android.os.IBinder                                          2
    ## createSubprocess                                            2
    ## NFC                                                         2
    ## ACCESS_LOCATION_EXTRA_COMMANDS                              2
    ## URLClassLoader                                              2
    ## WRITE_APN_SETTINGS                                          2
    ## abortBroadcast                                              2
    ## BIND_REMOTEVIEWS                                            2
    ## android.intent.action.TIME_SET                              2
    ## READ_PROFILE                                                2
    ## TelephonyManager.getDeviceId                                2
    ## MODIFY_AUDIO_SETTINGS                                       2
    ## getCallingPid                                               2
    ## READ_SYNC_STATS                                             2
    ## BROADCAST_STICKY                                            2
    ## android.intent.action.PACKAGE_REMOVED                       2
    ## android.intent.action.TIMEZONE_CHANGED                      2
    ## WAKE_LOCK                                                   2
    ## RECEIVE_BOOT_COMPLETED                                      2
    ## RESTART_PACKAGES                                            2
    ## Ljava.lang.Class.getPackage                                 2
    ## chmod                                                       2
    ## Ljava.lang.Class.getDeclaredClasses                         2
    ## android.intent.action.ACTION_POWER_DISCONNECTED             2
    ## android.intent.action.PACKAGE_ADDED                         2
    ## PathClassLoader                                             2
    ## TelephonyManager.getSimSerialNumber                         2
    ## Runtime.load                                                2
    ## TelephonyManager.getCallState                               2
    ## BLUETOOTH                                                   2
    ## READ_CALENDAR                                               2
    ## READ_CALL_LOG                                               2
    ## SUBSCRIBED_FEEDS_WRITE                                      2
    ## READ_EXTERNAL_STORAGE                                       2
    ## TelephonyManager.getSimCountryIso                           3
    ## sendMultipartTextMessage                                    2
    ## PackageInstaller                                            2
    ## VIBRATE                                                     2
    ## remount                                                     2
    ## android.intent.action.ACTION_SHUTDOWN                       2
    ## sendDataMessage                                             2
    ## ACCESS_NETWORK_STATE                                        2
    ## chown                                                       2
    ## HttpPost.init                                               2
    ## Ljava.lang.Class.getClasses                                 2
    ## SUBSCRIBED_FEEDS_READ                                       2
    ## TelephonyManager.isNetworkRoaming                           2
    ## CHANGE_WIFI_MULTICAST_STATE                                 2
    ## WRITE_CALENDAR                                              2
    ## android.intent.action.PACKAGE_DATA_CLEARED                  2
    ## MASTER_CLEAR                                                2
    ## HttpUriRequest                                              2
    ## UPDATE_DEVICE_STATS                                         2
    ## WRITE_CALL_LOG                                              2
    ## DELETE_PACKAGES                                             2
    ## GET_TASKS                                                   2
    ## GLOBAL_SEARCH                                               2
    ## DELETE_CACHE_FILES                                          2
    ## WRITE_USER_DICTIONARY                                       2
    ## android.intent.action.PACKAGE_CHANGED                       2
    ## android.intent.action.NEW_OUTGOING_CALL                     2
    ## REORDER_TASKS                                               2
    ## WRITE_PROFILE                                               2
    ## SET_WALLPAPER                                               2
    ## BIND_INPUT_METHOD                                           2
    ## divideMessage                                               2
    ## READ_SOCIAL_STREAM                                          2
    ## READ_USER_DICTIONARY                                        2
    ## PROCESS_OUTGOING_CALLS                                      2
    ## CALL_PRIVILEGED                                             2
    ## Runtime.exec                                                2
    ## BIND_WALLPAPER                                              2
    ## RECEIVE_WAP_PUSH                                            2
    ## DUMP                                                        2
    ## BATTERY_STATS                                               2
    ## ACCESS_COARSE_LOCATION                                      2
    ## SET_TIME                                                    2
    ## android.intent.action.SENDTO                                2
    ## WRITE_SOCIAL_STREAM                                         2
    ## WRITE_SETTINGS                                              2
    ## REBOOT                                                      2
    ## BLUETOOTH_ADMIN                                             2
    ## TelephonyManager.getNetworkOperator                         2
    ## X.system.bin                                                2
    ## MessengerService                                            2
    ## BIND_DEVICE_ADMIN                                           2
    ## WRITE_GSERVICES                                             2
    ## IRemoteService                                              2
    ## KILL_BACKGROUND_PROCESSES                                   2
    ## SET_ALARM                                                   2
    ## ACCOUNT_MANAGER                                             2
    ## X.system.app                                                2
    ## android.intent.action.CALL                                  2
    ## STATUS_BAR                                                  2
    ## TelephonyManager.getSimOperator                             2
    ## PERSISTENT_ACTIVITY                                         2
    ## CHANGE_NETWORK_STATE                                        2
    ## onBind                                                      2
    ## Process.start                                               2
    ## android.intent.action.SCREEN_ON                             2
    ## Context.bindService                                         2
    ## RECEIVE_MMS                                                 2
    ## SET_TIME_ZONE                                               2
    ## android.intent.action.BATTERY_OKAY                          2
    ## CONTROL_LOCATION_UPDATES                                    2
    ## BROADCAST_WAP_PUSH                                          2
    ## BIND_ACCESSIBILITY_SERVICE                                  2
    ## ADD_VOICEMAIL                                               2
    ## CALL_PHONE                                                  2
    ## ProcessBuilder                                              2
    ## BIND_APPWIDGET                                              2
    ## FLASHLIGHT                                                  2
    ## READ_LOGS                                                   2
    ## Ljava.lang.Class.getResource                                2
    ## defineClass                                                 2
    ## SET_PROCESS_LIMIT                                           2
    ## android.intent.action.PACKAGE_RESTARTED                     2
    ## MOUNT_UNMOUNT_FILESYSTEMS                                   2
    ## BIND_TEXT_SERVICE                                           2
    ## INSTALL_LOCATION_PROVIDER                                   2
    ## android.intent.action.CALL_BUTTON                           2
    ## android.intent.action.SCREEN_OFF                            2
    ## findClass                                                   2
    ## SYSTEM_ALERT_WINDOW                                         2
    ## MOUNT_FORMAT_FILESYSTEMS                                    2
    ## CHANGE_CONFIGURATION                                        2
    ## CLEAR_APP_USER_DATA                                         2
    ## intent.action.RUN                                           2
    ## android.intent.action.SET_WALLPAPER                         2
    ## CHANGE_WIFI_STATE                                           2
    ## READ_FRAME_BUFFER                                           2
    ## ACCESS_SURFACE_FLINGER                                      2
    ## Runtime.loadLibrary                                         2
    ## BROADCAST_SMS                                               2
    ## EXPAND_STATUS_BAR                                           2
    ## INTERNAL_SYSTEM_WINDOW                                      2
    ## android.intent.action.BATTERY_LOW                           2
    ## SET_ACTIVITY_WATCHER                                        2
    ## WRITE_CONTACTS                                              2
    ## android.intent.action.ACTION_POWER_CONNECTED                2
    ## BIND_VPN_SERVICE                                            2
    ## DISABLE_KEYGUARD                                            2
    ## ACCESS_MOCK_LOCATION                                        2
    ## GET_PACKAGE_SIZE                                            2
    ## MODIFY_PHONE_STATE                                          2
    ## CHANGE_COMPONENT_ENABLED_STATE                              2
    ## CLEAR_APP_CACHE                                             2
    ## SET_ORIENTATION                                             2
    ## READ_CONTACTS                                               2
    ## DEVICE_POWER                                                2
    ## HARDWARE_TEST                                               2
    ## ACCESS_WIFI_STATE                                           2
    ## WRITE_EXTERNAL_STORAGE                                      2
    ## ACCESS_FINE_LOCATION                                        2
    ## SET_WALLPAPER_HINTS                                         2
    ## SET_PREFERRED_APPLICATIONS                                  2
    ## WRITE_SECURE_SETTINGS                                       2
    ## class                                                       2
    ##                                                 missing_values
    ## transact                                                     0
    ## onServiceConnected                                           0
    ## bindService                                                  0
    ## attachInterface                                              0
    ## ServiceConnection                                            0
    ## android.os.Binder                                            0
    ## SEND_SMS                                                     0
    ## Ljava.lang.Class.getCanonicalName                            0
    ## Ljava.lang.Class.getMethods                                  0
    ## Ljava.lang.Class.cast                                        0
    ## Ljava.net.URLDecoder                                         0
    ## android.content.pm.Signature                                 0
    ## android.telephony.SmsManager                                 0
    ## READ_PHONE_STATE                                             0
    ## getBinder                                                    0
    ## ClassLoader                                                  0
    ## Landroid.content.Context.registerReceiver                    0
    ## Ljava.lang.Class.getField                                    0
    ## Landroid.content.Context.unregisterReceiver                  0
    ## GET_ACCOUNTS                                                 0
    ## RECEIVE_SMS                                                  0
    ## Ljava.lang.Class.getDeclaredField                            0
    ## READ_SMS                                                     0
    ## getCallingUid                                                0
    ## Ljavax.crypto.spec.SecretKeySpec                             0
    ## android.intent.action.BOOT_COMPLETED                         0
    ## USE_CREDENTIALS                                              0
    ## MANAGE_ACCOUNTS                                              0
    ## android.content.pm.PackageInfo                               0
    ## KeySpec                                                      0
    ## TelephonyManager.getLine1Number                              0
    ## DexClassLoader                                               0
    ## HttpGet.init                                                 0
    ## SecretKey                                                    0
    ## Ljava.lang.Class.getMethod                                   0
    ## System.loadLibrary                                           0
    ## android.intent.action.SEND                                   0
    ## Ljavax.crypto.Cipher                                         0
    ## WRITE_SMS                                                    0
    ## READ_SYNC_SETTINGS                                           0
    ## AUTHENTICATE_ACCOUNTS                                        0
    ## android.telephony.gsm.SmsManager                             0
    ## WRITE_HISTORY_BOOKMARKS                                      0
    ## TelephonyManager.getSubscriberId                             0
    ## mount                                                        0
    ## INSTALL_PACKAGES                                             0
    ## Runtime.getRuntime                                           0
    ## CAMERA                                                       0
    ## Ljava.lang.Object.getClass                                   0
    ## WRITE_SYNC_SETTINGS                                          0
    ## READ_HISTORY_BOOKMARKS                                       0
    ## Ljava.lang.Class.forName                                     0
    ## INTERNET                                                     0
    ## android.intent.action.PACKAGE_REPLACED                       0
    ## Binder                                                       0
    ## android.intent.action.SEND_MULTIPLE                          0
    ## RECORD_AUDIO                                                 0
    ## IBinder                                                      0
    ## android.os.IBinder                                           0
    ## createSubprocess                                             0
    ## NFC                                                          0
    ## ACCESS_LOCATION_EXTRA_COMMANDS                               0
    ## URLClassLoader                                               0
    ## WRITE_APN_SETTINGS                                           0
    ## abortBroadcast                                               0
    ## BIND_REMOTEVIEWS                                             0
    ## android.intent.action.TIME_SET                               0
    ## READ_PROFILE                                                 0
    ## TelephonyManager.getDeviceId                                 0
    ## MODIFY_AUDIO_SETTINGS                                        0
    ## getCallingPid                                                0
    ## READ_SYNC_STATS                                              0
    ## BROADCAST_STICKY                                             0
    ## android.intent.action.PACKAGE_REMOVED                        0
    ## android.intent.action.TIMEZONE_CHANGED                       0
    ## WAKE_LOCK                                                    0
    ## RECEIVE_BOOT_COMPLETED                                       0
    ## RESTART_PACKAGES                                             0
    ## Ljava.lang.Class.getPackage                                  0
    ## chmod                                                        0
    ## Ljava.lang.Class.getDeclaredClasses                          0
    ## android.intent.action.ACTION_POWER_DISCONNECTED              0
    ## android.intent.action.PACKAGE_ADDED                          0
    ## PathClassLoader                                              0
    ## TelephonyManager.getSimSerialNumber                          0
    ## Runtime.load                                                 0
    ## TelephonyManager.getCallState                                0
    ## BLUETOOTH                                                    0
    ## READ_CALENDAR                                                0
    ## READ_CALL_LOG                                                0
    ## SUBSCRIBED_FEEDS_WRITE                                       0
    ## READ_EXTERNAL_STORAGE                                        0
    ## TelephonyManager.getSimCountryIso                            0
    ## sendMultipartTextMessage                                     0
    ## PackageInstaller                                             0
    ## VIBRATE                                                      0
    ## remount                                                      0
    ## android.intent.action.ACTION_SHUTDOWN                        0
    ## sendDataMessage                                              0
    ## ACCESS_NETWORK_STATE                                         0
    ## chown                                                        0
    ## HttpPost.init                                                0
    ## Ljava.lang.Class.getClasses                                  0
    ## SUBSCRIBED_FEEDS_READ                                        0
    ## TelephonyManager.isNetworkRoaming                            0
    ## CHANGE_WIFI_MULTICAST_STATE                                  0
    ## WRITE_CALENDAR                                               0
    ## android.intent.action.PACKAGE_DATA_CLEARED                   0
    ## MASTER_CLEAR                                                 0
    ## HttpUriRequest                                               0
    ## UPDATE_DEVICE_STATS                                          0
    ## WRITE_CALL_LOG                                               0
    ## DELETE_PACKAGES                                              0
    ## GET_TASKS                                                    0
    ## GLOBAL_SEARCH                                                0
    ## DELETE_CACHE_FILES                                           0
    ## WRITE_USER_DICTIONARY                                        0
    ## android.intent.action.PACKAGE_CHANGED                        0
    ## android.intent.action.NEW_OUTGOING_CALL                      0
    ## REORDER_TASKS                                                0
    ## WRITE_PROFILE                                                0
    ## SET_WALLPAPER                                                0
    ## BIND_INPUT_METHOD                                            0
    ## divideMessage                                                0
    ## READ_SOCIAL_STREAM                                           0
    ## READ_USER_DICTIONARY                                         0
    ## PROCESS_OUTGOING_CALLS                                       0
    ## CALL_PRIVILEGED                                              0
    ## Runtime.exec                                                 0
    ## BIND_WALLPAPER                                               0
    ## RECEIVE_WAP_PUSH                                             0
    ## DUMP                                                         0
    ## BATTERY_STATS                                                0
    ## ACCESS_COARSE_LOCATION                                       0
    ## SET_TIME                                                     0
    ## android.intent.action.SENDTO                                 0
    ## WRITE_SOCIAL_STREAM                                          0
    ## WRITE_SETTINGS                                               0
    ## REBOOT                                                       0
    ## BLUETOOTH_ADMIN                                              0
    ## TelephonyManager.getNetworkOperator                          0
    ## X.system.bin                                                 0
    ## MessengerService                                             0
    ## BIND_DEVICE_ADMIN                                            0
    ## WRITE_GSERVICES                                              0
    ## IRemoteService                                               0
    ## KILL_BACKGROUND_PROCESSES                                    0
    ## SET_ALARM                                                    0
    ## ACCOUNT_MANAGER                                              0
    ## X.system.app                                                 0
    ## android.intent.action.CALL                                   0
    ## STATUS_BAR                                                   0
    ## TelephonyManager.getSimOperator                              0
    ## PERSISTENT_ACTIVITY                                          0
    ## CHANGE_NETWORK_STATE                                         0
    ## onBind                                                       0
    ## Process.start                                                0
    ## android.intent.action.SCREEN_ON                              0
    ## Context.bindService                                          0
    ## RECEIVE_MMS                                                  0
    ## SET_TIME_ZONE                                                0
    ## android.intent.action.BATTERY_OKAY                           0
    ## CONTROL_LOCATION_UPDATES                                     0
    ## BROADCAST_WAP_PUSH                                           0
    ## BIND_ACCESSIBILITY_SERVICE                                   0
    ## ADD_VOICEMAIL                                                0
    ## CALL_PHONE                                                   0
    ## ProcessBuilder                                               0
    ## BIND_APPWIDGET                                               0
    ## FLASHLIGHT                                                   0
    ## READ_LOGS                                                    0
    ## Ljava.lang.Class.getResource                                 0
    ## defineClass                                                  0
    ## SET_PROCESS_LIMIT                                            0
    ## android.intent.action.PACKAGE_RESTARTED                      0
    ## MOUNT_UNMOUNT_FILESYSTEMS                                    0
    ## BIND_TEXT_SERVICE                                            0
    ## INSTALL_LOCATION_PROVIDER                                    0
    ## android.intent.action.CALL_BUTTON                            0
    ## android.intent.action.SCREEN_OFF                             0
    ## findClass                                                    0
    ## SYSTEM_ALERT_WINDOW                                          0
    ## MOUNT_FORMAT_FILESYSTEMS                                     0
    ## CHANGE_CONFIGURATION                                         0
    ## CLEAR_APP_USER_DATA                                          0
    ## intent.action.RUN                                            0
    ## android.intent.action.SET_WALLPAPER                          0
    ## CHANGE_WIFI_STATE                                            0
    ## READ_FRAME_BUFFER                                            0
    ## ACCESS_SURFACE_FLINGER                                       0
    ## Runtime.loadLibrary                                          0
    ## BROADCAST_SMS                                                0
    ## EXPAND_STATUS_BAR                                            0
    ## INTERNAL_SYSTEM_WINDOW                                       0
    ## android.intent.action.BATTERY_LOW                            0
    ## SET_ACTIVITY_WATCHER                                         0
    ## WRITE_CONTACTS                                               0
    ## android.intent.action.ACTION_POWER_CONNECTED                 0
    ## BIND_VPN_SERVICE                                             0
    ## DISABLE_KEYGUARD                                             0
    ## ACCESS_MOCK_LOCATION                                         0
    ## GET_PACKAGE_SIZE                                             0
    ## MODIFY_PHONE_STATE                                           0
    ## CHANGE_COMPONENT_ENABLED_STATE                               0
    ## CLEAR_APP_CACHE                                              0
    ## SET_ORIENTATION                                              0
    ## READ_CONTACTS                                                0
    ## DEVICE_POWER                                                 0
    ## HARDWARE_TEST                                                0
    ## ACCESS_WIFI_STATE                                            0
    ## WRITE_EXTERNAL_STORAGE                                       0
    ## ACCESS_FINE_LOCATION                                         0
    ## SET_WALLPAPER_HINTS                                          0
    ## SET_PREFERRED_APPLICATIONS                                   0
    ## WRITE_SECURE_SETTINGS                                        0
    ## class                                                        0

``` r
#Create a dataframe of numeric columns
numeric_columns <- select_if(drebin_data, is.numeric)
head(numeric_columns)
```

    ##   transact onServiceConnected bindService attachInterface
    ## 1        0                  0           0               0
    ## 2        0                  0           0               0
    ## 3        0                  0           0               0
    ## 4        0                  0           0               0
    ## 5        0                  0           0               0
    ## 6        0                  0           0               0
    ##   ServiceConnection android.os.Binder SEND_SMS
    ## 1                 0                 0        1
    ## 2                 0                 0        1
    ## 3                 0                 0        1
    ## 4                 0                 0        0
    ## 5                 0                 0        0
    ## 6                 0                 0        0
    ##   Ljava.lang.Class.getCanonicalName Ljava.lang.Class.getMethods
    ## 1                                 0                           0
    ## 2                                 0                           0
    ## 3                                 0                           0
    ## 4                                 0                           0
    ## 5                                 0                           0
    ## 6                                 0                           0
    ##   Ljava.lang.Class.cast Ljava.net.URLDecoder android.content.pm.Signature
    ## 1                     0                    0                            0
    ## 2                     0                    0                            0
    ## 3                     0                    0                            0
    ## 4                     1                    1                            0
    ## 5                     0                    1                            0
    ## 6                     0                    0                            0
    ##   android.telephony.SmsManager READ_PHONE_STATE getBinder ClassLoader
    ## 1                            1                1         0           0
    ## 2                            1                1         0           0
    ## 3                            1                1         0           0
    ## 4                            0                1         0           0
    ## 5                            0                1         0           1
    ## 6                            0                1         0           0
    ##   Landroid.content.Context.registerReceiver Ljava.lang.Class.getField
    ## 1                                         1                         0
    ## 2                                         1                         0
    ## 3                                         0                         1
    ## 4                                         1                         0
    ## 5                                         0                         0
    ## 6                                         0                         0
    ##   Landroid.content.Context.unregisterReceiver GET_ACCOUNTS RECEIVE_SMS
    ## 1                                           1            0           0
    ## 2                                           1            0           1
    ## 3                                           0            0           0
    ## 4                                           0            0           0
    ## 5                                           0            0           0
    ## 6                                           0            0           0
    ##   Ljava.lang.Class.getDeclaredField READ_SMS getCallingUid
    ## 1                                 0        0             0
    ## 2                                 0        1             0
    ## 3                                 0        0             0
    ## 4                                 1        1             0
    ## 5                                 1        0             0
    ## 6                                 0        1             0
    ##   Ljavax.crypto.spec.SecretKeySpec android.intent.action.BOOT_COMPLETED
    ## 1                                0                                    1
    ## 2                                0                                    1
    ## 3                                0                                    0
    ## 4                                1                                    1
    ## 5                                0                                    1
    ## 6                                1                                    1
    ##   USE_CREDENTIALS MANAGE_ACCOUNTS android.content.pm.PackageInfo KeySpec
    ## 1               0               0                              0       0
    ## 2               0               0                              0       0
    ## 3               0               0                              0       0
    ## 4               0               0                              1       1
    ## 5               0               0                              0       0
    ## 6               0               0                              1       1
    ##   TelephonyManager.getLine1Number DexClassLoader HttpGet.init SecretKey
    ## 1                               0              0            0         0
    ## 2                               0              0            0         0
    ## 3                               0              0            0         0
    ## 4                               1              0            1         1
    ## 5                               0              0            1         0
    ## 6                               1              0            0         1
    ##   Ljava.lang.Class.getMethod System.loadLibrary android.intent.action.SEND
    ## 1                          0                  0                          0
    ## 2                          0                  0                          0
    ## 3                          1                  0                          0
    ## 4                          0                  1                          0
    ## 5                          1                  0                          0
    ## 6                          0                  1                          0
    ##   Ljavax.crypto.Cipher WRITE_SMS READ_SYNC_SETTINGS AUTHENTICATE_ACCOUNTS
    ## 1                    0         0                  0                     0
    ## 2                    0         0                  0                     0
    ## 3                    0         0                  0                     0
    ## 4                    1         1                  0                     0
    ## 5                    0         0                  0                     0
    ## 6                    1         1                  0                     0
    ##   android.telephony.gsm.SmsManager WRITE_HISTORY_BOOKMARKS
    ## 1                                0                       0
    ## 2                                0                       0
    ## 3                                0                       0
    ## 4                                0                       0
    ## 5                                0                       1
    ## 6                                0                       0
    ##   TelephonyManager.getSubscriberId mount INSTALL_PACKAGES
    ## 1                                0     0                0
    ## 2                                0     0                0
    ## 3                                0     0                0
    ## 4                                1     1                1
    ## 5                                0     1                0
    ## 6                                1     1                0
    ##   Runtime.getRuntime CAMERA Ljava.lang.Object.getClass WRITE_SYNC_SETTINGS
    ## 1                  0      0                          0                   0
    ## 2                  0      0                          0                   0
    ## 3                  0      0                          1                   0
    ## 4                  1      0                          1                   0
    ## 5                  0      0                          1                   0
    ## 6                  1      0                          1                   0
    ##   READ_HISTORY_BOOKMARKS Ljava.lang.Class.forName INTERNET
    ## 1                      0                        0        1
    ## 2                      0                        0        1
    ## 3                      0                        1        1
    ## 4                      0                        1        1
    ## 5                      1                        1        1
    ## 6                      0                        0        1
    ##   android.intent.action.PACKAGE_REPLACED Binder
    ## 1                                      0      0
    ## 2                                      0      0
    ## 3                                      0      0
    ## 4                                      0      1
    ## 5                                      0      0
    ## 6                                      1      1
    ##   android.intent.action.SEND_MULTIPLE RECORD_AUDIO IBinder
    ## 1                                   0            0       0
    ## 2                                   0            0       0
    ## 3                                   0            0       0
    ## 4                                   0            0       1
    ## 5                                   0            0       0
    ## 6                                   0            0       1
    ##   android.os.IBinder createSubprocess NFC ACCESS_LOCATION_EXTRA_COMMANDS
    ## 1                  0                0   0                              0
    ## 2                  0                0   0                              0
    ## 3                  0                0   0                              0
    ## 4                  1                0   0                              1
    ## 5                  0                0   0                              0
    ## 6                  1                1   0                              0
    ##   URLClassLoader WRITE_APN_SETTINGS abortBroadcast BIND_REMOTEVIEWS
    ## 1              0                  0              0                0
    ## 2              0                  0              0                0
    ## 3              0                  0              0                0
    ## 4              0                  0              0                0
    ## 5              0                  0              0                0
    ## 6              0                  0              0                0
    ##   android.intent.action.TIME_SET READ_PROFILE TelephonyManager.getDeviceId
    ## 1                              0            0                            0
    ## 2                              0            0                            0
    ## 3                              0            0                            0
    ## 4                              0            0                            1
    ## 5                              0            0                            1
    ## 6                              0            0                            1
    ##   MODIFY_AUDIO_SETTINGS getCallingPid READ_SYNC_STATS BROADCAST_STICKY
    ## 1                     0             0               0                0
    ## 2                     0             0               0                0
    ## 3                     0             0               0                0
    ## 4                     0             0               0                0
    ## 5                     0             0               0                0
    ## 6                     0             0               0                0
    ##   android.intent.action.PACKAGE_REMOVED
    ## 1                                     0
    ## 2                                     0
    ## 3                                     0
    ## 4                                     0
    ## 5                                     0
    ## 6                                     1
    ##   android.intent.action.TIMEZONE_CHANGED WAKE_LOCK RECEIVE_BOOT_COMPLETED
    ## 1                                      0         0                      0
    ## 2                                      0         1                      0
    ## 3                                      0         0                      0
    ## 4                                      0         0                      0
    ## 5                                      0         0                      1
    ## 6                                      0         1                      0
    ##   RESTART_PACKAGES Ljava.lang.Class.getPackage chmod
    ## 1                0                           0     0
    ## 2                0                           0     0
    ## 3                0                           0     0
    ## 4                0                           0     1
    ## 5                0                           1     0
    ## 6                0                           0     1
    ##   Ljava.lang.Class.getDeclaredClasses
    ## 1                                   0
    ## 2                                   0
    ## 3                                   0
    ## 4                                   0
    ## 5                                   0
    ## 6                                   0
    ##   android.intent.action.ACTION_POWER_DISCONNECTED
    ## 1                                               0
    ## 2                                               0
    ## 3                                               0
    ## 4                                               0
    ## 5                                               0
    ## 6                                               0
    ##   android.intent.action.PACKAGE_ADDED PathClassLoader
    ## 1                                   0               0
    ## 2                                   0               0
    ## 3                                   0               0
    ## 4                                   0               0
    ## 5                                   0               0
    ## 6                                   1               0
    ##   TelephonyManager.getSimSerialNumber Runtime.load
    ## 1                                   0            0
    ## 2                                   0            0
    ## 3                                   0            0
    ## 4                                   1            0
    ## 5                                   0            0
    ## 6                                   1            0
    ##   TelephonyManager.getCallState BLUETOOTH READ_CALENDAR READ_CALL_LOG
    ## 1                             0         0             0             0
    ## 2                             0         0             0             0
    ## 3                             0         0             0             0
    ## 4                             0         0             0             0
    ## 5                             0         0             0             0
    ## 6                             0         0             0             0
    ##   SUBSCRIBED_FEEDS_WRITE READ_EXTERNAL_STORAGE sendMultipartTextMessage
    ## 1                      0                     0                        0
    ## 2                      0                     0                        0
    ## 3                      0                     0                        0
    ## 4                      0                     1                        0
    ## 5                      0                     0                        0
    ## 6                      0                     0                        0
    ##   PackageInstaller VIBRATE remount android.intent.action.ACTION_SHUTDOWN
    ## 1                0       0       0                                     0
    ## 2                0       0       0                                     0
    ## 3                0       0       0                                     0
    ## 4                0       0       0                                     0
    ## 5                0       1       0                                     0
    ## 6                0       0       0                                     0
    ##   sendDataMessage ACCESS_NETWORK_STATE chown HttpPost.init
    ## 1               0                    0     0             0
    ## 2               0                    0     0             1
    ## 3               0                    1     0             0
    ## 4               0                    1     0             1
    ## 5               0                    1     0             1
    ## 6               0                    1     0             1
    ##   Ljava.lang.Class.getClasses SUBSCRIBED_FEEDS_READ
    ## 1                           0                     0
    ## 2                           0                     0
    ## 3                           0                     0
    ## 4                           0                     0
    ## 5                           0                     0
    ## 6                           0                     0
    ##   TelephonyManager.isNetworkRoaming CHANGE_WIFI_MULTICAST_STATE
    ## 1                                 0                           0
    ## 2                                 0                           0
    ## 3                                 0                           0
    ## 4                                 0                           0
    ## 5                                 0                           0
    ## 6                                 0                           0
    ##   WRITE_CALENDAR android.intent.action.PACKAGE_DATA_CLEARED MASTER_CLEAR
    ## 1              0                                          0            0
    ## 2              0                                          0            0
    ## 3              0                                          0            0
    ## 4              0                                          0            0
    ## 5              0                                          0            0
    ## 6              0                                          0            0
    ##   HttpUriRequest UPDATE_DEVICE_STATS WRITE_CALL_LOG DELETE_PACKAGES
    ## 1              0                   0              0               0
    ## 2              1                   0              0               0
    ## 3              0                   0              0               0
    ## 4              1                   0              0               0
    ## 5              1                   0              0               0
    ## 6              1                   0              0               0
    ##   GET_TASKS GLOBAL_SEARCH DELETE_CACHE_FILES WRITE_USER_DICTIONARY
    ## 1         0             0                  0                     0
    ## 2         0             0                  0                     0
    ## 3         0             0                  0                     0
    ## 4         1             0                  0                     0
    ## 5         0             0                  0                     0
    ## 6         0             0                  0                     0
    ##   android.intent.action.PACKAGE_CHANGED
    ## 1                                     0
    ## 2                                     0
    ## 3                                     0
    ## 4                                     0
    ## 5                                     0
    ## 6                                     0
    ##   android.intent.action.NEW_OUTGOING_CALL REORDER_TASKS WRITE_PROFILE
    ## 1                                       0             0             0
    ## 2                                       0             0             0
    ## 3                                       0             0             0
    ## 4                                       0             0             0
    ## 5                                       0             0             0
    ## 6                                       0             0             0
    ##   SET_WALLPAPER BIND_INPUT_METHOD divideMessage READ_SOCIAL_STREAM
    ## 1             0                 0             0                  0
    ## 2             0                 0             0                  0
    ## 3             0                 0             0                  0
    ## 4             1                 0             0                  0
    ## 5             0                 0             0                  0
    ## 6             0                 0             0                  0
    ##   READ_USER_DICTIONARY PROCESS_OUTGOING_CALLS CALL_PRIVILEGED Runtime.exec
    ## 1                    0                      0               0            0
    ## 2                    0                      0               0            0
    ## 3                    0                      0               0            0
    ## 4                    0                      0               0            1
    ## 5                    0                      0               0            0
    ## 6                    0                      0               0            1
    ##   BIND_WALLPAPER RECEIVE_WAP_PUSH DUMP BATTERY_STATS
    ## 1              0                0    0             0
    ## 2              0                0    0             0
    ## 3              0                0    0             0
    ## 4              0                0    0             0
    ## 5              0                0    0             0
    ## 6              0                0    0             0
    ##   ACCESS_COARSE_LOCATION SET_TIME android.intent.action.SENDTO
    ## 1                      0        0                            0
    ## 2                      0        0                            0
    ## 3                      0        0                            0
    ## 4                      1        0                            0
    ## 5                      1        0                            0
    ## 6                      1        0                            0
    ##   WRITE_SOCIAL_STREAM WRITE_SETTINGS REBOOT BLUETOOTH_ADMIN
    ## 1                   0              0      0               0
    ## 2                   0              0      0               0
    ## 3                   0              0      0               0
    ## 4                   0              0      0               0
    ## 5                   0              0      0               0
    ## 6                   0              0      0               0
    ##   TelephonyManager.getNetworkOperator X.system.bin MessengerService
    ## 1                                   1            0                0
    ## 2                                   1            0                0
    ## 3                                   1            0                0
    ## 4                                   1            1                0
    ## 5                                   0            0                0
    ## 6                                   1            1                0
    ##   BIND_DEVICE_ADMIN WRITE_GSERVICES IRemoteService
    ## 1                 0               0              0
    ## 2                 0               0              0
    ## 3                 0               0              0
    ## 4                 0               0              0
    ## 5                 0               0              0
    ## 6                 0               0              0
    ##   KILL_BACKGROUND_PROCESSES SET_ALARM ACCOUNT_MANAGER X.system.app
    ## 1                         0         0               0            0
    ## 2                         0         0               0            0
    ## 3                         0         0               0            0
    ## 4                         0         0               0            1
    ## 5                         0         0               0            0
    ## 6                         0         0               0            1
    ##   android.intent.action.CALL STATUS_BAR TelephonyManager.getSimOperator
    ## 1                          0          0                               0
    ## 2                          0          0                               0
    ## 3                          0          0                               0
    ## 4                          0          0                               0
    ## 5                          0          0                               0
    ## 6                          0          0                               0
    ##   PERSISTENT_ACTIVITY CHANGE_NETWORK_STATE onBind Process.start
    ## 1                   0                    0      0             0
    ## 2                   0                    0      0             0
    ## 3                   0                    0      0             0
    ## 4                   0                    0      1             0
    ## 5                   0                    0      0             0
    ## 6                   0                    1      1             0
    ##   android.intent.action.SCREEN_ON Context.bindService RECEIVE_MMS
    ## 1                               0                   0           0
    ## 2                               0                   0           0
    ## 3                               0                   0           0
    ## 4                               0                   0           0
    ## 5                               0                   0           0
    ## 6                               0                   0           0
    ##   SET_TIME_ZONE android.intent.action.BATTERY_OKAY
    ## 1             0                                  0
    ## 2             0                                  0
    ## 3             0                                  0
    ## 4             0                                  0
    ## 5             0                                  0
    ## 6             0                                  0
    ##   CONTROL_LOCATION_UPDATES BROADCAST_WAP_PUSH BIND_ACCESSIBILITY_SERVICE
    ## 1                        0                  0                          0
    ## 2                        0                  0                          0
    ## 3                        0                  0                          0
    ## 4                        0                  0                          0
    ## 5                        0                  0                          0
    ## 6                        0                  0                          0
    ##   ADD_VOICEMAIL CALL_PHONE ProcessBuilder BIND_APPWIDGET FLASHLIGHT
    ## 1             0          0              0              0          0
    ## 2             0          0              0              0          0
    ## 3             0          0              0              0          0
    ## 4             0          0              0              0          0
    ## 5             0          0              0              0          1
    ## 6             0          0              0              0          0
    ##   READ_LOGS Ljava.lang.Class.getResource defineClass SET_PROCESS_LIMIT
    ## 1         0                            0           0                 0
    ## 2         0                            0           0                 0
    ## 3         0                            1           0                 0
    ## 4         0                            1           0                 0
    ## 5         0                            1           0                 0
    ## 6         1                            1           0                 0
    ##   android.intent.action.PACKAGE_RESTARTED MOUNT_UNMOUNT_FILESYSTEMS
    ## 1                                       0                         0
    ## 2                                       0                         0
    ## 3                                       0                         0
    ## 4                                       0                         0
    ## 5                                       0                         0
    ## 6                                       0                         0
    ##   BIND_TEXT_SERVICE INSTALL_LOCATION_PROVIDER
    ## 1                 0                         0
    ## 2                 0                         0
    ## 3                 0                         0
    ## 4                 0                         0
    ## 5                 0                         0
    ## 6                 0                         0
    ##   android.intent.action.CALL_BUTTON android.intent.action.SCREEN_OFF
    ## 1                                 0                                0
    ## 2                                 0                                0
    ## 3                                 0                                0
    ## 4                                 0                                0
    ## 5                                 0                                0
    ## 6                                 0                                0
    ##   findClass SYSTEM_ALERT_WINDOW MOUNT_FORMAT_FILESYSTEMS
    ## 1         0                   0                        0
    ## 2         0                   0                        0
    ## 3         0                   0                        0
    ## 4         0                   0                        0
    ## 5         1                   0                        0
    ## 6         0                   0                        0
    ##   CHANGE_CONFIGURATION CLEAR_APP_USER_DATA intent.action.RUN
    ## 1                    0                   0                 0
    ## 2                    0                   0                 0
    ## 3                    0                   0                 0
    ## 4                    1                   0                 0
    ## 5                    0                   0                 0
    ## 6                    0                   0                 0
    ##   android.intent.action.SET_WALLPAPER CHANGE_WIFI_STATE READ_FRAME_BUFFER
    ## 1                                   0                 0                 0
    ## 2                                   0                 0                 0
    ## 3                                   0                 0                 0
    ## 4                                   0                 1                 0
    ## 5                                   0                 0                 0
    ## 6                                   0                 1                 0
    ##   ACCESS_SURFACE_FLINGER Runtime.loadLibrary BROADCAST_SMS
    ## 1                      0                   0             0
    ## 2                      0                   0             0
    ## 3                      0                   0             0
    ## 4                      0                   0             0
    ## 5                      0                   0             0
    ## 6                      0                   0             0
    ##   EXPAND_STATUS_BAR INTERNAL_SYSTEM_WINDOW
    ## 1                 0                      0
    ## 2                 0                      0
    ## 3                 0                      0
    ## 4                 0                      0
    ## 5                 0                      0
    ## 6                 0                      0
    ##   android.intent.action.BATTERY_LOW SET_ACTIVITY_WATCHER WRITE_CONTACTS
    ## 1                                 0                    0              0
    ## 2                                 0                    0              0
    ## 3                                 0                    0              0
    ## 4                                 0                    0              0
    ## 5                                 0                    0              0
    ## 6                                 0                    0              0
    ##   android.intent.action.ACTION_POWER_CONNECTED BIND_VPN_SERVICE
    ## 1                                            0                0
    ## 2                                            0                0
    ## 3                                            0                0
    ## 4                                            0                0
    ## 5                                            0                0
    ## 6                                            0                0
    ##   DISABLE_KEYGUARD ACCESS_MOCK_LOCATION GET_PACKAGE_SIZE
    ## 1                0                    0                0
    ## 2                0                    0                0
    ## 3                0                    0                0
    ## 4                0                    0                0
    ## 5                0                    0                0
    ## 6                0                    0                1
    ##   MODIFY_PHONE_STATE CHANGE_COMPONENT_ENABLED_STATE CLEAR_APP_CACHE
    ## 1                  0                              0               0
    ## 2                  0                              0               0
    ## 3                  0                              0               0
    ## 4                  0                              0               0
    ## 5                  0                              0               0
    ## 6                  0                              0               0
    ##   SET_ORIENTATION READ_CONTACTS DEVICE_POWER HARDWARE_TEST
    ## 1               0             0            0             0
    ## 2               0             0            0             0
    ## 3               0             0            0             0
    ## 4               0             0            0             0
    ## 5               0             0            0             0
    ## 6               0             0            0             0
    ##   ACCESS_WIFI_STATE WRITE_EXTERNAL_STORAGE ACCESS_FINE_LOCATION
    ## 1                 0                      1                    0
    ## 2                 0                      1                    0
    ## 3                 0                      0                    0
    ## 4                 1                      1                    1
    ## 5                 1                      0                    1
    ## 6                 1                      1                    0
    ##   SET_WALLPAPER_HINTS SET_PREFERRED_APPLICATIONS WRITE_SECURE_SETTINGS
    ## 1                   0                          0                     0
    ## 2                   0                          0                     0
    ## 3                   0                          0                     0
    ## 4                   0                          0                     0
    ## 5                   0                          0                     0
    ## 6                   0                          0                     0

``` r
sprintf("Number of numerical columns: %i", ncol(numeric_columns))
```

    ## [1] "Number of numerical columns: 214"

``` r
#Create a dataframe of categorical columns
categorical_columns <- select_if(drebin_data, is.factor)
head(categorical_columns)
```

    ##   TelephonyManager.getSimCountryIso class
    ## 1                                 0     S
    ## 2                                 0     S
    ## 3                                 0     S
    ## 4                                 0     S
    ## 5                                 0     S
    ## 6                                 0     S

``` r
sprintf("Number of categorical columns: %i", ncol(categorical_columns))
```

    ## [1] "Number of categorical columns: 2"

``` r
#The frequency of different levels of each categorical column
count(categorical_columns, 'TelephonyManager.getSimCountryIso')
```

    ##   TelephonyManager.getSimCountryIso  freq
    ## 1                                 ?     5
    ## 2                                 0 12508
    ## 3                                 1  2523

``` r
count(categorical_columns, 'class')
```

    ##   class freq
    ## 1     B 9476
    ## 2     S 5560
