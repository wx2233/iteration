iteration\_and\_listcols
================
Weijia Xiong
10/29/2019

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)

l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
mean(l$vec_numeric)
```

    ## [1] 6.5

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[[1]][1:3]
```

    ## [1] 5 6 7

``` r
l[[2]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
df = list(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

df 
```

    ## $a
    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840
    ##  [8] 1.087654 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036
    ## [15] 3.350910 2.825453 2.408572 1.665973 1.902701 5.036104
    ## 
    ## $b
    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
    ##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
    ## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
    ## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464
    ## 
    ## $c
    ##  [1] 10.094098 10.055644  9.804419  9.814683 10.383954 10.176256 10.148416
    ##  [8] 10.029515 10.097078 10.030371 10.008400 10.044684  9.797907 10.480244
    ## [15] 10.160392  9.949758 10.242578  9.874548 10.342232  9.921125
    ## 
    ## $d
    ##  [1] -5.321491 -1.635881 -1.867771 -3.774316 -4.410375 -4.834528 -3.269014
    ##  [8] -4.833929 -3.814468 -2.836428 -2.144481 -3.819963 -3.123603 -2.745052
    ## [15] -1.281074 -3.958544 -4.604310 -4.845609 -2.444263 -3.060119

``` r
is.list(df)
```

    ## [1] TRUE

## For loop

``` r
df = tibble(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

df 
```

    ## # A tibble: 20 x 4
    ##        a      b     c     d
    ##    <dbl>  <dbl> <dbl> <dbl>
    ##  1 3.77   1.21   9.98 -2.31
    ##  2 2.86  -5.66   9.91 -2.61
    ##  3 3.39   7.45   9.86 -4.31
    ##  4 3.22  -1.24   9.89 -1.78
    ##  5 3.02   0.918 10.2  -2.20
    ##  6 2.38   2.02   9.75 -3.49
    ##  7 4.26  -4.97   9.96 -3.90
    ##  8 2.59  -5.43   9.51 -3.39
    ##  9 3.67  -0.243  9.87 -2.19
    ## 10 3.16   2.88   9.90 -3.56
    ## 11 4.78   0.369 10.3  -4.87
    ## 12 3.71   3.53   9.81 -3.14
    ## 13 2.66   1.67   9.83 -2.23
    ## 14 2.99   2.73   9.72 -4.16
    ## 15 2.87  -7.01  10.0  -3.24
    ## 16 0.909  3.39  10.2  -4.22
    ## 17 4.70  -3.95   9.76 -2.88
    ## 18 4.06  -2.33   9.91 -3.13
    ## 19 2.23  -0.524 10.3  -3.03
    ## 20 3.38  -8.24   9.86 -3.62

``` r
is.list(df)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
mean_and_sd(df[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.23 0.897

``` r
mean_and_sd(df[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.672  4.11

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.93 0.205

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.21 0.832

``` r
output = vector("list", length = 4)
```

Write our first for loop\!

``` r
for (i in 1:4) {
  
  output[[i]] = mean_and_sd(df[[i]])
  
}
output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.23 0.897
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.672  4.11
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.93 0.205
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.21 0.832

## Map

``` r
output = map(df, mean_and_sd)
output_median = map(df, median)
output_summary = map(df, summary)
output_median = map_dbl(df, median)
output = map_dfr(df, mean_and_sd)
output = map(df, ~mean_and_sd(.x))
```

## Napoleon\!\!

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_urls = str_c(url_base, 1:5) #Join multiple strings into a single string.

vec_urls
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
    ## [2] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"
    ## [3] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"
    ## [4] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=4"
    ## [5] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
```

``` r
output = vector("list",length = 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}
```

    ## Warning: `data_frame()` is deprecated, use `tibble()`.
    ## This warning is displayed once per session.

``` r
output = map(vec_urls,read_page_reviews)

output
```

    ## [[1]]
    ## # A tibble: 10 x 3
    ##    title                               stars text                          
    ##    <chr>                               <dbl> <chr>                         
    ##  1 "Gotta watch it!\n            "         5 Format: Prime VideoVerified P…
    ##  2 "Great movie\n            "             5 Format: Blu-rayVerified Purch…
    ##  3 "Duh\n            "                     5 Format: Prime VideoVerified P…
    ##  4 "Great video\n            "             5 Format: DVDVerified Purchase  
    ##  5 "Give me some of your tots\n      …     5 Format: Prime VideoVerified P…
    ##  6 "Nostalgic\n            "               5 Format: Prime VideoVerified P…
    ##  7 "Make you giggle type movie\n     …     5 Format: Blu-rayVerified Purch…
    ##  8 "This movie is so stupid.\n       …     5 Format: Prime VideoVerified P…
    ##  9 "Hilarious\n            "               5 Format: Prime VideoVerified P…
    ## 10 "Waste of money\n            "          1 Format: Prime VideoVerified P…
    ## 
    ## [[2]]
    ## # A tibble: 10 x 3
    ##    title                                   stars text                      
    ##    <chr>                                   <dbl> <chr>                     
    ##  1 "Good movie\n            "                  5 Format: Prime VideoVerifi…
    ##  2 "A classic\n            "                   5 Format: Prime VideoVerifi…
    ##  3 "FRIKKEN SWEET MOVIE, GAWSH.\n        …     5 Format: Prime VideoVerifi…
    ##  4 "You gonna eat the rest of your tots?\…     5 Format: Prime VideoVerifi…
    ##  5 "Tina you fat lard come get some dinne…     5 Format: Prime VideoVerifi…
    ##  6 "Great family movie\n            "          5 Format: Blu-rayVerified P…
    ##  7 "Teens love it\n            "               5 Format: Prime VideoVerifi…
    ##  8 "Great\n            "                       5 Format: DVDVerified Purch…
    ##  9 "Great Movie, Bad Packaging\n         …     4 Format: Blu-rayVerified P…
    ## 10 "jeez napoleon\n            "               5 Format: Prime VideoVerifi…
    ## 
    ## [[3]]
    ## # A tibble: 10 x 3
    ##    title                                  stars text                       
    ##    <chr>                                  <dbl> <chr>                      
    ##  1 "👍\n            "                         5 Format: Prime VideoVerifie…
    ##  2 "A classic!\n            "                 5 Format: DVDVerified Purcha…
    ##  3 "A must own\n            "                 5 Format: Prime VideoVerifie…
    ##  4 "If you like 80s ...you must watch\n …     5 Format: Prime VideoVerifie…
    ##  5 "🤘\n            "                         5 Format: Prime VideoVerifie…
    ##  6 "Super Slow Mooovie...\n            "      1 Format: Prime VideoVerifie…
    ##  7 "Awesome!\n            "                   5 Format: Prime VideoVerifie…
    ##  8 "Very funny\n            "                 4 Format: Prime VideoVerifie…
    ##  9 "Eat your food tina\n            "         5 Format: Prime VideoVerifie…
    ## 10 "Dumb funny\n            "                 5 Format: DVDVerified Purcha…
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    title                                     stars text                    
    ##    <chr>                                     <dbl> <chr>                   
    ##  1 "Annoying! Not in a good way.\n         …     1 Format: Prime VideoVeri…
    ##  2 "Fun\n            "                           5 Format: DVDVerified Pur…
    ##  3 "such a great movie\n            "            5 Format: UMD for PSPVeri…
    ##  4 "Napoleon Dud\n            "                  3 Format: DVDVerified Pur…
    ##  5 "Five stars\n            "                    5 Format: Prime VideoVeri…
    ##  6 "Fun!\n            "                          5 Format: Prime VideoVeri…
    ##  7 "Funny movie- bravo for Amazon’s wide se…     5 Format: Prime VideoVeri…
    ##  8 "Movie\n            "                         5 Format: Prime VideoVeri…
    ##  9 "Funny movie, quotable lines\n          …     5 Format: Blu-rayVerified…
    ## 10 "Great for teenagers!\n            "          5 Format: Prime VideoVeri…
    ## 
    ## [[5]]
    ## # A tibble: 10 x 3
    ##    title                                  stars text                       
    ##    <chr>                                  <dbl> <chr>                      
    ##  1 "can't believe we fell for this\n    …     1 Format: Prime VideoVerifie…
    ##  2 "shut up tina you fat lard.\n        …     5 Format: Prime VideoVerifie…
    ##  3 "Laughter is the Best Medicine. 💯\n  …     5 Format: Prime VideoVerifie…
    ##  4 "New condition\n            "              5 Format: DVDVerified Purcha…
    ##  5 "Napoleon, give me some of your tots\…     5 Format: Prime VideoVerifie…
    ##  6 "Yes rent\n            "                   5 Format: Prime VideoVerifie…
    ##  7 "Cult classic.\n            "              5 Format: DVDVerified Purcha…
    ##  8 "DIDN'T WORK\n            "                1 Format: Prime VideoVerifie…
    ##  9 "I\n            "                          5 Format: Blu-rayVerified Pu…
    ## 10 "Laugh out loud\n            "             5 Format: Prime VideoVerifie…

## Nest

``` r
weather = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2016-01-01",
    date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'crul':
    ##   method                 from
    ##   as.character.form_file httr

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## file path:          /Users/osukuma/Library/Caches/rnoaa/ghcnd/USW00094728.dly

    ## file last updated:  2019-09-03 10:46:14

    ## file min/max dates: 1869-01-01 / 2019-08-31

    ## file path:          /Users/osukuma/Library/Caches/rnoaa/ghcnd/USC00519397.dly

    ## file last updated:  2019-09-03 10:46:22

    ## file min/max dates: 1965-01-01 / 2019-08-31

    ## file path:          /Users/osukuma/Library/Caches/rnoaa/ghcnd/USS0023B17S.dly

    ## file last updated:  2019-09-03 10:46:25

    ## file min/max dates: 1999-09-01 / 2019-08-31

``` r
weather_nest = 
  nest(weather, data = date:tmin)  # used nest by specifying a column range to collapse within remaining variable values

weather_nest
```

    ## # A tibble: 3 x 3
    ##   name           id                    data
    ##   <chr>          <chr>       <list<df[,4]>>
    ## 1 CentralPark_NY USW00094728      [366 × 4]
    ## 2 Waikiki_HA     USC00519397      [366 × 4]
    ## 3 Waterhole_WA   USS0023B17S      [366 × 4]

Is the list column really a list???

``` r
weather_nest %>% pull(name)  # pull reserve the order.
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## <list_of<
    ##   tbl_df<
    ##     date: date
    ##     prcp: double
    ##     tmax: double
    ##     tmin: double
    ##   >
    ## >[3]>
    ## [[1]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows
    ## 
    ## [[2]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0  29.4  16.7
    ##  2 2016-01-02     0  28.3  16.7
    ##  3 2016-01-03     0  28.3  16.7
    ##  4 2016-01-04     0  28.3  16.1
    ##  5 2016-01-05     0  27.2  16.7
    ##  6 2016-01-06     0  27.2  20  
    ##  7 2016-01-07    46  27.8  18.3
    ##  8 2016-01-08     3  28.3  17.8
    ##  9 2016-01-09     8  27.8  19.4
    ## 10 2016-01-10     3  28.3  18.3
    ## # … with 356 more rows
    ## 
    ## [[3]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   1.7  -5.9
    ##  2 2016-01-02    25  -0.1  -6  
    ##  3 2016-01-03     0  -5   -10  
    ##  4 2016-01-04    25   0.3  -9.8
    ##  5 2016-01-05    25   1.9  -1.8
    ##  6 2016-01-06    25   1.4  -2.6
    ##  7 2016-01-07     0   1.4  -3.9
    ##  8 2016-01-08     0   1.1  -4  
    ##  9 2016-01-09     0   1.4  -4.5
    ## 10 2016-01-10     0   2.3  -3.8
    ## # … with 356 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows

``` r
weather_nest %>% 
  unnest()
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(data)`

    ## # A tibble: 1,098 x 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2016-01-01     0   5.6   1.1
    ##  2 CentralPark_NY USW00094728 2016-01-02     0   4.4   0  
    ##  3 CentralPark_NY USW00094728 2016-01-03     0   7.2   1.7
    ##  4 CentralPark_NY USW00094728 2016-01-04     0   2.2  -9.9
    ##  5 CentralPark_NY USW00094728 2016-01-05     0  -1.6 -11.6
    ##  6 CentralPark_NY USW00094728 2016-01-06     0   5    -3.8
    ##  7 CentralPark_NY USW00094728 2016-01-07     0   7.8  -0.5
    ##  8 CentralPark_NY USW00094728 2016-01-08     0   7.8  -0.5
    ##  9 CentralPark_NY USW00094728 2016-01-09     0   8.3   4.4
    ## 10 CentralPark_NY USW00094728 2016-01-10   457  15     4.4
    ## # … with 1,088 more rows

## Operations on list columns

Can I do useful things with a list column?

``` r
central_park_df = weather_nest$data[[1]]

lm(tmax ~ tmin, data = weather_nest$data[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326

``` r
lm(tmax ~ tmin, data = weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

try a loop:

``` r
output = vector("list",length = 3)

for (i in 1:3 ) {
  output[[i]] = lm(tmax ~ tmin, weather_nest$data[[i]])
}

output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
## function 
weather_lm = function(df) { 
  lm(tmax ~ tmin, data = df)
}
```

loop

``` r
for (i in 1:3) { 
  output[[i]] = weather_lm(weather_nest$data[[i]])
}

output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

map statement :

``` r
output = map(weather_nest$data,weather_lm)
output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
## add linear models in nest data
weather_nest %>% 
  mutate(lin_models = map(data,weather_lm))
```

    ## # A tibble: 3 x 4
    ##   name           id                    data lin_models
    ##   <chr>          <chr>       <list<df[,4]>> <list>    
    ## 1 CentralPark_NY USW00094728      [366 × 4] <lm>      
    ## 2 Waikiki_HA     USC00519397      [366 × 4] <lm>      
    ## 3 Waterhole_WA   USS0023B17S      [366 × 4] <lm>

## Revisit napaleon … again

``` r
napaleon =
  tibble(
    page = 1:5,
    urls = str_c(url_base,page)
  ) %>% 
  mutate(
    reviews = map(urls,read_page_reviews)
  ) %>% 
  unnest(reviews) %>% 
  select(-urls)

napaleon
```

    ## # A tibble: 50 x 4
    ##     page title                            stars text                       
    ##    <int> <chr>                            <dbl> <chr>                      
    ##  1     1 "Gotta watch it!\n            "      5 Format: Prime VideoVerifie…
    ##  2     1 "Great movie\n            "          5 Format: Blu-rayVerified Pu…
    ##  3     1 "Duh\n            "                  5 Format: Prime VideoVerifie…
    ##  4     1 "Great video\n            "          5 Format: DVDVerified Purcha…
    ##  5     1 "Give me some of your tots\n   …     5 Format: Prime VideoVerifie…
    ##  6     1 "Nostalgic\n            "            5 Format: Prime VideoVerifie…
    ##  7     1 "Make you giggle type movie\n  …     5 Format: Blu-rayVerified Pu…
    ##  8     1 "This movie is so stupid.\n    …     5 Format: Prime VideoVerifie…
    ##  9     1 "Hilarious\n            "            5 Format: Prime VideoVerifie…
    ## 10     1 "Waste of money\n            "       1 Format: Prime VideoVerifie…
    ## # … with 40 more rows

``` r
dynamite_reviews = 
  tibble(page = 1:5,
         urls = str_c(url_base, page)) %>% 
  mutate(reviews = map(urls, read_page_reviews)) %>% 
  unnest()
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(reviews)`

``` r
lotr_cell_ranges = 
  tibble(
    movie = c("fellowship_ring", "two_towers", "return_king"),
    cells = c("B3:D6", "F3:H6", "J3:L6")
  )

lotr_tidy = 
  lotr_cell_ranges %>% 
  mutate(
    word_data = map(cells, ~readxl::read_excel("./data/LotR_Words.xlsx", range = .x))
  ) %>%  ## nest map 
  unnest(cols = word_data) %>%  ##展开
  janitor::clean_names() %>% 
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") %>%
  mutate(race = str_to_lower(race)) %>% 
  select(movie, everything(), -cells)

lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459
