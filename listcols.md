Iteration and List Columns
================
Kristal Quispe
11/3/2019

## This is gonna be so great

``` r
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
#each variable is diff type and has different number of rox/elements
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l[[2]] #pulls out second thing in list, in this case the matrix
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
mean(l$vec_numeric)
```

    ## [1] 6.5

``` r
df = list( #can be organized as a tibble, but we are more concerner with working on list right now
  a = rnorm(20, 3, 1), # length of varible, mean, SD
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)
df$a
```

    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840
    ##  [8] 1.087654 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036
    ## [15] 3.350910 2.825453 2.408572 1.665973 1.902701 5.036104

``` r
df[[2]]
```

    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
    ##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
    ## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
    ## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x) #function we are using
  sd_x = sd(x)
  
  tibble( #output
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
mean_and_sd(df[[1]]) #MEAN AND sd OF OBJECT a
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

``` r
mean_and_sd(df[[2]])#object b mean and sd, and so on
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
#df is the input, what you need to create is something to store the output of your iteratiion
output = vector("list", length = 4)
```

Write our first for loop\!

``` r
for (i in 1:4) {
 #save output into correct output slot 
  output[[i]] = mean_and_sd(df[[i]])
 #for ever iteration (i) in 1-4, you wil calculate the mean and SD 
}


output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
output = map(df, mean_and_sd) #CHANGE TO MAP STATEMENT, OUTPUT EQUALS MAP (INPUT, FUNCTION YOU CARE ABOUT). This is instead of the above. This is a fucntoin you mdae
output_median = map(df, median) #function part of R

output_summary = map(df, summary)#different function in R

output_median = map_dbl(df, median)#map_dbl, you dont have to map and spit out  a list, you can map_dbl and instead of a list it will spit out a vector of all of the things you are intererted in

output = map_dfr(df, mean_and_sd) #you format output to ahve all four out put dfs in one table. Map_df joins the output rowwise. 
output = map(df, ~mean_and_sd(.x)) #the . is a placeholder, the ~is explicit of what s being mapped and how it is behaving. This shoudlnt change the result shoudl just make it more explicit. 
```

## Napoleon\!\!

``` r
read_page_reviews = function(url) {
  #given a particularl url, is going to read in the html from that website, and then its going to pull out nodes, text, etc and put it into a data file
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
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
#here you will create a url base, and then add using the str_C function a 1-5 add end of url
vec_urls = str_c(url_base, 1:5)
vec_urls
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
    ## [2] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"
    ## [3] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"
    ## [4] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=4"
    ## [5] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

``` r
#to extract the data, you could write the following code for how many data pages there are

read_page_reviews(vec_urls[[1]])
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                    
    ##    <chr>                     <dbl> <chr>                                   
    ##  1 "Good comedy\n          ~     4 "Not as funny as I remember years ago. ~
    ##  2 "Awesome\n            "       5 "Favorite movie of all time\n          ~
    ##  3 "yes\n            "           5 "good\n            "                    
    ##  4 "Gotta watch it!\n      ~     5 "Super fun cult film. A must-see! Funni~
    ##  5 "Great movie\n          ~     5 "Love this movie.\n            "        
    ##  6 "Duh\n            "           5 "Best movie ever\n            "         
    ##  7 "Great video\n          ~     5 "Product as described.  Great transacti~
    ##  8 "Give me some of your to~     5 "This movie will always be my favorite ~
    ##  9 "Nostalgic\n            "     5 "One of the best nostalgic movies of my~
    ## 10 "Make you giggle type mo~     5 "I love, love, love this movie.  It mak~

``` r
read_page_reviews(vec_urls[[2]])
```

    ## # A tibble: 10 x 3
    ##    title                         stars text                                
    ##    <chr>                         <dbl> <chr>                               
    ##  1 "This movie is so stupid.\n ~     5 "No, really.  It's so stupid.  Your~
    ##  2 "Hilarious\n            "         5 "Hilarious\n            "           
    ##  3 "Waste of money\n           ~     1 "Terrible movie! Please don’t waste~
    ##  4 "Good movie\n            "        5 "Funny\n            "               
    ##  5 "A classic\n            "         5 "I like your sleeves. They're real ~
    ##  6 "FRIKKEN SWEET MOVIE, GAWSH.~     5 "It’s Napolean Dynamite. It’s charm~
    ##  7 "You gonna eat the rest of y~     5 "One of my favorite movies ever.  Y~
    ##  8 "Tina you fat lard come get ~     5 "It's a great movie\n            "  
    ##  9 "Great family movie\n       ~     5 "My kids as well as the adults love~
    ## 10 "Teens love it\n            "     5 "Original and funny\n            "

``` r
read_page_reviews(vec_urls[[3]])
```

    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "Great\n            "           5 "Funny\n            "                 
    ##  2 "Great Movie, Bad Packagin~     4 "First off, the stick-on label on the~
    ##  3 "jeez napoleon\n          ~     5 "gosh\n            "                  
    ##  4 "\U0001f44d\n            "      5 "\U0001f44d\n            "            
    ##  5 "A classic!\n            "      5 "A classic movie.  Hilarious!\n      ~
    ##  6 "A must own\n            "      5 "Great movie\n            "           
    ##  7 "If you like 80s ...you mu~     5 "My all time favorite movie. I have w~
    ##  8 "\U0001f918\n            "      5 "\U0001f918\n            "            
    ##  9 "Super Slow Mooovie...\n  ~     1 "Too slow and too damn quiet... My gi~
    ## 10 "Awesome!\n            "        5 "Love this movie !\n            "

``` r
read_page_reviews(vec_urls[[4]])
```

    ## # A tibble: 10 x 3
    ##    title                           stars text                              
    ##    <chr>                           <dbl> <chr>                             
    ##  1 "Very funny\n            "          4 "Very funny\n            "        
    ##  2 "Eat your food tina\n         ~     5 "Cant go wrong\n            "     
    ##  3 "Dumb funny\n            "          5 "Dumb funny\n            "        
    ##  4 "Annoying! Not in a good way.\~     1 "I know that I am one of the very~
    ##  5 "Fun\n            "                 5 "Fun\n            "               
    ##  6 "such a great movie\n         ~     5 "a true comedy classic\n         ~
    ##  7 "Napoleon Dud\n            "        3 "Not impressed w/movie.\n        ~
    ##  8 "Five stars\n            "          5 "Such a weird, awesome movie\n   ~
    ##  9 "Fun!\n            "                5 "Great movie\n            "       
    ## 10 "Funny movie- bravo for Amazon~     5 "My son loves this movie, so I wa~

``` r
read_page_reviews(vec_urls[[5]])
```

    ## # A tibble: 10 x 3
    ##    title                          stars text                               
    ##    <chr>                          <dbl> <chr>                              
    ##  1 "Movie\n            "              5 "Movie\n            "              
    ##  2 "Funny movie, quotable lines\~     5 "My kids quote this movie all the ~
    ##  3 "Great for teenagers!\n      ~     5 "My students loved this movie.\n  ~
    ##  4 "can't believe we fell for th~     1 "a pretty lame movie--can't believ~
    ##  5 "shut up tina you fat lard.\n~     5 "i LOVE napoleon.\n            "   
    ##  6 "Laughter is the Best Medicin~     5 "FAST SHIPPING! Love this Movie! L~
    ##  7 "New condition\n            "      5 "Classic for the kids to watch.\n ~
    ##  8 "Napoleon, give me some of yo~     5 "Cul\n            "                
    ##  9 "Yes rent\n            "           5 "Always an amazing movie, classic!~
    ## 10 "Cult classic.\n            "      5 "I should’ve bought this movie a l~

``` r
#Or you could do the following

#this is the four loop
output = vector("list", length = 5)
for (i in 1:5) {
  
  output[[i]] = read_page_reviews(vec_urls[[i]])
  
}
#this is the map statement which you can do instead of the four loop
output = map(vec_urls, read_page_reviews)
```

## list columns / weather df

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

    ## file path:          C:\Users\kriqu\AppData\Local\rnoaa\rnoaa\Cache/ghcnd/USW00094728.dly

    ## file last updated:  2019-10-02 16:18:47

    ## file min/max dates: 1869-01-01 / 2019-09-30

    ## file path:          C:\Users\kriqu\AppData\Local\rnoaa\rnoaa\Cache/ghcnd/USC00519397.dly

    ## file last updated:  2019-10-02 16:19:05

    ## file min/max dates: 1965-01-01 / 2019-09-30

    ## file path:          C:\Users\kriqu\AppData\Local\rnoaa\rnoaa\Cache/ghcnd/USS0023B17S.dly

    ## file last updated:  2019-10-02 16:19:13

    ## file min/max dates: 1999-09-01 / 2019-09-30

nest within station

``` r
weather_nest =
  weather %>% 
  nest(data = date:tmin)#here you are nesting columns date through tmin (aka date, prcp, tmax, and tmin) inside of the other variables (name/ID)

#SO THE NESTED colums becomes one varible and it is a data frame and variable type is a list

#So what do you do with list columns/variables?
```

is the list column really a list??

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data) #here you pull out the three lists
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
    ## # ... with 356 more rows
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
    ## # ... with 356 more rows
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
    ## # ... with 356 more rows

``` r
weather_nest$data[[1]] #here you pull out the first list
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
    ## # ... with 356 more rows

``` r
weather_nest %>% 
  unnest() # this unnests the lists
```

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
    ## # ... with 1,088 more rows

## Operations on list columns

can I do useful things with a list column…?

``` r
central_park_df = weather_nest$data[[1]]
lm(tmax ~ tmin, data = central_park_df) #here you fit a linera regression model between tmax and tmin 
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = central_park_df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
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

try a loop …

``` r
output = vector("list", length = 3)
for (i in 1:3) {
  
  output[[i]] = lm(tmax ~ tmin, weather_nest$data[[i]])
  
}
```

Make a function for the Liner regression model

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
```

New four loop

``` r
for (i in 1:3) {
  
  output[[i]] = weather_lm(weather_nest$data[[i]])
  
}
output = map(weather_nest$data, weather_lm)
```

``` r
weather_nest %>% 
  mutate(lin_models = map(data, weather_lm)) %>% #here you are adding the lin models colum, by doing something to the other columns. You have created 3 seperate linear modesl for each of the data frames. 
  select(-data) %>% 
  filter(name != "CentralPark_NY")
```

    ## # A tibble: 2 x 3
    ##   name         id          lin_models
    ##   <chr>        <chr>       <list>    
    ## 1 Waikiki_HA   USC00519397 <lm>      
    ## 2 Waterhole_WA USS0023B17S <lm>

## Revisit napoleon … again

``` r
napoleon = 
  tibble(
    page = 1:5,
    urls = str_c(url_base, page)
  ) %>% 
  mutate(
    reviews = map(urls, read_page_reviews)
  ) %>% 
  unnest(reviews) %>% 
  select(-urls)
```
