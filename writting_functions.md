Writting Functions
================
Kristal Quispe
11/3/2019

## Get started

We’re going to write some functions.

Here’s z scores

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = .3) 
y = rnorm(n = 30, mean = 24, sd = 2.3)
(x - mean(x)) / sd(x)
```

    ##  [1]  0.1300388  0.3893415 -0.1298950  0.5470806 -1.1820868 -0.5286774
    ##  [7]  0.9517271 -0.1517901 -1.5593349  1.1159586  0.9425988 -0.7543857
    ## [13] -0.3685304  0.9634488  0.5111004 -0.2376186  0.7211999 -0.3919225
    ## [19] -0.4928705  2.3746912 -0.1847257  0.1022414  1.9520476  0.7218221
    ## [25] -1.2399216 -1.9047831 -0.4694358  0.4674384 -1.4821412 -0.8126158

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1] -0.20371181 -1.05808760 -0.69725921 -0.57714082 -1.50515885
    ##  [6] -0.05827348 -0.76960519  0.21665484  1.59897755  0.53216537
    ## [11] -1.33128658  0.13755512 -0.80664907 -0.71787796  0.90532303
    ## [16]  0.66426323  0.93354092  2.07208795 -0.62853678  0.03839183
    ## [21] -0.59050821 -0.01678883 -1.42909775 -0.05784745  2.07093557
    ## [26]  1.11942533 -0.03668837  1.57701142 -0.92793742 -0.45387678

Now a function.

``` r
z_score = function(x_arg) {
  
  if (!is.numeric(x_arg)) {
    stop("x should be numeric")
  } else if (length(x_arg) < 3) {
    stop("x should be longer than 3")
  } 
  
  (x_arg - mean(x_arg)) / sd(x_arg)
  
}
```

Try out the function.

``` r
z_score(x_arg = y)
```

    ##  [1]  0.32413414  0.07109176  0.18854836 -0.48682462  0.16521772
    ##  [6] -0.18289650  1.34099268  0.75157387  0.24812159  0.72391557
    ## [11]  1.61829604 -1.56506474  0.62246059 -1.90713221  0.15130030
    ## [16] -0.56215468  0.42825602  0.78160832 -1.86372946  1.31542797
    ## [21] -0.88224356  0.21335191 -0.20252004 -0.36547368  0.06251092
    ## [26] -2.29651753 -0.03157062  1.74775681 -0.82164915  0.41321223

``` r
z_score(x_arg = 3)
```

    ## Error in z_score(x_arg = 3): x should be longer than 3

``` r
z_score(x_arg = "my name is jeff")
```

    ## Error in z_score(x_arg = "my name is jeff"): x should be numeric

``` r
z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE)): x should be numeric

``` r
z_score(x_arg = iris)
```

    ## Error in z_score(x_arg = iris): x should be numeric

## Multiple outputs

``` r
mean_and_sd = function(input_x) {
  
  if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  } 
  
  list(
    mean_input = mean(input_x),
    sd_input = sd(input_x),
    z_score = (input_x - mean(input_x)) / sd(input_x)
  )
  
}
```

test this function

``` r
mean_and_sd(input_x = y)
```

    ## $mean_input
    ## [1] 24.26537
    ## 
    ## $sd_input
    ## [1] 2.2137
    ## 
    ## $z_score
    ##  [1]  0.32413414  0.07109176  0.18854836 -0.48682462  0.16521772
    ##  [6] -0.18289650  1.34099268  0.75157387  0.24812159  0.72391557
    ## [11]  1.61829604 -1.56506474  0.62246059 -1.90713221  0.15130030
    ## [16] -0.56215468  0.42825602  0.78160832 -1.86372946  1.31542797
    ## [21] -0.88224356  0.21335191 -0.20252004 -0.36547368  0.06251092
    ## [26] -2.29651753 -0.03157062  1.74775681 -0.82164915  0.41321223

## Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)
sim_data %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="writting_functions_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

``` r
sim_regression = function(n, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
  
}
sim_regression(n = 3000, beta0 = 17, beta1 = -3)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      17.0     -3.00

``` r
sim_regression(n = 14, beta0 = 24)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      24.0      2.94

## Scrape lots of napoleon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
dynamite_html = read_html(url)
review_titles = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()
review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()
review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text()
reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Now as a function

``` r
read_page_reviews = function(page_url) {
  
  dynamite_html = read_html(page_url)
  review_titles = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  reviews
    
}
```

Now i can read a lot of page reviews\! Although I’m back to
copy-and-pasting code
…

``` r
read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1")
```

    ## # A tibble: 10 x 3
    ##    title                   stars       text                                
    ##    <chr>                   <chr>       <chr>                               
    ##  1 "Awesome\n            " 5.0 out of~ Favorite movie of all time          
    ##  2 "Gotta watch it!\n    ~ 5.0 out of~ Super fun cult film. A must-see! Fu~
    ##  3 "Great movie\n        ~ 5.0 out of~ Love this movie.                    
    ##  4 "Duh\n            "     5.0 out of~ Best movie ever                     
    ##  5 "Great video\n        ~ 5.0 out of~ Product as described.  Great transa~
    ##  6 "Give me some of your ~ 5.0 out of~ This movie will always be my favori~
    ##  7 "Nostalgic\n          ~ 5.0 out of~ One of the best nostalgic movies of~
    ##  8 "Make you giggle type ~ 5.0 out of~ "I love, love, love this movie.  It~
    ##  9 "This movie is so stup~ 5.0 out of~ No, really.  It's so stupid.  Your ~
    ## 10 "Hilarious\n          ~ 5.0 out of~ Hilarious

``` r
read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2")
```

    ## # A tibble: 10 x 3
    ##    title                       stars      text                             
    ##    <chr>                       <chr>      <chr>                            
    ##  1 "Waste of money\n         ~ 1.0 out o~ Terrible movie! Please don’t was~
    ##  2 "Good movie\n            "  5.0 out o~ Funny                            
    ##  3 "A classic\n            "   5.0 out o~ I like your sleeves. They're rea~
    ##  4 "FRIKKEN SWEET MOVIE, GAWS~ 5.0 out o~ It’s Napolean Dynamite. It’s cha~
    ##  5 "You gonna eat the rest of~ 5.0 out o~ One of my favorite movies ever. ~
    ##  6 "Tina you fat lard come ge~ 5.0 out o~ It's a great movie               
    ##  7 "Great family movie\n     ~ 5.0 out o~ My kids as well as the adults lo~
    ##  8 "Teens love it\n          ~ 5.0 out o~ Original and funny               
    ##  9 "Great\n            "       5.0 out o~ Funny                            
    ## 10 "Great Movie, Bad Packagin~ 4.0 out o~ First off, the stick-on label on~

``` r
read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3")
```

    ## # A tibble: 10 x 3
    ##    title                     stars       text                              
    ##    <chr>                     <chr>       <chr>                             
    ##  1 "jeez napoleon\n        ~ 5.0 out of~ gosh                              
    ##  2 "\U0001f44d\n           ~ 5.0 out of~ "\U0001f44d"                      
    ##  3 "A classic!\n           ~ 5.0 out of~ A classic movie.  Hilarious!      
    ##  4 "A must own\n           ~ 5.0 out of~ Great movie                       
    ##  5 "If you like 80s ...you ~ 5.0 out of~ My all time favorite movie. I hav~
    ##  6 "\U0001f918\n           ~ 5.0 out of~ "\U0001f918"                      
    ##  7 "Super Slow Mooovie...\n~ 1.0 out of~ "Too slow and too damn quiet... M~
    ##  8 "Awesome!\n            "  5.0 out of~ Love this movie !                 
    ##  9 "Very funny\n           ~ 4.0 out of~ Very funny                        
    ## 10 "Eat your food tina\n   ~ 5.0 out of~ Cant go wrong

``` r
read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=4")
```

    ## # A tibble: 10 x 3
    ##    title                         stars      text                           
    ##    <chr>                         <chr>      <chr>                          
    ##  1 "Dumb funny\n            "    5.0 out o~ Dumb funny                     
    ##  2 "Annoying! Not in a good way~ 1.0 out o~ I know that I am one of the ve~
    ##  3 "Fun\n            "           5.0 out o~ Fun                            
    ##  4 "such a great movie\n       ~ 5.0 out o~ a true comedy classic          
    ##  5 "Napoleon Dud\n            "  3.0 out o~ Not impressed w/movie.         
    ##  6 "Five stars\n            "    5.0 out o~ Such a weird, awesome movie    
    ##  7 "Fun!\n            "          5.0 out o~ Great movie                    
    ##  8 "Funny movie- bravo for Amaz~ 5.0 out o~ My son loves this movie, so I ~
    ##  9 "Movie\n            "         5.0 out o~ Movie                          
    ## 10 "Funny movie, quotable lines~ 5.0 out o~ My kids quote this movie all t~

## Scoping

Mean example …

``` r
f = function(x) {
  z = x + y
  z
}
x = 1
y = 2
f(x = 2)
```

    ## [1] 4
