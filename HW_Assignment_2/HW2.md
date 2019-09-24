HW02
================
Jenna Loesberg
9/23/2019

Instructions are provided
[here](https://stat545.stat.ubc.ca/evaluation/hw02/hw02/)

Loading packages:

``` r
library(gapminder)
library(tidyverse)
```

# Exercise 1

## 1.1

Using filter() to get data for 3 countries in 1970’s

``` r
gapminder %>%
  filter(country == "United States" | country == "Norway" | country == "New Zealand") %>%
  filter(year >= 1970 & year <= 1979)
```

    ## # A tibble: 6 x 6
    ##   country       continent  year lifeExp       pop gdpPercap
    ##   <fct>         <fct>     <int>   <dbl>     <int>     <dbl>
    ## 1 New Zealand   Oceania    1972    71.9   2929100    16046.
    ## 2 New Zealand   Oceania    1977    72.2   3164900    16234.
    ## 3 Norway        Europe     1972    74.3   3933004    18965.
    ## 4 Norway        Europe     1977    75.4   4043205    23311.
    ## 5 United States Americas   1972    71.3 209896000    21806.
    ## 6 United States Americas   1977    73.4 220239000    24073.

## 1.2

using pipe operator %\>% to select country and gdpPercap

``` r
gapminder %>%
  filter(country == "United States" | country == "Norway" | country == "New Zealand") %>%
  filter(year >= 1970 & year <= 1979) %>% 
  select(country, gdpPercap)
```

    ## # A tibble: 6 x 2
    ##   country       gdpPercap
    ##   <fct>             <dbl>
    ## 1 New Zealand      16046.
    ## 2 New Zealand      16234.
    ## 3 Norway           18965.
    ## 4 Norway           23311.
    ## 5 United States    21806.
    ## 6 United States    24073.

## 1.3

Filtering for drops in life expectancy (LE) and adding new variable for
change in LE (I’m filtering for drops in LE, so these will be negative)

``` r
(gapminder2 <- 
   gapminder %>% 
   arrange(year) %>% 
   group_by(country) %>% 
   mutate(LE_change = gdpPercap - lag(gdpPercap)) %>% 
   filter(LE_change < 0))
```

    ## # A tibble: 332 x 7
    ## # Groups:   country [115]
    ##    country              continent  year lifeExp     pop gdpPercap LE_change
    ##    <fct>                <fct>     <int>   <dbl>   <int>     <dbl>     <dbl>
    ##  1 Bangladesh           Asia       1957    39.3  5.14e7      662.     -22.6
    ##  2 Benin                Africa     1957    40.4  1.93e6      960.    -103. 
    ##  3 Bolivia              Americas   1957    41.9  3.21e6     2128.    -550. 
    ##  4 Haiti                Americas   1957    40.7  3.51e6     1727.    -113. 
    ##  5 Malaysia             Asia       1957    52.1  7.74e6     1810.     -21.1
    ##  6 Morocco              Africa     1957    45.4  1.14e7     1642.     -46.2
    ##  7 Sao Tome and Princi~ Africa     1957    48.9  6.13e4      861.     -18.8
    ##  8 Sri Lanka            Asia       1957    61.5  9.13e6     1073.     -11.0
    ##  9 Tanzania             Africa     1957    43.0  9.45e6      699.     -18.1
    ## 10 Tunisia              Africa     1957    47.1  3.95e6     1395.     -73.2
    ## # ... with 322 more rows

## 1.4

Showing max GDP per capita experienced by each country:

``` r
gapminder %>%
  group_by(country) %>% 
  filter(gdpPercap == max(gdpPercap))
```

    ## # A tibble: 142 x 6
    ## # Groups:   country [142]
    ##    country     continent  year lifeExp       pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>     <int>     <dbl>
    ##  1 Afghanistan Asia       1982    39.9  12881816      978.
    ##  2 Albania     Europe     2007    76.4   3600523     5937.
    ##  3 Algeria     Africa     2007    72.3  33333216     6223.
    ##  4 Angola      Africa     1967    36.0   5247469     5523.
    ##  5 Argentina   Americas   2007    75.3  40301927    12779.
    ##  6 Australia   Oceania    2007    81.2  20434176    34435.
    ##  7 Austria     Europe     2007    79.8   8199783    36126.
    ##  8 Bahrain     Asia       2007    75.6    708573    29796.
    ##  9 Bangladesh  Asia       2007    64.1 150448339     1391.
    ## 10 Belgium     Europe     2007    79.4  10392226    33693.
    ## # ... with 132 more rows

## 1.5

Making scatterplot of log transformed GDP and life expectancy in Canada

``` r
gapminder %>%
  filter(country == "Canada") %>% 
  ggplot(aes(gdpPercap, lifeExp))+
  geom_point()+
  scale_x_log10()+
  xlab("log(GDP per capita)")+
  ylab("Life Expectancy (years)")
```

![](HW2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Exercise 2

### Exploring individual variables with dyplr

Categorical: **continent** Quantitative: **lifeExp**

## 2.1

What are possible values (or range, whichever is appropriate) of each
variable?

``` r
summary(gapminder)
```

    ##         country        continent        year         lifeExp     
    ##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
    ##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
    ##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
    ##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
    ##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
    ##  Australia  :  12                  Max.   :2007   Max.   :82.60  
    ##  (Other)    :1632                                                
    ##       pop              gdpPercap       
    ##  Min.   :6.001e+04   Min.   :   241.2  
    ##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
    ##  Median :7.024e+06   Median :  3531.8  
    ##  Mean   :2.960e+07   Mean   :  7215.3  
    ##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
    ##  Max.   :1.319e+09   Max.   :113523.1  
    ## 

#### Life Expectancy:

Ranges from 23.60 to 82.60; Mean is 59.47… see summary table

Frequency of variables are skewed to the right…

``` r
hist(gapminder$lifeExp)
```

![](HW2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Continent:

5 categories, but that summary table is misleading because it includes
all of the years that data was collected. So, I’m isolating it to one
year:

``` r
(gapminder3 <- 
  gapminder %>% 
  filter(year == "2002"))
```

    ## # A tibble: 142 x 6
    ##    country     continent  year lifeExp       pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>     <int>     <dbl>
    ##  1 Afghanistan Asia       2002    42.1  25268405      727.
    ##  2 Albania     Europe     2002    75.7   3508512     4604.
    ##  3 Algeria     Africa     2002    71.0  31287142     5288.
    ##  4 Angola      Africa     2002    41.0  10866106     2773.
    ##  5 Argentina   Americas   2002    74.3  38331121     8798.
    ##  6 Australia   Oceania    2002    80.4  19546792    30688.
    ##  7 Austria     Europe     2002    79.0   8148312    32418.
    ##  8 Bahrain     Asia       2002    74.8    656397    23404.
    ##  9 Bangladesh  Asia       2002    62.0 135656790     1136.
    ## 10 Belgium     Europe     2002    78.3  10311970    30486.
    ## # ... with 132 more rows

Now the real summary…

``` r
summary(gapminder3)
```

    ##         country       continent       year         lifeExp     
    ##  Afghanistan:  1   Africa  :52   Min.   :2002   Min.   :39.19  
    ##  Albania    :  1   Americas:25   1st Qu.:2002   1st Qu.:55.52  
    ##  Algeria    :  1   Asia    :33   Median :2002   Median :70.83  
    ##  Angola     :  1   Europe  :30   Mean   :2002   Mean   :65.69  
    ##  Argentina  :  1   Oceania : 2   3rd Qu.:2002   3rd Qu.:75.46  
    ##  Australia  :  1                 Max.   :2002   Max.   :82.00  
    ##  (Other)    :136                                               
    ##       pop              gdpPercap      
    ##  Min.   :1.704e+05   Min.   :  241.2  
    ##  1st Qu.:4.174e+06   1st Qu.: 1409.6  
    ##  Median :1.037e+07   Median : 5319.8  
    ##  Mean   :4.146e+07   Mean   : 9917.9  
    ##  3rd Qu.:2.655e+07   3rd Qu.:13359.5  
    ##  Max.   :1.280e+09   Max.   :44684.0  
    ## 

Here we see the number of countries from each continent.

# Exercise 3

### Making graphs

## 3.1

A scatterplot of 2 quantitative variables:

comparing population and GDP in China…does gdp increase as population
increases? This spans from 1952 to 2007

``` r
gapminder %>% 
  filter(country == "China") %>%
  ggplot(aes(pop, gdpPercap))+
    geom_point()+
    xlab("Population")+
    ylab("GDP per capita")
```

![](HW2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## 3.2

A boxplot of life expectancy per continent in 2002:

``` r
gapminder %>% 
  filter(year == "2002") %>%
  ggplot(aes(continent, lifeExp))+
  geom_boxplot()+
  xlab("Continent")+
  ylab("Life expectancy (years)")
```

![](HW2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
