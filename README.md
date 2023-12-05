
## Fishvice

The concept is to turn objects from stock assessment packages (sam,
flr’s, muppet, …) to tibbles for further downstream tidyverse
processing. A case in point:

``` r
library(magrittr)
stockassessment::fitfromweb("BW-2021") %>% 
  fishvice::fv_rbx()
```

    ## $rby
    ## # A tibble: 205 × 5
    ##     year     est     low     high variable
    ##    <int>   <dbl>   <dbl>    <dbl> <chr>   
    ##  1     1 786026. 563271. 1096875. catch   
    ##  2     2 544001. 413221.  716170. catch   
    ##  3     3 511286. 394907.  661961. catch   
    ##  4     4 560913. 432749.  727035. catch   
    ##  5     5 637584. 500137.  812804. catch   
    ##  6     6 759594. 596217.  967739. catch   
    ##  7     7 638131. 501148.  812557. catch   
    ##  8     8 569422. 447815.  724051. catch   
    ##  9     9 619197. 490191.  782154. catch   
    ## 10    10 553363. 435299.  703448. catch   
    ## # ℹ 195 more rows
    ## 
    ## $rbya
    ## # A tibble: 4,920 × 4
    ##     year   age variable      val
    ##    <dbl> <dbl> <chr>       <dbl>
    ##  1  1981     1 oC        258000.
    ##  2  1981     2 oC        348000 
    ##  3  1981     3 oC        681000 
    ##  4  1981     4 oC        334000.
    ##  5  1981     5 oC        548000.
    ##  6  1981     6 oC        559000.
    ##  7  1981     7 oC        466000 
    ##  8  1981     8 oC        634000 
    ##  9  1981     9 oC        578000 
    ## 10  1981    10 oC       1460000.
    ## # ℹ 4,910 more rows
    ## 
    ## $opr
    ## # A tibble: 554 × 5
    ##     year   age fleet     o     p
    ##    <dbl> <dbl> <chr> <dbl> <dbl>
    ##  1  1981     1 catch  12.5  12.5
    ##  2  1981     2 catch  12.8  12.8
    ##  3  1981     3 catch  13.4  13.5
    ##  4  1981     4 catch  12.7  12.8
    ##  5  1981     5 catch  13.2  13.2
    ##  6  1981     6 catch  13.2  13.2
    ##  7  1981     7 catch  13.1  13.0
    ##  8  1981     8 catch  13.4  13.3
    ##  9  1981     9 catch  13.3  13.0
    ## 10  1981    10 catch  14.2  13.9
    ## # ℹ 544 more rows
    ## 
    ## $par
    ## # A tibble: 51 × 11
    ##    fleet   age     m    cv       est       low      high what   in_name out_name
    ##    <chr> <int> <dbl> <dbl>     <dbl>     <dbl>     <dbl> <chr>  <chr>   <chr>   
    ##  1 IBWSS     1 -9.66 0.204 0.0000641 0.0000426 0.0000964 catch… keyLog… logFpar 
    ##  2 IBWSS     2 -9.08 0.118 0.000114  0.0000901 0.000145  catch… keyLog… logFpar 
    ##  3 IBWSS     3 -7.91 0.123 0.000367  0.000287  0.000469  catch… keyLog… logFpar 
    ##  4 IBWSS     4 -7.31 0.111 0.000672  0.000538  0.000839  catch… keyLog… logFpar 
    ##  5 IBWSS     5 -7.02 0.105 0.000890  0.000721  0.00110   catch… keyLog… logFpar 
    ##  6 IBWSS     6 -7.02 0.105 0.000890  0.000721  0.00110   catch… keyLog… logFpar 
    ##  7 IBWSS     7 -7.02 0.105 0.000890  0.000721  0.00110   catch… keyLog… logFpar 
    ##  8 IBWSS     8 -7.02 0.105 0.000890  0.000721  0.00110   catch… keyLog… logFpar 
    ##  9 catch     1 -1.01 0.135 0.363     0.277     0.476     rwalkF keyVarF logSdLo…
    ## 10 catch     2 -1.01 0.135 0.363     0.277     0.476     rwalkF keyVarF logSdLo…
    ## # ℹ 41 more rows
    ## # ℹ 1 more variable: fleetage <chr>
