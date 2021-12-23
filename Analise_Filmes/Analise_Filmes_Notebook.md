Análise dos 1000 filmes mais bem avaliados
================

------------------------------------------------------------------------

# 1 - Tratando os dados

------------------------------------------------------------------------

## 1.1 - Importando

``` r
library(tidyverse)
library(forcats)
library(stringr)

df <- read_csv("imdb_top_1000.csv", show_col_types = FALSE)
```

------------------------------------------------------------------------

## 1.2 - Compreendendo

``` r
glimpse(df)
```

    ## Rows: 1,000
    ## Columns: 16
    ## $ Poster_Link   <chr> "https://m.media-amazon.com/images/M/MV5BMDFkYTc0MGEtZmN~
    ## $ Series_Title  <chr> "The Shawshank Redemption", "The Godfather", "The Dark K~
    ## $ Released_Year <chr> "1994", "1972", "2008", "1974", "1957", "2003", "1994", ~
    ## $ Certificate   <chr> "A", "A", "UA", "A", "U", "U", "A", "A", "UA", "A", "U",~
    ## $ Runtime       <chr> "142 min", "175 min", "152 min", "202 min", "96 min", "2~
    ## $ Genre         <chr> "Drama", "Crime, Drama", "Action, Crime, Drama", "Crime,~
    ## $ IMDB_Rating   <dbl> 9.3, 9.2, 9.0, 9.0, 9.0, 8.9, 8.9, 8.9, 8.8, 8.8, 8.8, 8~
    ## $ Overview      <chr> "Two imprisoned men bond over a number of years, finding~
    ## $ Meta_score    <dbl> 80, 100, 84, 90, 96, 94, 94, 94, 74, 66, 92, 82, 90, 87,~
    ## $ Director      <chr> "Frank Darabont", "Francis Ford Coppola", "Christopher N~
    ## $ Star1         <chr> "Tim Robbins", "Marlon Brando", "Christian Bale", "Al Pa~
    ## $ Star2         <chr> "Morgan Freeman", "Al Pacino", "Heath Ledger", "Robert D~
    ## $ Star3         <chr> "Bob Gunton", "James Caan", "Aaron Eckhart", "Robert Duv~
    ## $ Star4         <chr> "William Sadler", "Diane Keaton", "Michael Caine", "Dian~
    ## $ No_of_Votes   <dbl> 2343110, 1620367, 2303232, 1129952, 689845, 1642758, 182~
    ## $ Gross         <dbl> 28341469, 134966411, 534858444, 57300000, 4360000, 37784~

A base de dados contém dados dos 1000 filmes mais bem avaliados segundo
o Imdb.

Utilizando o comando glimpse podemos ter acesso às colunas da base.

-   **Poster\_Link**: Link para o poster do filme
-   **Series\_Title**: Nome do filme
-   **Released\_Year**: Ano que o filme foi lançado
-   **Certificate**: Classificação indicativa
-   **Runtime**: Duração do filme
-   **Genre**: Gêneros do filme
-   **IMDB\_Rating**: Nota de acordo com o site IMDB que leva em conta a
    avaliação popular (Crítica não especializada)
-   **Overview**: Descrição do filme
-   **Meta\_score**: Nota da crítica especializada
-   **Director**: Nome do diretor
-   **Star1-Star4**: Nome de atores principais
-   **No\_of\_Votes**: Número de votos no site IMDB para o filme
-   **Gross**: Faturamento bruto do filme

*Observando a descrição dos campos e os tipos de dados com glimpse vemos
que temos que transformar os dados para facilitar a análise*

1.  **Transformar a coluna Released\_Year de caractere para numérico.**
2.  **Transformar a coluna Runtime de caractere para numérico.**
3.  **Transformar Certificate em um fator com um número de níveis
    menores.**
4.  **Tratar a coluna Genre, pois é um caractere com vários gêneros
    separados por ,.**

------------------------------------------------------------------------

## 1.3 - Transformando

------------------------------------------------------------------------

### 1.3.1 - Released\_Year \| Caractere –&gt; Numérico

``` r
df <- df %>% mutate(Released_Year = parse_number(Released_Year))
```

    ## Warning: 1 parsing failure.
    ## row col expected actual
    ## 967  -- a number     PG

Parse\_number encontrou um erro na linha 967, pois encontrou os
caracteres “PG” que não dá para converter para número. Vamos olhar a
linha 967.

``` r
df[967,]
```

    ## # A tibble: 1 x 16
    ##   Poster_Link   Series_Title Released_Year Certificate Runtime Genre IMDB_Rating
    ##   <chr>         <chr>                <dbl> <chr>       <chr>   <chr>       <dbl>
    ## 1 https://m.me~ Apollo 13               NA U           140 min Adve~         7.6
    ## # ... with 9 more variables: Overview <chr>, Meta_score <dbl>, Director <chr>,
    ## #   Star1 <chr>, Star2 <chr>, Star3 <chr>, Star4 <chr>, No_of_Votes <dbl>,
    ## #   Gross <dbl>

O filme da linha 967 é “Apollo 13”, podemos descobrir o ano de
lançamento dele no google e consertar esse erro.

``` r
df <- df %>% mutate(Released_Year = ifelse(Released_Year=="Apollo 13", 1995, Released_Year))
```

Substituindo o dado faltante pelo ano de lançamento de “Apollo 13”
(1995), esta parte está concluída.

------------------------------------------------------------------------

### 1.3.2 - Runtime \| Caractere –&gt; Numérico

``` r
df <- df %>% mutate(Runtime = parse_number(Runtime))
```

------------------------------------------------------------------------

### 1.3.3 - Certificate \| Organizando os níveis

``` r
levels(factor(df$Certificate))
```

    ##  [1] "16"       "A"        "Approved" "G"        "GP"       "Passed"  
    ##  [7] "PG"       "PG-13"    "R"        "TV-14"    "TV-MA"    "TV-PG"   
    ## [13] "U"        "U/A"      "UA"       "Unrated"

Observando os possíveis certificados e fazendo uma pesquisa.

-   <https://en.wikipedia.org/wiki/Motion_picture_content_rating_system>
-   <https://en.wikipedia.org/wiki/Television_content_rating_system>
-   <https://en.wikipedia.org/wiki/Motion_Picture_Association_film_rating_system#From_M_to_GP_to_PG>
-   <https://en.wikipedia.org/wiki/Central_Board_of_Film_Certification>

| Desconhecido | Brasil |
|--------------|--------|
| 16           | 16+    |

| EUA    | Brasil |
|--------|--------|
| G      | L      |
| PG, GP | 10+    |
| PG-13  | 13+    |
| R      | 18+    |

| EUA TV | Brasil |
|--------|--------|
| TV-PG  | 10+    |
| TV-14  | 14+    |
| TV-MA  | 18+    |

| India   | Brasil |
|---------|--------|
| U       | L      |
| UA, U/A | 12+    |
| A       | 18+    |

| Inglaterra 1912-1932 | Brasil |
|----------------------|--------|
| Passed               | L      |
| Approved             | 16+    |

Agrupando eles em três grupos:

-   L, 12+, 16+

``` r
df <- df %>% mutate(Certificate=factor(Certificate))

df <- df %>% mutate(Certificate=fct_collapse(Certificate,
                                           "L"=c("G","PG","GP",
                                              "TV-PG","U","Passed"),"12+"=c("PG-13", "TV-14","UA","U/A"),
  "16+"=c("A","Approved","TV-MA","R","16"),NULL=c("Unrated")))

df <- df %>% mutate(Certificate=factor(Certificate,levels=c("L","12+","16+")))
```

------------------------------------------------------------------------

### 1.3.4 - Genre \| Separando

Um filme pode ter mais de um gênero, por isso a coluna Genre pode ter
até três gêneros por célula, separados por vírgula. Para filmes com mais
de um gênero, vamos separar os gêneros em linhas. Por exemplo, se um
filme é de Ação, Aventura e Sci-Fi, terá três linhas, um para cada
gênero.

Vamos criar um data frame novo para isso, pois isso irá aumentar o
número de linhas, logo dependendo de quais variáveis quisermos analisar,
será melhor usar o data frame original.

``` r
df_genero <- df %>% separate_rows(Genre, sep=",") %>%
  mutate(Genre=str_trim(Genre))
```

Por enquanto essas transformações serão suficientes, mais à frente
faremos mais a fim de analisar também os atores.

------------------------------------------------------------------------

# 2 - Analisando os dados

Vamos começar analisando a variação dentro das variáveis e depois
investigaremos a covariação entre as variáveis.

------------------------------------------------------------------------

## 2.1 - Variação

A fim de sermos suscintos não vamos analisar todas as variáveis
possíveis. Filtrando as mais importantes, analisaremos nesta ordem:

-   **IMDB\_Rating**
-   **Released\_Year**
-   **Runtime**
-   **Director**
-   **Genre**
-   **Stars**

Usaremos o data frame original (df) para todas as variáveis exceto Genre
(usaremos df\_genero) e para Stars criaremos outro data frame.

### 2.1.1 - IMDB\_Rating

Como é uma variável contínua usaremos um histograma

``` r
df %>% ggplot(aes(x=IMDB_Rating))+
  geom_histogram(binwidth=0.1)+
  labs(y="Número de Filmes")+
  geom_vline(xintercept = median(df$IMDB_Rating))+
  geom_text(aes(x=median(df$IMDB_Rating), y=160,
                label=str_c("Mediana: ",median(df$IMDB_Rating))),
            nudge_x=0.2)
```

![](Analise_Filmes_Notebook_files/figure-gfm/variacao%20IMDB%20hist-1.png)<!-- -->

A partir do rating 8.0 cada vez menos filmes se apresentam, indicando
que obter uma nota acima de 8.0 é bastante difícil. Vamos analisar o
gráfico de boxplot agora.

``` r
df %>% ggplot(aes(x="", y=IMDB_Rating))+
  geom_boxplot()+
  labs(x="")
```

![](Analise_Filmes_Notebook_files/figure-gfm/variacao%20IMDB%20boxplot-1.png)<!-- -->

Filmes acima de 8.7 são classificados como outliers, pois são raros.
Vamos ver quais filmes são esses.

``` r
df %>% filter(IMDB_Rating>=8.7) %>%
  select(Series_Title, IMDB_Rating) %>%
    arrange(desc(IMDB_Rating))
```

    ## # A tibble: 18 x 2
    ##    Series_Title                                      IMDB_Rating
    ##    <chr>                                                   <dbl>
    ##  1 The Shawshank Redemption                                  9.3
    ##  2 The Godfather                                             9.2
    ##  3 The Dark Knight                                           9  
    ##  4 The Godfather: Part II                                    9  
    ##  5 12 Angry Men                                              9  
    ##  6 The Lord of the Rings: The Return of the King             8.9
    ##  7 Pulp Fiction                                              8.9
    ##  8 Schindler's List                                          8.9
    ##  9 Inception                                                 8.8
    ## 10 Fight Club                                                8.8
    ## 11 The Lord of the Rings: The Fellowship of the Ring         8.8
    ## 12 Forrest Gump                                              8.8
    ## 13 Il buono, il brutto, il cattivo                           8.8
    ## 14 The Lord of the Rings: The Two Towers                     8.7
    ## 15 The Matrix                                                8.7
    ## 16 Goodfellas                                                8.7
    ## 17 Star Wars: Episode V - The Empire Strikes Back            8.7
    ## 18 One Flew Over the Cuckoo's Nest                           8.7

Podemos ver os dezoito filmes com notas atipicamente altas.
