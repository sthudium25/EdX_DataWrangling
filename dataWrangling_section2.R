glimpse(co2)
co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
co2_tidy <- co2_wide %>% 
  gather("month", "co2", -year) 

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

head(dat)

dat %>%
  spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

df1 <- data.frame(x = c("a", "b"), y = c("a", "a"))
df1
df2 <- data.frame(x = c("a", "a"), y = c("a", "b"))

union(df1, df2)
setdiff(df2, df1)
intersect(df1, df2)

install.packages("Lahman")
library(Lahman)


top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>%
  left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names, by = "playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary)

head(top_salary)

df <- AwardsPlayers %>%
  filter( yearID == 2016)

df$playerID %>%
  intersect(top_names$playerID)

df$playerID %>%
  setdiff(top_names$playerID) %>%
  length()


## Web scraping section
install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")

prep_time <- h %>% html_node(".o-RecipeInfo__m-Time") %>% html_node(".o-RecipeInfo__a-Description") %>% html_text
prep_time

## Assessment questions
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_table(nodes[[4]])
html_table(nodes[[8]])
html_table(nodes[[3]])

tab1 <- html_table(nodes[[10]], header = T)
tab2 <- html_table((nodes[[19]]), header = T)
tab1 <- tab1[,-1]

head(tab1)

tab1 %>% full_join(tab2, by = "Team")

d <- read_html("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054")

tab <- html_nodes(d, "table")
tab

class(tab)

html_table(tab[[5]], fill = T) %>% glimpse()


