data <- read.csv("incarceration_trends.csv")

library("dplyr")
library("ggplot2")
library("maps")
library("tidyverse")

#Summary Information------------------------------------------------------------

#What is the largest jail population in different race group in the U.S.?

total_jail_pop <- data %>% 
  filter(year == max(year)) %>%
  summarize(total_aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>% 
  mutate(year = "2018")
  

high_jail_pop <- total_jail_pop %>% 
  pull(max(total_aapi_jail_pop, total_black_jail_pop, total_latinx_jail_pop, 
           total_native_jail_pop, total_white_jail_pop))

rounded_jail_pop <- round(high_jail_pop, digits = 0)
#What is the highest jail proportion in a race's population nationwide?

total_race_pop <- data %>% 
  filter(year == max(year)) %>%
  summarize(aapi_pop = sum(aapi_pop_15to64, na.rm = TRUE),
            black_pop = sum(black_pop_15to64, na.rm = TRUE),
            latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE),
            native_pop = sum(native_pop_15to64, na.rm = TRUE),
            white_pop = sum(white_pop_15to64, na.rm = TRUE)) %>%
  mutate(year = "2018")

jail_total_pop <- left_join(total_jail_pop, total_race_pop, by = "year")

jail_total_prop <- jail_total_pop %>% 
  summarize(aapi_jail_ratio = total_aapi_jail_pop / aapi_pop * 100,
            black_jail_ratio = total_black_jail_pop / black_pop * 100,
            latinx_jail_ratio = total_latinx_jail_pop / latinx_pop * 100,
            native_jail_ratio = total_native_jail_pop / native_pop * 100,
            white_jail_ratio = total_white_jail_pop / white_pop * 100)

high_jail_race_ratio <- pmax(jail_total_prop$aapi_jail_ratio,
                             jail_total_prop$black_jail_ratio,
                             jail_total_prop$latinx_jail_ratio,
                             jail_total_prop$native_jail_ratio,
                             jail_total_prop$white_jail_ratio)

rounded_jail_ratio <- round(high_jail_race_ratio, 2)
  
#What does this proportion in this race group compared to it ten years ago?

total_jail_pop_2008 <- data %>% 
  filter(year == "2008") %>%
  summarize(total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  mutate(year = "2008")

total_race_pop_2008 <- data %>% 
  filter(year == "2008") %>%
  summarize(black_pop = sum(black_pop_15to64, na.rm = TRUE)) %>%
  mutate(year = "2008")

jail_total_pop_2008 <- left_join(total_jail_pop_2008, 
                                 total_race_pop_2008, by = "year")

high_jail_race_ratio_2008 <- jail_total_pop_2008 %>% 
  summarize(black_jail_ratio = total_black_jail_pop / black_pop * 100) %>%
  pull(black_jail_ratio)

rounded_jail_ratio_2008 <- round(high_jail_race_ratio_2008, 2)

ten_yr_compare <- round(high_jail_race_ratio_2008 - high_jail_race_ratio,
                        digits = 2)

#Which state has the highest jail population in 2018?

total_jail_pop_state <- data %>% filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) 

highest_jail_pop_state <- total_jail_pop_state %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% pull(state)

#Which county in that state has the highest jail population in 2018?

total_jail_pop_county <- data %>% filter(year == max(year), state == "CA") %>%
  summarize(total_jail_pop, county_name) %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

#Chart 1------------------------------------------------------------------------

#ratio of California jail population over total race population in different race group in ten years
cali_race_pop <- data %>% 
  filter(year < 2019, year > 2008, state == "CA") %>%
  group_by(year) %>% 
  summarize(aapi_pop = sum(aapi_pop_15to64, na.rm = TRUE),
            black_pop = sum(black_pop_15to64, na.rm = TRUE),
            latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE),
            native_pop = sum(native_pop_15to64, na.rm = TRUE),
            white_pop = sum(white_pop_15to64, na.rm = TRUE))

cali_race_jail_pop <- data %>%
  filter(year < 2019, year > 2008, state == "CA") %>%
  group_by(year) %>%
  summarize(total_aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE))

cali_jail_race_pop <- left_join(cali_race_pop, cali_race_jail_pop, by = "year")

cali_jail_race_ratio <- cali_jail_race_pop %>% 
  summarize(year,
            aapi_jail_ratio = total_aapi_jail_pop / aapi_pop *100,
            black_jail_ratio = total_black_jail_pop / black_pop * 100,
            latinx_jail_ratio = total_latinx_jail_pop / latinx_pop * 100,
            native_jail_ratio = total_native_jail_pop / native_pop * 100,
            white_jail_ratio = total_white_jail_pop / white_pop * 100)

line_chart_table <- 
  data.frame(race = rep(c("AAPI", "Black", "Latinx", "Native", "White"), 
                        each=10),
             year = cali_jail_race_ratio$year,
             percentage = c(cali_jail_race_ratio$aapi_jail_ratio, 
                            cali_jail_race_ratio$black_jail_ratio,
                            cali_jail_race_ratio$latinx_jail_ratio, 
                            cali_jail_race_ratio$native_jail_ratio,
                            cali_jail_race_ratio$white_jail_ratio))

line_chart <- ggplot(line_chart_table, aes(x=year, y= percentage, group=race, 
                                           color = race)) +
  geom_line() +
  ggtitle("Change in jail population ratio in California over ten years") +
  theme(plot.title = element_text(hjust = 0.5))

line_chart

#Chart 2------------------------------------------------------------------------

#Race composition in jail in different counties.
jail_race_ratio <- data %>% 
  mutate(aapi_ratio = aapi_jail_pop / total_jail_pop * 100,
         black_ratio = black_jail_pop / total_jail_pop * 100,
         latinx_ratio = latinx_jail_pop / total_jail_pop * 100,
         native_ratio = native_jail_pop / total_jail_pop * 100,
         white_ratio = white_jail_pop / total_jail_pop * 100) %>%
  select(year, state, county_name, aapi_ratio, black_ratio, latinx_ratio, 
         native_ratio, white_ratio)

#ratio of jail population over total population in different race group
race_ratio <- data %>% 
  mutate(aapi_pop_ratio = aapi_jail_pop / aapi_pop_15to64 * 100,
         black_pop_ratio = black_jail_pop / black_pop_15to64 * 100,
         latinx_pop_ratio = latinx_jail_pop / latinx_pop_15to64 * 100,
         native_pop_ratio = native_jail_pop / native_pop_15to64 * 100,
         white_pop_ratio = white_jail_pop / white_pop_15to64 * 100) %>%
  select(year, state, county_name, aapi_pop_ratio, black_pop_ratio, 
         latinx_pop_ratio, native_pop_ratio, white_pop_ratio)


#ratio of California jail population in different race group over total jail population  in ten years
cali_total_race_pop <- data %>% 
  filter(year < 2019, year > 2008, state == "CA") %>%
  group_by(year) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            total_aapi_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_black_pop = sum(black_jail_pop, na.rm = TRUE),
            total_latinx_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_pop = sum(white_jail_pop, na.rm = TRUE))

cali_race_ratio <- cali_total_race_pop %>% 
  summarize(year,
            aapi_pop_ratio = total_aapi_pop / total_jail_pop *100,
            black_pop_ratio = total_black_pop / total_jail_pop * 100,
            latinx_pop_ratio = total_latinx_pop / total_jail_pop * 100,
            native_pop_ratio = total_native_pop / total_jail_pop * 100,
            white_pop_ratio = total_white_pop / total_jail_pop * 100)

table <- data.frame(race = rep(c("AAPI", "Black", "Latinx", "Native", "White"), 
                               each=10),
                    year = cali_race_ratio$year,
                    percentage = c(cali_race_ratio$aapi_pop_ratio, 
                                   cali_race_ratio$black_pop_ratio,
                                   cali_race_ratio$latinx_pop_ratio, 
                                   cali_race_ratio$native_pop_ratio,
                                   cali_race_ratio$white_pop_ratio))

p1 <- ggplot(table, aes(fill=race, y=percentage, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Race composition in California jail over ten years") +
  theme(plot.title = element_text(hjust = 0.5))

p1

#Chart 3------------------------------------------------------------------------

#Distribution of black jail population in California
latest_data <- data %>% filter(year == max(year))

county_data <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_data %>% left_join(latest_data, by = "fips") %>%
  filter(state == "CA")

black_jail_pop_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white",
    size = .1
  ) + coord_map() +
  ggtitle("Distribution of black jail population in California in 2018")
black_jail_pop_map 

