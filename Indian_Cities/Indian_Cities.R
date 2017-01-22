
indian_cities <- read.csv('cities_r2.csv')
attach(indian_cities)


state.wise <- indian_cities[,list(sum(population_total),sum(population_male),sum(population_female),sum(total_graduates),sum(literates_total),sum(literates_male),sum(literates_female),sum(male_graduates),sum(female_graduates)),by = 'state_name']


indian_cities$state_name




