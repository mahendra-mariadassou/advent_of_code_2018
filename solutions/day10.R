library(tidyverse)

test <- c("position=< 9,  1> velocity=< 0,  2>",
          "position=< 7,  0> velocity=<-1,  0>",
          "position=< 3, -2> velocity=<-1,  1>",
          "position=< 6, 10> velocity=<-2, -1>",
          "position=< 2, -4> velocity=< 2,  2>",
          "position=<-6, 10> velocity=< 2, -2>",
          "position=< 1,  8> velocity=< 1, -1>",
          "position=< 1,  7> velocity=< 1,  0>",
          "position=<-3, 11> velocity=< 1, -2>",
          "position=< 7,  6> velocity=<-1, -1>",
          "position=<-2,  3> velocity=< 1,  0>",
          "position=<-4,  3> velocity=< 2,  0>",
          "position=<10, -3> velocity=<-1,  1>",
          "position=< 5, 11> velocity=< 1, -2>",
          "position=< 4,  7> velocity=< 0, -1>",
          "position=< 8, -2> velocity=< 0,  1>",
          "position=<15,  0> velocity=<-2,  0>",
          "position=< 1,  6> velocity=< 1,  0>",
          "position=< 8,  9> velocity=< 0, -1>",
          "position=< 3,  3> velocity=<-1,  1>",
          "position=< 0,  5> velocity=< 0, -1>",
          "position=<-2,  2> velocity=< 2,  0>",
          "position=< 5, -2> velocity=< 1,  2>",
          "position=< 1,  4> velocity=< 2,  1>",
          "position=<-2,  7> velocity=< 2, -2>",
          "position=< 3,  6> velocity=<-1, -1>",
          "position=< 5,  0> velocity=< 1,  0>",
          "position=<-6,  0> velocity=< 2,  0>",
          "position=< 5,  9> velocity=< 1, -2>",
          "position=<14,  7> velocity=<-2,  0>",
          "position=<-3,  6> velocity=< 2, -1>")

input <- read_lines(file = "input/day10.txt")

format_input <- function(x) { 
  tibble(raw = x) %>% mutate(position = str_remove_all(raw, pattern = 'position=<|> velocity=.*|[ ]'), 
                             velocity = str_remove_all(raw, pattern = '.*velocity=<|>|[ ]'), 
                             raw = NULL) %>% 
    separate(col = position, into = c("pos.x", "pos.y"), sep = ",") %>%
    separate(col = velocity, into = c("vel.x", "vel.y"), sep = ",") %>% 
    mutate_at(.vars = vars(pos.x:vel.y), as.integer)
}

compute_density <- function(x, time.span = 1:10) {
  format_input(x) %>% 
  crossing(time = time.span) %>% 
  group_by(time) %>%
  transmute(x = pos.x + time * vel.x, 
            y = pos.y + time * vel.y) %>%
  nest() %>% 
  mutate(x.scope = map_int(data, ~ diff(range(.x$x))), 
         y.scope = map_int(data, ~ diff(range(.x$y))), 
         density = map_int(data, nrow) / (x.scope * y.scope))
}

## plotdata <- compute_density(test)
plotdata <- compute_density(input, time = 9000:11000)
ggplot(plotdata, aes(x = time, y = density)) + geom_line()

index <- plotdata %>% pull(density) %>% which.max()
time <- plotdata %>% arrange(desc(density)) %>% pull(time) %>% first()

ggplot(plotdata %>% pull(data) %>% `[[`(index), aes(x = x, y = y)) + 
  geom_tile() + 
  coord_equal() + 
  theme_minimal()
