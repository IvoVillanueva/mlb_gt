
# libreries ---------------------------------------------------------------



library(tidyverse)
library(mlbstatsR)
library(gt)
library(ggtext)

# data table from mlb.com-------
tribble (~pos, ~name,
         "C", "Buster Posey",
         "1B", "Freddie Freeman",
         "2B", "Adam Frazier",
         "3B", "Nolan Arenado",
         "SS", "Fernando Tatís Jr.",
         "OF", "Ronald Acuña Jr.",
         "OF", "Nick Castellanos",
         "OF", "Jesse Winker",
         "C", "J.T. Realmuto",
         "C", "Yadier Molina",
         "C", "Omar Narváez",
         "2B", "Ozzie Albies",
         "3B", "Kris Bryant",
         "SS", "Brandon Crawford",
         "2B", "Jake Cronenworth",
         "3B", "Eduardo Escobar",
         "3B", "Justin Turner",
         "3B", "Manny Machado",
         "1B", "Max Muncy",
         "SS", "Trea Turner",
         "OF", "Mookie Betts",
         "OF", "Bryan Reynolds",
         "OF", "Kyle Schwarber",
         "OF", "Juan Soto",
         "OF", "Chris Taylor") -> na
na$num <- (1:25)

# extract standard batting stats mlbstatsR funtion changing names  puting some respect on their names------------------------------------------

playersnaBat <- get_reference_players_mlb() %>%
  mutate(name = case_when(name== "Ronald Acuna Jr."~ "Ronald Acuña Jr.",
                          name== "Fernando Tatis Jr."~"Fernando Tatís Jr.",
                          name== "Omar Narvaez" ~ "Omar Narváez",
                          TRUE~ name))

# join data table with stats ----------------------------------------------


playersnaBat <- left_join(na, playersnaBat) %>%
  select(pos, "name", "age", "tm", "ab", "hr", "rbi", "x2b", "x3b", "r", "obp", "slg", "sb") %>%
  mutate_at(c("age","ab", "hr", "rbi", "x2b", "x3b", "r", "obp", "slg", "sb"), as.numeric)


# extract logos png mlbstatsR funtion -------------------------------------



teams <- get_png_logos() %>%
  select(tm=team_nickname, logo = logologodefault)%>%
  mutate(tm = case_when(
    tm == "SF" ~ "SFG",
    tm == "SD" ~ "SDP",
    tm == "WSH" ~ "WSN",
    TRUE ~ tm))

# join table with logos ---------------------------------------------------

playersnaBat <- left_join(playersnaBat, teams)


# extract headshots players changing names  puting some respect on their names-----------------------------------------------

player <- get_mlb_players()%>%
  select(tm = team, name = espn_nombres, cabezas )%>%
  mutate(name = case_when(name== "Ronald Acuna Jr."~ "Ronald Acuña Jr.",
                          name== "Fernando Tatis Jr."~"Fernando Tatís Jr.",
                          name== "Omar Narvaez" ~ "Omar Narváez",
                          TRUE~ name))

# join the data -----------------------------------------------------------



playersnaBat <- left_join(playersnaBat, player)

# Code for Name/Team/age/logo Combo------

combine_word <- function(name, tm, age, logo){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{name}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;'><span style ='font-weight:bold;color:grey;font-size:10px'><img src='{logo}'
    style='width:20px; height:20px;vertical-align:middle'> {tm} | {age} Yr </span></div>"
  )
}


# table code --------------------------------------------------------------



playersnaBat %>%  select(pos, cabezas, name, logo, tm, everything()) %>%
  mutate(
    combo = combine_word(name, tm, age, logo),
    combo = map(combo, gt::html)

  ) %>% select(cabezas, combo, pos, ab, hr, rbi, x2b, x3b, r, obp, slg, sb) %>%
  gt()  %>%
  cols_label(
    cabezas = "",
    combo = gt::html("<span style='font-weight:bold;font-size:12px'>PLAYER</span>"),
    pos = gt::html("<span style='font-weight:bold;font-size:12px'>POSITION</span>"),
    ab = gt::html("<span style='font-weight:bold;font-size:12px'>AB</span>"),
    hr = gt::html("<span style='font-weight:bold;font-size:12px'>HR</span>"),
    rbi = gt::html("<span style='font-weight:bold;font-size:12px'>RBI</span>"),
    x2b = gt::html("<span style='font-weight:bold;font-size:12px'>2B</span>"),
    x3b = gt::html("<span style='font-weight:bold;font-size:12.7px'>3B</span>"),
    r = gt::html("<span style='font-weight:bold;font-size:12px'>R</span>"),
    obp = gt::html("<span style='font-weight:bold;font-size:12px'>OBP</span>"),
    slg = gt::html("<span style='font-weight:bold;font-size:12px'>SLG</span>"),
    sb = gt::html("<span style='font-weight:bold;font-size:12px'>SB</span>"),

  ) %>%

  tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/en/thumb/d/d4/MLB_National_League_logo.svg/400px-MLB_National_League_logo.svg.png' style='height:50px;'><br>NATIONAL LEAGUE"),
    subtitle = md(paste0(" MLB All-Star Game Selected Starters | A ",  format(Sys.Date(), format= "%d %B, %Y")))


  ) %>%
  text_transform(
    locations = cells_body(c(cabezas)),
    fn = function(x) {
      web_image(url = x,
                height = px(26.5))
    }
  ) %>%
  fmt_percent(
    columns = c(obp, slg),
    decimals = 1
  )  %>%

  cols_align(
    align = "center",
    columns = c(pos:sb)
  ) %>%
  tab_row_group(
    label = md("<span style='font-weight:bold;font-size:10px'>Reserves</span>"),
    rows = 10:25
  ) %>%
  tab_row_group(
    label = md("<span style='font-weight:bold;font-size:10px'>Selected Starters</span>"),
    rows = 1:9
  ) %>%

  tab_options(
    table.background.color = "#f4f4f4",
    column_labels.font.size =12,
    table.font.size =11,
    heading.title.font.size  = 22.652,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 12,
    table.font.names = "Chivo",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(1),
    footnotes.font.size = 10,
    source_notes.font.size = 10,
    footnotes.padding = px(1)
  ) %>%
  tab_source_note(
    source_note = md( "<div><b>Grafica por</b> : <i>\n Ivo Villanueva<i>
                       <div><b>Datos por</b> : \n<i>mlbstatsR y @baseball_ref<i>")) %>%
  gtsave("nat.html")



# pitchers ----------------------------------------------------------------
tribble(~pos, ~name,
        "RHP", "Corbin Burnes",
        "RHP", "Yu Darvish",
        "RHP", "Jacob deGrom",
        "RHP", "Kevin Gausman",
        "RHP", "Germán Márquez",
        "LHP", "Trevor Rogers",
        "RHP", "Zack Wheeler",
        "RHP", "Brandon Woodruff",
        "RHP", "Walker Buehler",
        "RHP", "Max Scherzer",
        "RHP", "Freddy Peralta",
        "RHP", "Taijuan Walker",
        "LHP", "Josh Hader",
        "RHP", "Craig Kimbrel",
        "RHP", "Mark Melancon",
        "RHP", "Alex Reyes")->np

np$row <- (1:16)

playernsp <- get_reference_players_mlb(stats = "pitching")
playernsp1 <- get_reference_players_mlb(stats = "pitching", type = "battingagainst") %>% select(name, ba)

playernsp <- left_join( playernsp, playernsp1) %>%
  mutate(name=case_when(name=="German Marquez"~"Germán Márquez",
                        TRUE~name))
playernsp <- left_join(np, playernsp)

playernsp <- playernsp %>% select(pos, name, age, tm, w, l, era, ip, bb, so, hr, ba) %>%
  mutate(record = paste0(w,"-", l )) %>%
  mutate_at(c("age", "era", "ip", "bb", "so", "hr", "ba"), as.numeric)

teams <- get_png_logos() %>%
  select(tm=team_nickname, logo = logologodefault) %>%
  mutate(tm = case_when(
    tm == "SF" ~ "SFG",
    tm == "SD" ~ "SDP",
    tm == "WSH" ~ "WSN",
    TRUE ~ tm))

playernsp <- left_join(playernsp, teams)

player <- get_mlb_players()%>%
  select(tm = team, name = espn_nombres, cabezas )%>%
  mutate(name=case_when(name=="German Marquez"~"Germán Márquez",
                        TRUE~name))
playernsp <- left_join(playernsp, player)


combine_word <- function(name, tm, age, logo){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{name}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;'><span style ='font-weight:bold;color:grey;font-size:10px'><img src='{logo}'
    style='width:20px; height:20px;vertical-align:middle'> {tm} | {age} Yr </span></div>"
  )
}



playernsp %>%  select(pos, cabezas, name, logo, tm, everything()) %>%
  mutate(
    combo = combine_word(name, tm, age, logo),
    combo = map(combo, gt::html)

  ) %>% select( cabezas, combo, pos, record, era, ip, bb, so, hr, ba) %>%
  gt()  %>%
  cols_label(
    cabezas = "",
    combo = gt::html("<span style='font-weight:bold;font-size:12px'>PLAYER</span>"),
    pos = gt::html("<span style='font-weight:bold;font-size:12px'>POSITION</span>"),
    record = gt::html("<span style='font-weight:bold;font-size:12px'>RECORD</span>"),
    era = gt::html("<span style='font-weight:bold;font-size:12px'>ERA</span>"),
    ip = gt::html("<span style='font-weight:bold;font-size:12px'>INNINGS</span>"),
    bb = gt::html("<span style='font-weight:bold;font-size:12px'>WALKS</span>"),
    so = gt::html("<span style='font-weight:bold;font-size:12.7px'>SO</span>"),
    hr = gt::html("<span style='font-weight:bold;font-size:12px'>HR</span>"),
    ba = gt::html("<span style='font-weight:bold;font-size:12px'>OPP AVG</span>"),

  ) %>%

  tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/en/thumb/d/d4/MLB_National_League_logo.svg/400px-MLB_National_League_logo.svg.png' style='height:50px;'><br>NATIONAL LEAGUE"),
    subtitle = md(paste0(" MLB All-Star Game Pitchers Roster | A ",  format(Sys.Date(), format= "%d %B, %Y")))


  ) %>%
  text_transform(
    locations = cells_body(c(cabezas)),
    fn = function(x) {
      web_image(url = x,
                height = px(26.5))
    }
  ) %>%
  fmt_percent(
    columns = c(ba),
    decimals = 1
  )  %>%



  cols_align(
    align = "center",
    columns = c(pos:ba)
  ) %>%

  tab_row_group(
    label = md("<span style='font-weight:bold;font-size:10px'>Relievers</span>"),
    rows = 13:16
  ) %>%
  tab_row_group(
    label = md("<span style='font-weight:bold;font-size:10px'>Starting Pitchers</span>"),
    rows = 1:12
  ) %>%


  tab_options(
    table.background.color = "#f4f4f4",
    column_labels.font.size =12,
    table.font.size =11,
    heading.title.font.size  = 22.652,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 12,
    table.font.names = "Chivo",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(1),
    footnotes.font.size = 10,
    source_notes.font.size = 10,
    footnotes.padding = px(1)
  ) %>%
  tab_source_note(
    source_note = md( "<div><b>Grafica por</b> : <i>\n Ivo Villanueva<i>
                       <div><b>Datos por</b> : \n<i>mlbstatsR y @baseball_ref<i>")) %>%
  gtsave("natp.html")


