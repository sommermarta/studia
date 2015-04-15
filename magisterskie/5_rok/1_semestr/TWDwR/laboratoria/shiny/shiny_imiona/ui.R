library(shiny)

shinyUI(fluidPage(
  titlePanel("Rzadkie imiona na przestrzeni dziesięciu lat (2004-2014)"),
  sidebarLayout(
    sidebarPanel(
    
     selectInput("imionaa", "Wybierz imie", 
                  choices = list("Aaron", "Abigail", "Adina", "Adriana", "Aldona", "Alma", "Alwin", 
                                   "Amnezja", "Anatol", "Anatolia", "Angela", "Angelina", "Anika", 
                                   "Annika", "Anton", "Antonia", "Antoniusz", "Apolinary", "Apoloniusz", 
                                   "Ariadna", "Ariana", "Arleta", "Armand", "Arnold", "Arseniusz", 
                                   "Augustyn", "Aura", "Aurora", "Bakary", "Balbina", "Barabasz", 
                                   "Barta", "Bastian", "Bazyli", "Belzebub", "Benedykt", "Benita", 
                                   "Berenike", "Bernadetta", "Bibianna", "Bogna", "Bogumił", "Bogumiła", 
                                   "Bogusław", "Bogusława", "Bogusz", "Bolesław", "Brajan", "Brian", 
                                   "Bronisław", "Bronisława", "Brygida", "Calineczka", "Carmen", 
                                   "Caspian", "Cecylia", "Chaim", "Chelsea", "Chiara", "Chloe", 
                                   "Cyntia", "Cypinia", "Dacjan", "Dagna", "Daida", "Dalia", "Daniela", 
                                   "Debora", "Delfina", "Denis", "Dionizy", "Dobrawa", "Dobromir", 
                                   "Dobrosława", "Donald", "Dymitr", "Edgar", "Edmund", "Edwin", 
                                   "Eleonora", "Eliasz", "Eliot", "Eljasz", "Elmira", "Elvis", "Elwira", 
                                   "Emanuel", "Emanuela", "Emilian", "Eryka", "Esmee", "Eugenia", 
                                   "Eunika", "Faustyna", "Felicjan", "Felicyta", "Ferdynand", "Filipina", 
                                   "Filomena", "Florian", "Fontanna", "Franciszek Józef", "Franciszka", 
                                   "Franczeska", "Frida", "Gabor", "Gaspar", "Gaweł", "Gerard", 
                                   "Gloria", "Gniewko", "Gniewosz", "Gotard", "Gracja", "Gracjana", 
                                   "Grażyna", "Greta", "Gryzelda", "Guantanamera", "Gwen", "Gwidon", 
                                   "Hadrian", "Hana", "Hektor", "Herbert", "Herman", "Hiacynta", 
                                   "Hieronim", "Hipolit", "Honorata", "Horacy", "Idalia", "Idzi", 
                                   "Ilia", "Ilian", "Ilona", "Imre", "Inetta", "Ingrid", "Ireneusz", 
                                   "Irma", "Irmina", "Ivar", "Ivo", "Izaak", "Jaga", "Jan Józef", 
                                   "Jan Paweł", "Jana", "Janko", "Jano", "Jaromir", "Jarowit", 
                                   "Jarzyna", "Jasmina", "Jerzy Michał", "Jeżyna", "Jonata", "Jordan", 
                                   "Jowita", "Józefina", "Jurand", "Justyn", "Kalista", "Karen", 
                                   "Karena", "Karin", "Karla", "Kasandra", "Kasjan", "Kasjana", 
                                   "Kasper", "Kayla", "Kazimiera", "Kevin", "Kiara", "Kira", "Klaudiusz", 
                                   "Klaudyna", "Klemens", "Kleofas", "Kolin", "Kora", "Kordian", 
                                   "Korina", "Korneliusz", "Krzesimir", "Krzysztofa", "Laila", "Lambert", 
                                   "Lana", "Lara", "Lars", "Larysa", "Leila", "Leja", "Leokadia", 
                                   "Leonardo", "Leonia", "Letycja", "Levente", "Lew", "Lewin", "Lili", 
                                   "Lilla", "Lilli", "Lisa", "Lorena", "Lotta", "Lubomir", "Łucjan", 
                                   "Lucjusz", "Ludwika", "Lukrecja", "Luna", "Magnolia", "Makary", 
                                   "Maksymilianna", "Manuel", "Marcela", "Marcella", "Marcianna", 
                                   "Marcjanna", "Marcus", "Margarita", "Maria Antonina", "Marian", 
                                   "Marietta", "Marina", "Mariola", "Marisa", "Marletta", "Martin", 
                                   "Maryla", "Maryna", "Marzena", "Masza", "Maura", "Melchior", 
                                   "Melisa", "Mercedes", "Mieczysław", "Mika", "Mikaela", "Mił", 
                                   "Miła", "Milada", "Milla", "Milo", "Mira", "Miranda", "Mirella", 
                                   "Miriam", "Mirosław", "Mirosława", "Morfeusz", "Myszon", "Nadzieja", 
                                   "Naomi", "Napoleon", "Nastazja", "Nawojka", "Nazar", "Nelly", 
                                   "Nestor", "Nika", "Nike", "Nikita", "Nikolas", "Nikole", "Nikoletta", 
                                   "Nikolina", "Niunia", "Noam", "Noe", "Norman", "Odo", "Odolan", 
                                   "Oksana", "Oktawia", "Oleg", "Olena", "Orest", "Oriana", "Oswald", 
                                   "Paloma", "Pamela", "Pascal", "Patrycjusz", "Paul", "Petronela", 
                                   "Rachel", "Rachela", "Radomił", "Radosz", "Rajmund", "Ramona", 
                                   "Raul", "Rebeka", "Regina", "Renata", "Rodion", "Roger", "Roland", 
                                   "Romeo", "Romuald", "Romualda", "Ronald", "Roxana", "Rudolf", 
                                   "Rufus", "Rupert", "Ruth", "Safira", "Salomea", "Samira", "Sasha", 
                                   "Saturnin", "Scott", "Selena", "Selina", "Selma", "Serafin", 
                                   "Sindi", "Sławomir", "Sofia", "Sylas", "Symeon", "Szarlota", 
                                   "Szczepan", "Telimena", "Teo", "Teofil", "Theo", "Thorgal", "Tito", 
                                   "Torkil", "Toro", "Tradycja", "Tristan", "Tycjan", "Uljana", 
                                   "Viktoria", "Viorika", "Viviana", "Waldemar", "Waleria", "Walerian", 
                                   "Walter", "Wasyl", "Wawrzyniec", "Wera", "Wida", "Wiera", "Wiesława", 
                                   "Wilhelm", "Wiliam", "Wisenna", "Wit", "Witomir", "Władysław", 
                                   "Włodzimierz", "Wolfgang", "Wolter", "Xawier", "Xenia", "Zachariasz", 
                                   "Zachary", "Żaklina", "Żaneta", "Zara", "Zeira", "Zenon", "Ziemosław", 
                                   "Zoe", "Zola", "Zoya", "Żyraf", "Zyta"
                  ),
                  selected="Zyta",multiple = TRUE)      
     
    ),
    
    mainPanel(
      plotOutput("errorbarPlot")
    )
  )
))