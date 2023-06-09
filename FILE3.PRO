/*
    Фамилия : Аристид
    Имя : Жан Лоэнс Аристобуль Надаль
    Группа : НКНбд-02-21
*/



%DOMAINS
    жанр("Science-fiction").
    жанр("Biography").
    жанр("Horror").
    жанр("Action").
%PREDICATES
кинотеатр(id_кинотеатр, название, адрес, телефон, количествоMест).
кинофильм(id_кинотеатр, название, годВыпуска, режиссер, жанр).
показывают(id_кинотеатр, id_кинофильм, дата, время, выручка).

%CLAUSES 
кинотеатр(1, "Cinema Secret", "Moscou, Home 15" , 84954780638, 300).
кинотеатр(2, "Cinema Pioner", "Moscou, Home 19", 83648383875, 450).
кинотеатр(3, "Illusion Cinema", "Moscou, Home 56", 84858475942, 500).
кинотеатр(4, "Moscow Cinema", "Moscou, Home 32", 89848473284,600).
кинотеатр(5, "Cinema Aurora", "St Petersbourg Home 7", 87882424545, 145).
кинотеатр(6, "Neva", "St Petersbourg Home 20", 8768986778, 110).


кинофильм(1,"Moonfall",2022,"Roland Emmerich", жанр("Science-fiction")).
кинофильм(2,"Archive",2020,"Cora Palfrey", жанр("Science-Fiction")).
кинофильм(3,"A Beautiful Mind",2001,"Ron Howard", жанр("Biography")).
кинофильм(4,"The Theory of Everything",2014,"James Marsh", жанр("Biography")).
кинофильм(5,"The Imitation Game",2014,"Morten Tyldum", жанр("Biography")).
кинофильм(6,"Codebreaker",2011,"Clare Beaven", жанр("Biography")).
кинофильм(7,"The Code Conspiracy",2002, жанр("Action")).
кинофильм(8,"Ghosted",2023,"Dexter Fletcher", жанр("Action")).
кинофильм(9,"Jhon Wick:Chapter",2023,"Chad Stahelski", жанр("Action")).
кинофильм(10,"Evil Dead Rise",2023,"Lee Cronin", жанр("Horror")).
кинофильм(11,"Scream 4",2023,"Matt Bettinelli-Olpin", жанр("Horror")).
кинофильм(12,"Evil Dead",2013,"Fede Alvarez", жанр("Horror")).
кинофильм(13,"Transformers",2023,"Steven Caple", жанр("Sciende-Fiction")).

показывают(1, 3, "08/05/2023", "21h30", 150).
показывают(1, 9, "08/05/2023", "19h30", 1000).
показывают(1, 13, "09/05/2023", "21h30", 450).
показывают(2, 2, "08/05/2023", "18h00", 500).
показывают(2, 4, "09/05/2023", "22h00", 200).
показывают(2, 10, "11/05/2023", "19h00", 350).
показывают(3, 1,  "08/05/2023", "21h00", 220).
показывают(4, 5, "09/05/2023", "18h30", 1000).
показывают(5, 6, "09/05/2023", "21h00", 1500).
показывают(6, 7, "23/05/2023", "22h30", 1600).
показывают(6, 12,  "08/05/2023", "21h45", 220).
показывают(6, 11, "09/05/2023", "18h00", 1100).
показывают(5, 8, "09/05/2023", "21h30", 1700).

where_to_watch(Movie):-
    кинофильм(Id_movie,Movie,_,_,_),
    показывают(Id_theater, Id_movie,_,_,_),
    кинотеатр(Id_theater, Name_theater,_,_,_),
    write("The movie theater to watch this movie is: ", Name_theater),
    nl(),
    fail.
    
address_movie_theater(Genre):-
    Genre1= жанр(Genre),
    кинофильм(Id_movie,_,_,_,Genre1),
    показывают(Id_theater,Id_movie,_,_,_),
    кинотеатр(Id_theater,_,Address,_,_),
    write("The address of the movie theaters showing this kind of movie is: ", Address),
    nl(),
    fail.
   
