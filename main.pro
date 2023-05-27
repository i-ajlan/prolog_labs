% Copyright

implement main
    open core, stdio, file

domains
    жанр = scienceFiction; biography; horror; action.

class facts - moviesDB
    кинотеатр : (integer Id_кинотеатр, string Название, string Адрес, real Телефон, integer КоличествоMест) nondeterm.
    кинофильм : (integer Id_кинотеатр, string Название, integer ГодВыпуска, string Режиссер, жанр Жанр) nondeterm.
    показывают : (integer Id_кинотеатр, integer Id_кинофильм, string Дата, string Время, integer Выручка) nondeterm.

class predicates
    where_to_watch : (string Movie) nondeterm anyflow.
clauses
    where_to_watch(Movie) :-
        nl(),
        кинофильм(Id_movie, Movie, _, _, _),
        показывают(Id_theater, Id_movie, _, _, _),
        кинотеатр(Id_theater, Name_theater, _, _, _),
        writef("The movie theater to watch this movie is: %. \n", Name_theater).

class predicates
    find_theaters_with_more_than : (integer MinPlace) nondeterm.
clauses
    find_theaters_with_more_than(MinPlace) :-
        nl(),
        кинотеатр(_, NameTheater, _, _, Place),
        Place >= MinPlace,
        writef("The theater,%,has %.\n", NameTheater, Place).

class predicates
    averagePlaces : (integer IdTheater1, integer IdTheater2, integer IdTheater3) nondeterm.
clauses
    averagePlaces(IdTheater1, IdTheater2, IdTheater3) :-
        nl(),
        кинотеатр(IdTheater1, Name1, _, _, Place1),
        кинотеатр(IdTheater2, Name2, _, _, Place2),
        кинотеатр(IdTheater3, Name3, _, _, Place3),
        AveragePlace = (Place1 + Place2 + Place3) / 3,
        writef("The average place in the theater : %, %, % is %.", Name1, Name2, Name3, AveragePlace).

class predicates
    address_movie_theater : (жанр Genre) nondeterm anyflow.
clauses
    address_movie_theater(Genre) :-
        nl(),
        кинофильм(IdMovie, _, _, _, Genre),
        показывают(IdTheater, IdMovie, _, _, _),
        кинотеатр(IdTheater, _, Address, _, _),
        writef("The address of the movie theaters showing this kind of movie is: %. \n", Address).

clauses
    run() :-
        stdio::write("Welcome in my program!\n"),
        fail.

    run() :-
        consult("../fa.txt", moviesDB),
        fail.

    run() :-
        address_movie_theater(biography),
        fail.

    run() :-
        where_to_watch("Moonfall"),
        fail.

    run() :-
        find_theaters_with_more_than(100),
        fail.

    run() :-
        averagePlaces(1, 2, 3),
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
