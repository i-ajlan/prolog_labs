% Copyright

implement main
    open core, stdio, file

domains
    жанр = scienceFiction; biography; horror; action.
    ilist = integer*.
    movie = movie(integer, string, integer, string, жанр).

class facts - moviesDB
    кинотеатр : (integer Id_кинотеатр, string Название, string Адрес, real Телефон, integer КоличествоMест) nondeterm.
    кинофильм : (integer Id_кинотеатр, string Название, integer ГодВыпуска, string Режиссер, жанр Жанр) nondeterm.
    показывают : (integer Id_кинотеатр, integer Id_кинофильм, string Дата, string Время, integer Выручка) nondeterm.

class predicates
    write_list : (A*).
clauses
    write_list([]).
    write_list([H]) :-
        write(H, "."),
        !.
    write_list([H | T]) :-
        write(H, ","),
        nl,
        write_list(T).

class predicates
    length : (A*) -> integer Len.
clauses
    length([]) = 0.
    length([_ | T]) = length(T) + 1.

class predicates
    contains : (movie, movie*) nondeterm.
clauses
    contains(X, [X | _]) :-
        !,
        writef("We found : % in the list.", X).
    contains(X, [_ | T]) :-
        contains(X, T),
        !.
    contains(_, [_ | _]) :-
        write("We didn't find it in the list.").

class predicates
    sum_count : (ilist, integer [out], integer [out]).
    rating_avg : (ilist, real [out]).

clauses
    rating_avg(L, S / C) :-
        sum_count(L, S, C).
    sum_count([], 0, 0).
    sum_count([H | T], St + H, Ct + 1) :-
        sum_count(T, St, Ct).

class predicates
    max_list : (ilist, integer [out]) nondeterm.
    max_number : (integer, integer, integer [out]) nondeterm.

clauses
    max_number(X, Y, Max) :-
        X > Y,
        Max = X.
    max_number(X, Y, Max) :-
        X < Y,
        Max = Y.
    max_number(X, X, Max) :-
        Max = X.
    max_list([X], X).
    max_list([H | T], Max) :-
        max_list(T, MaxT),
        max_number(H, MaxT, Max).

class predicates
    min_list : (ilist, integer [out]) nondeterm.
    min_number : (integer, integer, integer [out]) nondeterm.

clauses
    min_number(X, Y, Min) :-
        X > Y,
        Min = Y.
    min_number(X, Y, Min) :-
        X < Y,
        Min = X.
    min_number(X, X, Min) :-
        Min = X.
    min_list([X], X).
    min_list([H | T], Min) :-
        min_list(T, MinT),
        min_number(H, MinT, Min).

clauses
    run() :-
        stdio::write("Welcome in my program!\n"),
        fail.

    run() :-
        consult("../fa.txt", moviesDB),
        fail.

    run() :-
        write("The different genre of movies in the dataBase:"),
        nl,
        X = [ Genre || кинофильм(_, _, _, _, Genre) ],
        write(X),
        nl,
        fail.

    run() :-
        nl,
        write("The different facts in our dataBase are:"),
        nl,
        nl,
        X = [ кинотеатр(Id_кинотеатр, Название, Адрес, Телефон, КоличествоMест) || кинотеатр(Id_кинотеатр, Название, Адрес, Телефон, КоличествоMест) ],
        write_list(X),
        nl,
        nl,
        Y = [ кинофильм(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) || кинофильм(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) ],
        write_list(Y),
        nl,
        nl,
        Z = [ показывают(Id_кинотеатр, Id_кинофильм, Дата, Время, Выручка) || показывают(Id_кинотеатр, Id_кинофильм, Дата, Время, Выручка) ],
        write_list(Z),
        nl,
        nl,
        fail.

    run() :-
        nl,
        Y = [ movie(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) || кинофильм(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) ],
        contains(movie(1, "Moonfall", 2022, "Roland Emmerich", scienceFiction), Y),
        nl,
        fail.

    run() :-
        nl,
        L = [ КоличествоMест || кинотеатр(_, _, _, _, КоличествоMест) ],
        sum_count(L, S, C),
        writef("The total of places available in those theaters are: %", S),
        nl,
        rating_avg(L, A),
        writef("The average place in those theaters is %", A),
        nl,
        max_list(L, Max),
        writef("The maximum place in one theater is %.", Max),
        nl,
        min_list(L, Min),
        writef("The minimum place in one theater is %.", Min),
        nl,
        fail.

    run() :-
        nl,
        X = [ кинотеатр(Id_кинотеатр, Название, Адрес, Телефон, КоличествоMест) || кинотеатр(Id_кинотеатр, Название, Адрес, Телефон, КоличествоMест) ],
        writef("The number of theaters is: %", length(X)),
        nl,
        Y = [ кинофильм(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) || кинофильм(Id_кинотеатр, Название, ГодВыпуска, Режиссер, Жанр) ],
        writef("The number of Movies is: %.", length(Y)),
        nl,
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
