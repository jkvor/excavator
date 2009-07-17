-define(PREFIX, "ABC").

foobar(I) -> lists:concat([?PREFIX,I]).

main(a) -> 
    assign(a, foobar("D")),
    a;
    
main(b) -> 
    assign(b, foobar("E")),
    b.
    
main(a, b) -> 
    assign(c, foobar("F")).