structure print =
struct

fun print_red x = print ("\027[31m" ^ x ^"\027[0m");
fun print_white x = print ("\027[37m" ^ x ^"\027[0m");

end