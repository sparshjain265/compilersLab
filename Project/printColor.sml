structure printColor =
struct

fun print_red x = print ("\027[31m" ^ x ^"\027[0m");
fun print_white x = print ("\027[37m" ^ x ^"\027[0m");
fun print_green x = print ("\027[32m" ^ x ^"\027[0m");
fun print_yellow x = print ("\027[33m" ^ x ^"\027[0m");
fun print_blue x = print ("\027[34m" ^ x ^"\027[0m");
fun print_magneta x = print ("\027[35m" ^ x ^"\027[0m");
fun print_cyan x = print ("\027[36m" ^ x ^"\027[0m");
fun print_default x = print ("\027[49m" ^ x ^"\027[0m");

end

