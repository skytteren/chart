package no.skytteren.chart

class ChartsSpec extends App{

  var data = List(
    {"Locke"    ->  4},
    {"Reyes"    ->  8},
    {"Ford"     -> 15},
    {"Jarrah"   -> 16},
    {"Shephard" -> 23},
    {"Kwon"     -> 42}
  )

  Charts.barVertical(data, 500, 400)

}
