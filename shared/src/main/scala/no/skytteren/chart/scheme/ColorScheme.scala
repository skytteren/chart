package no.skytteren.chart.scheme

import no.skytteren.chart.RGB
import no.skytteren.chart.Color._


object ColorScheme {

  case class MaterialScheme(`50`: RGB, `100`: RGB, `200`: RGB, `300`: RGB, `400`: RGB, `500`: RGB, `600`: RGB, `700`: RGB, `800`: RGB, `900`: RGB){
    val all = List(`50`, `100`, `200`, `300`, `400`, `500`, `600`, `700`, `800`, `900`)
  }

  val red = MaterialScheme(
    hex("FFEBEE"), hex("FFCDD2"), hex("EF9A9A"), hex("E57373"), hex("EF5350"), hex("F44336"), hex("E53935"), hex("D32F2F"), hex("C62828"), hex("B71C1C")
  )

  val pink = MaterialScheme(
    hex("FCE4EC"), hex("F8BBD0"), hex("F48FB1"), hex("F06292"), hex("EC407A"), hex("E91E63"), hex("D81B60"), hex("C2185B"), hex("AD1457"), hex("880E4F")
  )

  val purple = MaterialScheme(
    hex("F3E5F5"), hex("E1BEE7"), hex("CE93D8"), hex("BA68C8"), hex("AB47BC"), hex("9C27B0"), hex("8E24AA"), hex("7B1FA2"), hex("6A1B9A"), hex("4A148C")
  )

  val deepPurple = MaterialScheme(
    hex("EDE7F6"), hex("D1C4E9"), hex("B39DDB"), hex("9575CD"), hex("7E57C2"), hex("673AB7"), hex("5E35B1"), hex("512DA8"), hex("4527A0"), hex("311B92")
  )

  val indigo = MaterialScheme(
    hex("E8EAF6"), hex("C5CAE9"), hex("9FA8DA"), hex("7986CB"), hex("5C6BC0"), hex("3F51B5"), hex("3949AB"), hex("303F9F"), hex("283593"), hex("1A237E")
  )

  val blue = MaterialScheme(
    hex("E3F2FD"), hex("BBDEFB"), hex("90CAF9"), hex("64B5F6"), hex("42A5F5"), hex("2196F3"), hex("1E88E5"), hex("1976D2"), hex("1565C0"), hex("0D47A1")
  )

  val lightBlue = MaterialScheme(
    hex("E1F5FE"), hex("B3E5FC"), hex("81D4FA"), hex("4FC3F7"), hex("29B6F6"), hex("03A9F4"), hex("039BE5"), hex("0288D1"), hex("0277BD"), hex("01579B")
  )

  val cyan = MaterialScheme(
    hex("E0F7FA"), hex("B2EBF2"), hex("80DEEA"), hex("4DD0E1"), hex("26C6DA"), hex("00BCD4"), hex("00ACC1"), hex("0097A7"), hex("00838F"), hex("006064")
  )

  val teal = MaterialScheme(
    hex("E0F2F1"), hex("B2DFDB"), hex("80CBC4"), hex("4DB6AC"), hex("26A69A"), hex("009688"), hex("00897B"), hex("00796B"), hex("00695C"), hex("004D40")
  )

  val green = MaterialScheme(
    hex("E8F5E9"), hex("C8E6C9"), hex("A5D6A7"), hex("81C784"), hex("66BB6A"), hex("4CAF50"), hex("43A047"), hex("388E3C"), hex("2E7D32"), hex("1B5E20")
  )

  val lightGreen = MaterialScheme(
    hex("F1F8E9"), hex("DCEDC8"), hex("C5E1A5"), hex("AED581"), hex("9CCC65"), hex("8BC34A"), hex("7CB342"), hex("689F38"), hex("558B2F"), hex("33691E")
  )

  val lime = MaterialScheme(
    hex("F9FBE7"), hex("F0F4C3"), hex("E6EE9C"), hex("DCE775"), hex("D4E157"), hex("CDDC39"), hex("C0CA33"), hex("AFB42B"), hex("9E9D24"), hex("827717")
  )
  val yellow = MaterialScheme(
    hex("FFFDE7"), hex("FFF9C4"), hex("FFF59D"), hex("FFF176"), hex("FFEE58"), hex("FFEB3B"), hex("FDD835"), hex("FBC02D"), hex("F9A825"), hex("F57F17")
  )
  val amber = MaterialScheme(
    hex("FFF8E1"), hex("FFECB3"), hex("FFE082"), hex("FFD54F"), hex("FFCA28"), hex("FFC107"), hex("FFB300"), hex("FFA000"), hex("FF8F00"), hex("FF6F00")
  )
  val orange = MaterialScheme(
    hex("FFF3E0"), hex("FFE0B2"), hex("FFCC80"), hex("FFB74D"), hex("FFA726"), hex("FF9800"), hex("FB8C00"), hex("F57C00"), hex("EF6C00"), hex("E65100")
  )
  val deepOrange = MaterialScheme(
    hex("FBE9E7"), hex("FFCCBC"), hex("FFAB91"), hex("FF8A65"), hex("FF7043"), hex("FF5722"), hex("F4511E"), hex("E64A19"), hex("D84315"), hex("BF360C")
  )
  val brown = MaterialScheme(
    hex("EFEBE9"), hex("D7CCC8"), hex("BCAAA4"), hex("A1887F"), hex("8D6E63"), hex("795548"), hex("6D4C41"), hex("5D4037"), hex("4E342E"), hex("3E2723")
  )
  val grey = MaterialScheme(
    hex("FAFAFA"), hex("F5F5F5"), hex("EEEEEE"), hex("E0E0E0"), hex("BDBDBD"), hex("9E9E9E"), hex("757575"), hex("616161"), hex("424242"), hex("212121")
  )
  val blueGrey = MaterialScheme(
    hex("ECEFF1"), hex("CFD8DC"), hex("B0BEC5"), hex("90A4AE"), hex("78909C"), hex("607D8B"), hex("546E7A"), hex("455A64"), hex("37474F"), hex("263238")
  )

  val all = List(red, pink, purple, deepPurple, indigo, blue, lightBlue, cyan, teal, green, lightGreen, lightBlue, yellow, amber, orange, deepOrange, brown, grey, blueGrey)

  val allColors = all.flatMap(_.all)

  val `50`:  List[RGB] = all.map(_.`50`)
  val `100`: List[RGB] = all.map(_.`100`)
  val `200`: List[RGB] = all.map(_.`200`)
  val `300`: List[RGB] = all.map(_.`300`)
  val `400`: List[RGB] = all.map(_.`400`)
  val `500`: List[RGB] = all.map(_.`500`)
  val `600`: List[RGB] = all.map(_.`600`)
  val `700`: List[RGB] = all.map(_.`700`)
  val `800`: List[RGB] = all.map(_.`800`)
  val `900`: List[RGB] = all.map(_.`900`)

}
