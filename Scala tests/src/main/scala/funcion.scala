object funcion {
  def integracion(f: Double => Double, a: Double, b: Double): Double =
    val xBarra = (a + b) / 2.0
    val h = (b - a)
    (h / 6.0) * (f(a) + 4 * f(xBarra) + f(b))

  def error(valorEsperado: Double, valorObtenido: Double): Double =
    math.abs(valorEsperado - valorObtenido)


  @main def ejecutar(): Unit =

    // 1) ∫3→5 (-x^2 + 8x − 12)
    val f1 = (x: Double) => -x * x + 8 * x - 12
    val r1 = integracion(f1, 3, 5)
    val e1 = error(7.33, r1)

    // 2) ∫0→2 3x^2
    val f2 = (x: Double) => 3 * x * x
    val r2 = integracion(f2, 0, 2)
    val e2 = error(8.0, r2)

    // 3) ∫−1→1 x + 2x^2 − x^3 + 5x^4
    val f3 = (x: Double) => x + 2 * x * x - x * x * x + 5 * math.pow(x, 4)
    val r3 = integracion(f3, -1, 1)
    val e3 = error(3.333, r3)

    // 4) ∫1→2 (2x + 1)/(x^2 + x)
    val f4 = (x: Double) => (2 * x + 1) / (x * x + x)
    val r4 = integracion(f4, 1, 2)
    val e4 = error(1.09861, r4)

    // 5) ∫0→1 e^x
    val f5 = (x: Double) => math.exp(x)
    val r5 = integracion(f5, 0, 1)
    val e5 = error(1.71828, r5)

    // 6) ∫2→3 1 / (x - 1)
    val f6 = (x: Double) => 1.0 / math.sqrt(x - 1)
    val r6 = integracion(f6, 2, 3)
    val e6 = error(0.828427, r6)

    // 7) ∫0→1 1 / (1 + x^2)
    val f7 = (x: Double) => 1.0 / (1 + x * x)
    val r7 = integracion(f7, 0, 1)
    val e7 = error(0.785398, r7)

    println("Resultados Simpson 1/3 en Scala 3\n")
    println(f"1) Resultado: $r1%.6f   Error: $e1%.6f")
    println(f"2) Resultado: $r2%.6f   Error: $e2%.6f")
    println(f"3) Resultado: $r3%.6f   Error: $e3%.6f")
    println(f"4) Resultado: $r4%.6f   Error: $e4%.6f")
    println(f"5) Resultado: $r5%.6f   Error: $e5%.6f")
    println(f"6) Resultado: $r6%.6f   Error: $e6%.6f")
    println(f"7) Resultado: $r7%.6f   Error: $e7%.6f")

}
