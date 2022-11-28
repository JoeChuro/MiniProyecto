val funcion1=(x: Double)=> -math.pow(x,2)+8*x-12
val funcion2=(x: Double)=>  3*math.pow(x,2)
val funcion3=(x: Double)=> x+(2*math.pow(x,2))-math.pow(x,3)+(5*math.pow(x,4))
val funcion4=(x: Double)=> (2*x+1)/(math.pow(x,2)+x)
val funcion5=(x: Double)=> math.pow(math.E,x)
val funcion6=(x: Double)=> 1/math.sqrt(x-1)
val funcion7=(x: Double)=> 1/(1+math.pow(x,2))

def simpsonSimple(f: Double => Double,b: Int, a: Int):Double =
  val h = b-a
  val x = (a+b)/2.0
  h * (f(a)+4*f(x) + f(b))/6

def simpsonCompuesta (f:Double => Double, b : Int, a : Int, nIntervalos : Int) : Double =
  val h = ((b - a)*1.0) / nIntervalos
  val xj = (j: Double) => a + ((j * h) * 1.0)
  val f1 = (j: Double) => f(xj((2 * j) - 2)) + (4 * f(xj((2 * j) - 1))) + f(xj(2 * j))
  val sumatoria = (1 to nIntervalos / 2).map(f1(_)).sum
  (sumatoria * h) / 3


def simpsonExtendido (f:Double => Double, b : Int, a : Int) : Double =
  val n = 2 * (b - a)
  val h = (b - a) / n.toDouble
  val func1 = (x: Double) => f(a + (x * h))
  val funciones = f(a) + 4 * (1 until n by 2).map(func1(_)).sum +
    2 * (2 to n - 2 by 2).map(func1(_)).sum + f(b)
  (funciones * h) / 3.0

@main def app():Unit=

  println("METODO SIMPSON 1/3 ")
  println(simpsonSimple(funcion1,5,3))
  println(simpsonSimple(funcion2,2,0))
  println(simpsonSimple(funcion3,1,-1))
  println(simpsonSimple(funcion4,2,1))
  println(simpsonSimple(funcion5,1,0))
  println(simpsonSimple(funcion6,3,2))
  println(simpsonSimple(funcion7,1,0))

  println("METODO SIMPSON COMPUESTA ")

  println(simpsonCompuesta(funcion1, 5, 3,2))
  println(simpsonCompuesta(funcion2, 2, 0,20))
  println(simpsonCompuesta(funcion3, 1, -1,1000))
  println(simpsonCompuesta(funcion4, 2, 1,1000))
  println(simpsonCompuesta(funcion5, 1, 0,1000))
  println(simpsonCompuesta(funcion6, 3, 2,1000))
  println(simpsonCompuesta(funcion7, 1, 0,1000))

  println("METODO SIMPSON EXTENDIDA ")

  println(simpsonExtendido(funcion1, 5, 3))
  println(simpsonExtendido(funcion2, 2, 0))
  println(simpsonExtendido(funcion3, 1, -1))
  println(simpsonExtendido(funcion4, 2, 1))
  println(simpsonExtendido(funcion5, 1, 0))
  println(simpsonExtendido(funcion6, 3, 2))
  println(simpsonExtendido(funcion7, 1, 0))