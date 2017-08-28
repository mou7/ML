import scala.util.Random

object NumericalGradientExample extends App {
// coefficients associated with function to learn
val coefficients: List[Double] = List(3, 4, -10)

def evaluate(input: List[Double], weights: List[Double]): Double = weights.zip(input ::: List(1.0)).map{case (x, y) => x * y}.sum

def error(input: List[Double], weights: List[Double]): Double = Math.pow(evaluate(input, coefficients) - evaluate(input, weights), 2)

def evaluateGradientError(input: List[Double], weights: List[Double], h: Double = 0.0001): List[Double] = {
  (0 to weights.length - 1).map {
    i => (error(input, weights.updated(i, weights(i) + h)) - error(input, weights)) / h}(scala.collection.breakOut)
}

def train(coefficients: List[Double], stepSize: Double = 0.1, nbIterations: Int = 1000): List[Double] = {
  // init randomly the weights
  var weights = List.fill(coefficients.length)(Random.nextDouble())
  for( i <- 1 to nbIterations){
    // generate a training sample
    val input = List.fill(coefficients.length - 1)(Random.nextDouble())
    println(s"Error: ${error(input, weights)}")
    // compute the gradient
    val gradient = evaluateGradientError(input, weights, 0.01)
    println(s"gradients: $gradient")
    // update the parameters
    weights = weights.zip(gradient).map { case (w, wGradient) => w + -stepSize * wGradient }
    println(s"weights: $weights")
  }
  weights
}

def run(): Unit = {
  train(coefficients)
}

run()
}