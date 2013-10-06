import com.github.hexx.deriving

import scalaz._, Scalaz._

object Main extends App {
  @deriving.show case class Hoge()

  @deriving.show case class Fuga()

  object Fuga {
    def hello = "hello"
  }

  println(Hoge().shows)
  println(Fuga().shows)
  println(Fuga.hello)
}
