package com.github.hexx.deriving

import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

object showMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def showInstance(name: Name) = q"implicit val showInstance = scalaz.Show.showA[$name]"

    val result =
      annottees.map(_.tree).toList match {
        case (c @ ClassDef(_, name, _, _)) :: Nil =>
          val moduleDef = q"object ${newTermName(name.toString)} { ${showInstance(name)} }"
          c :: moduleDef :: Nil
        case (c @ ClassDef(_, name, _, _)) :: ModuleDef(mods, mname, Template(parents, self, body)) :: Nil =>
          val moduleDef = ModuleDef(mods, mname, Template(parents, self, body :+ showInstance(name)))
          c :: moduleDef :: Nil
        case _ =>
          c.abort(c.enclosingPosition, s"@deriving.show should be attached to ClassDef.")
      }

    c.Expr[Any](Block(result, Literal(Constant(()))))
  }
}

class show extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro showMacro.impl
}
