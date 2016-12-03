package examples

import scala.meta._

case class MapFunction(func: Term, domain: Type.Arg, range: Type.Arg)

class mapargs extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val maps = args map {
      case q"$func: $domain => $range" => MapFunction(func, domain, range)
      case arg => abort(s"unexpected argument: $arg")
    }

    val q"..$classMods class $className extends ..$classParents { ..$body }" = defn
    val funcs = body.flatMap {
      case q"..$mods def $func(..$args): $resultType = $body" =>
        val (unmappedArgs: List[Term.Param], mappedArgs: List[Term.Arg]) = args.map {
          case arg@param"$name: $tpe" =>
            val mapFun = tpe.flatMap(t => maps.find(_.range.toString == t.toString))
            val named = Term.Name(name.value)
            mapFun.map(map => (param"$named: ${map.domain}", arg"${map.func}($named)")).getOrElse((arg, name))
          case x => abort(x.structure)
        }.unzip

        if (mappedArgs != args) Some(q"..$mods def $func(..$unmappedArgs): $resultType = $func(..$mappedArgs)")
        else None
      case _ => None
    }

    q"..$classMods class $className extends ..$classParents { ..$body; ..$funcs }"
  }
}
