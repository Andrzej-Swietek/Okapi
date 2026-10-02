package io.okapi.codegen.render

import io.okapi.codegen.*
import Source.{ doc, file, importsOf, signature, streamImports }

/** Renders the tagless-final API: a [[Group.traitName]] trait per group and the root trait with an accessor per group,
  * or the root trait with the operations of the single group.
  */
private[codegen] object ApiRenderer {

  def render(model: ClientModel, settings: Settings): List[SourceFile] = {
    val (packages, rootTrait, title) = (settings.packages, settings.api, settings.title)
    def groupSource(g: Group, comment: String) =
      SourceFile(packages.root.file(g.traitName.bare), groupFile(g, model, settings, comment))
    model.groups match {
      case List(single) if single.traitName == rootTrait => List(groupSource(single, s"The $title client."))
      case groups =>
        groups.map(g => groupSource(g, s"The operations of `${g.tag}`.")) :+
          SourceFile(packages.root.file(rootTrait.bare), rootFile(groups, packages, rootTrait, title))
    }
  }

  private def groupFile(group: Group, model: ClientModel, settings: Settings, comment: String): String = {
    val packages = settings.packages
    val operations = group.operations.map { op =>
      op.summary.fold("")(doc(_, "  ")) + "  " + signature(op, withDefaults = true, settings.streaming)
    }
    file(
      packages.root,
      importsOf(group.operations.flatMap(_.types), model, packages) ++ streamImports(
        group.operations,
        settings.streaming,
      ),
      List(doc(comment) + s"trait ${group.traitName.value}[F[_]] {\n${operations.mkString("\n\n")}\n}"),
    )
  }

  private def rootFile(groups: List[Group], packages: Packages, rootTrait: Identifier, title: String): String = {
    val accessors = groups.map { g =>
      doc(s"The operations of `${g.tag}`.", "  ") + s"  def ${g.accessor.value}: ${g.traitName.value}[F]"
    }
    val body = s"trait ${rootTrait.value}[F[_]] {\n${accessors.mkString("\n\n")}\n}"
    file(packages.root, Nil, List(doc(s"The $title client.") + body))
  }
}
