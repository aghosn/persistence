package scala.reflect.persistence

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.language.postfixOps
import scala.annotation.tailrec
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import Enrichments._

  val name = "persistence"
  val description = """Persists typed ASTs of the entire program.
  For more information visit https://github.com/scalareflect/persistence"""
  val components = List[NscPluginComponent](PluginComponent) // Might change name

  private object PluginComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "persist"
    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        val decomposedTree = TreeDecomposer(unit body)
        
        val folder = new File("asts")
        if(!folder.exists()) folder.mkdir()

        /* TODO: do hierarchy as package */
        val astCompressor = new AstCompressor(new DataOutputStream(new FileOutputStream(s"asts/${unit.source.toString}.ast")))
        astCompressor(decomposedTree.tree)
      }
    }

    /* Wrapper for treeDecomposer's function */
    case class DecomposedTree(tree: Node, namesBFS: Map[Name, List[Int]], symbBFS: Map[Symbol, List[Int]], typesBFS: Map[Type, List[Int]], constBFS: Map[Constant, List[Int]])

    /* Return a simplified tree along with maps of Names / Symbols / Types zipped with occurrences in BFS order */
    /* TODO: adapt, depending if we need to store names */
    object TreeDecomposer extends (Tree => DecomposedTree) {
      def apply(tree: Tree): DecomposedTree = {
        var nameList: RevList[Name] = List()
        var symbolList: RevList[Symbol] = List()
        var typeList: RevList[Type] = List()
        var constList: RevList[Constant] = List()
        /* Traverse the tree, save names, type, symbols into corresponding list
         * and replace them in the tree by default values*/
        @tailrec def loop(trees: List[Tree], dict: Map[Tree, Node]): Map[Tree, Node] = trees match {
          case Nil => dict
          case x :: xs =>
            symbolList :+= x.symbol
            typeList :+= x.tpe
            val res = x match {
              case PackageDef(pid, stats) =>
                Node(NodeTag.PackageDef, dict(pid) :: (stats map (dict(_))))
              case ClassDef(mods, name, tparams, impl) =>
                nameList :+= name
                Node(NodeTag.ClassDef, (tparams ::: List(impl) map (dict(_))))
              case ModuleDef(mods, name, impl) =>
                nameList :+= name
                Node(NodeTag.ModuleDef, List(dict(impl)))
              case ValDef(mods, name, tpt, rhs) =>
                nameList :+= name
                Node(NodeTag.ValDef, List(dict(tpt), dict(rhs)))
              case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
                nameList :+= name
                val vnodes = vparams.map(_.map(dict(_))).flatMap(_ :+ Node.separator)
                Node(NodeTag.DefDef, (tparams.map(dict(_)) ::: List(Node.separator) ::: vnodes ::: List(dict(tpt), dict(rhs))))
              case TypeDef(mods, name, tparams, rhs) =>
                nameList :+ name
                Node(NodeTag.TypeDef, (tparams ::: List(rhs)) map (dict(_)))
              case LabelDef(name, params, rhs) =>
                nameList :+= name
                Node(NodeTag.LabelDef, (params ::: List(rhs)) map (dict(_)))
              case Import(expr, selectors) =>
                Node(NodeTag.Import, List(dict(expr)))
              case Template(parents, self, body) =>
                Node(NodeTag.Template, (parents.map(dict(_)) ::: List(Node.separator, dict(self), Node.separator) ::: body.map(dict(_))))
              case Block(stats, expr) =>
                Node(NodeTag.Block, (stats ::: List(expr)) map (dict(_)))
              case CaseDef(pat, guard, body) =>
                Node(NodeTag.CaseDef, List(pat, guard, body) map (dict(_)))
              case Alternative(trees) =>
                Node(NodeTag.Alternative, trees map (dict(_)))
              case Star(elem) =>
                Node(NodeTag.Star, List(dict(elem)))
              case Bind(name, body) =>
                nameList :+= name
                Node(NodeTag.Bind, List(dict(body)))
              case UnApply(fun, args) =>
                Node(NodeTag.UnApply, fun :: args map (dict(_)))
              case ArrayValue(elemtpt, elems) =>
                Node(NodeTag.ArrayValue, elemtpt :: elems map (dict(_)))
              case Function(vparams, body) =>
                Node(NodeTag.Function, vparams ::: List(body) map (dict(_)))
              case Assign(lhs, rhs) =>
                Node(NodeTag.Assign, List(lhs, rhs) map (dict(_)))
              case AssignOrNamedArg(lhs, rhs) =>
                Node(NodeTag.AssignOrNamedArg, List(lhs, rhs) map (dict(_)))
              case If(cond, thenp, elsep) =>
                Node(NodeTag.If, List(cond, thenp, elsep) map (dict(_)))
              case Match(selector, cases) =>
                Node(NodeTag.Match, selector :: cases map (dict(_)))
              case Return(expr) =>
                Node(NodeTag.Return, List(dict(expr)))
              case Try(block, catches, finalizer) =>
                Node(NodeTag.Try, block :: catches ::: List(finalizer) map (dict(_)))
              case Throw(expr) =>
                Node(NodeTag.Throw, List(dict(expr)))
              case New(tpt) =>
                Node(NodeTag.New, List(dict(tpt)))
              case Typed(expr, tpt) =>
                Node(NodeTag.Typed, List(expr, tpt) map (dict(_)))
              case TypeApply(fun, args) =>
                Node(NodeTag.TypeApply, fun :: args map (dict(_)))
              case Apply(fun, args) =>
                Node(NodeTag.Apply, fun :: args map (dict(_)))
              case ApplyDynamic(qual, args) =>
                Node(NodeTag.ApplyDynamic, qual :: args map (dict(_)))
              case This(qual) =>
                nameList :+= qual
                Node(NodeTag.This, Nil)
              case Select(qualifier, selector) =>
                nameList :+= selector
                Node(NodeTag.Select, List(dict(qualifier)))
              case Ident(name) =>
                nameList :+= name
                Node(NodeTag.Ident, Nil)
              case ReferenceToBoxed(ident) =>
                Node(NodeTag.ReferenceToBoxed, List(dict(ident)))
              case Literal(value) =>
                constList :+= value
                Node(NodeTag.Literal)
              case Annotated(annot, arg) =>
                Node(NodeTag.Annotated, List(annot, arg) map (dict(_)))
              case SingletonTypeTree(ref) =>
                Node(NodeTag.SingletonTypeTree, List(dict(ref)))
              case SelectFromTypeTree(qualifier, selector) =>
                nameList :+= selector
                Node(NodeTag.SelectFromTypeTree, List(dict(qualifier)))
              case CompoundTypeTree(templ) =>
                Node(NodeTag.CompoundTypeTree, List(dict(templ)))
              case AppliedTypeTree(tpt, args) =>
                Node(NodeTag.AppliedTypeTree, tpt :: args map (dict(_)))
              case TypeBoundsTree(lo, hi) =>
                Node(NodeTag.TypeBoundsTree, List(lo, hi) map (dict(_)))
              case ExistentialTypeTree(tpt, whereClauses) =>
                Node(NodeTag.ExistentialTypeTree, tpt :: whereClauses map (dict(_)))
              case t: TypeTree =>
                Node(NodeTag.TypeTree, Nil)
              case Super(qual, mix) =>
                nameList :+= mix
                Node(NodeTag.Super, List(dict(qual)))
              case _ => sys.error(x.getClass().toString()) /* TODO : remove */
            }
            loop(xs, dict + (x -> res))
        }

        val newTree = loop(tree flattenBFS, Map((EmptyTree -> Node.empty)))(tree)
        DecomposedTree(newTree, nameList.zipWithIdxs, symbolList.zipWithIdxs, typeList.zipWithIdxs, constList.zipWithIdxs)
      }
    }
    class SymbolDecomposer { /* TODO */ }

    /* TODO: buggy version, bugs to fix */
    /* TODO: use or discard, depending if we need to store names */
    class NameCompressor(comp: LzwCompressor) extends (Map[Name, List[Int]] => Unit) {
      /* TODO: can we have null names? If yes, this would crash */
      def apply(namesBFS: Map[Name, List[Int]]) { /* TODO: This is just a tentative encoding as string */
        var flags: List[Int] = List()
        val nms = namesBFS.:\("") {
          (nm, acc) =>
            val p = acc + nm._1.toString + "(" + nm._2.mkString(",") + ")"
            (if (nm._1.isTermName) flags :+= 1 else flags :+= 0)
            p
        } + "#" + createTag(flags).mkString(",") + "#"
        comp(nms)
        println(nms)
      }
      /* Returns the tags grouped by 8 encoded as char */
      def createTag(l: List[Int]): List[Char] =
        (l.grouped(8).toList).map(x => bitsToChar(x.zipWithIndex, 0))

      /* Converts sequence of bits to char */
      def bitsToChar(l: List[(Int, Int)], acc: Byte): Char = l match {
        case Nil => acc.toChar
        case x :: xs => bitsToChar(xs, (acc + (x._1 << x._2)).toByte)
      }
    }

    class SymbolCompressor(comp: LzwCompressor) { /* TODO */ }
    object ConstantCompressor { /* TODO */ }
    object TypeCompressor { /* TODO */ }

    /* Generate a list of trees in BFS order */
    implicit class TreeToBFS(tree: Tree) {
      def flattenBFS = {
        @tailrec
        def loop(queue: List[Tree], acc: RevList[Tree]): RevList[Tree] = queue match {
          case expr :: exprs => loop(exprs ::: expr.children, expr.children.reverse ::: acc)
          case Nil => acc
        }
        loop(tree :: Nil, tree :: Nil)
      }
    }

    /* TODO: transpose to another class in the decompression library. */
    /* TODO: test and adapt */
    object TreeRecomposer extends (DecomposedTree => Tree) {
      def apply(decomp: DecomposedTree): Tree = {
        var nameList: RevList[Name] = decomp.namesBFS.unzipWithIdxs
        var symbolList: RevList[Symbol] = decomp.symbBFS.unzipWithIdxs
        var typeList: RevList[Type] = decomp.typesBFS.unzipWithIdxs
        var constList: RevList[Constant] = decomp.constBFS.unzipWithIdxs
        @tailrec def loop(trees: List[Node], dict: Map[Node, Tree]): Map[Node, Tree] = trees match {
          case Nil => dict
          case x :: xs =>
            val res = x.tpe match {
              case NodeTag.PackageDef =>
                PackageDef(dict(x.children.head).asInstanceOf[RefTree], x.children.tail map (dict(_)))
              case NodeTag.ClassDef =>
                val nm = fetchName.asInstanceOf[TypeName] /* Need to fetch name first to avoid swap with name of modifier */
                ClassDef(NoMods, nm, x.children.init map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last).asInstanceOf[Template])
              case NodeTag.ModuleDef =>
                val nm = fetchName.asInstanceOf[TermName]
                ModuleDef(NoMods, nm, dict(x.children.head).asInstanceOf[Template])
              case NodeTag.ValDef =>
                val nm = fetchName.asInstanceOf[TermName]
                ValDef(NoMods, nm, dict(x.children.head), dict(x.children.last))
              case NodeTag.DefDef =>
                val params = x.children.dropRight(2).splitOn(_ == Node.separator)
                val vparams = params.tail.map(x => x.map(dict(_).asInstanceOf[ValDef]))
                val nm = fetchName.asInstanceOf[TermName]
                DefDef(NoMods, nm, params.head.map(dict(_).asInstanceOf[TypeDef]), vparams, dict(x.children.init.last), dict(x.children.last))
              case NodeTag.TypeDef =>
                val nm = fetchName.asInstanceOf[TypeName]
                TypeDef(NoMods, nm, x.children.init map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last))
              case NodeTag.LabelDef =>
                LabelDef(fetchName.asInstanceOf[TermName], x.children.init map (dict(_).asInstanceOf[Ident]), dict(x.children.last))
              case NodeTag.Import =>
                Import(dict(x.children.head), Nil)
              case NodeTag.Template =>
                val children = x.children.splitOn(c => c.tpe == NodeTag.Separator).map(_.map(dict(_)))
                Template(children.head, children(1).head.asInstanceOf[ValDef], children.last)
              case NodeTag.Block =>
                Block(x.children.init map (dict(_)), dict(x.children.last))
              case NodeTag.CaseDef =>
                CaseDef(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
              case NodeTag.Alternative =>
                Alternative(x.children map (dict(_)))
              case NodeTag.Star =>
                Star(dict(x.children.head))
              case NodeTag.Bind =>
                Bind(fetchName, dict(x.children.head))
              case NodeTag.UnApply =>
                UnApply(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.ArrayValue =>
                ArrayValue(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.Function =>
                Function(x.children.init map (dict(_).asInstanceOf[ValDef]), dict(x.children.last))
              case NodeTag.Assign =>
                Assign(dict(x.children.head), dict(x.children.last))
              case NodeTag.AssignOrNamedArg =>
                AssignOrNamedArg(dict(x.children.head), dict(x.children.last))
              case NodeTag.If =>
                If(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
              case NodeTag.Match =>
                Match(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[CaseDef]))
              case NodeTag.Return =>
                Return(dict(x.children.head))
              case NodeTag.Try =>
                Try(dict(x.children.head), x.children.tail.init map (dict(_).asInstanceOf[CaseDef]), dict(x.children.last))
              case NodeTag.Throw =>
                Throw(dict(x.children.head))
              case NodeTag.New =>
                New(dict(x.children.head))
              case NodeTag.Typed =>
                Typed(dict(x.children.head), dict(x.children.last))
              case NodeTag.TypeApply =>
                TypeApply(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.Apply =>
                Apply(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.ApplyDynamic =>
                ApplyDynamic(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.This =>
                This(fetchName.asInstanceOf[TypeName])
              case NodeTag.Select =>
                Select(dict(x.children.head), fetchName)
              case NodeTag.Ident =>
                Ident(fetchName)
              case NodeTag.ReferenceToBoxed =>
                ReferenceToBoxed(dict(x.children.head).asInstanceOf[Ident])
              case NodeTag.Literal =>
                val const = constList.head
                constList = constList.tail
                Literal(const)
              case NodeTag.Annotated =>
                Annotated(dict(x.children.head), dict(x.children.last))
              case NodeTag.SingletonTypeTree =>
                SingletonTypeTree(dict(x.children.head))
              case NodeTag.SelectFromTypeTree =>
                SelectFromTypeTree(dict(x.children.head), fetchName.asInstanceOf[TypeName])
              case NodeTag.CompoundTypeTree =>
                CompoundTypeTree(dict(x.children.head).asInstanceOf[Template])
              case NodeTag.AppliedTypeTree =>
                AppliedTypeTree(dict(x.children.head), x.children.tail map (dict(_)))
              case NodeTag.TypeBoundsTree =>
                TypeBoundsTree(dict(x.children.head), dict(x.children.last))
              case NodeTag.ExistentialTypeTree =>
                ExistentialTypeTree(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[MemberDef]))
              case NodeTag.TypeTree =>
                TypeTree()
              case NodeTag.Super =>
                Super(dict(x.children.head), fetchName.asInstanceOf[TypeName])
              case NodeTag.EmptyTree => EmptyTree
              case _ => sys.error(x.getClass().toString()) /* TODO : remove */
            }
            /* TODO: cleaner */
            if (x.tpe != NodeTag.EmptyTree) {
              if (typeList.head != null) res.setType(typeList.head)
              typeList = typeList.tail
              if (symbolList.head != null && x.tpe != NodeTag.TypeTree) res.setSymbol(symbolList.head) /* TODO: cannot set symbols to TypeTree, figure out */
              symbolList = symbolList.tail
            }
            loop(xs, dict + (x -> res))
        }

        def fetchName = {
          val ret = nameList.head
          nameList = nameList.tail
          ret
        }
        loop(decomp.tree.flattenBFS.filter(x => x.tpe != NodeTag.Separator), Map((Node.empty -> EmptyTree)))(decomp.tree)
      }
    }
  }
}
