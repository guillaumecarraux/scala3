package dotty.tools.dotc
package transform

import core.*
import ast.tpd.*
import Contexts.*
import MegaPhase.*
import Annotations.*
import Symbols.defn
import Constants.*
import Types.*
import Decorators.*
import Flags.*
import Symbols.Symbol
import dotty.tools.dotc.report

import scala.collection.mutable
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.transform.PostTyper.methodLastUses

class VerifyLastUseAnnotations extends MiniPhase:

  override def phaseName: String = VerifyLastUseAnnotations.name
  override def description: String = VerifyLastUseAnnotations.description



  case class FoldInfo(allowVar: Boolean, allowAnnot: Boolean){
    def afterAnnot = FoldInfo(false, allowAnnot)
    def merge(other: FoldInfo) = FoldInfo(allowVar && other.allowVar, allowAnnot)
    def inLoop = FoldInfo(allowVar, false)
  }

  private class LastUseVerifier(target: Symbol, method: Tree) extends TreeAccumulator[FoldInfo]{
    override def apply(accInfo: FoldInfo, tree: Tree)(using Context): FoldInfo = traverse(tree)(accInfo)

    def verify(using Context): Unit =
      traverse(method)(FoldInfo(true, true))

    private def traverseExclusiveCases(cases: List[Tree])(info: FoldInfo)(using Context): FoldInfo =
      cases
      .map(c => traverse(c)(info))//traverse all of them with same source info
      .reduce((a,b)=> a.merge(b))//merge all info, return it

    private def traverse(tree: Tree)(accInfo: FoldInfo)(using Context): FoldInfo = {
      var info = accInfo//used to mutate and pass the information

      //by default, accInfo is propagated in the order of the arguments. so block is already good, and everything that has linear reading
      tree match
        case _: Ident if tree.symbol == target =>
          if !info.allowVar then
            report.error(s"reuse of variable ${tree.symbol.name} after @lastUse", tree.sourcePos)
          if tree.hasAttachment(PostTyper.lastUseAttachment) then
            if info.allowAnnot then info.afterAnnot else
              report.error(s"cannot use @lastUse annotation in a loop", tree.sourcePos)
              tree.removeAttachment(PostTyper.lastUseAttachment)
              info.afterAnnot
          else
            info

        case If(cond, thenp, elsep) =>
          info = traverse(cond)(info)
          val info_branch1 = traverse(thenp)(info)//both branches are exclusive
          val info_branch2 = traverse(elsep)(info)//so we combine the info at the end

          info_branch1.merge(info_branch2)

        case WhileDo(cond, body) =>
          //everything is run in a loop, so I cannot allow a lastUse here
          info = info.inLoop
          traverse(cond)(info)//I do not care about the result, since I cannot annotate anything inside
          traverse(body)(info)//I just need to check if there is any illegal variable use

        case Match(selector, cases) =>
          info = traverse(selector)(info)

          //each case are exclusive, merge at the end
          traverseExclusiveCases(cases)(info)

        case Try(expr, cases, finalizer) =>
          info = traverse(expr)(info)
          info = traverseExclusiveCases(cases)(info)
          traverse(finalizer)(info)

        // case Closure(env, _, _) =>
        //   env.find(_.symbol == target) match
        //     case None => ()
        //     case Some(t) => if(info.allowVar){
        //         report.warning(s"creating a lambda using the variable ${target.name} (annotated later with @lastUse) creates a new reference to the variable, making the annotation useless", t.sourcePos)
        //         report.echo("Note that changing the lambda to a function solves the problem\n")
        //     }



        //   foldOver(info, tree)

        // case vd @ ValDef(name, tpt, rhs) =>
        //   def maybeAlias(tree: Tree): Boolean = tree match//short check that it is not creating an obvious alias
        //     case Ident(_) => tree.symbol == target
        //     case Block(_, expr) => maybeAlias(expr)
        //     case Typed(expr, _) => maybeAlias(expr)
        //     case _ => false

        //   if maybeAlias(vd.rhs) then
        //     report.warning("Creating a possible alias to @lastUse annotated variable. this will keep the object alive longer", tree.sourcePos)

        //   foldOver(info, tree)//maybe creating a closure

        //   info


        case _ => foldOver(info, tree)//visit other trees
    }
  }

  override def transformDefDef(method: DefDef)(using Context): Tree =

    if method.hasAttachment(methodLastUses) then
      val lastUses = method.getAttachment(methodLastUses).get

      lastUses.foreach(sym =>
        val verifier = LastUseVerifier(sym, method)
        verifier.verify
      )

    method

object VerifyLastUseAnnotations:
  val name: String = "verifyLastUseAnnotations"
  val description: String = "verify that @lastUse annotations are use correctly"
