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
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.transform.VerifyLastUseAnnotations.{VariableStatus, VariableStatusMap}
import dotty.tools.dotc.transform.PostTyper.lastUseAttachment
import dotty.tools.dotc.transform.VerifyLastUseAnnotations.lastUseVariables
import dotty.tools.dotc.transform.VerifyLastUseAnnotations.capturedVariables

object VerifyLastUseAnnotations:
  val name: String = "verifyLastUseAnnotations"
  val description: String = "verify that @lastUse annotations are use correctly"

  case class VariableStatus(allowVar: Boolean, allowAnnot: Boolean, isAfterValDef: Boolean /*find better name*/){
  
    def afterAnnot = VariableStatus(false, allowAnnot, isAfterValDef)
    def afterValDef = VariableStatus(allowVar, allowAnnot, true)
    def enterLoop = VariableStatus(allowVar, !isAfterValDef, isAfterValDef)//when I enter a loop, I only accept an annotation if did not define the variable yet. this means the variable  cannot be annotated many times
    def merge(other: VariableStatus) = VariableStatus(allowVar && other.allowVar, allowAnnot && other.allowAnnot, isAfterValDef ||  other.isAfterValDef)//aftervd should not matter a valdef in a branch has no impact on the outside
  }

  //TODO find a better name
  type VariableStatusMap = Map[Symbol, VariableStatus]

  extension(ss: VariableStatusMap)
    def merge(other: VariableStatusMap): VariableStatusMap = 
      ss.map((sym, state) => (sym -> state.merge(other(sym))))

    def enterLoop: VariableStatusMap = 
      ss.map((sym, state) => (sym -> state.enterLoop))
    


  object VariableStatus {
    def start: VariableStatus = VariableStatus(allowVar = true, allowAnnot = true, isAfterValDef = false)
  }

  private val lastUseVariables = Property.Key[Set[Symbol]]
  private val capturedVariables = mutable.Map.empty[Symbol, Set[Symbol]] //method  => lastUse symbols from outside it is using


/*
  Verifies the use of the @lastUse annotation
  following rules are applied:
  1. the variable cannot be reused after being annotated
  2. the variable cannot be annotated in a loop (or in a lambda)  TODO modify
  3. exclusive branches (if-else, match cases) do not impact each other.

  the implementation goes as follows (considering nested functions):
    we follow all variables with a @lastUse annotation
    PrepareForDefDef: the miniphase dives in the deepest method in the tree, storing in context the lastUse variable from enclosing methods ("captured")
    transformDefDef: each method (children first) are verified. If it contains a lastUse captured variable, it is marked as such (in "capturedVariables")
    
    this allows illegal function calls to be detected, but all function definitions stay legal.
*/
class VerifyLastUseAnnotations extends MiniPhase:

  override def phaseName: String = VerifyLastUseAnnotations.name
  override def description: String = VerifyLastUseAnnotations.description


  //A TreeAccumulator is more expensive than a miniphase transforms, but I need to do custom information transfer and a miniphase is not flexible enough
  private class LastUseVerifier(method: DefDef) extends TreeAccumulator[VariableStatusMap]{
    override def apply(accInfo: VariableStatusMap, tree: Tree)(using Context): VariableStatusMap = traverse(tree)(accInfo)

    def verify(using Context): Unit = 
      method.getAttachment(methodLastUses) match
        case None if ctx.property(lastUseVariables).get.nonEmpty =>
          // enclosing method has lastUse variables => check if this one uses any lastUse variable
          traverse(method.rhs)(Map.empty)
        case None => () 
        case Some(symbols) =>
          val ss = symbols.map(s => (s -> VariableStatus.start)).toMap
          traverse(method.rhs)(ss)

    private def checkVarValidity(tree: Tree)(using Context): Unit = 
      if tree.symbol.is(Lazy) then
        report.warning("@lastUse annotation on a lazy val does not work (yet)", tree.sourcePos)

      if (tree.tpe.typeSymbol.isPrimitiveValueClass || tree.symbol.is(Flags.Method))
        report.error("`@lastUse` annotation cannot be used on primitives and methods", tree.sourcePos)
      if tree.symbol.owner != method.symbol  && !tree.isInstanceOf[This] then 
        report.error(s"@lastUse annotation can only be used in a local context, ${tree.symbol.owner}, ${method.symbol}", tree.sourcePos)



    private def traverseExclusiveCases(cases: List[Tree])(info: VariableStatusMap)(using Context): VariableStatusMap =
      cases
      .map(c => traverse(c)(info))//traverse all of them with same source info
      .reduce((a,b)=> a.merge(b))//merge all info, return it

    private def traverseCaseDefs(casedefs: List[CaseDef])(accInfo: VariableStatusMap)(using Context): VariableStatusMap =
      var mergedBodies = accInfo //contains the output after each body, 'ored' together
      var info = accInfo

      casedefs.foreach(cd =>
        info = traverse(cd.pat)(info)
        info = traverse(cd.guard)(info)
        val afterBody = traverse(cd.body)(info)

        mergedBodies = mergedBodies.merge(afterBody)
      )

      mergedBodies 

    private def traverse(tree: Tree)(accInfo: VariableStatusMap)(using Context): VariableStatusMap = {
      var info = accInfo//used to mutate and pass the information
      val sym = tree.symbol

      //by default, accInfo is propagated in the order of the arguments. so block is already good, and everything that has linear reading
      tree match
        case vd: ValDef if info.contains(sym) => // definition of a lastUse variable
          val info1 = info(sym)
          info = info.updated(sym, info1.afterValDef)
          foldOver(info, vd)

        case _: Ident | This(_) if info.contains(sym) => //lastUse variable in current method
          val info1 = info(sym)
          if !info1.allowVar then
            report.error(s"reuse of variable ${tree.symbol.name} after @lastUse", tree.sourcePos) //reuse after annot
          if tree.hasAttachment(PostTyper.lastUseAttachment) then
            checkVarValidity(tree)
            if info1.allowAnnot then info.updated(sym, info1.afterAnnot) else //annot here
              report.error(s"cannot use @lastUse annotation in a loop", tree.sourcePos) //annot here but illegal
              tree.removeAttachment(PostTyper.lastUseAttachment)
              info
          else
            info

        case _: Ident if ctx.property(lastUseVariables).get.contains(sym) =>//lastUse variable from an enclosing method
          val others = capturedVariables.getOrElse(method.symbol, Set.empty[Symbol])
          capturedVariables.update(method.symbol, others + sym)
          info

        case _: Ident if capturedVariables.contains(sym) =>// calling a method which uses a lastUse variable
          val freeVars = capturedVariables.apply(sym)
          freeVars.foreach(fv => 
            info.get(fv) match
              case Some(value) => if !value.allowVar then report.error(s"calling function ${sym.name}, using variable ${fv.name} previously annotated with @lastUse", tree.sourcePos)
              case None => //lastUse variable is defined in an enclosing method. mark the current method as using it
                val others = capturedVariables.getOrElse(method.symbol, Set.empty[Symbol])
                capturedVariables.update(method.symbol, others + fv)
          )
          info

        case If(cond, thenp, elsep) =>
          info = traverse(cond)(info)
          val info_branch1 = traverse(thenp)(info)//both branches are exclusive
          val info_branch2 = traverse(elsep)(info)//so we combine the info at the end
          info_branch1.merge(info_branch2)

        case WhileDo(cond, body) =>
          info = info.enterLoop
          info = traverse(cond)(info)
          // cannot be annotated or valdef'd inside the loop => no information to propagate further.
          traverse(body)(info)

        case Match(selector, cases) =>
          info = traverse(selector)(info)
          traverseCaseDefs(cases)(info)

        case Try(expr, cases, finalizer) =>
          info = traverse(expr)(info)
          info = traverseCaseDefs(cases)(info)
          traverse(finalizer)(info)

        case DefDef(name, paramss, tpt, preRhs) => info//already checked, do not enter it

        case _ => foldOver(info, tree)//visit other trees
    }
  }


  override def prepareForDefDef(tree: DefDef)(using Context): Context = 
    val freeVars = ctx.property(lastUseVariables).getOrElse(Set.empty[Symbol]) ++ tree.getAttachment(methodLastUses).getOrElse(Set.empty[Symbol])
    ctx.withProperty(lastUseVariables, Some(freeVars))


  override def transformDefDef(mdef: DefDef)(using Context): Tree =
    LastUseVerifier(mdef).verify
    mdef


