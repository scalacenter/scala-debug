package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*

class EvaluationCompiler(using EvaluationContext)(using Context)
    extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: typer :: others = super.frontendPhases
    parser ::
      List(InsertExpression()) ::
      typer ::
      List(PrepareExtractExpression()) ::
      List(ExtractExpression()) ::
      others

  override protected def transformPhases: List[List[Phase]] =
    super.transformPhases :+ List(ResolveReflectEval())
