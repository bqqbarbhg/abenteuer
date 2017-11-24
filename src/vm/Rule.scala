package vm

import db.Pattern
import db.Row
import vm.Rule._

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayStack

/** Any kind of condition that can be a part of a rule */
trait Condition {
  def query(context: Context, rule: Rule, binds: db.Pattern): Iterator[db.Pattern]
}

/**
  * A condition that maps some binds to a queryable table.
  * @param table Name of the queryable to match
  * @param mapping The query is formed by indirectly accessing bound values using these indices
  */
class QueryCondition(val table: String, val mapping: Vector[Int]) extends Condition {
  def query(context: Context, rule: Rule, binds: db.Pattern): Iterator[db.Pattern] = {
    val it = context.query(table, rule.mapArgs(mapping, binds))
    new RemappingRowIterator(it, binds, mapping)
  }
}

/**
  * Negation of any condition. If the condition has even one match this fails, otherwise
  * just pass the currently bound values forwards.
  * @param condition Inner condition to negate
  */
class NegationCondition(val condition: Condition) extends Condition {
  def query(context: Context, rule: Rule, binds: db.Pattern): Iterator[db.Pattern] = {
    if (condition.query(context, rule, binds).hasNext) {
      return new util.NoneIterator[db.Pattern]()
    } else {
      return new util.SingleValueIterator(binds)
    }
  }
}

/**
  * Another rule can be used as a condition, which kinda resembles a function call.
  * @param rule Inner rule to use as condition
  * @param mapping Translation between the rules' bindings (indices to the top level rule binds)
  */
class RuleCondition(val otherRule: Rule, val mapping: Vector[Int]) extends Condition {
  def query(context: Context, rule: Rule, binds: db.Pattern): Iterator[db.Pattern] = {
    val it = otherRule.query(rule.mapArgs(mapping, binds))
    new RemappingPatternIterator(it, binds, mapping)
  }
}

object Rule {

  /** Used for mapping values returned from a query that returns data rows to the bound values of a rule.
    */
  class RemappingRowIterator(val iterator: Iterator[Row], val binds: Pattern, val mapping: Vector[Int]) extends Iterator[Pattern] {
    def hasNext: Boolean = iterator.hasNext
    def next(): Pattern = {
      val result = iterator.next()
      val copy = binds.clone()
      for ((ix, i) <- mapping.zipWithIndex) {
        if (ix >= 0)
          copy(ix) = Some(result(i))
      }
      copy
    }
  }

  /** Used for mapping values returned from a query that returns patterns, such as other rules,
    * to the bound values of a rule.
    */
  class RemappingPatternIterator(val iterator: Iterator[Pattern], val binds: Pattern, val mapping: Vector[Int]) extends Iterator[Pattern] {
    def hasNext: Boolean = iterator.hasNext
    def next(): Pattern = {
      val result = iterator.next()
      val copy = binds.clone()
      for ((ix, i) <- mapping.zipWithIndex) {
        if (ix >= 0)
          copy(ix) = result(i)
      }
      copy
    }
  }

  /** This is the core algorithm of matching rules. It goes through the conditions in
    * order, each potentially adding more bound values. Yields bound values when all
    * the conditions have been satisfied. Note that each rule can generate multiple
    * potential solutions so we need to recursively iterate through all the potential
    * bindings before accepting or rejecting them.
    *
    * This could be done with coroutines, but it's not supported natively by Scala it's
    * an iterator now.
    *
    * Note: Expects to be only instantiated for rules with at least one condition!
    */
  class ConditionIterator(val rule: Rule, val context: Context, val initialBinds: Pattern) extends util.SimpleIterator[db.Pattern] {
    // Would use ArrayStack but can't get over the fact that I know the exact amount of
    // elements we're gonna need so the dynamic scaling feels wasteful!
    val iterators = new Array[Iterator[db.Pattern]](rule.conditions.length)
    var top: Int = 1

    // Initialization: Query the first condition as this is done only once
    iterators(0) = rule.conditions(0).query(context, rule, initialBinds)

    def nextOption(): Option[db.Pattern] = {
      while (top < iterators.length || !iterators(top - 1).hasNext) {
        // Pop exhausted stack frames, if we pop the root frame then we're done
        while (top > 0 && !iterators(top - 1).hasNext)
          top -= 1
        if (top == 0) return None

        // Execute next condition
        val next = iterators(top - 1).next()
        iterators(top) = rule.conditions(top).query(context, rule, next)
        top += 1
      }

      // All conditions satisfied, yield value
      Some(iterators(top - 1).next())
    }

  }
}

/**
  * A rule is a combination of conditions, usually some kind of queries. Rules have
  * bound values that are shared between conditions and all the conditions have to
  * be satisfied for the _same_ bound values.
  * @param bindNames Names of the bindings, kinda like local variable names (mostly for debugging)
  * @param argNames Names of the arguments to be provided for the query
  * @param conditions List of conditions required, kinda like instructions
  * @param actions List of actiosn that can be applied to the results of the query
  * @param constants List of constants used by the rule, indexed by negative mappings
  */
class Rule(val context: Context, val bindNames: Vector[String], val argNames: Vector[String], val conditions: Vector[Condition], val actions: Vector[Action], val constants: Vector[Any]) {

  override def toString: String = s"Rule(${argNames.mkString(" ")})"

  def mapArgs(mapping: Vector[Int], binds: db.Pattern): db.Pattern = mapping.map(ix => {
    if (ix >= 0) binds(ix) else Some(this.constants(-1 - ix))
  }).toArray

  def query(initialBinds: Pattern = Pattern()): Iterator[db.Pattern] = {
    conditions.length match {
      case 0 => new util.NoneIterator[db.Pattern]
      case _ =>
        val binds = Array.fill[Option[Any]](bindNames.length)(None)
        for ((bind, ix) <- initialBinds.zipWithIndex)
          binds(ix) = bind
        new ConditionIterator(this, context, binds)
    }
  }

  def execute(binds: db.Pattern): Unit = {
    for (action <- actions) {
      action.run(this, binds)
    }
  }

}