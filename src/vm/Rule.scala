package vm

import db.Pattern
import db.Row
import vm.Rule._

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayStack

/** Any kind of condition that can be a part of a rule */
trait Condition {
  def query(context: Context, binds: db.Pattern): Iterator[db.Pattern]
}

/**
  * A condition that maps some binds to a queryable table.
  * @param table Name of the queryable to match
  * @param mapping The query is formed by indirectly accessing bound values using these indices
  */
class QueryCondition(val table: String, val mapping: Vector[Int]) extends Condition {
  def query(context: Context, binds: db.Pattern): Iterator[db.Pattern] = {
    val it = context.query(table, mapping.map(binds(_)).toArray)
    new RemappingRowIterator(it, binds, mapping)
  }
}

/**
  * Negation of any condition. If the condition has even one match this fails, otherwise
  * just pass the currently bound values forwards.
  * @param condition Inner condition to negate
  */
class NegationCondition(val condition: Condition) extends Condition {
  def query(context: Context, binds: db.Pattern): Iterator[db.Pattern] = {
    if (condition.query(context, binds).hasNext) {
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
class RuleCondition(val rule: Rule, val mapping: Vector[Int]) extends Condition {
  def query(context: Context, binds: db.Pattern): Iterator[db.Pattern] = {
    val it = rule.query(context, Some(mapping.map(binds(_)).toArray))
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
  class ConditionIterator(val rule: Rule, val context: Context, val initialBinds: Option[Pattern]) extends util.SimpleIterator[db.Pattern] {
    // Would use ArrayStack but can't get over the fact that I know the exact amount of
    // elements we're gonna need so the dynamic scaling feels wasteful!
    val iterators = new Array[Iterator[db.Pattern]](rule.conditions.length)
    var top: Int = 1

    // Initialization: Query the first condition as this is done only once
    val binds = initialBinds.getOrElse(Array.fill[Option[Any]](rule.bindNames.length)(None))
    iterators(0) = rule.conditions(0).query(context, binds)

    def nextOption(): Option[db.Pattern] = {
      while (top < iterators.length || !iterators(top - 1).hasNext) {
        // Pop exhausted stack frames, if we pop the root frame then we're done
        while (top > 0 && !iterators(top - 1).hasNext)
          top -= 1
        if (top == 0) return None

        // Execute next condition
        val next = iterators(top - 1).next()
        iterators(top) = rule.conditions(top).query(context, next)
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
  * @param conditions List of conditions required, kinda like instructions
  */
class Rule(val bindNames: Vector[String], val conditions: Vector[Condition]) {

  def query(context: Context, initialBinds: Option[Pattern] = None): Iterator[db.Pattern] = {
    conditions.length match {
      case 0 => new util.NoneIterator[db.Pattern]
      case _ => new ConditionIterator(this, context, initialBinds)
    }
  }

}