package vm

import Action._

object Action {
  type ExternalFunc = (Rule, db.Pattern, Vector[Int]) => Unit
}

abstract class Action {
  def run(rule: Rule, binds: db.Pattern): Unit
}

class TableUpdateAction(val table: Table, mapping: Vector[Int]) extends Action {
  def run(rule: Rule, binds: db.Pattern): Unit = {
    val args = rule.mapArgs(mapping, binds).map(_.get).toArray
    table.insert(args)
  }
}

class TableDeleteAction(val table: Table, mapping: Vector[Int]) extends Action {
  def run(rule: Rule, binds: db.Pattern): Unit = {
    val args = rule.mapArgs(mapping, binds).toArray
    table.remove(args)
  }
}

class IndirectAction(val bindIndex: Int, mapping: Vector[Int]) extends Action {
  def run(rule: Rule, binds: db.Pattern): Unit = {
    val args = rule.mapArgs(mapping, binds).toArray
    val bind = binds(bindIndex) match {
      case Some(bind) => bind
      case None => throw new RuntimeException("Trying to execute action on unbound indirect value!")
    }
    bind match {
      case rule: vm.Rule =>
        rule.query(args).foreach(rule.execute(_))
      case other =>
        throw new RuntimeException("Trying to indirectly execute unspported value!")
    }

  }
}

class ExternalFuncAction(val func: ExternalFunc, mapping: Vector[Int]) extends Action {
  def run(rule: Rule, binds: db.Pattern): Unit = func(rule, binds, mapping)
}

class RuleAction(val otherRule: Rule, mapping: Vector[Int]) extends Action {
  def run(rule: Rule, binds: db.Pattern): Unit = {
    val args = rule.mapArgs(mapping, binds).toArray
    otherRule.execute(args)
  }
}
