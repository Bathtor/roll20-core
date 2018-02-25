package com.lkroll.roll20.core

import org.scalatest._

class AccessTransformationTests extends FunSuite with Matchers {

  import CoreImplicitsExplicitLabels._;

  val testField = TestField("test_field");
  val testField2 = TestField("another_test_field");
  val testAbility = AutocalcExprs.Ability[Int]("test-roll");

  test("A field should be correctly transformed") {
    val expr: AutocalcExpression[Int] = testField;
    val selExpr = expr.forSelected();
    selExpr.render should be ("@{selected|test_field}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("@{target|test_field}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("@{target|Nerd|test_field}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("@{Nerd|test_field}");
  }

  test("An ability should be correctly transformed") {
    val expr: AutocalcExpression[Int] = testAbility;
    val selExpr = expr.forSelected();
    selExpr.render should be ("%{selected|test-roll}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("%{target|test-roll}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("%{target|Nerd|test-roll}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("%{Nerd|test-roll}");
  }

  test("A simple roll expression should be correctly transformed") {
    val expr = Dice.d100 + testField;
    val selExpr = expr.forSelected();
    selExpr.render should be ("(1)d(100)+@{selected|test_field}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("(1)d(100)+@{target|test_field}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("(1)d(100)+@{target|Nerd|test_field}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("(1)d(100)+@{Nerd|test_field}");
  }

  test("A non-trivial roll expression should be correctly transformed") {
    val expr = Dice.d100 + InputQuery("test-modifier", None).arith + testField;
    val selExpr = expr.forSelected();
    selExpr.render should be ("(1)d(100)+?{test-modifier}+@{selected|test_field}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("(1)d(100)+?{test-modifier}+@{target|test_field}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("(1)d(100)+?{test-modifier}+@{target|Nerd|test_field}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("(1)d(100)+?{test-modifier}+@{Nerd|test_field}");
  }

  test("Another non-trivial roll expression should be correctly transformed") {
    val expr = Dice.d100 + testField + InputQuery("test-modifier", None).arith;
    val selExpr = expr.forSelected();
    selExpr.render should be ("(1)d(100)+@{selected|test_field}+?{test-modifier}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("(1)d(100)+@{target|test_field}+?{test-modifier}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("(1)d(100)+@{target|Nerd|test_field}+?{test-modifier}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("(1)d(100)+@{Nerd|test_field}+?{test-modifier}");
  }

  test("A non-trivial roll expression with 2 field accesses should be correctly transformed") {
    val expr = Dice.d100 + testField2 + InputQuery("test-modifier", None).arith + testField;
    val selExpr = expr.forSelected();
    selExpr.render should be ("(1)d(100)+@{selected|another_test_field}+?{test-modifier}+@{selected|test_field}");
    val tarExpr = expr.forTarget();
    tarExpr.render should be ("(1)d(100)+@{target|another_test_field}+?{test-modifier}+@{target|test_field}");
    val tar2Expr = expr.forTarget("Nerd");
    tar2Expr.render should be ("(1)d(100)+@{target|Nerd|another_test_field}+?{test-modifier}+@{target|Nerd|test_field}");
    val charExpr = expr.forCharacter("Nerd");
    charExpr.render should be ("(1)d(100)+@{Nerd|another_test_field}+?{test-modifier}+@{Nerd|test_field}");
  }
}

case class TestField(fname: String) extends FieldLike[Int] {
  override def editable(): Boolean = false;
  override def ctx: RenderingContext = APIContext;
  override def attr: String = fname;
  override def initialValue: String = "24";
  override def reader: Readable[Int] = CoreImplicits.readableInt;
}
