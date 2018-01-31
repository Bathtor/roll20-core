package com.lkroll.roll20.core

import org.scalatest._

class LabelledRenderingTest extends FunSuite with Matchers {
  val testField = TestField("test_field");

  test("CoreImplicitsLabelFields should render fields labelled") {
    import CoreImplicitsLabelFields._;
    val expr: AutocalcExpression[Int] = testField;
    expr.render should be ("@{test_field}[test field]");
  }

  test("CoreImplicitsExplicitLabels should render fields unlabelled") {
    import CoreImplicitsExplicitLabels._;
    val expr: AutocalcExpression[Int] = testField;
    expr.render should be ("@{test_field}");
  }
}
