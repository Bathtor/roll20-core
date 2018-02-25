/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Lars Kroll <bathtor@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.lkroll.roll20.core

sealed trait ArithmeticExpression[T] extends Renderable {
  import Arith._;
  import AccessTransformation._;

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = PlusExpr[T](this, other);
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = MinusExpr[T](this, other);
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = DivExpr[T](this, other);
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = MultExpr[T](this, other);
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = ModExpr[T](this, other);

  def paren(implicit n: Numeric[T]) = Parenthesised(this);
  def as[N: Numeric](): ArithmeticExpression[N] = CastExpr[T, N](this);

  def transformForAccess(f: AccessTransformer): ArithmeticExpression[T];
  def forCharacter(characterName: String): ArithmeticExpression[T] = transformForAccess(Character(characterName));
  def forSelected(): ArithmeticExpression[T] = transformForAccess(Selected);
  def forTarget(): ArithmeticExpression[T] = transformForAccess(Targeted);
  def forTarget(targetName: String): ArithmeticExpression[T] = transformForAccess(Target(targetName));

  def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T];
}

object Arith {
  import AccessTransformation.AccessTransformer;

  case class Literal[T: Numeric](t: T) extends ArithmeticExpression[T] {
    override def render: String = t.toString();
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] = this;
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] = this;
  }

  case class Parenthesised[T: Numeric](expr: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"(${expr.render})";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] = Parenthesised(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] = Parenthesised(expr.replaceQuery(replacement));
  }

  case class PlusExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}+${right.render}";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      PlusExpr(left.transformForAccess(f), right.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      PlusExpr(left.replaceQuery(replacement), right.replaceQuery(replacement));
  }

  case class MinusExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}-${right.render}";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      MinusExpr(left.transformForAccess(f), right.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      MinusExpr(left.replaceQuery(replacement), right.replaceQuery(replacement));
  }

  case class DivExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}/${right.render}";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      DivExpr(left.transformForAccess(f), right.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      DivExpr(left.replaceQuery(replacement), right.replaceQuery(replacement));
  }

  case class MultExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}*${right.render}";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      MultExpr(left.transformForAccess(f), right.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      MultExpr(left.replaceQuery(replacement), right.replaceQuery(replacement));
  }

  case class ModExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}%${right.render}";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      ModExpr(left.transformForAccess(f), right.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      ModExpr(left.replaceQuery(replacement), right.replaceQuery(replacement));
  }

  case class RoundingExpr[T: Numeric](function: RoundingFunction, expr: ArithmeticExpression[T]) extends ArithmeticExpression[Int] {
    def render: String = s"${function.name}(${expr.render})";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[Int] =
      RoundingExpr(function, expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[Int] =
      RoundingExpr(function, expr.replaceQuery(replacement));
  }

  def ceil[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Ceil, expr);
  def floor[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Floor, expr);
  def round[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Round, expr);

  case class AbsExpr[T: Numeric](expr: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"abs(${expr.render})";
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      AbsExpr(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      AbsExpr(expr.replaceQuery(replacement));
  }

  def abs[T: Numeric](expr: ArithmeticExpression[T]) = AbsExpr(expr);

  case class CastExpr[I, O](expr: ArithmeticExpression[I]) extends ArithmeticExpression[O] {
    def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[O] =
      CastExpr[I, O](expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[O] =
      CastExpr[I, O](expr.replaceQuery(replacement));
  }

  case class AutoArith[T](expr: AutocalcExpression[T]) extends ArithmeticExpression[T] {
    override def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      AutoArith(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      AutoArith(expr.replaceQuery(replacement));
  }

  case class RollArith[T: Numeric](expr: RollExpression[T]) extends ArithmeticExpression[T] {
    override def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): ArithmeticExpression[T] =
      RollArith(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): ArithmeticExpression[T] =
      RollArith(expr.replaceQuery(replacement));
  }
}
