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

trait RollQuery[T] extends Renderable {
  def name: String;
  def expr(implicit ev: T =:= Int) = RollExprs.WithIntQuery(this.asInstanceOf[RollQuery[Int]]);
  def arith(implicit ev: T =:= Int) = Arith.RollArith(expr(ev));
  def param(implicit ev: T =:= Int) = DiceParams.QueryParameter(this.asInstanceOf[RollQuery[Int]]);
}

case class InputQuery[T](name: String, defaultValue: Option[T]) extends RollQuery[T] {
  override def render: String = defaultValue match {
    case Some(default) => s"?{$name|$default}";
    case None          => s"?{$name}";
  };
}

case class SelectQuery[T](name: String, options: Seq[T]) extends RollQuery[T] {
  override def render: String = s"?{Select $name|${options.mkString("|")}}";
}

case class LabelledSelectQuery[T](name: String, options: Seq[(String, T)]) extends RollQuery[T] {
  lazy val stringOptions = options
    .map {
      case (l, t) => s"$l,$t"
    }
    .mkString("|");
  override def render: String = s"?{Select $name|$stringOptions}";
}

case class QueryReplacer[+T](name: String, value: T)
