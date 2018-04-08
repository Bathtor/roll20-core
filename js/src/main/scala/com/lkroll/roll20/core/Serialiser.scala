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

import scalajs.js;

trait Serialiser[S, T] {
  def serialise(o: T): S
}

trait JSSerialiser[T] extends Serialiser[js.Any, T];
trait StringSerialiser[T] extends Serialiser[String, T];

object JSDefaultSerialiser extends JSSerialiser[Any] {
  override def serialise(o: Any): js.Any = o match {
    case b: Boolean => if (b) "on" else 0 // because Roll20 -.-
    case _          => o.asInstanceOf[js.Any]
  }
}

object StringDefaultSerialiser extends StringSerialiser[Any] {
  override def serialise(o: Any): String = o.toString();
}

class IdSerialiser[T] extends Serialiser[T, T] {
  override def serialise(o: T): T = o;
}

object StringIdSerialiser extends IdSerialiser[String] with StringSerialiser[String];
object JsIdSerialiser extends IdSerialiser[js.Any] with JSSerialiser[js.Any];

trait PrimitiveStringSerialisers {
  def default[T]: StringSerialiser[T] = new PrimitiveStringSerialisers.DefaultWrapper[T];
  def ser[T](f: T => String): StringSerialiser[T] = new PrimitiveStringSerialisers.FunctionWrapper(f);

  implicit val nullSer: StringSerialiser[Void] = ser(s => "");
  implicit val intSer: StringSerialiser[Int] = default;
  implicit val longSer: StringSerialiser[Long] = default;
  implicit val floatSer: StringSerialiser[Float] = default;
  implicit val doubleSer: StringSerialiser[Double] = default;
  implicit val booleanSer: StringSerialiser[Boolean] = ser(b => if (b) "on" else "0"); // because Roll20 -.-
  implicit val stringSer: StringSerialiser[String] = StringIdSerialiser;
}

object PrimitiveStringSerialisers extends PrimitiveStringSerialisers {

  class DefaultWrapper[T] extends StringSerialiser[T] {
    override def serialise(o: T): String = StringDefaultSerialiser.serialise(o);
  }

  class FunctionWrapper[T](f: T => String) extends StringSerialiser[T] {
    override def serialise(o: T): String = f(o);
  }

}

