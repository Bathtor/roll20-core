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

trait Renderable {
  def render: String;
}

trait RenderingContext {
  def qualifier: Option[String] = None;
  def mapAccess(rowId: String, s: String): String;
  def mapAccess(s: String): String;
  def mapSelect(s: String): String;
  def mapMatcher(s: String): String => Boolean;
  def mapMatcher(rowId: String, s: String): String => Boolean;
}

case object UIContext extends RenderingContext {
  def mapAccess(rowId: String, s: String): String = s;
  def mapAccess(s: String): String = s;
  def mapSelect(s: String): String = s;
  def mapMatcher(s: String): String => Boolean = _.equals(s);
  def mapMatcher(rowId: String, s: String): String => Boolean = _.equals(s);
}

case object APIContext extends RenderingContext {
  def mapAccess(rowId: String, s: String): String = s;
  def mapAccess(s: String): String = s;
  def mapSelect(s: String): String = s;
  def mapMatcher(s: String): String => Boolean = _.equals(s);
  def mapMatcher(rowId: String, s: String): String => Boolean = _.equals(s);
}
